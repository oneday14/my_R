# 1. 교습현황.csv 파일을 읽고
df1 <- read.csv('교습현황.csv', stringsAsFactors=F, skip=1)
df1 <- df1[,-c(3,4)]
str(df1)

library(plyr) ; library(reshape2) ; library(stringr)

# 1) 구별 교습과정의 총 금액을 막대그래프로 시각화
# step1) 구 추출
str_extract_all(df1$교습소주소[1], '..구')
str_extract_all(df1$교습소주소[1], '[가-힣]{1,}구')
str_extract_all(df1$교습소주소[1], '[:alpha:]{1,}구')[[1]]

f1 <- function(x) {
  str_extract_all(x, '..구')[[1]][1]
}

df1$구 <- sapply(df1$교습소주, f1)
unique(df1$구)

# step2) 교습금액 컬럼 추출
df2 <- df1[, str_detect(colnames(df1), '^X')]

# step3) 학원별 총 교습금액 계산(행별 합)
f_num <- function(x) {
  as.numeric(str_remove_all(x, ','))
}

df2[,] <- apply(df2, c(1,2), f_num)

df1$total <- apply(df2, 1, sum)

# step4) 구별, 교습과정별 교습금액 합 계산
str_remove_all('실용외국어(유아/초·중·고)','\\(.{1,}\\)')
str_remove_all('컴퓨터(소)','\\(.{1,}\\)')

unique(str_remove_all(df1$교습과정,'\\(.{1,}\\)'))
df1$교습과정 <- str_remove_all(df1$교습과정,'\\(.{1,}\\)')

df1_total <- dcast(df1, 교습과정 ~ 구, sum, value.var='total')

# step5) 시각화
dev.new()
barplot(as.matrix(df1_total[,-1])/1000000, beside = T, 
        ylim = c(0,60000), col=1:nrow(df1_total))

legend(1,60000,df1_total$교습과정, fill = 1:nrow(df1_total))

# 2) 년도별 보습 교과과정의 지출금액이 가장 큰 동이름, 지출금액 출력
# step1) 동이름 추출
# --
str_extract_all(df1$교습소주소[1], '[가-힣]{1,}동')[[1]][1]

f2 <- function(x) {
  str_extract_all(x, '[가-힣]{1,}동')[[1]][1]
}

unique(sapply(df1$교습소주소, f2))
df1$교습소주[sapply(df1$교습소주소, f2) == '관악푸르지오상가동']

# --
str_extract_all(df1$교습소주소[1], '\\([가-힣]{1,}동')[[1]][1]

f2 <- function(x) {
  str_extract_all(x, '\\([가-힣]{1,}동')[[1]][1]
}

unique(sapply(df1$교습소주소, f2))
df1$교습소주소[is.na(sapply(df1$교습소주소, f2))]

# --
str_extract_all(df1$교습소주소[1], '\\([가-힣0-9]{1,}동')[[1]][1]

f2 <- function(x) {
  str_extract_all(x, '\\([가-힣0-9]{1,}동')[[1]][1]
}

unique(sapply(df1$교습소주소, f2))
df1$동 <- str_remove_all(sapply(df1$교습소주소, f2), '[(0-9]')

# step2) 필요데이터 추출
df2$동 <- df1$동
df2$교습과정 <- df1$교습과정

# step3) 년도 컬럼 stack
df3 <- melt(df2, id.vars = c('교습과정','동'), 
            variable.name='년도', value.name='금액')
df3$년도 <- as.numeric(str_sub(df3$년도,2,5))

# step4) 년도별 동별 교습과정별 금액의 총합
df4 <- ddply(df3, .(년도,동,교습과정), summarise, vsum=sum(금액))

# step5) 년도별 교습과정별 금액의 최대를 갖는 행 선택 
ddply(df4, .(년도,교습과정), subset, vsum==max(vsum))

# 3) 각 보습과정별 매출이 가장 높은 교습소명을 출력한뒤
#    각 교습소명(교습과정) 과 매출액을 비교할 수 있는 막대그래프 출력
# step1) 보습과정별 교습소명별 매출액
df2$교습소명 <- df1$교습소명
df2$total <- df1$total

df_total <- ddply(df2, .(교습과정, 교습소명), summarise, vsum=sum(total))

# step2) 보습과정별 최대금액을 갖는 행 선택
df_total2 <- ddply(df_total, .(교습과정), subset, vsum==max(vsum))

# step3) 시각화
dev.new()
par(oma=c(5,0,0,0))  # 하,좌,상,우
vname <- str_c(df_total2$교습소명,'\n',df_total2$교습과정)
barplot(df_total2$vsum/100000, col = rainbow(nrow(df_total2)),
        ylim = c(0,30000), names.arg = vname, las=2)

# 2. total.csv 파일을 읽고
data1 <- read.csv('total.csv', stringsAsFactors = F)

# 1) 년도별 각 품목에 대한 매출을 막대그래프로 시각화
# step1) 년도와 지점을 결합한 형태로 컬럼이름 변경(stack을 위한 처리)
colnames(data1) <- str_c(str_sub(colnames(data1),2,5), '_', data1[1,])
data1 <- data1[-1,]

# step2) stack
colnames(data1)[1] <- 'name'
data2 <- melt(data1, id.vars = 'name',
              variable.name = '년도', value.name='금액')

# step3) 년도와 지검 컬럼 분리
data2$지점 <- str_sub(data2$년도,6,6)
data2$년도 <- str_sub(data2$년도,1,4)

# step4) 금액컬럼 숫자 변경
data2$금액 <- sapply(data2$금액, f_num)

# step5) 년도별 제품별 매출에 대한 교차테이블 생성
data_total <- dcast(data2, name ~ 년도, sum, value.var='금액')

# step6) 시각화
dev.new()
barplot(as.matrix(data_total[,-1])/1000, beside = T, col = 2:4,
        ylim = c(0,100), legend = data_total$name,
        args.legend = list(cex=0.7))

# 2) 지점별로 가장 매출이 높은 품목과 총 매출액을 함께 출력
# step1) 지점별 품목별 매출액 총 합
data3 <- ddply(data2, .(지점, name), summarise, vsum=sum(금액))

# step2) 지점별 최대값 갖는 행 선택
ddply(data3, .(지점), subset, vsum==max(vsum))

########## 여기까지는 복습입니다. ##########

# [ 연습 문제 ]
# taxi_call.csv 파일을 읽고
# 각 요일별로 시간대별 택시 이용률을 파이차트로 출력(7개파이)
# 1) 날짜 파싱
taxi <- read.csv('taxi_call.csv', stringsAsFactors = F)
taxi$기준년월일 <- as.Date(as.character(taxi$기준년월일), '%Y%m%d')

# 2) 요일 추출
taxi$요일 <- as.character(taxi$기준년월일, '%A')

# 3) 시간대별 요일별 교차테이블 생성
taxi_total <- dcast(taxi, 시간대 ~ 요일, sum, value.var='통화건수')

# 4) 각 요일별 시간대의 통화건수 비율
f_rate <- function(x) {
  round(x / sum(x) * 100, 1)
}

taxi_total[,-1] <- apply(taxi_total[,-1],2,f_rate)

# 5) 각 파이의 label 값 가공
# 0시(8.9%)
str_c(taxi_total$시간대,'시(',taxi_total$월요일,'%)')

dev.new()
par(mfrow=c(2,4))
library(plotrix)
pie3D(taxi_total$월요일, 
      labels=str_c(taxi_total$시간대,'시(',taxi_total$월요일,'%)'),
      labelcex=0.5, main='월요일')

pie3D(taxi_total$화요일, 
      labels=str_c(taxi_total$시간대,'시(',taxi_total$화요일,'%)'),
      labelcex=0.5, main='화요일')

pie3D(taxi_total$수요일, 
      labels=str_c(taxi_total$시간대,'시(',taxi_total$수요일,'%)'),
      labelcex=0.5, main='수요일')

pie3D(taxi_total$목요일, 
      labels=str_c(taxi_total$시간대,'시(',taxi_total$목요일,'%)'),
      labelcex=0.5, main='목요일')

pie3D(taxi_total$금요일, 
      labels=str_c(taxi_total$시간대,'시(',taxi_total$금요일,'%)'),
      labelcex=0.5, main='금요일')


# 데이터 분석
# - 데이터 마이닝
# - 미래 예측
# 1. 머신러닝(정형데이터)
#   - 트리기반 모델
#   - 확률/통계 모델
#     ...
#   - 신경망 모델(딥러닝)
# 2. 딥러닝(비정형데이터)


