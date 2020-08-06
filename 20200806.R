# 1. kimchi_test.csv 파일을 읽고
kimchi <- read.csv('kimchi_test.csv', stringsAsFactors = F)
head(kimchi)
library(plyr)

# 1) 각 (년도별 제품별) 판매량과 판매금액의 평균을 구하고
#    년도별 각 제품의 판매량의 증가추이를 plot 그래프로 표현
kimchi2 <- ddply(kimchi, .(판매년도, 제품), summarise, 
                 v1=mean(수량), v2=mean(판매금액))

kimchi3 <- dcast(kimchi, 판매년도 ~ 제품, sum, value.var='수량')

min(kimchi3[,-1]) 
max(kimchi3[,-1]) 

plot(kimchi3$무김치/10000, type='o', col=2, ylim=c(40, 80),
     ann=F, axes=F)
lines(kimchi3$열무김치/10000, type='o', col=3)
lines(kimchi3$총각김치/10000, type='o', col=4)

axis(1, at=1:nrow(kimchi3), kimchi3$판매년도)
axis(2, ylim=c(40, 80))

title(main='김치별 판매량 증감추이', 
      xlab='판매년도',
      ylab='판매량(만)')

legend(1,80, colnames(kimchi3)[-1], lty = 1, col = 2:4)

# 2) 각 년도별로 제품의 판매량을 비교할수 있도록 막대그래프로 표현
rownames(kimchi3) <- kimchi3$판매년도
kimchi3$판매년도 <- NULL

dev.new()
barplot(as.matrix(t(kimchi3))/10000, beside = T, col = rainbow(3),
        ylim=c(0, 80), legend = colnames(kimchi3))

# 2. 병원현황.csv 파일을 읽고
df_data <- read.csv('병원현황.csv', stringsAsFactors=F, skip=1)
head(df_data)

df_data <- df_data[, -c(3,4)]

# 표시과목의 '계' 데이터의 분리
df_data_tt <- df_data[df_data$표시과목 == '계', -2]
df_data <- df_data[df_data$표시과목 != '계', ]

head(df_data)

# 1) 병원수가 가장 많은 5개 과목에 대해 년도별 병원수 증감추이를 시각화
# step1) 년도/분기 컬럼을 stack 처리
df_data2 <- melt(df_data, id.vars = c('시군구명칭','표시과목'), 
                 variable.name='날짜', value.name='병원수')

# step2) 년도와 분기 각각 분리 후 컬럼 생성
library(stringr)
df_data2$년도 <- as.numeric(str_sub(df_data2$날짜, 2, 5))
df_data2$분기 <- as.numeric(str_sub(df_data2$날짜, 8, 8))

# step3) 표시과목별 병원수 총합
df_data3 <- ddply(df_data2, .(표시과목), summarise, cnt=sum(병원수))
str(df_data2)

vrn <- order(df_data3$cnt, decreasing = T)[1:5]
vname <- df_data3$표시과목[vrn]

# step4) 선택된 5개 과목에 대한 데이터 추출
df_data4 <- df_data2[df_data2$표시과목 %in% vname, ]
df_data5 <- dcast(df_data4, 년도 ~ 표시과목, sum, value.var='병원수')

# step5) 시각화
dev.new()

plot(df_data5$내과, type='o', col=2, ylim=c(1000, 8000),
     ann=F, axes=F)

lines(df_data5$소아청소년과 , type='o', col=3)
lines(df_data5$이비인후과, type='o', col=4)
lines(df_data5$일반의, type='o', col=5)
lines(df_data5$전문과목미표시전문의, type='o', col=6)

axis(1, at=1:5, df_data5$년도)
axis(2, ylim=c(1000, 8000))

legend(1,8000, colnames(df_data5)[-1], col=2:6, lty=1)

# 2) 병원수가 가장 많은 5개 과목에 대해 분기별 각 과목의 병원수를
#    비교하는 막대그래프 생성

# step1) 선택된 5개 과목에 대한 데이터 추출
df_data4 <- df_data2[df_data2$표시과목 %in% vname, ]

# step2) 선택된 5개 과목에 대한 분기별 데이터 정리
total <- dcast(df_data4, 표시과목 ~ 분기, sum, value.var='병원수')
rownames(total) <- total$표시과목
total$표시과목 <- NULL

# step3) 시각화
barplot(as.matrix(total), beside = T, col = rainbow(5),
        ylim = c(0,10000), legend=rownames(total))

# 3. 영화이용현황.csv 파일을 읽고,
# 지역.시도별 성별 이용비율의 평균을 비교하기 위한 막대그래프 출력
df1 <- read.csv('영화이용현황.csv', stringsAsFactors=F)
head(df1)

library(reshape2)

total <- dcast(df1, 성별 ~ 지역.시도, sum)
rownames(total) <- total$성별
total$성별 <- NULL
total

dev.new()
barplot(as.matrix(total), beside = T, col = rainbow(2),
        legend = rownames(total))

########## 여기까지는 복습입니다. ##########

# 3. hist : 히스토그램
hist(x,                  # 벡터
     breaks = ,          # 막대별 범위
     include.lowest = T, # 최소값 포함 여부
     right = T)          # 오른쪽 포함 여부

# [ 참고 : 데이터의 범위 정의 시 닫혀있다의 의미 ]
# a <= x <  b   => [a,b) => 왼쪽이 닫혀있다
# a <  x <= b   => (a,b] => 오른쪽이 닫혀있다

# 예제) student.csv 파일을 읽고 키에 대한 히스토그램 출력
std <- read.csv('student.csv', stringsAsFactors = F)
dev.new()
par(mfrow=c(1,2))
hist(std$HEIGHT)
hist(std$HEIGHT, breaks = c(160,170,180,190))
hist(std$HEIGHT, angle = c(0,90,45,10,60), density = 30, col = 2:6,
     border = 1)

# 예제) 다음의 벡터에 대한 히스토그램을 출력
v1 <- c(160, 161, 163, 165, 166, 168, 171, 174)
hist(v1)  # right=T, include.lowest = T

# 160 초과 165 이하 : 3의 도수에 최소값인 160이 포함되어 도수는 4가 됌
# 165 초과 170 이하 : 2  

dev.new()
hist(v1, breaks = c(160,165,170,175), 
     include.lowest = F)               # right=T, include.lowest = F
                                       # 160을 포함시킬 구간이 없음
                                       # 에러 발생


# 연습 문제) 영화이용현황.csv 파일을 읽고,
movie <- read.csv('영화이용현황.csv', stringsAsFactors=F)
head(movie)

# 1) 20세미만, 20대, 30대, 40대, 50대, 60세이상 별 이용비율의 평균
unique(str_c(str_sub(movie$연령대,1,1),'0대'))
v1 <- str_c(str_sub(movie$연령대,1,1),'0대')
v1 <- str_replace_all(v1,'10대','20세미만')
v1 <- str_replace_all(v1,'60대','60세이상')
v1 <- str_replace_all(v1,'70대','60세이상')

movie2 <- ddply(movie, .(v1), 
                summarise, vsum=sum(이용_비율...))[c(2,1,3:6),]

barplot(movie2$vsum, col = 2:7, 
        names.arg = movie2$v1,
        angle = c(0,90,45,10,80), density = 30)

# 2) 각 지역.시도별 이용률이 가장 높은 연령대 출력
movie3 <- ddply(movie, .(지역.시도, v1), 
                summarise, vsum=sum(이용_비율...))

ddply(movie3, .(지역.시도), subset, vsum==max(vsum))

# 3) 동별 이용비율의 총 합을 구한뒤 히스토그램으로 출력
movie4 <- ddply(movie, .(지역.읍면동), summarise, vsum=sum(이용_비율...))
hist(movie4$vsum)

movie4[movie4$vsum > 3.5, ]

# 4. pie : 파이 차트
pie(x,              # 벡터
    labels = ,      # 각 파이 이름
    radius = ,      # 파이 크기
    clockwise = F,  # 파이 진행방향(시계방향 여부)
    init.angle = ,  # 파이 시작점
    col = ,         # 각 파이 색
    ...)            # 기타 옵션

vec1 <- c(10,11,14,15,2)
dev.new()

vrate <- vec1 / sum(vec1) * 100
vlabel <- str_c(c('월','화','수','목','금'), '\n', 
                round(vrate,1), '%')

par(mfrow=c(1,2))
pie(vec1, labels = c('월','화','수','목','금'))
pie(vec1, labels = vlabel, init.angle = 90)

# plotrix::pie3D
install.packages('plotrix')
library(plotrix)

pie3D(x,            # 벡터
      radius = ,    # 파이 원 크기
      height = ,    # 파이 높이
      labels = ,    # 각 파이 이름
      labelcex = ,  # 라벨 글자 크기
      labelcol = ,  # 라벨 글자 색
      explode = ,   # 파이간 간격
      ...)

pie(vec1, labels = c('월','화','수','목','금'))
pie3D(vec1, labels = c('월','화','수','목','금'), explode=0.1)

# [ 연습 문제 ] 영화 데이터를 사용하여
# 각 시군구별 영화이용률이 가장 높은 상위 3개 시군구에 대해
# 각각 성별 이용비율을 비교할 수 있는 pie차트 시각화
movie2 <- ddply(movie, .(지역.시군구), 
                summarise, v1=sum(이용_비율...))

library(doBy)
vname <- doBy::orderBy( ~ -v1, movie2)[1:3, 1]

movie3 <- movie[movie$지역.시군구 %in% vname, ]
total <- dcast(movie3, 성별 ~ 지역.시군구, sum)

dev.new()
par(mfrow=c(1,3))

# 각 구내 성별 비율 출력
f1 <- function(x) {
  round(x / sum(x) * 100, 2)
}

vrate <- as.data.frame(apply(total[,-1], 2, f1))

# 시각화
pie3D(total$강남구, col=c(2,4), explode=0.1, main = '강남구',
      cex.main=3, labels = str_c(vrate$강남구,'%'))
pie3D(total$서구, col=c(2,4), explode=0.1, main = '서구',
      cex.main=3, labels = str_c(vrate$서구,'%'))
pie3D(total$중구, col=c(2,4), explode=0.1, main = '중구',
      cex.main=3, labels = str_c(vrate$중구,'%'))

legend(0.5,1, total$성별, fill = c(2,4), cex=1.5)


