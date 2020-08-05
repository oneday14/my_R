# 그래프 
# figure : 그래프를 그릴 공간
dev.new() # 새로운 하나의 figure 생성

# 1. plot : 선그래프, 산점도, 교차산점도, ...
plot(x,     # x축 좌표
     y,     # y축 좌표
     ...)   # 그래프 옵션

plot(c(1,2,3), c(10,11,12))

# 1) type : 선그래프 스타일
plot(c(1,2,3), c(10,11,12), type='o')  # 점과 선 동시 출력
plot(c(1,2,3), c(10,11,12), type='l')  # 선만 출력

# 2) col : 선의 색
plot(c(1,2,3), c(10,11,12), type='l', col='red')  # 선만 출력
plot(c(1,2,3), c(10,11,12), type='l', col=1)  # 선만 출력

# 3) lty : 선 스타일(점선, 실선, ...)
par(mfrow=c(1,3))                            # 여러 그래프 동시 출력
plot(c(1,2,3), c(10,11,12), type='l', lty=1) # 실선
plot(c(1,2,3), c(10,11,12), type='l', lty=2) # 대쉬선
plot(c(1,2,3), c(10,11,12), type='l', lty=3) # 점선

# 4) xlab, ylab, main : 각 축 이름, 메인 제목
dev.new()
plot(c(1,2,3), c(10,11,12), type='l',
     xlab = 'x축 이름', ylab = 'y축 이름')

# 5) xlim, ylim : 각 축 범위 설정
plot(c(1,2,3), c(10,11,12), type='l',
     xlab = 'x축 이름', ylab = 'y축 이름', ylim = c(0,12))

# 6) axis : x축, y축 눈금 설정(함수)
axis(side=,       # 눈금 설정 방향(x축:1, y축:2)
     at=,         # 눈금 벡터
     labels = ,   # 각 눈금의 이름
     ...)         # 기타옵션

# 7) axes : x축, y축 눈금 출력 여부
plot(1:5, c(10,9,11,7,13), type = 'o', col = 4, ylim = c(0,15),
     axes = F)

# 8) ann : x축, y축 이름 출력 여부

# 9) title : 각 축, 메인 제목을 동시 전달하는 함수
title(main = ,  # 메인제목
      sub = ,   # 서브제목
      xlab = ,  # x축 이름
      ylab = ,  # y축 이름
      ...)      # 기타옵션

# 10) legend : 범례를 출력하는 함수
legend(x,          # 범례 출력 x 위치
       y,          # 범례 출력 y 위치
       legend = ,  # 범례 표현 값
       fill = ,    # 범례 색 표현(막대그래프 출력시 주로 사용)
       col = ,     # 범계 색 표현(선그래프 출력시 주로 사용)
       ...)

# 예제) 위 그래프에 x축 눈금을 월화수목금 설정
axis(1, 1:5, c('월','화','수','목','금'))
axis(2, ylim=c(0,10)) # y축 눈금 출력만 가능, ylim 변경 불가

# 데이터프레임의 선 그래프 출력
# - plot에 데이터 프레임 전달 시 각 컬럼별(로우별) 선그래프 출력불가
#   (교차 산점도 출력)
# - 각 컬럼별, 행별 분리, 하나씩 생성 필요(파이썬에선 동시 출력 가능)
# - 여러 그래프를 하나의 figure에 동시 그릴 시 plot -> lines 대체

# 예제) 다음이 데이터 프레임에서 각 과일별 판매량 증감 추이를
# 선 그래프로 출력
df1 <- data.frame(apple=c(100,120,150),
                  banana=c(200,210,250),
                  mango=c(90,80,110))

rownames(df1) <- 2010:2012
plot(df1)

dev.new()
plot(df1$apple, type = 'o', col=2, ylim = c(50, 300), 
     axes = F, ann = F)
lines(df1$banana, type = 'o', col=3)
lines(df1$mango, type = 'o', col=4)

axis(1, at=1:3, labels = rownames(df1))
axis(2, ylim = c(50, 300))


title(main = '과일별 판매량 증감추이', col.main = 'red',
      xlab = '년도', cex.lab = 2,
      ylab = '판매량', font.lab = 4)

legend(1,300, colnames(df1), col = 2:4, lty=1)

# [ 참고 : 교차 산점도 ]
# - plot 함수에 데이터프레임(숫자컬럼으로 구성된)을 전달하면 자동 출력
# - 각 변수간 상관관계 파악 시 사용
# - 분류분석시 종속변수의 분류에 영향력이 큰 설명변수 찾는 과정에 사용

# 예제) iris 데이터의 4개 설명변수의 교차 산점도 출력
plot(iris[,-5])

# 예제) iris 데이터의 4개 설명변수의 교차 산점도 출력
#       출력시 species의 값마다 서로 다른 색 부여
plot(iris[,-5], col=iris$Species)


# [ 연습 문제 ]
# data.csv 파일에서 년도별 총구직자수의 변화추이 선그래프 출력
# (각 년도별로 월별 구직자수의 변화를 확인할 수 있는...)

df1 <- read.csv('data.csv', stringsAsFactors = F)

library(reshape2)

total <- dcast(df1, 월 ~ 년도)

dev.new()

plot(total$`2014`, type = 'o', col = 1, ylim = c(4000,13000),
     ann = F, axes = F)
lines(total$`2015`, type = 'o', col = 2)
lines(total$`2016`, type = 'o', col = 3)
lines(total$`2017`, type = 'o', col = 4)
lines(total$`2018`, type = 'o', col = 5)

library(stringr)
axis(1, at=1:12, labels = str_c(total$월,'월'))
axis(2, ylim = c(4000,13000))

title(main='년도별 월별 구직자수 변화', col.main=4, cex.main=2,
      xlab='월', cex.lab=1.3,
      ylab='구직자수', font.lab=2)

legend(11, 13000, colnames(total)[-1], col=1:5, lty=1, cex = 1.5)


# [ 참고 : plot 도표에 특정 행 하나 전달 시 ]
# - 데이터 프레임에서 특정 행 하나 선택은 데이터프레임 출력
# - plot에 데이터프레임 입력 => 교차 산점도 출력
# - 행,열 전치 혹은 unlist로 벡터로 만든 후 plot에 전달 필요

total <- dcast(df1, 년도 ~ 월)

dev.new()
plot(total[1,-1], type='o')

total[1,-1]             # 데이터프레임 출력
as.vector(total[1,-1])  # key구조 유지, 데이터프레임 출력
unlist(total[1,-1])     # key구조 해제, 벡터 출력

plot(unlist(total[1,-1]), type='o')

# [ 연습 문제 ]
# subway2.csv 파일의 데이터를 기반으로
# 승차가 가장 많은 top 5개의 역을 구하고 
# 각 역의 시간대별 승차의 증감추세를 도표화
sub <- read.csv('subway2.csv', stringsAsFactors = F, skip = 1)
head(sub)

# 1) 승차 데이터 선택
sub2 <- sub[sub$구분 == '승차', -2]
sub2

# 2) 역이름을 로우이름으로 설정
rownames(sub2) <- sub2$전체
sub2$전체 <- NULL   # sub2 <- sub2[,-1]  (특정 컬럼(KEY) 삭제)

# 3) 역별 승차 총 합
str(sub2)
vsum <- apply(sub2, 1, sum)

# 4) 승차의 총 합으로 정렬
sort(vsum, decreasing = T)[1:5]
vname <- names(sort(vsum, decreasing = T)[1:5])

# 5) top5 역에 대한 시간별 승차 수 추출
total <- t(sub2[rownames(sub2) %in% vname, ])

# 6) 그래프 그리기
dev.new()
rownames(total) <- as.numeric(str_sub(rownames(total),2,3))

min(total)
max(total)

plot(total[,1]/1000, type = 'o', col = 1, ylim = c(1,400),
     ann = F, axes = F)
lines(total[,2]/1000, type = 'o', col = 2)
lines(total[,3]/1000, type = 'o', col = 3)
lines(total[,4]/1000, type = 'o', col = 4)
lines(total[,5]/1000, type = 'o', col = 5)

axis(1, at=1:nrow(total), labels = rownames(total))
axis(2, ylim = c(1,380))

title(main='시간대별 승차 변화량', 
      xlab='시간대',
      ylab='승차수/(천)')

legend(18, 400, colnames(total), col = 1:5, lty = 1)


# 2. barplot
# - 막대 그래프
# - 컬럼별 서로 다른 그룹의 막대 생성
# - 하나의 컬럼 내 서로 다른 행 데이터가 stack된 형식 출력이 기본

barplot(height = ,    # 2차원 데이터
        ...)


# 예제) 다음의 데이터에 대해 각 과일별 판매량을 비교하는
# 막대그래프 출력 
fruit <- data.frame(apple=c(100,120,150),
                  banana=c(200,210,250),
                  mango=c(90,80,110))

rownames(fruit) <- 2010:2012

dev.new()
barplot(fruit)
barplot(as.matrix(fruit))               # stack된 형식
barplot(as.matrix(fruit), beside = T)   # 행별 서로 다른 막대


barplot(as.matrix(t(fruit)), beside = T, col = rainbow(3), 
        ylim = c(0,300))

# legend(1,300, colnames(fruit), col = rainbow(3), lty=1 ) # 선스타일
legend(1,300, colnames(fruit), fill = rainbow(3))        # 박스스타일

# [ 연습 문제 ]
# 상반기사원별월별실적현황_new.csv을 읽고,
# 월별 각 직원의 성취도를 비교하기 위한 막대그래프 출력
df1 <- read.csv('상반기사원별월별실적현황_new.csv', stringsAsFactors=F)

df2 <- dcast(df1, 이름 ~ 월)
rownames(df2) <- df2$이름
df2$이름 <- NULL

dev.new()
barplot(as.matrix(df2), beside = T, col = rainbow(nrow(df2)), 
        ylim = c(0, 1.5),
        angle = 45,        # 막대의 빗금 각도
        density = 80,      # 막대의 빗금 농도
        names.arg = str_c(1:6, '월'),
        legend = rownames(df2), 
        args.legend = list(cex=1, x='topleft'))
     

# 조건별 서로 다른 색 전달(사용자 정의 팔레트 생성)
barplot(as.matrix(fruit[,1]), col = vcol, beside = T)
v1 <- fruit[,1]

vcol <- c()

for (i in v1) {
  if (i <= 110) {
    vcol <- c(vcol, 'green')
  } else if (i <= 130) {
    vcol <- c(vcol, 'yellow')
  } else {
    vcol <- c(vcol, 'red')
  }
}



