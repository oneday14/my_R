# 1. card_history.csv 파일을 읽고
# 각 일별 품목별 지출 비율을 출력

# NUM   식료품     의복    외식비      책값 온라인소액결제    의료비
# 1   8.629893 63.61210  3.825623 12.900356       2.491103  8.540925
# 2  11.568525 62.74101  3.647733 13.548723       1.719646  6.774362
# 3  14.757049 53.08938  4.499100 13.197361       4.499100  9.958008
df1 <- read.csv('card_history.csv', stringsAsFactors = F)
library(stringr)

df1 <- df1[,-1]

f1 <- function(x) {
  as.numeric(str_remove_all(x,','))
}

# step1) 천단위 구분기호 제거 및 숫자 변경
df1[,] <- apply(df1, c(1,2), f1)

# step2) 연산의 패턴 찾기
df1[1,] / sum(df1[1,]) * 100
df1[2,] / sum(df1[2,]) * 100
...
df1[30,] / sum(df1[30,]) * 100

# step3) 적용 범위 설정 및 전체 적용
f2 <- function(x) {
  x / sum(x) * 100
}

df1[,] <- t(apply(df1, 1, f2))

# 2. kimchi_test.csv 파일을 읽고
# 1) 김치별 판매량이 가장 높은 월 출력
df2 <- read.csv('kimchi_test.csv', stringsAsFactors = F)

# step1) 김치별 월별 판매량의 총 합 데이터 생성
# 총각김치 1   9865
# 총각김치 2   9754
# 총각김치 3   8755
library(plyr)
df2_1 <- ddply(df2, .(제품, 판매월), summarise, v1=sum(수량))

# step2) 김치별 판매량의 최대값을 갖는 행 선택(월)
ddply(df2_1, .(제품), subset, v1==max(v1))

# 3. 부동산_매매지수현황.csv 파일을 읽고
df3 <- read.csv('부동산_매매지수현황.csv', stringsAsFactors = F, 
                skip=1)

colnames(df3)
head(df3)

# 1) 월별 각 지역의 활발함 지수의 평균 출력
# step1) 불필요한 행과 컬럼 삭제 및 행이름 생성
df3_1 <- df3[-c(1,2), ]
rownames(df3_1) <- df3_1[,1]
df3_1 <- df3_1[,-1]
head(df3_1)

# step2) 활발함 지수와 한산함 지수 분리
df3_2 <- df3_1[ , df3[1,-1] == '활발함']
df3_3 <- df3_1[ , df3[1,-1] == '한산함']

# step3) 컬럼이름 정리
colnames(df3_2) <- str_sub(colnames(df3_2),1,2)
colnames(df3_3) <- str_sub(colnames(df3_2),1,2)

# step4) 숫자로 변경
df3_2[,] <- apply(df3_2, 2, as.numeric)

# step5) 월 추출, 월별 활발함 지수 평균 출력
df3_2$month <- str_sub(rownames(df3_2), 6,7)
ddply(df3_2, .(month), summarise, v1=mean(서울),
      v2=mean(부산),
      v3=mean(대구))

str(df3_2)

# 2) 년도별 활발함 지수와 한산함 지수의 평균 출력
df3_2$year <- str_sub(rownames(df3_2), 1,4)
df3_2_mean <- as.data.frame(apply(df3_2[,1:7],1,mean))
colnames(df3_2_mean) <- 'mean'

ddply(df3_2_mean, .(df3_2$year), summarise, v1=mean(mean))

########## 여기까지는 복습입니다. ##########

# 데이터 구조
# 1. long data(tidy data)
# - 관계형 데이터베이스의 테이블 형식
# - 하나의 관찰 대상이 하나의 컬럼을 구성하는 방식
# - 조인 가능
# - group by 연산 가능
# - 구조 변경이 거의 없음

# 2. wide data(cross table)
# - 주로 공공기관 데이터 표현 형식(정리표)
# - 행별, 열별 연산이 용이(group by 연산의 대체)
# - 시각화 시 필요
# - 조인 수행 불가
# - 컬럼의 잦은 추가/삭제 발생

# 데이터 구조 변경
# 1. stack : wide -> long
stack(x,      # data frame
      ...)    # 기타 옵션

# 2. unstack : long -> wide
unstack(x,    # data frame
        ...)  # formular : value column ~ index column

df1 <- data.frame(apple=c(10,20,30),
                  banana=c(20,25,30),
                  mango=c(5,6,7))

df2 <- data.frame(month=c(1,2,3),
                  apple=c(10,20,30),
                  banana=c(20,25,30),
                  mango=c(5,6,7))

df3 <- stack(df1)
stack(df2)  #  month 컬럼도 stack 처리됌(좋은 표현 X)

unstack(df3, values ~ ind)

# [ 연습 문제 ]
# melt_ex.csv 파일을 읽고 라떼의 수량에 대해 아래의 교차 테이블 완성
#         1   2   3  4  5 ....   12(월)
# 2000  400 401 402  .
# 2001  412

df1 <- read.csv('melt_ex.csv', stringsAsFactors = F)
df2 <- unstack(df1, latte ~ mon)
rownames(df2) <- c(2000,2001)
colnames(df2) <- str_c(1:12,'월')

stack(df1)



# reshape2 패키지
# - stack과 unstack 처리를 조금 더 깔끔하게 표현
# - stack에서는 stack될 컬럼과 stack되지 말아야 할 컬럼 지정 가능
# - unstack에서는 각각 행방향, 컬럼방향에 표현해야할 대상 지정 가능

install.packages('reshape2')
library(reshape2)

# 1. melt
# - stack 처리 함수(wide -> long)
# - stack 컬럼 지정 가능
# 
# melt(data,             # 원본 데이터 프레임
#      id.vars=,         # stack시 제외 컬럼
#      measure.vars=,    # stack 처리 할 컬럼(생략시 id.vars 제외 모두)
#      na.rm = F,        # NA 표현 여부
#      value.name = ,    # value column 이름 지정
#      variable.name = ) # index column 이름 지정

df2 <- data.frame(month=c(1,2,3),
                  apple=c(10,20,30),
                  banana=c(20,25,30),
                  mango=c(5,6,7))
stack(df2)
melt(df2, id.vars = 'month')
melt(df2, id.vars = 'month', variable.name='과일이름', 
                             value.name='수량')


# 예제) melt_ex.csv 파일을 의미있는 tidy data로 변경
df1 <- read.csv('melt_ex.csv', stringsAsFactors = F)
melt(df1, id.vars = c('year','mon'), variable.name = '이름',
                                     value.name = '수량')


# [ 연습 문제 ] 
# 2000-2013년_연령별실업율_40-49세.csv 파일을 읽고
# 해당 데이터를 년도별 월별 정리된 형태(tidy)로 출력
library(reshape2)
library(stringr)

df1 <- read.csv('2000-2013년_연령별실업율_40-49세.csv')
df2 <- melt(df1, id.vars = '월', variable.name = '년도',
                                 value.name='실업율')

df2$년도 <- as.numeric(str_remove_all(df2$년도, '[X년]'))

# 2. dcast
# - unstack 처리(long -> wide)
# - 교차 테이블 생성
# - 행고정(index column), 컬럼 고정, value 표현 컬럼 필요

dcast(data,              # data frame
      formula = ,        # 행 고정 ~ 컬럼 고정
      fun.aggregate = ,  # 요약함수(필요시 지정, 생략시 개수)
      ...,               # 함수 추가 인자
      drop = T,          # 차원 축소
      value.var = )      # value column(생략 시 맨 끝 컬럼)

# 예제1) dcast_ex1.csv 를 읽고 다음과 같은 형식으로 배치
#       qty	price
# latte	100	2200
# mocha	80	2500
dcast1 <- read.csv('dcast_ex1.csv')
dcast(dcast1, name ~ info)

# 예제2) dcast_ex2.csv를 읽고 년도별 품목별 교차 테이블 생성
dcast2 <- read.csv('dcast_ex2.csv')
dcast(dcast2, year ~ name)                    # price에 대한 교차테이블
dcast(dcast2, year ~ name, value.var = 'qty') # qty에 대한 교차테이블

# 예제3) dcast_ex3.csv를 읽고 년도별 지점별 교차 테이블 생성
dcast3 <- read.csv('dcast_ex3.csv')
dcast(dcast3, 년도 ~ 지점, sum)

# [ 연습 문제 ]
# 상반기사원별월별실적현황_new.csv 파일을 읽고
df_data <- read.csv('상반기사원별월별실적현황_new.csv')
library(reshape2)

# 1) 다음과 같은 교차테이블로 표현
#       1     2     3     4     5     6
#박동주 1     0.85 0.75  0.98  0.92  0.97
#최경우 0.90 0.92  0.68  0.87  0.89  0.89

df_data2 <- dcast(df_data, 이름 ~ 월)

# 2) 사원별 성취도 평균
rownames(df_data2) <- df_data2$이름
df_data2 <- df_data2[,-1]

apply(df_data2, 1, mean)

rowSums(df_data2)
rowMeans(df_data2)
colSums(df_data2)
colMeans(df_data2)

