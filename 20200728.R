# 파일 입출력
# 6. 바이너리 입출력
# - 현재까지 작업 환경(변수,함수)에 대한 저장 시
# - 사용자 정의 함수의 저장 용이

v1 <- 1 ; v2 <- 2 ; v3 <- 3

f1 <- function(x) {
  x + 1
}

save(...,      # 입력 대상 나열
     list = ,  # 입력 대상 벡터로 전달
     file = )  # 저장할 파일명

load(file = )  # 불러올 파일명

save(list = ls(), file = 'vlist')
rm(list=ls())
v1             # 출력 불가
load('vlist')
v1             # 출력 가능
f1(10)         # 함수 사용 가능

# 7. 데이터베이스 입출력(oracle)
# 1) 준비사항
# - 접속할 DB의 정보 : ip, port, db name, username, passwd
# ip : 192.168.0.115
# port : 1521
# db name : orcl
# username : scott
# passwd : oracle

# [ 참고 - oracle service port/ db name 확인 방법 ]
# C:\Users\KITCOOP> lsnrctl status

# - DB내의 통신 대상 : ojdbc6.jar(64bit)
# - R내의 통신 대상 : RJDBC(64bit)

# [ 참고 : oracle 설치 종류 ]
# oracle
# - server : instance(memory) + DB(disk)
# - client : instance(memory)


# 2. R의 패키지 설치 : RJDBC
install.packages('RJDBC')
library(RJDBC)

# 3. oracle 설치(client) : ojdbc6.jar 파일 생성

# 4. connection 생성
jdbcDriver <- JDBC(driverClass = "oracle.jdbc.OracleDriver",
                   classPath = 'C:/app/KITCOOP/product/11.2.0/client_1/ojdbc6.jar')

con1 <- dbConnect(jdbcDriver,
                  "jdbc:oracle:thin:@ip:port:db_name",
                  username,
                  passwd)

con1 <- dbConnect(jdbcDriver,
                  "jdbc:oracle:thin:@192.168.0.115:1521:orcl",
                  "scott",
                  "oracle")
# 5. 쿼리 수행
dbGetQuery(conn = ,       # 연결이름
           statement = )  # 실행 쿼리

df1 <- dbGetQuery(conn = con1,       # 연결이름
                  statement = 'select * from student')  # 실행 쿼리

class(df1)


# 적용함수 : 반복연산을 도와주는 함수
# 1. apply(X,           # 대상 
#          MARGIN = ,   # 방향(1:행별, 2:열별, c(1,2):원소별)
#          FUN = ,      # 적용함수
#          ...)         # 적용함수의 추가 인자

# - X에는 1차원 객체 전달 불가, 2차원 데이터 전달 가능
# - 출력 결과는 데이터프레임 제외 모든 객체 가능(벡터, 리스트, 행렬)
# - 주로 행별, 열별 그룹연산 수행을 위해 사용
# - R에서는 "원소별" 적용도 가능(파이썬 불가)

# 예제) 다음의 행렬에서 행별 총 합 연산
ma1 <- matrix(1:25, nrow = 5)
apply(ma1, 1, sum)             # NA 포함
apply(ma1, 1, sum, na.rm = T)  # NA 무시

ma1[1,1] <- NA

sum(..., na.rm = F)

# 예제) iris 데이터에서 컬럼별 평균 연산
mean(iris$Sepal.Length)
mean(iris$Sepal.Width)

apply(iris[,-5], 2, mean)

# 예제) 다음의 벡터에서 천단위 구분기호 제거 후 숫자로 변경
v1 <- c('1,100','2,200','3,300')

library(stringr)
as.numeric(str_remove_all(v1, ','))

# 적용함수 전달 방법으로 풀이)
f1 <- function(x) {
  as.numeric(str_remove_all(x, ','))
}

apply(v1, c(1,2), f1)       # 벡터 적용 불가
sapply(v1, f1)              # 벡터 적용 가능

# [ 연습 문제 ]
# 사용자로부터 삭제할 대상을 전달받아 삭제하는 사용자 정의함수 생성,
# 다음의 데이터 프레임에 적용
df1 <- data.frame(col1=c('1,100','1,200'),
                  col2=c('2,200','2,300'),
                  stringsAsFactors = F)

f2 <- function(data, y) {
  as.numeric(str_remove_all(data, y))
}

f2(data=df1,y=',')            # 2차원 전달 불가
apply(df1, c(1,2), f2, ',')   # 2차원 전달 가능
sapply(df1, f2, ',')          # 2차원 전달 가능

df1 <- as.data.frame(apply(df1, c(1,2), f2, ','))
df1 <- apply(df1, c(1,2), f2, ',')     # matrix로 생성
df1[,] <- apply(df1, c(1,2), f2, ',')  # 기존 data frame 유지

# 2. lapply(list, function, ...)
# - 원소별 적용
# - 주로 벡터의 원소별 적용
# - 데이터 프레임 전달 시 키별 적용
# - 출력 결과 주로 리스트
lapply(v1, f1)                 # 리스트 출력
as.data.frame(lapply(v1, f1))  # 데이터프레임 출력(층 -> 컬럼)
as.vector(lapply(v1, f1))      # 벡터 출력 불가(key 구조 유지)
unlist(lapply(v1, f1))         # 벡터 출력 가능(key 구조 해제)

lapply(iris[,-5], mean)        # 컬럼별(key) 적용

# 3. sapply(list, function, ...)
# - 원소별 적용
# - 주로 벡터의 원소별 적용
# - 2차원 데이터 셋의 원소별 적용도 가능
# - 주로 벡터, 행렬로 리턴
sapply(v1, f1) 

# 4. mapply(function, ...)
# - 원소별 적용
# - lapply, sapply와는 다르게 적용 함수를 첫 번째 인자로 전달
# - 주로 벡터, 행렬 리턴
mapply(f1, v1)
mapply(f2, v1, ',')

# 5. tapply(vector, index, function)
# tapply(vector,    # 그룹연산 수행 대상(벡터)
#        index,     # 그룹벡터(group by 컬럼)
#        function)  # 적용함수(주로 그룹함수)

# - 그룹별 적용
# - vector 자리에는 2차원 적용 불가
# - sql의 group by 연산과 비슷
# - 주로 벡터 리턴

tapply(iris[,-5], iris$Species, mean)  # 2차원 데이터 셋 적용 불가

# 예제) googleVis 패키지에 있는 Fruits 데이터에서
# 과일별 sales의 총 합 출력

install.packages('googleVis')
library(googleVis)
Fruits

tapply(Fruits$Sales, Fruits$Fruit, sum)

# 예제) profit이 15이상일 경우와 미만일 경우 각 그룹의 sales의 총합
g1 <- ifelse(Fruits$Profit>=15,'15이상','15미만')
tapply(Fruits$Sales, g1, sum)

vsum <- tapply(Fruits$Sales, Fruits$Profit>=15, sum)
names(vsum) <- c('15이하','15이상')

# [ 참고 - in sql ]
# select sum(Sales)
#   from Fruits
#  group by Fruit


# [ 연습문제 ]
# 1. emp.csv 파일을 읽고 상/하반기 입사자의 평균연봉
df1 <- read.csv('emp.csv', stringsAsFactors = F)
vmonth <- as.numeric(str_sub(df1$HIREDATE, 6,7))
vgroup <- ifelse(vmonth < 7, '상반기','하반기')

tapply(df1$SAL, vgroup, mean, na.rm=T)

# 2. 2000-2013년_연령별실업율_40-49세.csv 파일을 읽고
# 2005년~2009년에 대해 각 월별, 년도별 실업률 평균
# 단, 년도 선택은 년도만 사용하여 표현, 예) year >= 2005
df2 <- read.csv('2000-2013년_연령별실업율_40-49세.csv')

rownames(df2) <- str_c(df2$월,'월')
df2 <- df2[,-1]

# 1) 월별 실업률 평균
apply(df2, 1, mean)

# 2) 년도별 실업률 평균
colnames(df2) <- as.numeric(str_remove_all(colnames(df2), '[X년]'))
apply(df2,2,mean)


# [ 연습 문제 ]
# apply_test.csv 파일을 읽고
# 부서별 판매량의 총 합을 구하세요.
# 단, 각 쉘이 -인 경우는 0으로 치환 후 계산
# (치환함수의 적용으로 풀이)

df3 <- read.csv('apply_test.csv', stringsAsFactors = F)

# 1) NA 또는 '-'값 0으로 수정
ifelse(is.na(df3), 0, df3)     # 2차원 적용 불가
df3[is.na(df3)] <- 0           # 데이터의 선택 후 수정 가능
str_replace_na(df3,0)          # 2차원 적용 불가

f_na <- function(x) {
  if ((x=='-') | is.na(x)) {
    return(0)
  } else {
    return(x)
  }
}

f_na('-')
f_na(NA)
f_na(3)

df3[,] <- apply(df3, c(1,2), f_na)   # 2차원 데이터 셋 적용 불가
sapply(df3, f_na)                    # 2차원 데이터 셋 적용 불가

# 2차원 데이터 셋의 원소별 적용 : apply
# 1차원 데이터 셋의 원소별 적용 : sapply

# 2) 2~5번째 컬럼 숫자컬럼으로 변경
as.numeric(df3[,-1])
df3[,-1] <- apply(df3[,-1], c(1,2), as.numeric)  # 가능
sapply(df3[,-1], as.numeric)                     # 가능

# 3) 년도와 상관없이 각 행의 총 합
df3$total <- apply(df3[,-1], 1, sum)

# 4) 부서번호 가공
str_split(df3$deptno.name,'-')

f_split <- function(x) {
  str_split(x,'-')[[1]][1]
}

f_split(df3$deptno.name)                       # 벡터 연산 불가
df3$deptno <- sapply(df3$deptno.name, f_split) # 벡터 연산 가능(원소별 적용)

# 5) 부서별 총 합
tapply(df3$total, df3$deptno, sum)

# sapply, lapply, mapply의 2차원 데이터 셋 적용 과정
# 1. 2차원 데이터 셋의 key별 데이터 분리
# 2. 각 key의 데이터를 벡터 형태로 함수에 전달
#    이 과정에서 함수는 벡터를 input 인자로 받게 됌
# => 함수 자체가 벡터연산이 가능한 경우만 적용함수를 통한 연산 가능

df4 <- data.frame(col1=1:5, col2=6:10)

f1 <- function(x) {
  x * 10                # 벡터연산 가능(x에 벡터 input 가능)
}

f2 <- function(x) {
  if (x%%2 == 0) {      # 벡터연산 불가(x에 scalar만 input 가능)
    x * 10
  } else {
    x * 20
  }
}

apply(df4, c(1,2), f1)
apply(df4, 1, f1)
sapply(df4, f1)         # key별 적용, 2차원 데이터 셋 적용 가능
sapply(df4, f2)         # key별 적용, 2차원 데이터 셋 적용 불가
                        # f2가 벡터를 input 인자로 허용 X 









