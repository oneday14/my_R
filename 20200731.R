# 1. movie_ex1.csv 파일을 읽고 
df1 <- read.csv('movie_ex1.csv', stringsAsFactors = F)
head(df1)
str(df1)
# 1) 연령대별 성별 이용비율의 평균을 구하여라
aggregate(df1$이용_비율... , list(df1$성별 ,df1$연령대), FUN=mean)
aggregate(이용_비율... ~ 성별 + 연령대, data = df1, FUN = mean)

# 2) 요일별 이용비율의 평균을 구하여라.
library(stringr)
vdate <- str_c(df1$년, df1$월, df1$일, sep='/')

# sol1) 날짜 결합 시 분리 구분기호 삽입
as.Date("2018/2/1", '%Y/%m/%d')

df1$DAY <- as.character(as.Date(vdate, '%Y/%m/%d'), '%A')

aggregate(이용_비율... ~ DAY, data = df1, FUN = mean)[c(4,7,3,2,1,6,5),]

# sol2) 2자리 월, 일 형식으로 가공
vdate2 <- str_c(df1$년, 
                sprintf('%02d', df1$월), 
                sprintf('%02d', df1$일))

as.Date(vdate2, '%Y%m%d')

# 2. delivery.csv 파일을 읽고
df2 <- read.csv('delivery.csv', stringsAsFactors = F)
head(df2)

# 1) 일자별 총 통화건수를 구하여라
df2_day <- aggregate(통화건수~일자, data=df2, FUN=sum)

# 2) 음식점별 주문수가 많은 시간대를 출력
# 중국음식 1  6
#          2  10
#          3  30
#          ...
#          24 30

# step1) 업종별 시간대별 통화건수 총 합
df2_1 <- aggregate(통화건수 ~시간대+업종, data=df2, FUN=sum)

# step2) 업종별 통화건수 최대값
df2_max <- aggregate(통화건수 ~업종, data=df2_1, FUN=max)

# step3) 1,2 데이터 조인
merge(df2_1, df2_max, by=c('업종','통화건수'))

# 3) 일자별 전일대비 증감률을 구하여라
df2_day

# 20180201  39653  39653   
# 20180202  46081  39653
# 20180203  54124  46081
# 
# (46081 - 39653) / 39653 * 100

# step1) 이전 행 값을 가져오는 사용자 정의 함수 생성
f_shift <- function(x) {
  vshift <- c()
  for (i in 1:length(x)) {
    if (i==1) {
      vshift <- c(vshift, x[1])
    } else {
      vshift <- c(vshift, x[i-1])
    }
  }
  return(vshift)
}

f_shift <- function(x) {
  vshift <- c()
  for (i in 1:length(x)) {
    vshift <- c(vshift, x[max(i-1,1)])
  }
  return(vshift)
}

# [ 참고 - 외부 패키지 사용 ]
install.packages('data.table')
library(data.table)

v1 <- 1:10
shift(v1,n=1, type='lag')           # NA  1  2  3  4  5  6  7  8  9
shift(v1,n=1, fill=0, type='lag')   # 0  1  2  3  4  5  6  7  8  9
shift(v1,n=1, type='lead')          # 2  3  4  5  6  7  8  9 10 NA

# step2) 전일자 통화건수 컬럼 생성
df2_day$before <- f_shift(df2_day$통화건수)

# step3) 전일자 대비 증감률 계산
# (46081 - 39653) / 39653 * 100

df2_day$rate <- (df2_day$통화건수-df2_day$before)/df2_day$before * 100

# 3. get_query 사용자 정의 함수 생성
get_query <- function(sql, ip='localhost', port='1521',
                      sid='orcl', user='scott', pwd='oracle') {
  library(RJDBC)
  library(stringr)
  
  jdbcDriver <- JDBC(driverClass = "oracle.jdbc.OracleDriver", 
                     classPath = "C:/app/KITCOOP/product/11.2.0/client_1/ojdbc6.jar")
  
  vaddr <- str_c('jdbc:oracle:thin:@', ip, ':', port, ':', sid )
  
  con <- dbConnect(jdbcDriver, 
                   vaddr,
                   user,
                   pwd)
  
  df1 <- dbGetQuery(con, sql)
  
  return(df1)
}

save(list=ls(), file='my_function')
get_query('select * from employees', user='hr', port='1521')

########## 여기까지는 복습입니다. ##########

# sqldf 
# - R 프로그래밍을 sql문법으로 처리 가능하도록 만든 패키지
# - sql 문법과 완전히 동일하지는 않을 수 있음
# - 특히 조인(non equi join) 수행 시 유리

install.packages('sqldf')
library(sqldf)

emp <- read.csv('emp.csv', stringsAsFactors = F)

# 1) ENAME, EMPNO 컬럼 추출
emp[,c('ENAME', 'EMPNO')]
sqldf('select ENAME, EMPNO from emp')

# 2) ALLEN의 이름과 SAL 출력 
emp[emp$ENAME=='ALLEN',c('ENAME', 'SAL')]
sqldf('select ENAME, SAL from emp where ENAME='ALLEN'')    # X
sqldf('select ENAME, SAL from emp where ENAME=\'ALLEN\'')  # O
sqldf("select ENAME, SAL from emp where ENAME='ALLEN'")    # O


# [ 연습문제 ]
# 1) ALLEN과 FORD의 이름, 부서번호, 연봉 출력
emp[emp$ENAME %in% c('ALLEN','FORD'), c('ENAME','DEPTNO','SAL')]

v_sql1 <- "select ENAME, DEPTNO, SAL
             from emp
            where ENAME in ('ALLEN','FORD')"

sqldf(v_sql1)

# 2) emp 데이터의 각 부서별 최대연봉자 이름, 부서번호, 연봉출력
emp_max <- aggregate(SAL~DEPTNO, data=emp, FUN=max)
merge(emp, emp_max, by=c('DEPTNO','SAL'))[, c('ENAME','DEPTNO','SAL')]

v_sql2 <- "select ENAME, DEPTNO, SAL
             from emp
            where (DEPTNO, SAL) in (select DEPTNO, max(SAL)
                                      from emp
                                     group by DEPTNO)"

sqldf(v_sql2)

v_sql3 <- "select e.ENAME, e.DEPTNO, e.SAL
             from emp e, (select DEPTNO, max(SAL) as max_sal
                            from emp
                           group by DEPTNO) i
            where e.DEPTNO = i.DEPTNO
              and e.SAL    = i.max_sal"

sqldf(v_sql3)


# [ sqldf로 outer join 수행 ]
student <- read.csv('student.csv', stringsAsFactors = F)
professor <- read.csv('professor.csv', stringsAsFactors = F)

v_sql4 <- "select s.name, p.name
             from student s, professor p
            where s.profno = p.profno(+)"

sqldf(v_sql4)   # 에러 발생

v_sql5 <- "select s.name, p.name
             from student s left outer join professor p
               on s.profno = p.profno"

sqldf(v_sql5) 

v_sql6 <- "select s.name, p.name
             from professor p right outer join student s
               on s.profno = p.profno"

sqldf(v_sql6)

# plyr
- apply 계열 함수의 변형 형태
- data frame으로의 출력 가능
- {}{}ply 형식의 다양한 함수 포함
 input      output
데이터형식  데이터형식

install.packages('plyr')
library(plyr)

# 1. plyr::adply
# - array input(matrix, data frame), data frame output
# - apply 함수와 비슷

# 예제) iris data의 행별 열별 총 합
apply(iris[, -5], 1, sum)
apply(iris[, -5], 2, sum)

adply(iris[, -5], 1, sum)  # 행별 연산시 기존 데이터프레임에 추가
adply(iris[, -5], 2, sum)  # 기존 데이터프레임과 함께 출력 불가

# [ 참고 - adply에 mean 함수 전달 에러 ]
apply(iris[, -5], 1, mean) # 하나의 행 전달시 벡터로 전달
adply(iris[, -5], 1, mean) # 하나의 행 전달시 데이터프레임으로 전달
adply(as.matrix(iris[, -5]), 1, mean) # 가능

# [ 참고 - sum과 mean의 데이터 전달 방식에 따른 차이 ]
v1 <- 1:10
v2 <- data.frame(v1)

sum(v1)  # 가능
sum(v2)  # 가능
 
mean(v1) # 가능  
mean(v2) # 불가, mean함수는 데이터프레임의 연산 불가

# 2. plyr::ddply
# - 그룹연산 수행(group by 연산)
# - 여러 개의 group by 컬럼 가능
# - 여러 컬럼의 그룹 연산 가능
# - 각 컬럼마다 서로 다른 그룹함수 전달 가능
# - 그룹별 비교 가능

# ddply(.data = ,        # 데이터프레임
#       .variables = ,   # 그룹 연산 컬럼
#       .fun = ,         # 내부 함수
#       ...)             # 연산수식
# 

# ddply 내부 함수
# 1. summarise : 그룹별 데이터 요약(sql group by 결과와 비슷)
# 2. transform : 원본 데이터와 그룹 연산 결과 조인 형태
# 3. mutate : transform과 유사, 이전 그룹 연산 결과 재사용 가능
# 4. subset : 그룹별 조건 전달 가능(조건 추출)

# 예제) emp에서 부서별 평균 연봉
ddply(emp, .(DEPTNO), summarise, avg_mean=mean(SAL))
ddply(emp, .(DEPTNO), transform, avg_mean=mean(SAL))

# 예제) emp에서 부서별 평균 연봉보다 높은 연봉을 받는 직원 출력
emp_1 <- ddply(emp, .(DEPTNO), transform, avg_mean=mean(SAL))
emp_1[emp_1$SAL > emp_1$avg_mean, c('ENAME','DEPTNO','SAL','avg_mean')]

# 예제) student에서 학년별 키의 평균, 몸무게의 최대값 출력
ddply(student, .(GRADE), summarize, v1=mean(HEIGHT), v2=max(WEIGHT))

# 예제) student에서 학년별, 학과별 키의 평균
ddply(student, .(GRADE, DEPTNO1), summarize, v1=mean(HEIGHT))

# [ 연습문제 ] 
# delivery.csv 파일을 읽고
# 각 읍면동별 통화건수의 총 합을 구하되, 
# (단, 각 동은 숫자를 포함하고 있는 경우 
# 숫자를 제외한 동까지 표현하도록 함 (ex 을지로6가 => 을지로))
de <- read.csv('delivery.csv', stringsAsFactors = F)

unique(de$읍면동)

# 1) 주소 정리
str_split('신문로2가','[0-9]')[[1]][1]
str_remove_all('신문가로2가','[0-9가]')
str_remove_all('신문가로2동동','[0-9].{1,}')

de$동 <- str_remove_all(de$읍면동,'[0-9].{1,}')

# 2) 그룹 연산
ddply(de, .(동), summarise, CNT=sum(통화건수))


# 예제) subset을 사용한 그룹 조건 전달
#       student 데이터에서 각 학년별 키가 가장 큰 학생 출력
ddply(student, .(GRADE), subset, HEIGHT==max(HEIGHT))


# mutate 사용 예
ddply(student, .(GRADE), mutate, v1=mean(HEIGHT), v2=log(v1))



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
# 2. unstack : long -> wide




