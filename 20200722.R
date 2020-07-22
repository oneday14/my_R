# 1. emp.csv 파일을 읽고
emp <- read.csv('emp.csv')

# 1) 10번 부서 직원의 comm의 평균을 계산
#    (단, NA인 경우는 100 할당, NA 수정은 ifelse문 또는 for문 활용)
emp$COMM2 <- ifelse(is.na(emp$COMM), 100, emp$COMM)
mean(emp[emp$DEPTNO == 10, 'COMM2'])

vcomm <- c()

for (i in emp$COMM) {
  if (is.na(i)) {
    vcomm <- c(vcomm, 100)
  } else {
    vcomm <- c(vcomm, i)
  }
}

emp$COMM <- vcomm
mean(emp[emp$DEPTNO == 10, 'COMM'])

# 2) 상반기 입사한 사람은 연봉의 10%, 하반기는 15% bonus 컬럼에 추가
emp$HIREDATE <- as.Date(emp$HIREDATE)
emp$MONTH <- as.numeric(as.character(emp$HIREDATE, '%m'))
ifelse(emp$MONTH < 7, emp$SAL*1.1, emp$SAL*1.15)

vsal <- c()

for (i in 1:nrow(emp)) {
  if (emp$MONTH[i] < 7) {
    vsal <- append(vsal, emp$SAL[i]*1.1)
  } else {
    vsal <- append(vsal, emp$SAL[i]*1.15)
  }
}

# 2. student.csv 파일을 읽고
std <- read.csv('student.csv')

# 1) DEPTNO라는 컬럼을 생성하되, 제2전공이 있는 경우는 
#    제2전공번호를, 없는 경우는 제1전공번호 출력
std$DEPTNO <- ifelse(is.na(std$DEPTNO2), std$DEPTNO1, std$DEPTNO2)

vno <- c()

for (i in 1:nrow(std)) {
  if (is.na(std$DEPTNO2[i])) {
    vno <- c(vno, std$DEPTNO1[i])
  } else {
    vno <- c(vno, std$DEPTNO2[i])
  }
}

for (i in 1:nrow(student)) {
  if (is.na(student$DEPTNO2[i])) {
    student$DEPTNO[i] <- student$DEPTNO1[i]
  } else {
    student$DEPTNO[i] <- student$DEPTNO2[i]
  }
}

# 2) 지역번호 추출
library(stringr)

# step1) ')' 위치 확인
v1 <- str_locate(std$TEL, '\\)')
v2 <- v1[,1]

# step2) 추출
str_sub(std$TEL, 1, v1-1)
str_sub(std$TEL, 1, v2-1)

# 3. emp.csv 파일을 읽고
# 각 직원의 deptno값을 확인하여 같은 부서 직원들의 평균 연봉을 
# 각 행마다 다음과 같이 출력(mean() : 평균 구하는 함수)

#                                                          부서별평균
# 7369	SMITH	CLERK	    7902	1980-12-17 0:00	800		    20  2175
# 7499	ALLEN	SALESMAN	7698	1981-02-20 0:00	1600	300	30  1566.667
# 7521	WARD	SALESMAN	7698	1982-02-22 0:00	1250	500	30  1566.667
# 7566	JONES	MANAGER	  7839	1981-04-02 0:00	2975		  20  2175

mean(emp[emp$DEPTNO == 20, 'SAL'])
mean(emp[emp$DEPTNO == 30, 'SAL'])
mean(emp[emp$DEPTNO == 30, 'SAL'])
mean(emp[emp$DEPTNO == 20, 'SAL'])

vmean <- c()

for (i in emp$DEPTNO) {
  vmean <- c(vmean, mean(emp[emp$DEPTNO == i, 'SAL']))
}

emp$SAL_MEAN <- vmean

# 각 부서에서 그 부서의 평균연봉보다 높은 연봉을 받는 직원의
#  이름, JOB, SAL, DEPTNO 출력

emp[, c('SAL','SAL_MEAN')]
emp[emp$SAL > emp$SAL_MEAN, c('ENAME','JOB','SAL','DEPTNO')]

########## 여기까지는 복습입니다. ##########

library(stringr)

# 6. str_length(strings) : 문자열의 길이 리턴
v1 <- c('abc','aaab','231vg')

length(v1)      # 3, 벡터의 원소의 개수
str_length(v1)  # 3 4 5, 벡터의 각 원소의 문자열의 크기


# [ 연습 문제 ]
# emp.csv파일을 읽고 아래와 같은 형식으로 출력
# 'SMITH의 10% 인상된 연봉은 880이다.'

str_c(emp$ENAME, '의 10% 인상된 연봉은 ', emp$SAL*1.1, '이다.')

# [ 연습 문제 ]
# student.csv파일을 읽고 ID에 숫자가 2회이상 반복된 
# 학생 데이터 제외(행삭제)
v1 <- c('a123','a2b6')

str_count(v1, '[0-9]') >= 2  # 숫자의 반복은 확인 불가
str_detect(v1,'[0-9][0-9]')
str_detect(v1,'[0-9]{2,}')

v1[!str_detect(v1,'[0-9]{2,}')]

std$ID[str_detect(std$ID,'[0-9]{2,}')] # ID 확인

std[!str_detect(std$ID,'[0-9]{2,}'), ]


# 7. str_replace : 치환함수
str_replace(string = ,       # 원본 문자열
            pattern = ,      # 찾을 문자열
            replacement = )  # 바꿀 문자열

str_replace('abcba12','ab','AB')      # 단어 치환(글자 대 글자 치환X)
str_replace('abcba12','ab','')        # 삭제
str_remove('abcba12','ab')            # remove 함수로도 삭제 가능
str_replace_all('abcba12','[ab]','')  # 찾을 문자열 정규식 표현 가능

v1 <- c('12ab', NA, 'abc')
str_replace_all(v1,'[0-9]','a')       # 벡터의 원소별 치환 가능
str_replace_all(v1,NA,'a')            # NA의 치환 불가
str_replace_all(v1,'abc',NA)          # NA로 치환 불가

v2 <- c('a','b','c')
str_replace_all(v2,'a',0)             # 문자로만 치환 가능

# [ NA 치환 방법 ]
v1 <- c('12ab', NA, 'abc')
v1[is.na(v1)] <- 'aa'        # NA만 선택 후 수정 방식
ifelse(is.na(v1), 'aa', v1)  # 전체 벡터 수정 방식
str_replace_na(v1, 'aa')     # sql의 NVL과 비슷


# [ 연습 문제 ]
# 다음의 변수의 10%인상된 값 출력
v_sal <- c('1,200','5,000','10,003,300')
v_sal * 1.1

class(v_sal)
as.numeric(str_replace_all(v_sal,',','')) * 1.1

# 8. 대소치환
str_to_upper('abc')   # upper
str_to_lower('ABC')   # lower
str_to_title('abc')   # initcap

toupper('abc')
tolower('ABC')

# 9. str_split : 문자열 분리
str_split(string = ,
          pattern = ,
          n = )

a1 <- 'a#b#c#'
str_split(a1, '#')          # 리스트로 출력
str_split(a1, '#')[[1]][2]  # 분리된 특정 위치 원소 추출

# 연습문제 ) 
# vtel 에서 국번 추출(034)
vtel <- '02)034-1234'

# 1) ')', '-' 위치기반
vno1 <- str_locate(vtel, '\\)')[,1]
vno2 <- str_locate(vtel, '-')[,1]

str_sub(vtel, vno1+1, vno2-1)

# 2) 분리기반(split 사용)
vtel2 <- str_split(vtel, '\\)')[[1]][2]
str_split(vtel2, '-')[[1]][1]

# 연습문제) student.csv 파일의 TEL 컬럼에서 각 행의 국번 추출
# 1) 위치기반
vno1 <- str_locate(std$TEL, '\\)')[,1]
vno2 <- str_locate(std$TEL, '-')[,1]

str_sub(std$TEL, vno1+1, vno2-1)

# 2) 분리기반***
vtel2 <- str_split(std$TEL, '\\)')[[1]][2]
str_split(vtel2, '-')[[1]][1]


vtel2 <- str_split(std$TEL, '\\)')[[2]][2]
str_split(vtel2, '-')[[1]][1]


vtel2 <- str_split(std$TEL, '\\)')[[3]][2]
str_split(vtel2, '-')[[1]][1]


vtel3 <- c()

for (i in 1:20) {
  vtel2 <- str_split(std$TEL, '\\)')[[i]][2]
  vtel3 <- c(vtel3, str_split(vtel2, '-')[[1]][1])
}


# 반복문
# 1. for문
# - 정해진 반복 횟수
# - 반복대상이 자동으로 다음으로 넘어감

for (변수 in 반복대상) {
  반복할 문장
}

# 2. while문
# - 반복 횟수가 정해져 있지 않음(조건이 거짓이 될때까지 무한 반복)
# - 반복대상이 자동으로 넘어가지 않으므로 넘기는 작업 수행 필요
# - 반복대상의 초기값의 선언 필요

# [ 문법 ]
while (조건) {
  반복할 문장
}

# 예제) 1부터 10까지의 숫자에 10을 더해 출력
i <- 1

while (i <= 10) {
  print(i + 10)
  i <- i + 1
}


# [ 연습 문제 ] 1 ~ 100까지의 총 합 출력
# i       vsum 
# 1         1
# 2       1 + 2
# 3      1 + 2 + 3
# ...
# 100   1 + 2 + ... + 100


i <- 1
vsum <- 0

while (i <= 100) {
  vsum <- vsum + i
  i <- i + 1
}

# i      vsum
# 1      0+1
# 2      0+1+2
# 3      0+1+2+3
# ...
# 100    0+1+2+3+....+100


# [ 연습 문제 ] 1 ~ 100까지의 총 합 출력(짝수만)
# sol1)
i <- 1
vsum <- 0

while (i <= 100) {
  if (i%%2 == 0) {
    vsum <- vsum + i
  }  
  i <- i + 1
}

# sol2)
i <- 2
vsum <- 0

while (i <= 100) {
  vsum <- vsum + i
  i <- i + 2
}

vsum


# factor 변수
# - 변수의 범주(level)가 정해져 있는 형태
# - 주로 문자일 경우
# - read.csv로 불러올때 문자 타입의 컬럼은 자동으로 factor로 생성

# 1. 생성
f1 <- factor('m', c('m','f'))
f2 <- factor('m', c('m','f'), ordered = T)
f3 <- factor('m', c('f','m'), ordered = T)
f4 <- ordered('m', c('f','m'))

# 2. level 확인
levels(f1)

# 3. factor 변수 <-> 일반변수
v1 <- c('a','b','c')
v2 <- as.factor(v1)
v3 <- as.character(v2)

# 4. factor 변수의 수정
v2[3] <- 'd'                      # 에러발생, NA로 치환 
levels(v2) <- c('a','b','c','d')  # 레벨 확장
v2[3] <- 'd'                      # 정상수행

levels(v2) <- toupper(c('a','b','c','d')) # 레벨 수정을 통한 값 수정

# 5. 문자열 컬럼이 자동으로 factor로 생성되는 경우
# 1) read.csv, read.table
emp <- read.csv('emp.csv')
str(emp)

emp <- read.csv('emp.csv', stringsAsFactors = F)
str(emp)

# 2) cbind로 문자 컬럼 추가 시

# 예제) emp 데이터의 이름의 맨 앞 두자를 추출, 새로운 컬럼에 추가
emp$ENAME1 <- str_sub(emp$ENAME,1,2)
emp <- cbind(emp, 'ENAME2'=str_sub(emp$ENAME,1,2))
emp <- cbind(emp, 'ENAME3'=str_sub(emp$ENAME,1,2), 
             stringsAsFactors=F)
str(emp)

