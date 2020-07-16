# R 설치시 주의 사항
# 1. R 설치
# - 설치 위치 : Program Files 하위
# - 32 bit용 설치파일 선택 해제
# 
# 2. java 설치 
# - 설치 위치 : Program Files 하위
# 
# 3. R 실행 후 rJava 패키치 설치 및 로딩 확인
# (에러시 자바 패스 등록)
# install.packages('rJava')
# library('rJava')

# 4. cmd에서 R 실행 가능하도록 R설치위치 PATH 등록
# 
# 5. RStudio 설치 및 환경설정

# 참고 : 여러 라인 주석처리 및 해제(ctrl + shift + c)

# 환경설정
# - 작업 디렉토리(홈디렉토리) : C:/Users/KITCOOP/Documents
getwd()                      # 확인
setwd('C:/Users/KITCOOP/')   # 변경(현재 세션에서만 유효)
                             # rstudio 환경설정에서 영구 변경 가능

# 변수 : 연산결과나 상수를 저장하기 위한 객체
변수명 <- 상수 혹은 연산결과
a1 = 1
1 -> a1

a1 <- (1 + 10) * 101 / 200
a1 * 100
b1 <- 100
a1 + b1

c1 <- 'abc'
d1 <- "abc"

# 변수의 타입을 확인하는 함수 : class
class(a1)  # numeric
class(c1)  # character

# 변수명 주의 : sum, mean, c
sum1 <- 1 + 1
vsum <- 1 + 1

c(1,2,3)


# 산술연산
a1 + 1        # 숫자 + 숫자 가능
a1 + b1       # 숫자변수 + 숫자변수 가능
e1 <- '100'
e1 + 1        # 문자 + 숫자 에러, 숫자처럼 생긴 문자의 형 변환 발생 X

d1 <- Sys.Date()  # sysdate
class(d1)    
d1 + 100          # 날짜 + 숫자 연산 가능(기본단위 day)

# 형 변환 함수
as.numeric()     # to_number
as.character()   # to_char
as.Date()        # to_date

as.numeric(e1) + 1

# 변수에 연속적인 값 대입
1:10     # 숫자의 연속 배열 생성 가능
'a':'f'  # 문자의 연속 배열 생성 불가

seq()
help(seq)

seq(from = 1,  # 시작값 
    to = 1,    # 끝값
    by = 1)    # 증가값

seq(from=1, to=10, by=2)
seq(1, 10, 2)
seq(to=20,from=10,by=5)

# 문자 -> 날짜 파싱
2020/01/01 ~ 2020/01/31
seq(as.Date('2020/01/01'), as.Date('2020/01/31'), by=1)

as.Date('2020/01/01')
as.Date(x='06/30/2020', format='%m/%d/%Y')
as.Date('2020/01/01','%Y/%m/%d')

# 날짜 -> 문자(날짜의 포맷 변경)
d1 <- Sys.Date()
d2 <- as.character(d1, '%m-%d,%Y')
class(d2)

# [ 참고 : 날짜 형식 확인 방법 ]
help(as.Date)   # 불가
help(strptime)  # 가능

# %Y : 4자리 년도
# %y : 2자리 년도
# %m : 월
# %d : 일
# %H : 시
# %M : 분 
# %S : 초
# %w : 숫자요일(일요일:0)
# %A : 문자요일

# 함수의 사용 방법
# - 정해진 순서대로 인자 전달, 각 인자의 이름 생략 가능
# - 정해진 순서와 다른 순서로 인자 전달 시 반드시 인자 이름 명시


# [ 연습 문제 ]
# 1. 2020년 8월 1일부터 2020년 8월 31일까지의 날짜를 모두 출력,
# 해당 날짜의 요일을 출력
v1 <- seq(as.Date('2020/08/01'), as.Date('2020/08/31'), 1)
class(v1)  # 날짜 벡터

as.character(v1, '%A')

# 2. 2020년 8월 15일로부터 오늘 날짜까지 남은 일수 출력
v3 <- as.Date('2020/08/15') - Sys.Date()
as.numeric(v3 + 100)
class(v3)


# package : 함수의 묶음, 외부 package를 다운받게 되면
# 세션마다 해당 package를 로딩해서 사용

# 날짜 관련 외부 패키지 : lubridate
install.packages('lubridate')
library(lubridate)

todate <- Sys.Date()
as.character(todate, '%Y')  # as.character를 사용한 날짜 변환

year(todate)   # 년도
month(todate)             # 월, 숫자형식
month(todate, label = T)  # 월, 영문날짜일 경우 문자형식 출력

day(todate)    # 일

wday(todate)              # 요일, 숫자형식
wday(todate, label = T)   # 요일, 문자형식

hour(todate)   # 시간
minute(todate) # 분
second(todate) # 초

todate + 7         # 7일 후
todate + days(7)   # 7일 후
todate + hours(7)  # 7시간 후
todate + months(7) # 7개월 후
todate + years(7)  # 7년 후

lubridate::

# [ 참고 : 날짜 언어 변경 ]  
Sys.setlocale('LC_TIME','C')       # 영문
Sys.setlocale('LC_TIME','KOREAN')  # 한글

# [연습문제]
# 2020년 7월의 일별 데이터를 출력,
# 그중 v_year라는 컬럼(변수)에 년도만,
# v_month라는 컬럼(변수)에 월만, 
# v_day라는 컬럼(변수)에 일만 분리저장
# v_bonus_date 컬럼에 6개월 후 데이터를 입력
s1 <- seq(as.Date('2020/07/01'), as.Date('2020/07/31'),1)

v_year <- year(s1)
v_month <- month(s1)
v_day <- day(s1)

v_bonus_date <- s1 + months(6)

# [ 변수 확인 및 제거 ]
objects()   # 현재 세션에 정의된 변수 목록 확인
ls()        # 현재 세션에 정의된 변수 목록 확인

sum <- 10
rm(sum)                 # sum 변수 하나 제거
rm(list = c('v1','v3')) # 변수 여러개 제거(v1,v3)

rm(list = objects()) # 현재 세션의 모든 변수 제거
rm(list = ls())      # 현재 세션의 모든 변수 제거


# 산술 연산자
7 %/% 3   # 몫
7 %% 3    # 나머지
2 ^ 3     # 거듭제곱(2의 3승)
2 ** 3    # 거듭제곱(2의 3승)
1e1       # 10
2e1       # 20
1e2       # 100
1e-1      # 0.1


# 자료구조
# 0. 스칼라 : 단 하나의 상수
# 1. 벡터(vector) : 일차원, 여러 값이 동시에 나열, 서로 같은 타입
# 2. 리스트(list) : 일차원, 여러 값이 동시에 나열, 서로 다른 타입
# 3. 행렬(matrix) : 2차원, 서로 같은 타입
# 4. 배열(array) : 3차원 이상, 서로 같은 타입
# 5. 데이터프레임(data.frame) : 2차원, 서로 다른 타입

a1 <- c(1,2,3,10)
b1 <- c(1,2,3,'a')





