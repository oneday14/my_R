# 1. emp.csv 데이터를 활용하여
getwd()
emp <- read.csv('emp.csv')

# 1) 1월에 입사한 직원의 이름, 입사일, 연봉 출력
v_bool <- as.character(as.Date(emp$HIREDATE),'%m') == '01' # to_date
emp[v_bool, c('ENAME','HIREDATE','SAL')]

# 2) 각 직원의 입사요일을 DAY 컬럼에 추가
emp$DAY <- as.character(as.Date(emp$HIREDATE),'%A')

# 3) 다음의 계산식으로 퇴직금 계산 후 R_PAY 컬럼으로 추가
# (퇴직금 = 현재 SAL * trunc(근속년수/12))
emp$HIREDATE <- as.Date(emp$HIREDATE)
v_year <- as.numeric(trunc((Sys.Date() - emp$HIREDATE) / 365))
emp$R_PAY <- emp$SAL * trunc(v_year/12)

# 4) 이름이 KING인 행 삭제
emp <- emp[emp$ENAME != 'KING', ]

# 5) 20번 부서직원 중 SAL이 2000이상인 직원의 SAL의 평균
mean(emp[(emp$DEPTNO == 20) & (emp$SAL >= 2000), 'SAL'])

# 2. 다음의 리스트 생성
# no : 1,2,3,4
# name : apple, banana, peach, berry
# price : 500, 200, 200, 50
# qty : 5,2,7,9
v_no <- 1:4
v_name <- c('apple', 'banana', 'peach', 'berry')
v_price <- c(500, 200, 200, 50)
v_qty <- c(5,2,7,9)

l1 <- list(no = v_no,
           name = v_name,
           price = v_price,
           qty = v_qty)

# 1) banana를 BANANA로 변경
l1$name[2]
l1[[2]][2] <- 'BANANA' ; l1

# 2) 10% 인상된 가격을 NEW_PRICE 키에 추가
l1$NEW_PRICE <- l1$price * 1.1

# 3) peach의 qty를 출력(단, peach의 위치를 모른다 가정)
l1$qty[3]

l1$name == 'peach'
l1$qty[l1$name == 'peach']

########## 여기까지는 복습입니다. ##########

# 논리 연산자
# 1) and : &
# 2) or : |
# 3) not : !
  
# 참고 : 숫자를 사용한 논리값의 전달
# 0 : FALSE(0이 아니면 TRUE)

!0
!100
1 & 3  # TRUE & TRUE = TRUE
1 & 0  # TRUE & FALSE = FALSE


c(TRUE,TRUE,FALSE) & c(TRUE,FALSE,FALSE)  # 각 결과마다 논리연산 O
c(TRUE,TRUE,FALSE) && c(TRUE,FALSE,FALSE) # 각 결과마다 논리연산 X, 첫번째것만 논리연산

v1 <- 1:3
(v1 < 3) & (v1 == 1)

# 예제) emp 데이터에서 sal > 2000이면서 deptno = 10인 직원선택
(emp$SAL > 2000) & (emp$DEPTNO == 10)
(emp$SAL > 2000) && (emp$DEPTNO == 10)

emp[(emp$SAL > 2000) && (emp$DEPTNO == 10), ]  # 잘못된 전달
emp[(emp$SAL > 2000) & (emp$DEPTNO == 10), ]   # 올바른 전달


# 형(데이터 타입) 확인 함수
v1 <- c(1,NA,3)
v2 <- c(1,NULL,3)

is.vector(v1)
is.numeric(v1)
is.na(v1)       # 벡터의 각 원소마다 NA 여부 리턴
is.na(v2)       # 벡터의 각 원소마다 NA 여부 리턴, NULL은 생략
is.null(v2)     # 벡터 자체가 NULL인지 리턴

# 연습문제) 위 v1 벡터에서 NA인 부분을 찾아 0으로 수정
sum(v1)              # NA를 포함하는 산술연산 NA리턴
v1[is.na(v1)] <- 0   # NA 치환
sum(v1)              # 치환후 연산

# [ 벡터 연습 문제 ]
# 1. vec1 벡터 생성
vec1 <- c('사과','배','감','버섯','고구마')

# 1) vec1에서 3번째 요소인 감을 제외, vec1 출력
vec1[vec1 != '감']

# 2. vec1, vec2 생성
vec1 <- c('봄','여름','가을','겨울')
vec2 <- c('봄','여름','늦여름','초가을')

# 1) vec1과 vec2를 합친 결과 출력
c(vec1, vec2)                       # union all
append(vec1, vec2[!vec1 %in% vec2]) # union 
append(vec1, vec2[vec1 != vec2])
# 2) vec1에는 있는데 vec2에는 없는 결과 출력
vec1[!vec1 %in% vec2]

# 3) vec1과 vec2 둘다 있는 결과 출력
vec1[vec1 %in% vec2]


# 벡터의 집합연산자
# 1) 합집합 : union
# 2) 차집합 : setdiff
# 3) 교집합 : intersect
# 4) 동등비교 : identical, setequal
v1 <- c(1,2,3,4,5)
v2 <- c(1,2,30,40,50)
v3 <- c(1,2,3,4,5,6)
v4 <- c(1,2,3,4,5,5)

union(v1,v2)      # 1  2  3  4  5 30 40 50
c(v1,v2)          # 1  2  3  4  5  1  2 30 40 50

intersect(v1,v2)  # 1,2
setdiff(v1,v2)    # 3 4 5

identical(v1,v3)  # 서로 다른 원소가 있으므로, FALSE
setequal(v1,v3)   # 서로 다른 원소가 있으므로, FALSE

identical(v1,v4)  # 구성 원소는 같으나 크기가 달라서 FALSE
setequal(v1,v4)   # 크기 상관없이 구성 원소는 같으므로 TRUE



# 행렬 : matrix
# - 2차원
# - 하나의 데이터 타입만 허용
# - 주로 숫자 연산을 빠르게 하기 위해 만듬

# 1. 생성
matrix(data = ,        # matrix 구성 data(벡터)
       nrow = ,        # 행 수
       ncol = ,        # 컬럼 수 
       byrow = FALSE,  # 행 우선순위 배치 여부
       dimnames = )    # 행과 열의 이름, 리스트로 전달

m1 <- matrix(1:9, nrow = 3, ncol = 3) ; m1
m1 <- matrix(1:9, nrow = 3) ; m1
m2 <- matrix(1:9, nrow = 3, byrow = T) ; m2
m3 <- matrix(1:9, 
             nrow = 3, 
             byrow = T,
             dimnames = list(c('a','b','c'),
                             c('A','B','C'))) ; m3

# 2. 색인
m1[1,1]
m1[,1]
m1[1,] 
m1[, c(2,3)]
m1[, 2:3]
m1[, -1]      # 첫번째 컬럼 제외
m3['a','A']   # 이름이 있는 경우 이름색인 가능
m1[m1 > 5]    # 2차원 형식의 불리언을 색인에 그대로 전달

m1[,1]        # 차원축소 발생, 1차원인 벡터로 출력
m1[,1,drop=F] # 2차원 출력, 차원축소 발생 X

# 연습문제) m3에서 두번째 컬럼이 5이상인 행을 선택
m3[m3[,2] >= 5, ]

# 3. 수정
m1[1,1] <- 10 ; m1
 
# 연습문제) 1부터 20값을 갖는 5X4행렬 생성 후
# 짝수값을 모두 0으로 수정하여라.
m1 <- matrix(1:20, nrow = 5, byrow = T)
4 %% 2
5 %% 2

m1[m1 %% 2 == 0] <- 0


# 4. 구조변경
# 1) 행, 열 이름 변경
rownames(m2) <- c('a','b','c')
colnames(m2) <- c('A','B','C')
dimnames(m2) <- list(c('a','b','c'), c('A','B','C'))

# 2) 행, 컬럼 추가
m2[,4] <- c(10,20,30)                # 기존 범위를 벗어나서 불가
cbind(m2, c(10,20,30))               # m2 맨 뒤에 컬럼 추가
cbind(m2[,1], c(10,20,30), m2[,2:3]) # 중간 삽입 시

rbind(m2, c(10,11,12))


# 5. 연산
m1 <- matrix(1:4, nrow = 2)
m2 <- matrix(seq(10,40,10), nrow = 2)
m3 <- matrix(1:9, nrow = 3)
m1 * m2    # 각 원소마다 곱하기 연산
m1 %*% m2  # matrix inner product

m1 + m2    # 크기가 같은 행렬끼리 연산 가능
m1 + m3    # 크기가 다른 행렬끼리 연산 불가

# [ 참고 : 행렬 곱 ]
# [a1,a2   [b1,b2
#  a3,a4]   b3,b4]
# 
# (2X2) * (2X2) = (2X2)
# (3X2) * (2X6) = (3X6)
# 
# a1*b1 + a2*b3, a1*b2 + a2*b4
# a3*b1 + a4*b3, a3*b2 + a4*b4


# 6. 크기 확인
nrow(m1)  # 행의 수
ncol(m1)  # 컬럼 수 
NROW(m1)  # 행의 수 
NCOL(m1)  # 컬럼 수
dim(m1)   # 2 X 2

m1 <- matrix(1:20, nrow = 5)
dim(m1)            # 5 X 4
dim(m1) <- c(4,5)  # 5 X 4, matrix reshape
m1

# [ matrix 연습 문제 ]
# 1. 행렬 생성
v1 <- c('봄','여름','가을','겨울')
seasons <- matrix(v1, nrow = 2)
t(seasons)

seasons <- matrix(v1, nrow = 2, byrow = T)

# 2. 여름과 겨울만 조회
seasons <- matrix(v1, nrow = 2, byrow = T)
seasons[,2,drop=F]

# 3. 3번 행 추가
seasons_2 <- rbind(seasons, c('초봄','초가을'))

# 4. 3번째 컬럼 추가
seasons_3 <- cbind(seasons_2, c('초여름','초겨울','한겨울'))


# 배열 : array
# 1. 생성
array(data = ,     # 배열을 구성하는 data(벡터)
      dim = ,      # 차원(벡터로 전달)
      dimnames = ) # 각 차원의 이름(리스트로 전달)

array(1:18, dim = c(2,3,3))  # 차원은 행, 열, 층, ... 순서
array(1:18, 
      dim = c(2,3,3),
      dimnames = list(c('a','b'),
                      c('A','B','C'),
                      c('1F','2F','3F')))  

# [ 참고 : 다차원의 배열 순서 비교 ]
# in R)
# 행 열 층 ...
 
# in python)
# ... 층 행 열


# 2. 색인
a1 <- array(1:18, dim = c(3,3,2))
a1[,,1]
a1[,,1,drop=F]  # 사용 가능 
a1[1,,]
a1[1,,drop=F]   # 사용 불가


# 데이터 프레임
# - 2차원 구조
# - 행과 열의 구조
# - 열은 KEY를 갖는 구조
# - 엑셀에서의 표, 데이터베이스에서의 테이블과 비슷

# 1. 생성
data.frame(...,                  # data자리, key-value 구조 나열
           row.names = ,         # row 이름
           stringsAsFactors = T) # 문자의 팩터화 여부

data.frame('ename'=c('홍길동','김길동'),
           'sal'=c(800,900))

df1 <- data.frame(ename=c('홍길동','김길동'),
                  sal=c(800,900))

df2 <- data.frame(ename=c('홍길동','김길동'),
                  sal=c(800,900),
                  row.names = c('a','b'))

# 2. 구조확인
ncol(df1)      # 컬럼 수
nrow(df1)      # 행의 수
dim(df1)       # 행과 컬럼 수

rownames(df1)  # 행 이름
colnames(df1)  # 컬럼 이름

str(df1)

# 3. 색인
df1[1,1]              # 위치(정수)색인
df1[1,'ename']        # 이름색인
df1[ ,'ename']        # 특정 컬럼 하나의 선택은 차원 축소 됌
df1[ ,'ename',drop=F] # 차원 축소 방지 가능
df1$ename             # key 색인

# 4. 구조변경
# 1) 행추가
df2 <- rbind(df1, c('최길동', 1000, 300))  # 에러 발생
df2[3,1] <- '최길동'
str(df1)

# [ factor로 생성된 컬럼에 값을 추가하는 방법 ]
# 1. factor의 레벨을 수정
# 2. non factor변수로 수정
df2$ename <- as.character(df2$ename)
str(df2)
df2[3,1] <- '최길동'
df2$ename <- as.factor(df2$ename)

# 2) 컬럼추가
df1$comm <- c(100,200) ; df1         
cbind(df1, 'deptno' = c(9411,9511))  # 컬럼 추가

# factor형 변수
# 정해져 있는 레벨을 갖는 경우의 범주형 변수로
# 주로 문자형 컬럼일 경우 생성

# ex) 성별 : 남, 여
#     학점 : A, B, C, D
#     학년 : 1, 2, 3, 4


# 연습문제) 다음의 데이터 프레임 생성
# name : apple, mango, banana
# price : 1000, 1500, 500
# qty : 10, 5, 20
v_name <- c('apple', 'mango', 'banana')
v_price <- c(1000, 1500, 500)
v_qty <- c(10, 5, 20)

df1 <- data.frame(name=v_name, price=v_price, qty=v_qty)
str(df1)

# 1) 다음의 행 추가
# berry, 2000, 3
df2 <- rbind(df1, c('berry', 2000, 3))
str(df2) 

df2$name <- as.character(df2$name)
df2[4,1] <- 'berry'
df2$name <- as.factor(df2$name)

# 2) sales 컬럼에 price * qty 값 계산하여 추가
df2$price <- as.numeric(df2$price)
df2$qty <- as.numeric(df2$qty)

df2$sales <- df2$price * df2$qty

