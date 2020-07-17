# 1. 2020년 전체 날짜를 갖는 v1 변수 생성
v1 <- seq(from=as.Date('2020/01/01',fotmat='%Y/%m/%d'),
          to=as.Date('2020/12/31',fotmat='%Y/%m/%d'),
          by=1)

# 2. 위의 벡터를 년도를 제외한 월/일 형식으로만 출력하여 v2 생성
v2 <- as.character(v1,'%m/%d')

# 3. '2020/04/25'일로부터 100일 뒤의 요일 출력
as.character(as.Date('2020/04/25') + 100, '%A')
as.character(as.Date('2020/04/25') + 100, '%w')

# 4. 사원의 입사일이 다음과 같을때
# 현재까지 근무일수가 몇주, 몇일인가 각각 출력
v_hiredate <- c('2018/04/06','2019/12/23','2019/05/04')
class(v_hiredate)

v_day <- as.numeric(Sys.Date() - as.Date(v_hiredate))
v_week <- v_day %/% 7

v_week2 <- trunc(v_day/7) ; v_week2

########## 여기까지는 복습입니다. ##########


# DB의 NULL : 아직 정의되지 않은 값

# R에서의 NA와 NULL
# - NA   : 잘못 들어온 값, 물리적 위치를 갖음
# - NULL : 정해지지 않은 값, 물리적 위치를 갖지 않음

as.Date('2020/01/01','%Y/%M/%D')

cat(..., 
    file= , )

cat(1,2,NA,3)   # 1 2 NA 3
cat(1,2,NULL,3) # 1 2 3 (NULL이 자리를 차지하고 있지 않아 출력X)

sum(1,2,NA)     # NA, NA는 무시되지 X
sum(1,2,NULL)   # 3, NULL은 무시됌

NULL + 3        # numeric(0)
NA + 3          # NA

# [ 참고 : 가변형 인자 전달 방식 ]
# 함수의 인자 자리에 ...이 있는 경우는 여러개 대상이 들어갈수 있음
sum(..., na.rm = F)
sum(1,2,3)
sum(c(1,2,3))

mean(x, ...)
mean(1,2,3)     # 1만 전달, 2,3은 무시
mean(c(1,2,3))  # c(1,2,3)는 하나의 객체이므로 전체 전달 가능


# [ R에서의 자료구조 ]
# 벡터
# - 1차원
# - 여러개 값을 동시에 담거나 전달하기 위해 사용
# - 하나의 데이터 타입만 허용
# - 데이터프레임(테이블)의 컬럼의 구조이기도 함

# 1. 생성
v1 <- c(1,2,3) ; v1
v2 <- c(1,2,'a') ; v2
v3 <- 1:10 ; v3

# 2. 확장
c(v1, 4)
append(x,                   # 벡터
       values = ,           # 추가 값
       after = length(x))   # 위치, 생략시 맨 끝에 추가
append(v1,4)
append(v1,4,after=1)


# 3. 산술연산
v1 <- c(1,2,3)
v2 <- c(10,20,30)
v3 <- c(10,20,30,40)

v1 + 1   # 벡터와 숫자 산술연산 가능
v1 + v2  # 동일한 크기의 벡터는 같은 위치 원소끼리 연산
v1 + v3  # 서로 다른 크기의 벡터의 연산은 작은 크기의 벡터가
         # 반복되면서 연산됌
         # 10 20 30 40
         # 1  2  3  1
         # ===========
         # 11 22 33 41  

# 4. 색인(indexing)  *****
# 1) 정수색인
v_sal <- c(800,900,1000,1100,1200)
v_sal[1]
v_sal[1,3]     # 첫번째 행의 3번째 컬럼만 선택(2차원의 색인)
v_sal[c(1,3)]  # 1,3번째 원소 추출

# 2) 이름색인 
names(v_sal) <- c('a','b','c','d','e') ; v_sal
v_sal[c('a','d')]

# 3) 슬라이스 색인
v_sal[c(1,2,3)]
v_sal[1:3]
v_sal[2:5]

# 4) 조건 색인(boolean indexing)
v_sal[v_sal >= 1000]
v_sal[c(F,F,T,T,T)]

# 불리언 벡터의 색인 매칭
# 800,900,1000,1100,1200
# F   F   T    T    T


# [ 연습문제 : emp.csv 파일을 읽고 ] 
df1 <- read.csv('emp.csv')
class(df1)

# 1) 10번 부서원의 이름, job, sal 출력
DEPTNO == 10          # DEPTNO라는 변수가 10이냐는 질문
df1[,'DEPTNO'] == 10
df1[,8] == 10
df1$DEPTNO == 10

df1[df1$DEPTNO == 10, c('ENAME','JOB','SAL')]
df1[c(7,9,14), c('ENAME','JOB','SAL')]

# 2) 20번 부서원의 sal의 총 합 출력
df1[c(1,4,8,11,13), 6]
df1[c(1,4,8,11,13), 'SAL']
sum(df1[c(1,4,8,11,13), 'SAL'])

sum(df1[df1$DEPTNO == 20, 'SAL'])

# 3) sal이 2000이상인 직원 이름, sal 추출
df1[df1[,'SAL'] >= 2000 , c('ENAME','SAL')]
df1[df1$SAL >= 2000 , c('ENAME','SAL')]

# 4) smith와 allen의 이름과 연봉 추출
df1[c(1,2), c(2,6)]
df1[c(1,2), c('ENAME','SAL')]

# in sql)
# select ename, sal
#   from df1
#  where ename in ('smith','ALLEN')
# ;
 
# [ sql -> R 문법 변경 ]
# ename in ('smith','ALLEN')
# df1$ENAME %in% c('smith','ALLEN')  # %in% : 포함연산자
 
df1[df1$ENAME %in% c('smith','ALLEN'), c('ENAME','SAL')]  
 
v_bool <- df1$ENAME == c('smith','ALLEN')
df1[v_bool, c('ENAME','SAL')]

# df1[행색인 , 컬럼색인 ]


# 5. 벡터 수정
v_sal[2] <- 1100

# 연습문제) v_sal의 이름 중 d를 D로 변경
names(v_sal[4]) <- 'D' ; v_sal
names(v_sal)[4] <- 'D' ; v_sal
names(v_sal) <- c('a','b','c','D','e')

# 예제) df1의 SAL 컬럼을 SALARY로 변경
names(df1)[6] <- 'SALARY' ; df1

# 연습문제) df1의 COMM 컬럼을 BONUS로 변경하되,
# COMM 컬럼의 위치를 모른다는 가정하에 처리
names(df1)[7]
names(df1)['COMM']
names(df1)[names(df1) == 'COMM'] <- 'BONUS' ; df1

# 예제) df1에서 MGR 컬럼을 선택(조건색인)
df1[,'MGR']
df1['MGR']
df1$MGR
df1[ , names(df1) == 'MGR']

# 예제) df1에서 MGR 컬럼 제외 다른 컬럼 모두 선택(조건색인)
df1[ , names(df1) != 'MGR']  
df1[ , -4]                   # 4번째 컬럼 제외
df1[ , -'MGR']               # 문자값을 제외할수는 없음

# 6. 벡터 크기 확인
length(v_sal)      # 원소의 개수
NROW(v_sal)        # 벡터의 원소의 개수, 2차원의 행의 수
nrow(v_sal)        # 2차원 데이터의 행의 수

NROW(df1)          
nrow(df1) 

# 논리 연산자
# 1) and
(v_sal > 1000) & (v_sal < 2000)
v_sal[(v_sal > 1000) & (v_sal < 2000)]

# 2) or
(v_sal <= 1000) | (v_sal >= 2000)
v_sal[(v_sal <= 1000) | (v_sal >= 2000)]

# 3) not
(v_sal > 1000)
!(v_sal > 1000)

v_sal == 1000
v_sal != 1000
!(v_sal == 1000)


# 포함 연산자
v_sal[(v_sal == 800) | (v_sal == 1000)]
v_sal[v_sal %in% c(800, 1000)]


# 연습문제 : df1에서) 
# 1) 1987년에 입사한 사람의 이름, 입사일, SAL 출력
# in sql)
# where to_char(hiredate,'YYYY') = '1987'
class(df1$HIREDATE)

# 날짜 파싱(980/12/17 00:00:00)
v_date <- as.Date(df1$HIREDATE, '%Y/%m/%d %H:%M:%S')

df1[as.character(v_date, '%Y') == '1987', 
    c('ENAME','HIREDATE','SALARY')]

# 2) 사원번호가 7900, 7902, 7934인 직원의 SAL의 총합
v_bool  <- (df1$EMPNO == 7900) | (df1$EMPNO == 7902) | (df1$EMPNO == 7934)
v_bool2 <- df1$EMPNO %in% c(7900, 7902, 7934)

sum(df1[v_bool, 'SALARY'])
sum(df1[v_bool2, 'SALARY'])



# 리스트 
# - 1차원
# - key 구조(데이터를 빠르게 검색/저장할 수 있도록 만든 자료구조)
# - 서로 다른 데이터 타입 허용

# 1. 생성
l1 <- list('name'='홍길동', 'tel'='02)043-0875', 'addr'='서울시')
l2 <- list('name'='홍길동', 'tel'='02)043-0875', 'addr'='서울시',
           'sal'=4000)

l3 <- list('name'=c('홍길동','김길동'), 
           'tel'=c('02)043-0875', '031)384-3944'), 
           'addr'=c('서울시','경기도'),
           'sal'=c(4000,3900))

# 2. 색인
l3[1]           # 첫번째 key(층) 선택
l3[c(1,3)]      # 첫번째 key(층) 선택

l3['name']
l3[c('name','tel')]

l3$name
l3$c(name,tel)       # 불가, key 색인은 하나의 key만 전달 가능

# 연습문제) l3에서 김길동만 추출
l3$name[2]
l3[1]        # 리스트 리턴
l3[[1]]      # 벡터 리턴
l3[[1]][2]   


# 3. 수정
l3$name[2] <- '최길동' ; l3
l3$comm <- c(500,400)            # key 추가

df1$new_sal <- df1$SALARY * 1.1  # 데이터프레임의 key 추가
l3$comm <- NULL ; l3             # key 삭제

# key를 갖는 자료형태
'name' : '홍길동'
'tel'  : '02)043-0875'
'addr' : '서울시'

# key를 갖지 않는 자료형태
'홍길동', '02)043-0875', '서울시'


# [ 참고 : R과 파이썬의 자료구조 비교 ]
#   R             python 
# 벡터            리스트
# 리스트*         딕셔너리*
# 행렬             행렬
# 배열             배열
# 데이터프레임*  데이터프레임*


