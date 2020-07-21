# 1. emp.csv 파일을 읽고
emp <- read.csv('emp.csv')

# 1) 상반기 입사한 사람은 연봉의 10%, 하반기는 15%의 보너스를
# bonus 컬럼에 추가(조건문 사용 불가)
class(as.Date(emp$HIREDATE))
emp$HIREDATE <- as.Date(emp$HIREDATE)
v_bool <- as.numeric(as.character(emp$HIREDATE,'%m')) >= 7

emp$bonus <- emp$SAL * 1.1
emp$bonus[v_bool] <- emp$SAL[v_bool] * 1.15

# 2) comm이 NA인 경우 0으로 수정후 comm의 평균 출력
emp$COMM[is.na(emp$COMM)] <- 0
mean(emp$COMM)

# 3) empno, ename, sal, deptno 컬럼만 선택 후 emp2 생성
emp2 <- emp[ , c('EMPNO','ENAME','SAL','DEPTNO')]

# 4) 위 emp2 데이터프레임에 아래 행 추가
# 9400, HONG, 3000, 40
str(emp2)

emp2$ENAME <- as.character(emp2$ENAME)
emp2 <- rbind(emp2, c(9400, 'HONG', 3000, 40))
emp2$ENAME <- as.factor(emp2$ENAME)

str(emp2)

emp2$SAL <- as.numeric(emp2$SAL)
emp2$DEPTNO <- as.numeric(emp2$DEPTNO)
emp2$EMPNO <- as.numeric(emp2$EMPNO)

# 2. disease.txt 파일을 읽고
read.csv('disease.txt')
read.table('disease.txt', header = T)

df2 <- read.table('disease.txt')
str(df2)
# 첫번째 행의 값으로 컬럼 이름 변경
colnames(df2) <- df2[1,]  # 컬럼이름이 숫자인 이유는 각 컬럼이
# factor이기 때문에 각 레벨의 숫자로 치환

# step1) 각 컬럼을 모두 문자 컬럼으로 변경
df2[,1] <- as.character(df2[,1])
df2[,2] <- as.character(df2[,2])
df2[,3] <- as.character(df2[,3])
df2[,4] <- as.character(df2[,4])
df2[,5] <- as.character(df2[,5])
df2[,6] <- as.character(df2[,6])

# step2) 컬럼 이름 변경
colnames(df2) <- df2[1,]

# step3) 첫 번째 행 제거
df2 <- df2[-1,]

# step4) 숫자 컬럼으로 변경
df2[,2] <- as.numeric(df2[,2])
df2[,3] <- as.numeric(df2[,3])
df2[,4] <- as.numeric(df2[,4])
df2[,5] <- as.numeric(df2[,5])
df2[,6] <- as.numeric(df2[,6])

# [ 참고 - 외부파일 불러오는 함수 비교 ]
# read.csv   : csv파일 전용, 분리구분자가 ','가 기본, header 컬럼화
# read.table : 일반파일 대상, 분리구분자가 탭이 기본, header 컬럼화X

# 1) A형간염과 콜레라의 NA값을 각 컬럼의 최소값으로 수정
vmin1 <- min(df2$A형간염[!is.na(df2$A형간염)])
df2$A형간염[is.na(df2$A형간염)] <- vmin1

vmin2 <- min(df2$콜레라[!is.na(df2$콜레라)])
df2$콜레라[is.na(df2$콜레라)] <- vmin2

# 2) "월별" 컬럼을 데이터프레임의 행이름으로 설정,
#    본문에서 "월별" 컬럼 삭제
rownames(df2) <- df2$월별
df2 <- df2[,-1]

# 3) A형간염의 컬럼 이름을 A간염으로 변경(위치 색인 불가)
colnames(df2)[colnames(df2)=='A형간염'] <- 'A간염'

# 4) NA를 하나라도 포함한 행 제외(df2)

df2[!(is.na(df2$콜레라) | is.na(df2$장티푸스) |
        is.na(df2$A간염)  | is.na(df2$이질) |
        is.na(df2$대장균)), ]

########## 여기까지는 복습입니다. ##########

# 조건문
# 1. if문
# - 조건에 따른 치환 혹은 프로그래밍 처리 시 사용
# - 벡터 연산 불가(벡터의 원소별 조건 전달이 불가)

# 1) 문법
# if ( 조건1 ) {
#   조건1 참일때 수행 문장
# } else if ( 조건2 ) {
#   조건2 참일때 수행 문장
# } else {
#   조건2 거짓일때 수행 문장
# }

# 예제) v1의 값이 0 이상이면 'A' 미만이면 'B' 리턴
v1 <- 11

if (v1 >= 0) {
  'A'
} else {
  'B'
}

# 예제) v1의 값이 30이상이면 'A', 20이상이면 'B' ,
# 20보다 작으면 'C'리턴
if (v1 >= 30) {
  'A'
} else if (v1 >= 20) {
  'B'
} else {
  'C'
}

# [ if문에 벡터 전달 불가 현상 ]
v1 <- c(10,25,55)

if (v1 >= 30) {
  'A'
} else if (v1 >= 20) {
  'B'
} else {
  'C'
}

# "C"
# length > 1 이라는 조건이 있고, => if문의 조건 결과는 반드시 하나
# 첫번째 요소만이 사용될 것입니다 => 반복이 안되므로 첫번째 결과만 출력

# 예제) emp 데이터에서 sal이 3000 이상인 경우는 'A',
# 아닌 경우는 'B' 리턴
emp$SAL >= 3000

if (emp$SAL >= 3000) {
  'A'
} else {
  'B'
}


# 연습문제) 10번 부서의 경우 SAL의 10% 증가값을, 나머지는 20% 증가값
# NEW_SAL 컬럼에 추가
# 1) if문
emp$NEW_SAL <- if (emp$DEPTNO == 10) {
                    emp$SAL * 1.1
                  } else {
                    emp$SAL * 1.2
                  }

# 2) ifelse문
emp$NEW_SAL2 <- ifelse(emp$DEPTNO==10, emp$SAL * 1.1, emp$SAL * 1.2)

# 2. ifelse문
# - 조건문(oracle의 decode함수와 비슷)
# - 벡터 연산 가능
# - 리턴만 가능, 프로그래밍 처리 불가

# 문법
ifelse(test,  # 조건
       yes,   # 참일때 리턴 값
       no)    # 거짓일때 리턴 값(생략불가)

v1 <- c(1,11,20)
ifelse(v1 > 10, 'A','B')

# 연습문제) emp 데이터에서 dname 컬럼 추가
# 10번 부서는 인사부, 20번은 재무부, 30번은 총무부
emp$dname <- ifelse(emp$DEPTNO==10, '인사부', 
                    ifelse(emp$DEPTNO==20, '재무부', '총무부'))


# 반복문 
# 1. for문

# 문법)
# for (반복변수 in 반복대상) {
#   반복 수행할 문장
# }

v1 <- c(1,2,4,5) 
v1 + 10

for (i in v1) {
  print(v1 + 10)
}

step1) i = 1
print(v1 + 10) # 11 12 14 15

step2) i = 2
print(v1 + 10) # 11 12 14 15

step3) i = 4
print(v1 + 10) # 11 12 14 15

step4) i = 5
print(v1 + 10) # 11 12 14 15


for (i in v1) {
  print(i + 10)
}


# if + for문의 결합
# if문은 조건의 결과가 하나여야 하므로
# for문을 통해 하나씩 전달, 반복연산 되도록 처리 필요

# 예제) emp 데이터에서 dname 컬럼 추가
# 10번 부서는 인사부, 20번은 재무부, 30번은 총무부(if + for문)

for (i in emp$DEPTNO) {
  if (i == 10) {
    print('인사부')
  } else if (i ==20) {
    print('재무부')
  } else {
    print('총무부')
  }
}


emp$dname2 <- for (i in emp$DEPTNO) {   # 오른쪽 수행결과가
                if (i == 10) {          # 벡터가 아니므로
                  print('인사부')       # 화면에 출력만 될뿐
                } else if (i ==20) {    # 컬럼에 추가 불가
                  print('재무부')
                } else {
                  print('총무부')
                }
              }

vdname <- c()

for (i in emp$DEPTNO) {  
  if (i == 10) {         
      vdname <- c(vdname, '인사부')
 } else if (i ==20) {   
      vdname <- c(vdname, '재무부')
 } else {
      vdname <- c(vdname, '총무부')
 }
}

emp$dname2 <- vdname


# 연습문제) 10번 부서의 경우 SAL의 10% 증가값을, 나머지는 20% 증가값
# NEW_SAL 컬럼에 추가(if + for)

vsal <- c()
for (vno in emp$DEPTNO) {
  if (vno == 10) {
    vsal <- c(vsal, emp$SAL * 1.1)
  } else (
    vsal <- c(vsal, emp$SAL * 1.2)
  )
}

length(vsal)  # 14 * 14

# 2) 위치변수 활용
vsal <- c()

for (nrow in 1:nrow(emp)) {
  if (emp$DEPTNO[nrow] == 10) {
    vsal <- c(vsal, emp$SAL[i] * 1.1)
  } else (
    vsal <- c(vsal, emp$SAL[i] * 1.2)
  )
}

length(vsal)  # 14 

# 연습문제) emp에서 sal이 1000이하면 C, 1000 초과 2000이하 B,
# 2000초과면 A 리턴, grade 컬럼에 추가
# 1) 반복대상이 특정 컬럼인 경우

vgrade <- c()

for (i in emp$SAL) {
  if (i <= 1000) {
    vgrade <- append(vgrade, 'C')
  } else if (i <= 2000) {
    vgrade <- append(vgrade, 'B')
  } else {
    vgrade <- append(vgrade, 'A')
  }
}

emp$grade <- vgrade


# 2) 반복대상이 행의 위치값인 경우 
#    주로 반복 처리해야할 컬럼이 여러개인 경우 사용
vgrade <- c()

for (i in 1:nrow(emp)) {
  if (emp$SAL[i] <= 1000) {
    vgrade <- append(vgrade, 'C')
  } else if (emp$SAL[i] <= 2000) {
    vgrade <- append(vgrade, 'B')
  } else {
    vgrade <- append(vgrade, 'A')
  }
}

emp$grade <- vgrade


# 2. while 문








# 문자열 함수 
# stringr 패키지 사용
install.packages('stringr')
library(stringr)


# 1. str_detect : 문자의 포함 여부 확인(패턴확인함수)
str_detect(strings = ,
           pattern = )

v1 <- c('abc','bcd','Abc', 'acd', 'cabd')
v2 <- c('ab12','1a34','cds','1234', 'cde!')
v3 <- c('abc','aabc','aaabc','aaaabc')

str_detect(v1, 'a')         # 'a'를 포함하는지 여부
str_detect(v1, '[aA]')      # 'a' 또는 'A'를 포함하는지 여부
str_detect(v1, '[aA][bB]')  # 'a' 또는 'A'를 포함하는지 여부

str_detect(v1, '^a')        # 'a'로 시작하는지 여부
str_detect(v1, 'd$')        # 'd'로 끝나는지 여부

str_detect(v2, '.a')        # 두번째 글자가 'a'인지 여부
str_detect(v2, '[0-9]')     # 숫자를 포함하는지 여부
str_detect(v2, '[:digit:]') # 숫자를 포함하는지 여부
str_detect(v2, '[a-zA-Z]')  # 영문을 포함하는지 여부
str_detect(v2, '[:alpha:]') # 영문을 포함하는지 여부
str_detect(v2, '[:punct:]') # 특수기호를 포함하는지 여부

str_detect(v3, 'a{2,4}')    # 'a'가 연속적으로 2회이상 4회이하 포함

# [ 연습 문제 ] emp 데이터에서
# 1) 이름이 S로 시작하는(대소 구분X) 직원의 이름, 연봉 출력
emp[str_detect(emp$ENAME, '^[sS]'), c('ENAME','SAL')] # like 'A%'

# 2) 이름의 세번째 글자가 A인 직원의 이름, 연봉 출력   
emp[str_detect(emp$ENAME, '..[A]'), c('ENAME','SAL')] # like '__A%'


# 2. str_locate : 문자열에서의 특정 패턴의 위치 리턴
str_locate(string = ,
           pattern = )

v1 <- c('a#b#c#', 'aa##b##')
str_locate(v1, '#')
str_locate(v1, '##')
str_locate_all(v1, '#')

# f_instr(email,'@',1,1)

# 3. str_count : 문자열에서의 특정 값의 포함 횟수 리턴
str_count(string = ,
          pattern = )

str_count(v3, 'a')

# 4. str_c : 분리되어진 문자열의 결합(concat)
str_c(..., 
      sep = , 
      collapse = NULL)

str_c('a','b','c')            # "abc"
str_c('a','b','c', sep=' ')   # "a b c"

v1 <- c('a','b','c')
v2 <- c('A','B','C')

str_c(v1,v2)
str_c(v1,v2,sep='_')        #  v1||'_'||v2


# 5. substr, str_sub : 문자열 추출
substr(x, start, stop)                # stop 생략 불가
stringr::str_sub(string, start, stop) # stop 생략 가능

v1 <- 'abcde'
substr(v1, 1,2)
substr(v1, 2,2)
str_sub(v1,2,2)

substr(v1, 2)
str_sub(v1,2)

