# 1. professor.csv 파일을 읽고
df1 <- read.csv('professor.csv')
library(stringr)

# 1) 교수번호가 40으로 시작하는 교수의 이름, 교수번호, pay 출력
df1[str_detect(df1$PROFNO, '^40'), c('NAME','PROFNO','PAY')]
# ^$.{2,4}  '\\)' []

# 2) email_id라는 각 교수의 이메일 아이디를 담는 컬럼 생성
# 1) 위치기반
vno <- str_locate(df1$EMAIL, '@')[,1]
df1$email_id <- str_sub(df1$EMAIL, 1, vno-1)

# 2) 분리기반
str_split(df1$EMAIL,'@')[[1]][1]
str_split(df1$EMAIL,'@')[[2]][1]

vid <- c()

for (i in 1:nrow(df1)) {
  vid <- c(vid, str_split(df1$EMAIL,'@')[[i]][1])
}

# 3) POSITION의 전임강사를 부교수로 수정
# sol1) level 수정
levels(df1$POSITION)[1] <- '부교수'

# sol2) 값 직접 수정
df1$POSITION[df1$POSITION == '정교수'] <- '교수'     # NA 삽입
levels(df1$POSITION) <- c('부교수','교수','조교수')
df1$POSITION[is.na(df1$POSITION)] <- '교수'

# 4) 홈페이지 주소가 없는 경우 다음과 같이 수정
#    http://www.kic.com/email_id
is.na(df1$HPAGE)         # NA가 아님
str_length(df1$HPAGE)    # 빈 문자열(길이가 0) 삽입 확인
df1$HPAGE == ''

df1$HPAGE <- as.character(df1$HPAGE)

df1$HPAGE2 <- ifelse(df1$HPAGE=='', 
                     str_c('http://www.kic.com/',df1$email_id),
                     df1$HPAGE)

vid2 <- c()

for (i in 1:16) {
  if (df1$HPAGE[i] == '') {
    vid2 <- c(vid2, str_c('http://www.kic.com/', df1$email_id[i]))
  } else {
    vid2 <- c(vid2, df1$HPAGE[i])
  }
}

# 5) 각 입사년도별 최대연봉을 받는 직원의 이름, 입사일, 연봉 출력
df1$HIREDATE <- as.Date(df1$HIREDATE)
df1$HYEAR <- as.numeric(as.character(df1$HIREDATE, '%Y'))

max(df1[df1$HYEAR == 1980, 'PAY'])
max(df1[df1$HYEAR == 1987, 'PAY'])
...
max(df1[df1$HYEAR == 2001, 'PAY'])

vmax <- c()

for (i in df1$HYEAR) {
  vmax <- c(vmax, max(df1[df1$HYEAR == i, 'PAY']))
}

df1$MAX_PAY <- vmax
df1[, c('NAME','HYEAR', 'PAY', 'MAX_PAY')]

df1[df1$PAY == df1$MAX_PAY, c('NAME', 'HIREDATE', 'PAY')]

# 2. data2.csv를 읽고
df2 <- read.csv('data2.csv')
# 1) 4호선 라인의 전체 시간의 승차의 총합
sum(df2$승차)

df2$승차 <- as.numeric(str_remove_all(df2$승차,','))
df2$하차 <- as.numeric(str_remove_all(df2$하차,','))

str(df2)

sum(df2[df2$노선번호 == 'line_4', '승차'])

# 2) 1호선 라인의 9시~12시 시간대까지의 하차의 총합
# sol1) 시간의 길이에 따른 서로 다른 개수 추출
# sol2) 시간 컬럼의 뒤의 두자 추출로 시간 가공
str_sub('607',-2)
str_sub('0607',-2)

df2$TIME <- as.numeric(str_sub(df2$시간, -2)) - 1

# sol3) pad함수로 시간을 4자리로 만든 후 앞에서 두자 추출
str_pad(string,                          # 원본 문자열
        width = ,                        # 총 길이
        side = c('left','right','both'), # 삽입 방향
        pad = '')                        # 삽입문자

str_pad('506', 4, 'left', '0')

# 3. emp.csv 파일을 읽고
# 각 직원의 상위관리자 이름을 mgr_name이라는 컬럼으로 추가
# (단, 상위관리자가 없는 경우 NA)
emp <- read.csv('emp.csv', stringsAsFactors = F)

emp[emp$EMPNO == 7902, 'ENAME']
emp[emp$EMPNO == 7968, 'ENAME']
..
emp[emp$EMPNO == NA, 'ENAME']
emp[emp$EMPNO == 7782, 'ENAME']

vmgr <- c()

for (i in emp$MGR) {
  if (is.na(i)) {
    vmgr <- c(vmgr, 'NA')
  } else {
    vmgr <- c(vmgr, emp[emp$EMPNO == i, 'ENAME'])
  }
}

emp$mgr_name <- vmgr

########## 여기까지는 복습입니다. ##########

# 반복 제어문 : 반복문의 흐름을 제어하는 구문
# 1. next : 반복문내 next 뒤에 실행되는 문장스킵(in python continue)

for (i in 1:10) {
  cmd1            # 10
  if (i==5) {
    next
  }
  cmd2            # 9
  cmd3            # 9
}
cmd4              # 1

# 2. break : 반복문 즉시 종료(in python 동일)
for (i in 1:10) {
  cmd1            # 5
  if (i==5) {
    break
  }
  cmd2            # 4
}
cmd3              # 1

# 3. quit : 프로그램 즉시 종료(in python exit)
for (i in 1:10) {
  cmd1            # 5
  if (i==5) {
    quit('no')
  }
  cmd2            # 4
}
cmd3              # 0



# 예제) 1~100중 짝수의 합 출력(for문 사용, 반복제어문 사용)
vsum <- 0

for (i in 1:100) {
  if (i%%2 != 0) {
    next
  }
  vsum <- vsum + i
}

vsum <- 0

for (i in 1:100) {
  if (i%%2 == 0) {
    vsum <- vsum + i
  }
}



# 연습문제)
# 1) 다음의 벡터에 반복문을 사용하여 가격의 누적 총 합 리턴
v1 <- c(1000,1500,NA,3000,4000)

vsum <- 0

for (i in v1) {
  if (is.na(i)) {
    next
  }
  vsum <- vsum + i
}

vsum 

# i     vsum
# 1000  1000
# 1500  1000 + 1500
# NA    skip
# 3000  1000 + 1500 + 3000
# 4000  1000 + 1500 + 3000 + 4000

# 2) NA 이전까지 print
for (i in v1) {
  if (is.na(i)) {
    break
  }
  print(i)
}

# 사용자 정의 함수 : 사용자가 직접 만드는 함수
# Y = f(x,y,z)

# [ 문법 ]
함수명 <- function(...) {
  cmd1
  cmd2
  ...
  return(object)
}

# 예제) input값의 10을 더한 값 리턴하는 함수 생성
f_add1 <- function(x) {
  return(x+10)
}

f_add1(x=10)

# 예제) 세개 수를 전달 받아 (x + y)*z 형태의 값 리턴하는 함수 생성
f_add2 <- function(x,y,z=1) {
  vresult <- (x + y)*z
  return(vresult)
}

f_add2(5,4,10)
f_add2(y=4,z=10,x=5)
f_add2(5,4)


v1 <- c(1,2,3)
v2 <- c(10,20,30)
v3 <- c(2,4,6)

f_add2(v1,v2,v3)

# 예제) sign의 기능을 갖는 f_sign 함수 생성
# f_sign(10) -> 1
v1 <- -10

if (v1 > 0) {
  1
} else if (v1 < 0) {
  -1
} else {
  0
}

f_sign <- function(x) {
  if (x > 0) {
    return(1)
  } else if (x < 0) {
    return(-1)
  } else {
    return(0)
  }
}

f_sign <- function(x) {
  if (x > 0) {
    vresult <- 1
  } else if (x < 0) {
    vresult <- -1
  } else {
    vresult <-0
  }
  return(vresult)
}

f_sign(0)               # x가 스칼라인 경우 연산 정상 수행
f_sign(c(0,500,-5,2,9)) # x가 벡터인 경우 반복 수행 불가

# 연습문제) 위 함수가 벡터연산이 가능하도록 함수 본문 수정
# sol1) 함수 생성 없이 if + for문 수행
vresult <- c()

for (i in c(0,500,-5,2,9)) {
  if (i > 0) {
    vresult <- c(vresult, 1)
  } else if (i < 0) {
    vresult <- c(vresult, -1)
  } else {
    vresult <- c(vresult, 0)
  }
}
  
# sol2) 반복문을 갖는 함수 생성
f_sign2 <- function(x) {
  vresult <- c()
  
  for (i in x) {
    if (i > 0) {
      vresult <- c(vresult, 1)
    } else if (i < 0) {
      vresult <- c(vresult, -1)
    } else {
      vresult <- c(vresult, 0)
    }
  }
  return(vresult)
}

f_sign2(c(0,500,-5,2,9))

# sol3) 사용자 정의함수를 적용함수에 전달(반복문 대체 연산)
f_sign(c(0,500,-5,2,9))          # 벡터연산 불가
sapply(c(0,500,-5,2,9), f_sign)  # 벡터연산 가능
sapply(list, function)

# [ 연습 문제 ]
# sal값이 입력되면 등급을 출력하는 사용자 정의 함수 생성
# 1000미만 'C' 1000이상 3000 미만 'B', 3000 이상 'A'
# 1) 반복문 갖는 사용자 정의함수
f_sal <- function(x) {
  vsal <- c()
  for (i in x) {
    if (i < 1000) {
      vsal <- c(vsal, 'C')
    } else if (i < 3000) {
      vsal <- c(vsal, 'B')
    } else {
      vsal <- c(vsal, 'A')
    }
  }
  return(vsal)
}

f_sal(emp$SAL) 

# 2) 반복문 없는 사용자 정의함수 + sapply
# f_sal(3000)
v1 <- 3000

if (v1 < 1000) {
  'C'
} else if (v1 < 3000) {
  'B'
} else {
  'A'
}

f_sal <- function(x) {
  if (x < 1000) {
    return('C')
  } else if (x < 3000) {
    return('B')
  } else {
    return('A')
  }
}

f_sal(3000)
f_sal(emp$SAL)          # 벡터연산 불가
sapply(emp$SAL, f_sal)  # 벡터연산 가능

# [ 연습 문제 ]
# emp 데이터에서 보너스를 계산해주는 사용자 정의함수 생성
# 보너스는 10번 부서는 SAL의 10%, 20번은 15%, 30번은 20% 리턴
# step1) 먼저 7902직원의 보너스를 출력하는 수식 작성
vsal <- emp[emp$EMPNO == 7902, 'SAL']
vdeptno <- emp[emp$EMPNO == 7902, 'DEPTNO']

if (vdeptno == 10) {
  vsal * 1.1
} else if (vdeptno == 20) {
  vsal * 1.15
} else {
  vsal * 1.2
}


# step2) 사용자 정의 함수 본문에 위 수식 전달, 7902 자리를 x로 변경

f_bonus <- function(x) {
  vsal <- emp[emp$EMPNO == x, 'SAL']
  vdeptno <- emp[emp$EMPNO == x, 'DEPTNO']
  
  if (vdeptno == 10) {
    return(vsal * 1.1)
  } else if (vdeptno == 20) {
    return(vsal * 1.15)
  } else {
    return(vsal * 1.2)
  }
}

f_bonus(7902)
f_bonus(7369)
f_bonus(emp$EMPNO)            # 경고 발생, 정확한 벡터 연산 수행X
sapply(emp$EMPNO, f_bonus)    # 벡터 연산 가능

# [ 연습 문제 ]
# 부서번호에 따른 부서명 출력 함수 생성 및 적용
# 10번이면 인사부, 20은 재무부, 30은 총무부
f_dname <- function(x) {
  if (x == 10) {
    return('인사부')
  } else if (x == 20) {
    return('재무부')
  } else {
    return('총무부')
  }
}

f_dname(10)
f_dname(emp$DEPTNO)           # 벡터연산 불가
sapply(emp$DEPTNO, f_dname)   # 벡터연산 가능

# [ 사용자 정의함수 활용 예제 ]
c('a#b#c', 'A#B#C')
f_split(string, sep, n)
f_split(df1$EMAIL, '@', 1)















