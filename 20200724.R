# 1. exam_01.csv 파일을 읽고, 
# f_hakjum함수를 생성하여 각 학생의 학번을 입력하면 
# 학점이 나오도록, 단 학번 컬럼 입력시 전체 출력도 가능하게
# (f_hakjum(9411) = A+)
# 95이상 A+
# 90이상 A
# 85이상 B+
# 80이상 B
# 나머지 C

exam[exam$STUDNO == 9411, 2]

exam <- read.csv('exam_01.csv')

f_hakjum <- function(x) {
  vjumsu <- exam[exam$STUDNO == x, 2]
  if (vjumsu >= 95) {
    'A+'
  } else if (vjumsu >= 90) {
    'A'
  } else if (vjumsu >= 85) {
    'B+'
  } else if (vjumsu >= 80) {
    'B'
  } else {
    'C'
  }
}

f_hakjum(9411)
f_hakjum(exam$STUDNO)          # 벡터 연산 불가
sapply(exam$STUDNO, f_hakjum)  # 벡터 연산 가능

f_hakjum <- function(x) {
  vhakjum <- c()
  for (i in x) {
    vjumsu <- exam[exam$STUDNO == i, 2]
    if (vjumsu >= 95) {
      vhakjum <- c(vhakjum, 'A+')
    } else if (vjumsu >= 90) {
      vhakjum <- c(vhakjum, 'A')
    } else if (vjumsu >= 85) {
      vhakjum <- c(vhakjum, 'B+')
    } else if (vjumsu >= 80) {
      vhakjum <- c(vhakjum, 'B')
    } else {
      vhakjum <- c(vhakjum, 'C')
    }
  }
  return(vhakjum)
}

f_hakjum(exam$STUDNO) 


# 2. emp.csv 파일을 읽고,
# f_bonus라는 함수를 생성, 각 직원의 보너스를 출력
# (입사일이 1985년 이전인 경우는 근속년수 * 100, 
# 이후인 경우는 90으로 계산)
emp <- read.csv('emp.csv', stringsAsFactors = F)

emp$HIREDATE <- as.Date(emp$HIREDATE)

f_bonus(7369)

# step1) 7369의 보너스를 출력
emp$year <- as.numeric(as.character(emp$HIREDATE,'%Y'))
emp$hyear <- trunc((Sys.Date() - emp$HIREDATE)/365)

vyear <- emp[emp$EMPNO == 7369, 'year']
vhyear <- as.numeric(emp[emp$EMPNO == 7369, 'hyear'])

if (vyear < 1985) {
  vhyear * 100
} else {
  vhyear * 90
}

# step2) 사용자 정의 함수 생성
f_bonus <- function(x) {
  vyear <- emp[emp$EMPNO == x, 'year']
  vhyear <- as.numeric(emp[emp$EMPNO == x, 'hyear'])
  
  if (vyear < 1985) {
    vhyear * 100
  } else {
    vhyear * 90
  }
}

f_bonus(7369)
f_bonus(7499)
f_bonus(emp$EMPNO)          # 벡터 연산 불가
sapply(emp$EMPNO, f_bonus)  # 벡터 연산 가능

emp$hyear <- as.numeric(as.character(emp$HIREDATE,'%Y'))
f_bonus <- function(x) {    # 벡터 연산 가능
  bonus <- c()
  for (i in x) {
    if (emp[emp$EMPNO == i, 'hyear'] < 1985) {
      bonus <- c(bonus,(as.numeric(as.character(Sys.Date(),'%Y')) - emp[emp$EMPNO == i, 'hyear']) * 100)
    } else {
      bonus <- c(bonus,(as.numeric(as.character(Sys.Date(),'%Y')) - emp[emp$EMPNO == i, 'hyear']) * 90)
    }
  }
  return(bonus)
}

# 3. 다음의 사용자 정의함수 생성
# f_split(string, sep, n)
library(stringr)

str_split('a;b;c',';')[[1]][2]

# 1) input - scalar
f_split <- function(string, sep, n=1) {
  str_split(string,sep)[[1]][n]
}

f_split('a;b;c',';',3)

# 1) input - vector
f_split2 <- function(string, sep, n=1) {
  vresult <- c()
  for (i in string) {
    vresult <- c(vresult, str_split(i,sep)[[1]][n])
  }
  return(vresult)
}

f_split2(c('a;b;c','A;B;C;D'),';',1)

pro <- read.csv('professor.csv', stringsAsFactors = F)
f_split2(pro$EMAIL,'@')
sapply(list, function, ...)

sapply(pro$EMAIL, f_split, '@', 1)

f_split3 <- function(string, sep, n) {
  library(stringr)
  word <- c()
  for (i in 1:length(string)) {
    word <- c(word, str_split(string, sep)[[i]][n])
  }
  return(word)
}

# 4. oracle의 translate함수 구현(f_translate로 생성)
# (단, 두번째와 세번째 인자의 길이 같을 경우만 고려)

f(x,y,z)
sapply(x,f,y,z)  O
sapply(x,f(y,z)) X

# translate('abcba', 'ab', 'AB') => 'ABcBA'
# translate('abcba', 'ab', 'A') => 'AcA

# f_translate(string, old, new)

# step1) old, new에 들어오는 문자열을 한 글자씩 분리하기
# sol1) str_sub
str_sub('ab',1,1)
str_sub('ab',2,2)
str_length('ab')

# sol2) str_split
str_split('ab','')[[1]][1]
str_split('ab','')[[1]][2]
str_length('ab')

# step2) old, new의 분리된 문자열로 치환 반복
f_translate <- function(string, old, new) {
  vold <- str_split(old,'')[[1]]
  vnew <- str_split(new,'')[[1]]
  vn <- length(vold)
  for (i in 1:vn) {
    string <- str_replace_all(string,vold[i],vnew[i])
  }
  return(string)
}

f_translate <- function(string, before, after) {               # 벡터 연산 불가
  library(stringr)
  for (i in 1:str_length(before)) {
    if (str_detect(string, str_sub(before,i,i))) {
      string <- str_replace_all(string, str_sub(before,i,i), str_sub(after,i,i))
    }
  }
  return(string)
}

f_translate('abcba','ab','AB')
f_translate(c('abcba','ababc'),'ab','AB')

########## 여기까지는 복습입니다. ##########

df1 <- as.data.frame(matrix(1:25, ncol = 5))
df1[1,1] <- NA
df1[2,2:3] <- NA
df1[3,3:5] <- NA
df1[4,1:4] <- NA

# 연습문제) 데이터 프레임에서 각 행에 NA를 n개 이상 포함하는 경우
# 삭제하여 리턴하도록 사용자 정의함수 생성
# f_dropna(df1,3)

is.na(df1)

# 행별 NA의 총 개수
sum(is.na(df1))  # 전체 NA 개수 

# 1) for문을 사용하여 각 행별 카운트
vcnt <- c()
for (i in 1:5) {
  vcnt <- c(vcnt, sum(is.na(df1[i,])))
}

df1[!vcnt >= 3, ]

# 2) apply : 행별, 열별 특정 함수의 적용
apply(array,   # 2차원 데이터 
      margin,  # 방향(1:행별, 2:열별)
      ...)     # 적용함수와 추가 인자

vcnt2 <- apply(is.na(df1),1,sum)
df1[!vcnt2 >= 3, ]

# 사용자 정의 함수 생성
f_dropna <- function(x, n) {
  vcnt <- c()
  for (i in 1:nrow(x)) {
    vcnt <- c(vcnt, sum(is.na(x[i,])))
  }
  x <- x[!vcnt >= n, ]
  return(x)
} 

f_dropna(df1,1)
f_dropna(df1,2)

# self call(재귀함수)
f1 <- function(x) {
  f1(x-1) + x
}

f1(10)    # 무한 루프 발생, 에러

# f1(10) = f1(9) + 10
#        = f1(8) + 9 + 10
#        = f1(7) + 8 + 9 + 10
#        .....
#        = f1(1) + 2 + ... + 8 + 9 + 10   # stop point 필요, f1(1)=1
#        = f1(0) + 1 + ... + 10
#        = f1(-1) + ......

# stop point 지정
f1 <- function(x) {
  if(x==1) {
    1
  } else {
    f1(x-1) + x
  }
}

f1(10)


# [ 연습문제 ] 
# 팩토리얼 함수를 self call 형태로 생성 f_fac 
f2 <- function(x) {
  if(x==1) {
    1
  } else {
    f2(x-1) * x
  }
}

f2(10)
factorial(10)

# f1(10) = f1(9) * 10
#        = f1(8) * 9 * 10
#        ...
#        = f1(2) * 3 * 4 * ... * 10
#        = f1(1) * 2 * 3 * ... * 10
#        = 1 *  2 * 3 * ... * 10

# [ 연습문제 ]
# 피보나치 수열을 self call 형식으로 생성
# 1 1 2 3 5 8 13 21 ...
# f(1) f(2)  f(3)  f(4)  f(5)
# 1     1    1+1   1+2   2+3

f_fibo <- function(x) {
  if ((x == 1) | (x == 2)) {
    1
  } else {
    f_fibo(x-1) + f_fibo(x-2)
  }
}

# f_fibo(5) = f_fibo(4) + f_fibo(3)
#           = f_fibo(3) + f_fibo(2) + f_fibo(2) + f_fibo(1)
#           = f_fibo(2) + f_fibo(1) + f_fibo(2) + f_fibo(2) + f_fibo(1)

f_fibo(5)
f_fibo(4)
f_fibo(8)







