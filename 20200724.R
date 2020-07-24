# 1. exam_01.csv ������ �а�, 
# f_hakjum�Լ��� �����Ͽ� �� �л��� �й��� �Է��ϸ� 
# ������ ��������, �� �й� �÷� �Է½� ��ü ��µ� �����ϰ�
# (f_hakjum(9411) = A+)
# 95�̻� A+
# 90�̻� A
# 85�̻� B+
# 80�̻� B
# ������ C

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
f_hakjum(exam$STUDNO)          # ���� ���� �Ұ�
sapply(exam$STUDNO, f_hakjum)  # ���� ���� ����

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


# 2. emp.csv ������ �а�,
# f_bonus��� �Լ��� ����, �� ������ ���ʽ��� ���
# (�Ի����� 1985�� ������ ���� �ټӳ�� * 100, 
# ������ ���� 90���� ���)
emp <- read.csv('emp.csv', stringsAsFactors = F)

emp$HIREDATE <- as.Date(emp$HIREDATE)

f_bonus(7369)

# step1) 7369�� ���ʽ��� ���
emp$year <- as.numeric(as.character(emp$HIREDATE,'%Y'))
emp$hyear <- trunc((Sys.Date() - emp$HIREDATE)/365)

vyear <- emp[emp$EMPNO == 7369, 'year']
vhyear <- as.numeric(emp[emp$EMPNO == 7369, 'hyear'])

if (vyear < 1985) {
  vhyear * 100
} else {
  vhyear * 90
}

# step2) ����� ���� �Լ� ����
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
f_bonus(emp$EMPNO)          # ���� ���� �Ұ�
sapply(emp$EMPNO, f_bonus)  # ���� ���� ����

emp$hyear <- as.numeric(as.character(emp$HIREDATE,'%Y'))
f_bonus <- function(x) {    # ���� ���� ����
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

# 3. ������ ����� �����Լ� ����
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

# 4. oracle�� translate�Լ� ����(f_translate�� ����)
# (��, �ι�°�� ����° ������ ���� ���� ��츸 ����)

f(x,y,z)
sapply(x,f,y,z)  O
sapply(x,f(y,z)) X

# translate('abcba', 'ab', 'AB') => 'ABcBA'
# translate('abcba', 'ab', 'A') => 'AcA

# f_translate(string, old, new)

# step1) old, new�� ������ ���ڿ��� �� ���ھ� �и��ϱ�
# sol1) str_sub
str_sub('ab',1,1)
str_sub('ab',2,2)
str_length('ab')

# sol2) str_split
str_split('ab','')[[1]][1]
str_split('ab','')[[1]][2]
str_length('ab')

# step2) old, new�� �и��� ���ڿ��� ġȯ �ݺ�
f_translate <- function(string, old, new) {
  vold <- str_split(old,'')[[1]]
  vnew <- str_split(new,'')[[1]]
  vn <- length(vold)
  for (i in 1:vn) {
    string <- str_replace_all(string,vold[i],vnew[i])
  }
  return(string)
}

f_translate <- function(string, before, after) {               # ���� ���� �Ұ�
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

########## ��������� �����Դϴ�. ##########

df1 <- as.data.frame(matrix(1:25, ncol = 5))
df1[1,1] <- NA
df1[2,2:3] <- NA
df1[3,3:5] <- NA
df1[4,1:4] <- NA

# ��������) ������ �����ӿ��� �� �࿡ NA�� n�� �̻� �����ϴ� ���
# �����Ͽ� �����ϵ��� ����� �����Լ� ����
# f_dropna(df1,3)

is.na(df1)

# �ະ NA�� �� ����
sum(is.na(df1))  # ��ü NA ���� 

# 1) for���� ����Ͽ� �� �ະ ī��Ʈ
vcnt <- c()
for (i in 1:5) {
  vcnt <- c(vcnt, sum(is.na(df1[i,])))
}

df1[!vcnt >= 3, ]

# 2) apply : �ະ, ���� Ư�� �Լ��� ����
apply(array,   # 2���� ������ 
      margin,  # ����(1:�ະ, 2:����)
      ...)     # �����Լ��� �߰� ����

vcnt2 <- apply(is.na(df1),1,sum)
df1[!vcnt2 >= 3, ]

# ����� ���� �Լ� ����
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

# self call(����Լ�)
f1 <- function(x) {
  f1(x-1) + x
}

f1(10)    # ���� ���� �߻�, ����

# f1(10) = f1(9) + 10
#        = f1(8) + 9 + 10
#        = f1(7) + 8 + 9 + 10
#        .....
#        = f1(1) + 2 + ... + 8 + 9 + 10   # stop point �ʿ�, f1(1)=1
#        = f1(0) + 1 + ... + 10
#        = f1(-1) + ......

# stop point ����
f1 <- function(x) {
  if(x==1) {
    1
  } else {
    f1(x-1) + x
  }
}

f1(10)


# [ �������� ] 
# ���丮�� �Լ��� self call ���·� ���� f_fac 
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

# [ �������� ]
# �Ǻ���ġ ������ self call �������� ����
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






