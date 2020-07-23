# 1. emp.csv ������ �а�
emp <- read.csv('emp.csv')

# 1) 10�� �μ� ������ comm�� ����� ���
#    (��, NA�� ���� 100 �Ҵ�, NA ������ ifelse�� �Ǵ� for�� Ȱ��)
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

# 2) ��ݱ� �Ի��� ����� ������ 10%, �Ϲݱ�� 15% bonus �÷��� �߰�
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

# 2. student.csv ������ �а�
std <- read.csv('student.csv')

# 1) DEPTNO��� �÷��� �����ϵ�, ��2������ �ִ� ���� 
#    ��2������ȣ��, ���� ���� ��1������ȣ ���
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

# 2) ������ȣ ����
library(stringr)

# step1) ')' ��ġ Ȯ��
v1 <- str_locate(std$TEL, '\\)')
v2 <- v1[,1]

# step2) ����
str_sub(std$TEL, 1, v1-1)
str_sub(std$TEL, 1, v2-1)

# 3. emp.csv ������ �а�
# �� ������ deptno���� Ȯ���Ͽ� ���� �μ� �������� ��� ������ 
# �� �ึ�� ������ ���� ���(mean() : ��� ���ϴ� �Լ�)

#                                                          �μ������
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

# �� �μ����� �� �μ��� ��տ������� ���� ������ �޴� ������
#  �̸�, JOB, SAL, DEPTNO ���

emp[, c('SAL','SAL_MEAN')]
emp[emp$SAL > emp$SAL_MEAN, c('ENAME','JOB','SAL','DEPTNO')]

########## ��������� �����Դϴ�. ##########

library(stringr)

# 6. str_length(strings) : ���ڿ��� ���� ����
v1 <- c('abc','aaab','231vg')

length(v1)      # 3, ������ ������ ����
str_length(v1)  # 3 4 5, ������ �� ������ ���ڿ��� ũ��


# [ ���� ���� ]
# emp.csv������ �а� �Ʒ��� ���� �������� ���
# 'SMITH�� 10% �λ�� ������ 880�̴�.'

str_c(emp$ENAME, '�� 10% �λ�� ������ ', emp$SAL*1.1, '�̴�.')

# [ ���� ���� ]
# student.csv������ �а� ID�� ���ڰ� 2ȸ�̻� �ݺ��� 
# �л� ������ ����(�����)
v1 <- c('a123','a2b6')

str_count(v1, '[0-9]') >= 2  # ������ �ݺ��� Ȯ�� �Ұ�
str_detect(v1,'[0-9][0-9]')
str_detect(v1,'[0-9]{2,}')

v1[!str_detect(v1,'[0-9]{2,}')]

std$ID[str_detect(std$ID,'[0-9]{2,}')] # ID Ȯ��

std[!str_detect(std$ID,'[0-9]{2,}'), ]


# 7. str_replace : ġȯ�Լ�
str_replace(string = ,       # ���� ���ڿ�
            pattern = ,      # ã�� ���ڿ�
            replacement = )  # �ٲ� ���ڿ�

str_replace('abcba12','ab','AB')      # �ܾ� ġȯ(���� �� ���� ġȯX)
str_replace('abcba12','ab','')        # ����
str_remove('abcba12','ab')            # remove �Լ��ε� ���� ����
str_replace_all('abcba12','[ab]','')  # ã�� ���ڿ� ���Խ� ǥ�� ����

v1 <- c('12ab', NA, 'abc')
str_replace_all(v1,'[0-9]','a')       # ������ ���Һ� ġȯ ����
str_replace_all(v1,NA,'a')            # NA�� ġȯ �Ұ�
str_replace_all(v1,'abc',NA)          # NA�� ġȯ �Ұ�

v2 <- c('a','b','c')
str_replace_all(v2,'a',0)             # ���ڷθ� ġȯ ����

# [ NA ġȯ ��� ]
v1 <- c('12ab', NA, 'abc')
v1[is.na(v1)] <- 'aa'        # NA�� ���� �� ���� ���
ifelse(is.na(v1), 'aa', v1)  # ��ü ���� ���� ���
str_replace_na(v1, 'aa')     # sql�� NVL�� ���


# [ ���� ���� ]
# ������ ������ 10%�λ�� �� ���
v_sal <- c('1,200','5,000','10,003,300')
v_sal * 1.1

class(v_sal)
as.numeric(str_replace_all(v_sal,',','')) * 1.1

# 8. ���ġȯ
str_to_upper('abc')   # upper
str_to_lower('ABC')   # lower
str_to_title('abc')   # initcap

toupper('abc')
tolower('ABC')

# 9. str_split : ���ڿ� �и�
str_split(string = ,
          pattern = ,
          n = )

a1 <- 'a#b#c#'
str_split(a1, '#')          # ����Ʈ�� ���
str_split(a1, '#')[[1]][2]  # �и��� Ư�� ��ġ ���� ����

# �������� ) 
# vtel ���� ���� ����(034)
vtel <- '02)034-1234'

# 1) ')', '-' ��ġ���
vno1 <- str_locate(vtel, '\\)')[,1]
vno2 <- str_locate(vtel, '-')[,1]

str_sub(vtel, vno1+1, vno2-1)

# 2) �и����(split ���)
vtel2 <- str_split(vtel, '\\)')[[1]][2]
str_split(vtel2, '-')[[1]][1]

# ��������) student.csv ������ TEL �÷����� �� ���� ���� ����
# 1) ��ġ���
vno1 <- str_locate(std$TEL, '\\)')[,1]
vno2 <- str_locate(std$TEL, '-')[,1]

str_sub(std$TEL, vno1+1, vno2-1)

# 2) �и����***
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


# �ݺ���
# 1. for��
# - ������ �ݺ� Ƚ��
# - �ݺ������ �ڵ����� �������� �Ѿ

for (���� in �ݺ����) {
  �ݺ��� ����
}

# 2. while��
# - �ݺ� Ƚ���� ������ ���� ����(������ ������ �ɶ����� ���� �ݺ�)
# - �ݺ������ �ڵ����� �Ѿ�� �����Ƿ� �ѱ�� �۾� ���� �ʿ�
# - �ݺ������ �ʱⰪ�� ���� �ʿ�

# [ ���� ]
while (����) {
  �ݺ��� ����
}

# ����) 1���� 10������ ���ڿ� 10�� ���� ���
i <- 1

while (i <= 10) {
  print(i + 10)
  i <- i + 1
}


# [ ���� ���� ] 1 ~ 100������ �� �� ���
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


# [ ���� ���� ] 1 ~ 100������ �� �� ���(¦����)
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


# factor ����
# - ������ ����(level)�� ������ �ִ� ����
# - �ַ� ������ ���
# - read.csv�� �ҷ��ö� ���� Ÿ���� �÷��� �ڵ����� factor�� ����

# 1. ����
f1 <- factor('m', c('m','f'))
f2 <- factor('m', c('m','f'), ordered = T)
f3 <- factor('m', c('f','m'), ordered = T)
f4 <- ordered('m', c('f','m'))

# 2. level Ȯ��
levels(f1)

# 3. factor ���� <-> �Ϲݺ���
v1 <- c('a','b','c')
v2 <- as.factor(v1)
v3 <- as.character(v2)

# 4. factor ������ ����
v2[3] <- 'd'                      # �����߻�, NA�� ġȯ 
levels(v2) <- c('a','b','c','d')  # ���� Ȯ��
v2[3] <- 'd'                      # �������

levels(v2) <- toupper(c('a','b','c','d')) # ���� ������ ���� �� ����

# 5. ���ڿ� �÷��� �ڵ����� factor�� �����Ǵ� ���
# 1) read.csv, read.table
emp <- read.csv('emp.csv')
str(emp)

emp <- read.csv('emp.csv', stringsAsFactors = F)
str(emp)

# 2) cbind�� ���� �÷� �߰� ��

# ����) emp �������� �̸��� �� �� ���ڸ� ����, ���ο� �÷��� �߰�
emp$ENAME1 <- str_sub(emp$ENAME,1,2)
emp <- cbind(emp, 'ENAME2'=str_sub(emp$ENAME,1,2))
emp <- cbind(emp, 'ENAME3'=str_sub(emp$ENAME,1,2), 
             stringsAsFactors=F)
str(emp)
