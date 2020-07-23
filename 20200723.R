# 1. professor.csv ������ �а�
df1 <- read.csv('professor.csv')
library(stringr)

# 1) ������ȣ�� 40���� �����ϴ� ������ �̸�, ������ȣ, pay ���
df1[str_detect(df1$PROFNO, '^40'), c('NAME','PROFNO','PAY')]
# ^$.{2,4}  '\\)' []

# 2) email_id��� �� ������ �̸��� ���̵� ��� �÷� ����
# 1) ��ġ���
vno <- str_locate(df1$EMAIL, '@')[,1]
df1$email_id <- str_sub(df1$EMAIL, 1, vno-1)

# 2) �и����
str_split(df1$EMAIL,'@')[[1]][1]
str_split(df1$EMAIL,'@')[[2]][1]

vid <- c()

for (i in 1:nrow(df1)) {
  vid <- c(vid, str_split(df1$EMAIL,'@')[[i]][1])
}

# 3) POSITION�� ���Ӱ��縦 �α����� ����
# sol1) level ����
levels(df1$POSITION)[1] <- '�α���'

# sol2) �� ���� ����
df1$POSITION[df1$POSITION == '������'] <- '����'     # NA ����
levels(df1$POSITION) <- c('�α���','����','������')
df1$POSITION[is.na(df1$POSITION)] <- '����'

# 4) Ȩ������ �ּҰ� ���� ��� ������ ���� ����
#    http://www.kic.com/email_id
is.na(df1$HPAGE)         # NA�� �ƴ�
str_length(df1$HPAGE)    # �� ���ڿ�(���̰� 0) ���� Ȯ��
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

# 5) �� �Ի�⵵�� �ִ뿬���� �޴� ������ �̸�, �Ի���, ���� ���
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

# 2. data2.csv�� �а�
df2 <- read.csv('data2.csv')
# 1) 4ȣ�� ������ ��ü �ð��� ������ ����
sum(df2$����)

df2$���� <- as.numeric(str_remove_all(df2$����,','))
df2$���� <- as.numeric(str_remove_all(df2$����,','))

str(df2)

sum(df2[df2$�뼱��ȣ == 'line_4', '����'])

# 2) 1ȣ�� ������ 9��~12�� �ð�������� ������ ����
# sol1) �ð��� ���̿� ���� ���� �ٸ� ���� ����
# sol2) �ð� �÷��� ���� ���� ����� �ð� ����
str_sub('607',-2)
str_sub('0607',-2)

df2$TIME <- as.numeric(str_sub(df2$�ð�, -2)) - 1

# sol3) pad�Լ��� �ð��� 4�ڸ��� ���� �� �տ��� ���� ����
str_pad(string,                          # ���� ���ڿ�
        width = ,                        # �� ����
        side = c('left','right','both'), # ���� ����
        pad = '')                        # ���Թ���

str_pad('506', 4, 'left', '0')

# 3. emp.csv ������ �а�
# �� ������ ���������� �̸��� mgr_name�̶�� �÷����� �߰�
# (��, ���������ڰ� ���� ��� NA)
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

########## ��������� �����Դϴ�. ##########

# �ݺ� ��� : �ݺ����� �帧�� �����ϴ� ����
# 1. next : �ݺ����� next �ڿ� ����Ǵ� ���彺ŵ(in python continue)

for (i in 1:10) {
  cmd1            # 10
  if (i==5) {
    next
  }
  cmd2            # 9
  cmd3            # 9
}
cmd4              # 1

# 2. break : �ݺ��� ��� ����(in python ����)
for (i in 1:10) {
  cmd1            # 5
  if (i==5) {
    break
  }
  cmd2            # 4
}
cmd3              # 1

# 3. quit : ���α׷� ��� ����(in python exit)
for (i in 1:10) {
  cmd1            # 5
  if (i==5) {
    quit('no')
  }
  cmd2            # 4
}
cmd3              # 0



# ����) 1~100�� ¦���� �� ���(for�� ���, �ݺ���� ���)
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



# ��������)
# 1) ������ ���Ϳ� �ݺ����� ����Ͽ� ������ ���� �� �� ����
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

# 2) NA �������� print
for (i in v1) {
  if (is.na(i)) {
    break
  }
  print(i)
}

# ����� ���� �Լ� : ����ڰ� ���� ����� �Լ�
# Y = f(x,y,z)

# [ ���� ]
�Լ��� <- function(...) {
  cmd1
  cmd2
  ...
  return(object)
}

# ����) input���� 10�� ���� �� �����ϴ� �Լ� ����
f_add1 <- function(x) {
  return(x+10)
}

f_add1(x=10)

# ����) ���� ���� ���� �޾� (x + y)*z ������ �� �����ϴ� �Լ� ����
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

# ����) sign�� ����� ���� f_sign �Լ� ����
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

f_sign(0)               # x�� ��Į���� ��� ���� ���� ����
f_sign(c(0,500,-5,2,9)) # x�� ������ ��� �ݺ� ���� �Ұ�

# ��������) �� �Լ��� ���Ϳ����� �����ϵ��� �Լ� ���� ����
# sol1) �Լ� ���� ���� if + for�� ����
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
  
# sol2) �ݺ����� ���� �Լ� ����
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

# sol3) ����� �����Լ��� �����Լ��� ����(�ݺ��� ��ü ����)
f_sign(c(0,500,-5,2,9))          # ���Ϳ��� �Ұ�
sapply(c(0,500,-5,2,9), f_sign)  # ���Ϳ��� ����
sapply(list, function)

# [ ���� ���� ]
# sal���� �ԷµǸ� ����� ����ϴ� ����� ���� �Լ� ����
# 1000�̸� 'C' 1000�̻� 3000 �̸� 'B', 3000 �̻� 'A'
# 1) �ݺ��� ���� ����� �����Լ�
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

# 2) �ݺ��� ���� ����� �����Լ� + sapply
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
f_sal(emp$SAL)          # ���Ϳ��� �Ұ�
sapply(emp$SAL, f_sal)  # ���Ϳ��� ����

# [ ���� ���� ]
# emp �����Ϳ��� ���ʽ��� ������ִ� ����� �����Լ� ����
# ���ʽ��� 10�� �μ��� SAL�� 10%, 20���� 15%, 30���� 20% ����
# step1) ���� 7902������ ���ʽ��� ����ϴ� ���� �ۼ�
vsal <- emp[emp$EMPNO == 7902, 'SAL']
vdeptno <- emp[emp$EMPNO == 7902, 'DEPTNO']

if (vdeptno == 10) {
  vsal * 1.1
} else if (vdeptno == 20) {
  vsal * 1.15
} else {
  vsal * 1.2
}


# step2) ����� ���� �Լ� ������ �� ���� ����, 7902 �ڸ��� x�� ����

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
f_bonus(emp$EMPNO)            # ��� �߻�, ��Ȯ�� ���� ���� ����X
sapply(emp$EMPNO, f_bonus)    # ���� ���� ����

# [ ���� ���� ]
# �μ���ȣ�� ���� �μ��� ��� �Լ� ���� �� ����
# 10���̸� �λ��, 20�� �繫��, 30�� �ѹ���
f_dname <- function(x) {
  if (x == 10) {
    return('�λ��')
  } else if (x == 20) {
    return('�繫��')
  } else {
    return('�ѹ���')
  }
}

f_dname(10)
f_dname(emp$DEPTNO)           # ���Ϳ��� �Ұ�
sapply(emp$DEPTNO, f_dname)   # ���Ϳ��� ����

# [ ����� �����Լ� Ȱ�� ���� ]
c('a#b#c', 'A#B#C')
f_split(string, sep, n)
f_split(df1$EMAIL, '@', 1)














