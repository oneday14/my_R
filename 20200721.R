# 1. emp.csv ������ �а�
emp <- read.csv('emp.csv')

# 1) ��ݱ� �Ի��� ����� ������ 10%, �Ϲݱ�� 15%�� ���ʽ���
# bonus �÷��� �߰�(���ǹ� ��� �Ұ�)
class(as.Date(emp$HIREDATE))
emp$HIREDATE <- as.Date(emp$HIREDATE)
v_bool <- as.numeric(as.character(emp$HIREDATE,'%m')) >= 7

emp$bonus <- emp$SAL * 1.1
emp$bonus[v_bool] <- emp$SAL[v_bool] * 1.15

# 2) comm�� NA�� ��� 0���� ������ comm�� ��� ���
emp$COMM[is.na(emp$COMM)] <- 0
mean(emp$COMM)

# 3) empno, ename, sal, deptno �÷��� ���� �� emp2 ����
emp2 <- emp[ , c('EMPNO','ENAME','SAL','DEPTNO')]

# 4) �� emp2 �����������ӿ� �Ʒ� �� �߰�
# 9400, HONG, 3000, 40
str(emp2)

emp2$ENAME <- as.character(emp2$ENAME)
emp2 <- rbind(emp2, c(9400, 'HONG', 3000, 40))
emp2$ENAME <- as.factor(emp2$ENAME)

str(emp2)

emp2$SAL <- as.numeric(emp2$SAL)
emp2$DEPTNO <- as.numeric(emp2$DEPTNO)
emp2$EMPNO <- as.numeric(emp2$EMPNO)

# 2. disease.txt ������ �а�
read.csv('disease.txt')
read.table('disease.txt', header = T)

df2 <- read.table('disease.txt')
str(df2)
# ù��° ���� ������ �÷� �̸� ����
colnames(df2) <- df2[1,]  # �÷��̸��� ������ ������ �� �÷���
# factor�̱� ������ �� ������ ���ڷ� ġȯ

# step1) �� �÷��� ��� ���� �÷����� ����
df2[,1] <- as.character(df2[,1])
df2[,2] <- as.character(df2[,2])
df2[,3] <- as.character(df2[,3])
df2[,4] <- as.character(df2[,4])
df2[,5] <- as.character(df2[,5])
df2[,6] <- as.character(df2[,6])

# step2) �÷� �̸� ����
colnames(df2) <- df2[1,]

# step3) ù ��° �� ����
df2 <- df2[-1,]

# step4) ���� �÷����� ����
df2[,2] <- as.numeric(df2[,2])
df2[,3] <- as.numeric(df2[,3])
df2[,4] <- as.numeric(df2[,4])
df2[,5] <- as.numeric(df2[,5])
df2[,6] <- as.numeric(df2[,6])

# [ ���� - �ܺ����� �ҷ����� �Լ� �� ]
# read.csv   : csv���� ����, �и������ڰ� ','�� �⺻, header �÷�ȭ
# read.table : �Ϲ����� ���, �и������ڰ� ���� �⺻, header �÷�ȭX

# 1) A�������� �ݷ����� NA���� �� �÷��� �ּҰ����� ����
vmin1 <- min(df2$A������[!is.na(df2$A������)])
df2$A������[is.na(df2$A������)] <- vmin1

vmin2 <- min(df2$�ݷ���[!is.na(df2$�ݷ���)])
df2$�ݷ���[is.na(df2$�ݷ���)] <- vmin2

# 2) "����" �÷��� �������������� ���̸����� ����,
#    �������� "����" �÷� ����
rownames(df2) <- df2$����
df2 <- df2[,-1]

# 3) A�������� �÷� �̸��� A�������� ����(��ġ ���� �Ұ�)
colnames(df2)[colnames(df2)=='A������'] <- 'A����'

# 4) NA�� �ϳ��� ������ �� ����(df2)

df2[!(is.na(df2$�ݷ���) | is.na(df2$��ƼǪ��) |
        is.na(df2$A����)  | is.na(df2$����) |
        is.na(df2$�����)), ]

########## ��������� �����Դϴ�. ##########

# ���ǹ�
# 1. if��
# - ���ǿ� ���� ġȯ Ȥ�� ���α׷��� ó�� �� ���
# - ���� ���� �Ұ�(������ ���Һ� ���� ������ �Ұ�)

# 1) ����
# if ( ����1 ) {
#   ����1 ���϶� ���� ����
# } else if ( ����2 ) {
#   ����2 ���϶� ���� ����
# } else {
#   ����2 �����϶� ���� ����
# }

# ����) v1�� ���� 0 �̻��̸� 'A' �̸��̸� 'B' ����
v1 <- 11

if (v1 >= 0) {
  'A'
} else {
  'B'
}

# ����) v1�� ���� 30�̻��̸� 'A', 20�̻��̸� 'B' ,
# 20���� ������ 'C'����
if (v1 >= 30) {
  'A'
} else if (v1 >= 20) {
  'B'
} else {
  'C'
}

# [ if���� ���� ���� �Ұ� ���� ]
v1 <- c(10,25,55)

if (v1 >= 30) {
  'A'
} else if (v1 >= 20) {
  'B'
} else {
  'C'
}

# "C"
# length > 1 �̶�� ������ �ְ�, => if���� ���� ����� �ݵ�� �ϳ�
# ù��° ��Ҹ��� ���� ���Դϴ� => �ݺ��� �ȵǹǷ� ù��° ����� ���

# ����) emp �����Ϳ��� sal�� 3000 �̻��� ���� 'A',
# �ƴ� ���� 'B' ����
emp$SAL >= 3000

if (emp$SAL >= 3000) {
  'A'
} else {
  'B'
}


# ��������) 10�� �μ��� ��� SAL�� 10% ��������, �������� 20% ������
# NEW_SAL �÷��� �߰�
# 1) if��
emp$NEW_SAL <- if (emp$DEPTNO == 10) {
                    emp$SAL * 1.1
                  } else {
                    emp$SAL * 1.2
                  }

# 2) ifelse��
emp$NEW_SAL2 <- ifelse(emp$DEPTNO==10, emp$SAL * 1.1, emp$SAL * 1.2)

# 2. ifelse��
# - ���ǹ�(oracle�� decode�Լ��� ���)
# - ���� ���� ����
# - ���ϸ� ����, ���α׷��� ó�� �Ұ�

# ����
ifelse(test,  # ����
       yes,   # ���϶� ���� ��
       no)    # �����϶� ���� ��(�����Ұ�)

v1 <- c(1,11,20)
ifelse(v1 > 10, 'A','B')

# ��������) emp �����Ϳ��� dname �÷� �߰�
# 10�� �μ��� �λ��, 20���� �繫��, 30���� �ѹ���
emp$dname <- ifelse(emp$DEPTNO==10, '�λ��', 
                    ifelse(emp$DEPTNO==20, '�繫��', '�ѹ���'))


# �ݺ��� 
# 1. for��

# ����)
# for (�ݺ����� in �ݺ����) {
#   �ݺ� ������ ����
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


# if + for���� ����
# if���� ������ ����� �ϳ����� �ϹǷ�
# for���� ���� �ϳ��� ����, �ݺ����� �ǵ��� ó�� �ʿ�

# ����) emp �����Ϳ��� dname �÷� �߰�
# 10�� �μ��� �λ��, 20���� �繫��, 30���� �ѹ���(if + for��)

for (i in emp$DEPTNO) {
  if (i == 10) {
    print('�λ��')
  } else if (i ==20) {
    print('�繫��')
  } else {
    print('�ѹ���')
  }
}


emp$dname2 <- for (i in emp$DEPTNO) {   # ������ ��������
                if (i == 10) {          # ���Ͱ� �ƴϹǷ�
                  print('�λ��')       # ȭ�鿡 ��¸� �ɻ�
                } else if (i ==20) {    # �÷��� �߰� �Ұ�
                  print('�繫��')
                } else {
                  print('�ѹ���')
                }
              }

vdname <- c()

for (i in emp$DEPTNO) {  
  if (i == 10) {         
      vdname <- c(vdname, '�λ��')
 } else if (i ==20) {   
      vdname <- c(vdname, '�繫��')
 } else {
      vdname <- c(vdname, '�ѹ���')
 }
}

emp$dname2 <- vdname


# ��������) 10�� �μ��� ��� SAL�� 10% ��������, �������� 20% ������
# NEW_SAL �÷��� �߰�(if + for)

vsal <- c()
for (vno in emp$DEPTNO) {
  if (vno == 10) {
    vsal <- c(vsal, emp$SAL * 1.1)
  } else (
    vsal <- c(vsal, emp$SAL * 1.2)
  )
}

length(vsal)  # 14 * 14

# 2) ��ġ���� Ȱ��
vsal <- c()

for (nrow in 1:nrow(emp)) {
  if (emp$DEPTNO[nrow] == 10) {
    vsal <- c(vsal, emp$SAL[i] * 1.1)
  } else (
    vsal <- c(vsal, emp$SAL[i] * 1.2)
  )
}

length(vsal)  # 14 

# ��������) emp���� sal�� 1000���ϸ� C, 1000 �ʰ� 2000���� B,
# 2000�ʰ��� A ����, grade �÷��� �߰�
# 1) �ݺ������ Ư�� �÷��� ���

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


# 2) �ݺ������ ���� ��ġ���� ��� 
#    �ַ� �ݺ� ó���ؾ��� �÷��� �������� ��� ���
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


# 2. while ��








# ���ڿ� �Լ� 
# stringr ��Ű�� ���
install.packages('stringr')
library(stringr)


# 1. str_detect : ������ ���� ���� Ȯ��(����Ȯ���Լ�)
str_detect(strings = ,
           pattern = )

v1 <- c('abc','bcd','Abc', 'acd', 'cabd')
v2 <- c('ab12','1a34','cds','1234', 'cde!')
v3 <- c('abc','aabc','aaabc','aaaabc')

str_detect(v1, 'a')         # 'a'�� �����ϴ��� ����
str_detect(v1, '[aA]')      # 'a' �Ǵ� 'A'�� �����ϴ��� ����
str_detect(v1, '[aA][bB]')  # 'a' �Ǵ� 'A'�� �����ϴ��� ����

str_detect(v1, '^a')        # 'a'�� �����ϴ��� ����
str_detect(v1, 'd$')        # 'd'�� �������� ����

str_detect(v2, '.a')        # �ι�° ���ڰ� 'a'���� ����
str_detect(v2, '[0-9]')     # ���ڸ� �����ϴ��� ����
str_detect(v2, '[:digit:]') # ���ڸ� �����ϴ��� ����
str_detect(v2, '[a-zA-Z]')  # ������ �����ϴ��� ����
str_detect(v2, '[:alpha:]') # ������ �����ϴ��� ����
str_detect(v2, '[:punct:]') # Ư����ȣ�� �����ϴ��� ����

str_detect(v3, 'a{2,4}')    # 'a'�� ���������� 2ȸ�̻� 4ȸ���� ����

# [ ���� ���� ] emp �����Ϳ���
# 1) �̸��� S�� �����ϴ�(��� ����X) ������ �̸�, ���� ���
emp[str_detect(emp$ENAME, '^[sS]'), c('ENAME','SAL')] # like 'A%'

# 2) �̸��� ����° ���ڰ� A�� ������ �̸�, ���� ���   
emp[str_detect(emp$ENAME, '..[A]'), c('ENAME','SAL')] # like '__A%'


# 2. str_locate : ���ڿ������� Ư�� ������ ��ġ ����
str_locate(string = ,
           pattern = )

v1 <- c('a#b#c#', 'aa##b##')
str_locate(v1, '#')
str_locate(v1, '##')
str_locate_all(v1, '#')

# f_instr(email,'@',1,1)

# 3. str_count : ���ڿ������� Ư�� ���� ���� Ƚ�� ����
str_count(string = ,
          pattern = )

str_count(v3, 'a')

# 4. str_c : �и��Ǿ��� ���ڿ��� ����(concat)
str_c(..., 
      sep = , 
      collapse = NULL)

str_c('a','b','c')            # "abc"
str_c('a','b','c', sep=' ')   # "a b c"

v1 <- c('a','b','c')
v2 <- c('A','B','C')

str_c(v1,v2)
str_c(v1,v2,sep='_')        #  v1||'_'||v2


# 5. substr, str_sub : ���ڿ� ����
substr(x, start, stop)                # stop ���� �Ұ�
stringr::str_sub(string, start, stop) # stop ���� ����

v1 <- 'abcde'
substr(v1, 1,2)
substr(v1, 2,2)
str_sub(v1,2,2)

substr(v1, 2)
str_sub(v1,2)
