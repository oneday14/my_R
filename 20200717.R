# 1. 2020�� ��ü ��¥�� ���� v1 ���� ����
v1 <- seq(from=as.Date('2020/01/01',fotmat='%Y/%m/%d'),
          to=as.Date('2020/12/31',fotmat='%Y/%m/%d'),
          by=1)

# 2. ���� ���͸� �⵵�� ������ ��/�� �������θ� ����Ͽ� v2 ����
v2 <- as.character(v1,'%m/%d')

# 3. '2020/04/25'�Ϸκ��� 100�� ���� ���� ���
as.character(as.Date('2020/04/25') + 100, '%A')
as.character(as.Date('2020/04/25') + 100, '%w')

# 4. ����� �Ի����� ������ ������
# ������� �ٹ��ϼ��� ����, �����ΰ� ���� ���
v_hiredate <- c('2018/04/06','2019/12/23','2019/05/04')
class(v_hiredate)

v_day <- as.numeric(Sys.Date() - as.Date(v_hiredate))
v_week <- v_day %/% 7

v_week2 <- trunc(v_day/7) ; v_week2

########## ��������� �����Դϴ�. ##########


# DB�� NULL : ���� ���ǵ��� ���� ��

# R������ NA�� NULL
# - NA   : �߸� ���� ��, ������ ��ġ�� ����
# - NULL : �������� ���� ��, ������ ��ġ�� ���� ����

as.Date('2020/01/01','%Y/%M/%D')

cat(..., 
    file= , )

cat(1,2,NA,3)   # 1 2 NA 3
cat(1,2,NULL,3) # 1 2 3 (NULL�� �ڸ��� �����ϰ� ���� �ʾ� ���X)

sum(1,2,NA)     # NA, NA�� ���õ��� X
sum(1,2,NULL)   # 3, NULL�� ���É�

NULL + 3        # numeric(0)
NA + 3          # NA

# [ ���� : ������ ���� ���� ��� ]
# �Լ��� ���� �ڸ��� ...�� �ִ� ���� ������ ����� ���� ����
sum(..., na.rm = F)
sum(1,2,3)
sum(c(1,2,3))

mean(x, ...)
mean(1,2,3)     # 1�� ����, 2,3�� ����
mean(c(1,2,3))  # c(1,2,3)�� �ϳ��� ��ü�̹Ƿ� ��ü ���� ����


# [ R������ �ڷᱸ�� ]
# ����
# - 1����
# - ������ ���� ���ÿ� ��ų� �����ϱ� ���� ���
# - �ϳ��� ������ Ÿ�Ը� ���
# - ������������(���̺�)�� �÷��� �����̱⵵ ��

# 1. ����
v1 <- c(1,2,3) ; v1
v2 <- c(1,2,'a') ; v2
v3 <- 1:10 ; v3

# 2. Ȯ��
c(v1, 4)
append(x,                   # ����
       values = ,           # �߰� ��
       after = length(x))   # ��ġ, ������ �� ���� �߰�
append(v1,4)
append(v1,4,after=1)


# 3. �������
v1 <- c(1,2,3)
v2 <- c(10,20,30)
v3 <- c(10,20,30,40)

v1 + 1   # ���Ϳ� ���� ������� ����
v1 + v2  # ������ ũ���� ���ʹ� ���� ��ġ ���ҳ��� ����
v1 + v3  # ���� �ٸ� ũ���� ������ ������ ���� ũ���� ���Ͱ�
         # �ݺ��Ǹ鼭 �����
         # 10 20 30 40
         # 1  2  3  1
         # ===========
         # 11 22 33 41  

# 4. ����(indexing)  *****
# 1) ��������
v_sal <- c(800,900,1000,1100,1200)
v_sal[1]
v_sal[1,3]     # ù��° ���� 3��° �÷��� ����(2������ ����)
v_sal[c(1,3)]  # 1,3��° ���� ����

# 2) �̸����� 
names(v_sal) <- c('a','b','c','d','e') ; v_sal
v_sal[c('a','d')]

# 3) �����̽� ����
v_sal[c(1,2,3)]
v_sal[1:3]
v_sal[2:5]

# 4) ���� ����(boolean indexing)
v_sal[v_sal >= 1000]
v_sal[c(F,F,T,T,T)]

# �Ҹ��� ������ ���� ��Ī
# 800,900,1000,1100,1200
# F   F   T    T    T


# [ �������� : emp.csv ������ �а� ] 
df1 <- read.csv('emp.csv')
class(df1)

# 1) 10�� �μ����� �̸�, job, sal ���
DEPTNO == 10          # DEPTNO��� ������ 10�̳Ĵ� ����
df1[,'DEPTNO'] == 10
df1[,8] == 10
df1$DEPTNO == 10

df1[df1$DEPTNO == 10, c('ENAME','JOB','SAL')]
df1[c(7,9,14), c('ENAME','JOB','SAL')]

# 2) 20�� �μ����� sal�� �� �� ���
df1[c(1,4,8,11,13), 6]
df1[c(1,4,8,11,13), 'SAL']
sum(df1[c(1,4,8,11,13), 'SAL'])

sum(df1[df1$DEPTNO == 20, 'SAL'])

# 3) sal�� 2000�̻��� ���� �̸�, sal ����
df1[df1[,'SAL'] >= 2000 , c('ENAME','SAL')]
df1[df1$SAL >= 2000 , c('ENAME','SAL')]

# 4) smith�� allen�� �̸��� ���� ����
df1[c(1,2), c(2,6)]
df1[c(1,2), c('ENAME','SAL')]

# in sql)
# select ename, sal
#   from df1
#  where ename in ('smith','ALLEN')
# ;
 
# [ sql -> R ���� ���� ]
# ename in ('smith','ALLEN')
# df1$ENAME %in% c('smith','ALLEN')  # %in% : ���Կ�����
 
df1[df1$ENAME %in% c('smith','ALLEN'), c('ENAME','SAL')]  
 
v_bool <- df1$ENAME == c('smith','ALLEN')
df1[v_bool, c('ENAME','SAL')]

# df1[����� , �÷����� ]


# 5. ���� ����
v_sal[2] <- 1100

# ��������) v_sal�� �̸� �� d�� D�� ����
names(v_sal[4]) <- 'D' ; v_sal
names(v_sal)[4] <- 'D' ; v_sal
names(v_sal) <- c('a','b','c','D','e')

# ����) df1�� SAL �÷��� SALARY�� ����
names(df1)[6] <- 'SALARY' ; df1

# ��������) df1�� COMM �÷��� BONUS�� �����ϵ�,
# COMM �÷��� ��ġ�� �𸥴ٴ� �����Ͽ� ó��
names(df1)[7]
names(df1)['COMM']
names(df1)[names(df1) == 'COMM'] <- 'BONUS' ; df1

# ����) df1���� MGR �÷��� ����(���ǻ���)
df1[,'MGR']
df1['MGR']
df1$MGR
df1[ , names(df1) == 'MGR']

# ����) df1���� MGR �÷� ���� �ٸ� �÷� ��� ����(���ǻ���)
df1[ , names(df1) != 'MGR']  
df1[ , -4]                   # 4��° �÷� ����
df1[ , -'MGR']               # ���ڰ��� �����Ҽ��� ����

# 6. ���� ũ�� Ȯ��
length(v_sal)      # ������ ����
NROW(v_sal)        # ������ ������ ����, 2������ ���� ��
nrow(v_sal)        # 2���� �������� ���� ��

NROW(df1)          
nrow(df1) 

# ���� ������
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


# ���� ������
v_sal[(v_sal == 800) | (v_sal == 1000)]
v_sal[v_sal %in% c(800, 1000)]


# �������� : df1����) 
# 1) 1987�⿡ �Ի��� ����� �̸�, �Ի���, SAL ���
# in sql)
# where to_char(hiredate,'YYYY') = '1987'
class(df1$HIREDATE)

# ��¥ �Ľ�(980/12/17 00:00:00)
v_date <- as.Date(df1$HIREDATE, '%Y/%m/%d %H:%M:%S')

df1[as.character(v_date, '%Y') == '1987', 
    c('ENAME','HIREDATE','SALARY')]

# 2) �����ȣ�� 7900, 7902, 7934�� ������ SAL�� ����
v_bool  <- (df1$EMPNO == 7900) | (df1$EMPNO == 7902) | (df1$EMPNO == 7934)
v_bool2 <- df1$EMPNO %in% c(7900, 7902, 7934)

sum(df1[v_bool, 'SALARY'])
sum(df1[v_bool2, 'SALARY'])



# ����Ʈ 
# - 1����
# - key ����(�����͸� ������ �˻�/������ �� �ֵ��� ���� �ڷᱸ��)
# - ���� �ٸ� ������ Ÿ�� ���

# 1. ����
l1 <- list('name'='ȫ�浿', 'tel'='02)043-0875', 'addr'='�����')
l2 <- list('name'='ȫ�浿', 'tel'='02)043-0875', 'addr'='�����',
           'sal'=4000)

l3 <- list('name'=c('ȫ�浿','��浿'), 
           'tel'=c('02)043-0875', '031)384-3944'), 
           'addr'=c('�����','��⵵'),
           'sal'=c(4000,3900))

# 2. ����
l3[1]           # ù��° key(��) ����
l3[c(1,3)]      # ù��° key(��) ����

l3['name']
l3[c('name','tel')]

l3$name
l3$c(name,tel)       # �Ұ�, key ������ �ϳ��� key�� ���� ����

# ��������) l3���� ��浿�� ����
l3$name[2]
l3[1]        # ����Ʈ ����
l3[[1]]      # ���� ����
l3[[1]][2]   


# 3. ����
l3$name[2] <- '�ֱ浿' ; l3
l3$comm <- c(500,400)            # key �߰�

df1$new_sal <- df1$SALARY * 1.1  # �������������� key �߰�
l3$comm <- NULL ; l3             # key ����

# key�� ���� �ڷ�����
'name' : 'ȫ�浿'
'tel'  : '02)043-0875'
'addr' : '�����'

# key�� ���� �ʴ� �ڷ�����
'ȫ�浿', '02)043-0875', '�����'


# [ ���� : R�� ���̽��� �ڷᱸ�� �� ]
#   R             python 
# ����            ����Ʈ
# ����Ʈ*         ��ųʸ�*
# ���             ���
# �迭             �迭
# ������������*  ������������*

