# 1. emp.csv �����͸� Ȱ���Ͽ�
getwd()
emp <- read.csv('emp.csv')

# 1) 1���� �Ի��� ������ �̸�, �Ի���, ���� ���
v_bool <- as.character(as.Date(emp$HIREDATE),'%m') == '01' # to_date
emp[v_bool, c('ENAME','HIREDATE','SAL')]

# 2) �� ������ �Ի������ DAY �÷��� �߰�
emp$DAY <- as.character(as.Date(emp$HIREDATE),'%A')

# 3) ������ �������� ������ ��� �� R_PAY �÷����� �߰�
# (������ = ���� SAL * trunc(�ټӳ��/12))
emp$HIREDATE <- as.Date(emp$HIREDATE)
v_year <- as.numeric(trunc((Sys.Date() - emp$HIREDATE) / 365))
emp$R_PAY <- emp$SAL * trunc(v_year/12)

# 4) �̸��� KING�� �� ����
emp <- emp[emp$ENAME != 'KING', ]

# 5) 20�� �μ����� �� SAL�� 2000�̻��� ������ SAL�� ���
mean(emp[(emp$DEPTNO == 20) & (emp$SAL >= 2000), 'SAL'])

# 2. ������ ����Ʈ ����
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

# 1) banana�� BANANA�� ����
l1$name[2]
l1[[2]][2] <- 'BANANA' ; l1

# 2) 10% �λ�� ������ NEW_PRICE Ű�� �߰�
l1$NEW_PRICE <- l1$price * 1.1

# 3) peach�� qty�� ���(��, peach�� ��ġ�� �𸥴� ����)
l1$qty[3]

l1$name == 'peach'
l1$qty[l1$name == 'peach']

########## ��������� �����Դϴ�. ##########

# ���� ������
# 1) and : &
# 2) or : |
# 3) not : !
  
# ���� : ���ڸ� ����� �������� ����
# 0 : FALSE(0�� �ƴϸ� TRUE)

!0
!100
1 & 3  # TRUE & TRUE = TRUE
1 & 0  # TRUE & FALSE = FALSE


c(TRUE,TRUE,FALSE) & c(TRUE,FALSE,FALSE)  # �� ������� �������� O
c(TRUE,TRUE,FALSE) && c(TRUE,FALSE,FALSE) # �� ������� �������� X, ù��°�͸� ��������

v1 <- 1:3
(v1 < 3) & (v1 == 1)

# ����) emp �����Ϳ��� sal > 2000�̸鼭 deptno = 10�� ��������
(emp$SAL > 2000) & (emp$DEPTNO == 10)
(emp$SAL > 2000) && (emp$DEPTNO == 10)

emp[(emp$SAL > 2000) && (emp$DEPTNO == 10), ]  # �߸��� ����
emp[(emp$SAL > 2000) & (emp$DEPTNO == 10), ]   # �ùٸ� ����


# ��(������ Ÿ��) Ȯ�� �Լ�
v1 <- c(1,NA,3)
v2 <- c(1,NULL,3)

is.vector(v1)
is.numeric(v1)
is.na(v1)       # ������ �� ���Ҹ��� NA ���� ����
is.na(v2)       # ������ �� ���Ҹ��� NA ���� ����, NULL�� ����
is.null(v2)     # ���� ��ü�� NULL���� ����

# ��������) �� v1 ���Ϳ��� NA�� �κ��� ã�� 0���� ����
sum(v1)              # NA�� �����ϴ� ������� NA����
v1[is.na(v1)] <- 0   # NA ġȯ
sum(v1)              # ġȯ�� ����

# [ ���� ���� ���� ]
# 1. vec1 ���� ����
vec1 <- c('���','��','��','����','������')

# 1) vec1���� 3��° ����� ���� ����, vec1 ���
vec1[vec1 != '��']

# 2. vec1, vec2 ����
vec1 <- c('��','����','����','�ܿ�')
vec2 <- c('��','����','�ʿ���','�ʰ���')

# 1) vec1�� vec2�� ��ģ ��� ���
c(vec1, vec2)                       # union all
append(vec1, vec2[!vec1 %in% vec2]) # union 
append(vec1, vec2[vec1 != vec2])
# 2) vec1���� �ִµ� vec2���� ���� ��� ���
vec1[!vec1 %in% vec2]

# 3) vec1�� vec2 �Ѵ� �ִ� ��� ���
vec1[vec1 %in% vec2]


# ������ ���տ�����
# 1) ������ : union
# 2) ������ : setdiff
# 3) ������ : intersect
# 4) ����� : identical, setequal
v1 <- c(1,2,3,4,5)
v2 <- c(1,2,30,40,50)
v3 <- c(1,2,3,4,5,6)
v4 <- c(1,2,3,4,5,5)

union(v1,v2)      # 1  2  3  4  5 30 40 50
c(v1,v2)          # 1  2  3  4  5  1  2 30 40 50

intersect(v1,v2)  # 1,2
setdiff(v1,v2)    # 3 4 5

identical(v1,v3)  # ���� �ٸ� ���Ұ� �����Ƿ�, FALSE
setequal(v1,v3)   # ���� �ٸ� ���Ұ� �����Ƿ�, FALSE

identical(v1,v4)  # ���� ���Ҵ� ������ ũ�Ⱑ �޶� FALSE
setequal(v1,v4)   # ũ�� ������� ���� ���Ҵ� �����Ƿ� TRUE



# ��� : matrix
# - 2����
# - �ϳ��� ������ Ÿ�Ը� ���
# - �ַ� ���� ������ ������ �ϱ� ���� ����

# 1. ����
matrix(data = ,        # matrix ���� data(����)
       nrow = ,        # �� ��
       ncol = ,        # �÷� �� 
       byrow = FALSE,  # �� �켱���� ��ġ ����
       dimnames = )    # ��� ���� �̸�, ����Ʈ�� ����

m1 <- matrix(1:9, nrow = 3, ncol = 3) ; m1
m1 <- matrix(1:9, nrow = 3) ; m1
m2 <- matrix(1:9, nrow = 3, byrow = T) ; m2
m3 <- matrix(1:9, 
             nrow = 3, 
             byrow = T,
             dimnames = list(c('a','b','c'),
                             c('A','B','C'))) ; m3

# 2. ����
m1[1,1]
m1[,1]
m1[1,] 
m1[, c(2,3)]
m1[, 2:3]
m1[, -1]      # ù��° �÷� ����
m3['a','A']   # �̸��� �ִ� ��� �̸����� ����
m1[m1 > 5]    # 2���� ������ �Ҹ����� ���ο� �״�� ����

m1[,1]        # ������� �߻�, 1������ ���ͷ� ���
m1[,1,drop=F] # 2���� ���, ������� �߻� X

# ��������) m3���� �ι�° �÷��� 5�̻��� ���� ����
m3[m3[,2] >= 5, ]

# 3. ����
m1[1,1] <- 10 ; m1
 
# ��������) 1���� 20���� ���� 5X4��� ���� ��
# ¦������ ��� 0���� �����Ͽ���.
m1 <- matrix(1:20, nrow = 5, byrow = T)
4 %% 2
5 %% 2

m1[m1 %% 2 == 0] <- 0


# 4. ��������
# 1) ��, �� �̸� ����
rownames(m2) <- c('a','b','c')
colnames(m2) <- c('A','B','C')
dimnames(m2) <- list(c('a','b','c'), c('A','B','C'))

# 2) ��, �÷� �߰�
m2[,4] <- c(10,20,30)                # ���� ������ ����� �Ұ�
cbind(m2, c(10,20,30))               # m2 �� �ڿ� �÷� �߰�
cbind(m2[,1], c(10,20,30), m2[,2:3]) # �߰� ���� ��

rbind(m2, c(10,11,12))


# 5. ����
m1 <- matrix(1:4, nrow = 2)
m2 <- matrix(seq(10,40,10), nrow = 2)
m3 <- matrix(1:9, nrow = 3)
m1 * m2    # �� ���Ҹ��� ���ϱ� ����
m1 %*% m2  # matrix inner product

m1 + m2    # ũ�Ⱑ ���� ��ĳ��� ���� ����
m1 + m3    # ũ�Ⱑ �ٸ� ��ĳ��� ���� �Ұ�

# [ ���� : ��� �� ]
# [a1,a2   [b1,b2
#  a3,a4]   b3,b4]
# 
# (2X2) * (2X2) = (2X2)
# (3X2) * (2X6) = (3X6)
# 
# a1*b1 + a2*b3, a1*b2 + a2*b4
# a3*b1 + a4*b3, a3*b2 + a4*b4


# 6. ũ�� Ȯ��
nrow(m1)  # ���� ��
ncol(m1)  # �÷� �� 
NROW(m1)  # ���� �� 
NCOL(m1)  # �÷� ��
dim(m1)   # 2 X 2

m1 <- matrix(1:20, nrow = 5)
dim(m1)            # 5 X 4
dim(m1) <- c(4,5)  # 5 X 4, matrix reshape
m1

# [ matrix ���� ���� ]
# 1. ��� ����
v1 <- c('��','����','����','�ܿ�')
seasons <- matrix(v1, nrow = 2)
t(seasons)

seasons <- matrix(v1, nrow = 2, byrow = T)

# 2. ������ �ܿ︸ ��ȸ
seasons <- matrix(v1, nrow = 2, byrow = T)
seasons[,2,drop=F]

# 3. 3�� �� �߰�
seasons_2 <- rbind(seasons, c('�ʺ�','�ʰ���'))

# 4. 3��° �÷� �߰�
seasons_3 <- cbind(seasons_2, c('�ʿ���','�ʰܿ�','�Ѱܿ�'))


# �迭 : array
# 1. ����
array(data = ,     # �迭�� �����ϴ� data(����)
      dim = ,      # ����(���ͷ� ����)
      dimnames = ) # �� ������ �̸�(����Ʈ�� ����)

array(1:18, dim = c(2,3,3))  # ������ ��, ��, ��, ... ����
array(1:18, 
      dim = c(2,3,3),
      dimnames = list(c('a','b'),
                      c('A','B','C'),
                      c('1F','2F','3F')))  

# [ ���� : �������� �迭 ���� �� ]
# in R)
# �� �� �� ...
 
# in python)
# ... �� �� ��


# 2. ����
a1 <- array(1:18, dim = c(3,3,2))
a1[,,1]
a1[,,1,drop=F]  # ��� ���� 
a1[1,,]
a1[1,,drop=F]   # ��� �Ұ�


# ������ ������
# - 2���� ����
# - ��� ���� ����
# - ���� KEY�� ���� ����
# - ���������� ǥ, �����ͺ��̽������� ���̺��� ���

# 1. ����
data.frame(...,                  # data�ڸ�, key-value ���� ����
           row.names = ,         # row �̸�
           stringsAsFactors = T) # ������ ����ȭ ����

data.frame('ename'=c('ȫ�浿','��浿'),
           'sal'=c(800,900))

df1 <- data.frame(ename=c('ȫ�浿','��浿'),
                  sal=c(800,900))

df2 <- data.frame(ename=c('ȫ�浿','��浿'),
                  sal=c(800,900),
                  row.names = c('a','b'))

# 2. ����Ȯ��
ncol(df1)      # �÷� ��
nrow(df1)      # ���� ��
dim(df1)       # ��� �÷� ��

rownames(df1)  # �� �̸�
colnames(df1)  # �÷� �̸�

str(df1)

# 3. ����
df1[1,1]              # ��ġ(����)����
df1[1,'ename']        # �̸�����
df1[ ,'ename']        # Ư�� �÷� �ϳ��� ������ ���� ��� ��
df1[ ,'ename',drop=F] # ���� ��� ���� ����
df1$ename             # key ����

# 4. ��������
# 1) ���߰�
df2 <- rbind(df1, c('�ֱ浿', 1000, 300))  # ���� �߻�
df2[3,1] <- '�ֱ浿'
str(df1)

# [ factor�� ������ �÷��� ���� �߰��ϴ� ��� ]
# 1. factor�� ������ ����
# 2. non factor������ ����
df2$ename <- as.character(df2$ename)
str(df2)
df2[3,1] <- '�ֱ浿'
df2$ename <- as.factor(df2$ename)

# 2) �÷��߰�
df1$comm <- c(100,200) ; df1         
cbind(df1, 'deptno' = c(9411,9511))  # �÷� �߰�

# factor�� ����
# ������ �ִ� ������ ���� ����� ������ ������
# �ַ� ������ �÷��� ��� ����

# ex) ���� : ��, ��
#     ���� : A, B, C, D
#     �г� : 1, 2, 3, 4


# ��������) ������ ������ ������ ����
# name : apple, mango, banana
# price : 1000, 1500, 500
# qty : 10, 5, 20
v_name <- c('apple', 'mango', 'banana')
v_price <- c(1000, 1500, 500)
v_qty <- c(10, 5, 20)

df1 <- data.frame(name=v_name, price=v_price, qty=v_qty)
str(df1)

# 1) ������ �� �߰�
# berry, 2000, 3
df2 <- rbind(df1, c('berry', 2000, 3))
str(df2) 

df2$name <- as.character(df2$name)
df2[4,1] <- 'berry'
df2$name <- as.factor(df2$name)

# 2) sales �÷��� price * qty �� ����Ͽ� �߰�
df2$price <- as.numeric(df2$price)
df2$qty <- as.numeric(df2$qty)

df2$sales <- df2$price * df2$qty
