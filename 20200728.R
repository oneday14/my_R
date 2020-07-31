# ���� �����
# 6. ���̳ʸ� �����
# - ������� �۾� ȯ��(����,�Լ�)�� ���� ���� ��
# - ����� ���� �Լ��� ���� ����

v1 <- 1 ; v2 <- 2 ; v3 <- 3

f1 <- function(x) {
  x + 1
}

save(...,      # �Է� ��� ����
     list = ,  # �Է� ��� ���ͷ� ����
     file = )  # ������ ���ϸ�

load(file = )  # �ҷ��� ���ϸ�

save(list = ls(), file = 'vlist')
rm(list=ls())
v1             # ��� �Ұ�
load('vlist')
v1             # ��� ����
f1(10)         # �Լ� ��� ����

# 7. �����ͺ��̽� �����(oracle)
# 1) �غ����
# - ������ DB�� ���� : ip, port, db name, username, passwd
# ip : 192.168.0.115
# port : 1521
# db name : orcl
# username : scott
# passwd : oracle

# [ ���� - oracle service port/ db name Ȯ�� ��� ]
# C:\Users\KITCOOP> lsnrctl status

# - DB���� ��� ��� : ojdbc6.jar(64bit)
# - R���� ��� ��� : RJDBC(64bit)

# [ ���� : oracle ��ġ ���� ]
# oracle
# - server : instance(memory) + DB(disk)
# - client : instance(memory)


# 2. R�� ��Ű�� ��ġ : RJDBC
install.packages('RJDBC')
library(RJDBC)

# 3. oracle ��ġ(client) : ojdbc6.jar ���� ����

# 4. connection ����
jdbcDriver <- JDBC(driverClass = "oracle.jdbc.OracleDriver",
                   classPath = 'C:/app/KITCOOP/product/11.2.0/client_1/ojdbc6.jar')

con1 <- dbConnect(jdbcDriver,
                  "jdbc:oracle:thin:@ip:port:db_name",
                  username,
                  passwd)

con1 <- dbConnect(jdbcDriver,
                  "jdbc:oracle:thin:@192.168.0.115:1521:orcl",
                  "scott",
                  "oracle")
# 5. ���� ����
dbGetQuery(conn = ,       # �����̸�
           statement = )  # ���� ����

df1 <- dbGetQuery(conn = con1,       # �����̸�
                  statement = 'select * from student')  # ���� ����

class(df1)


# �����Լ� : �ݺ������� �����ִ� �Լ�
# 1. apply(X,           # ��� 
#          MARGIN = ,   # ����(1:�ະ, 2:����, c(1,2):���Һ�)
#          FUN = ,      # �����Լ�
#          ...)         # �����Լ��� �߰� ����

# - X���� 1���� ��ü ���� �Ұ�, 2���� ������ ���� ����
# - ��� ����� ������������ ���� ��� ��ü ����(����, ����Ʈ, ���)
# - �ַ� �ະ, ���� �׷쿬�� ������ ���� ���
# - R������ "���Һ�" ���뵵 ����(���̽� �Ұ�)

# ����) ������ ��Ŀ��� �ະ �� �� ����
ma1 <- matrix(1:25, nrow = 5)
apply(ma1, 1, sum)             # NA ����
apply(ma1, 1, sum, na.rm = T)  # NA ����

ma1[1,1] <- NA

sum(..., na.rm = F)

# ����) iris �����Ϳ��� �÷��� ��� ����
mean(iris$Sepal.Length)
mean(iris$Sepal.Width)

apply(iris[,-5], 2, mean)

# ����) ������ ���Ϳ��� õ���� ���б�ȣ ���� �� ���ڷ� ����
v1 <- c('1,100','2,200','3,300')

library(stringr)
as.numeric(str_remove_all(v1, ','))

# �����Լ� ���� ������� Ǯ��)
f1 <- function(x) {
  as.numeric(str_remove_all(x, ','))
}

apply(v1, c(1,2), f1)       # ���� ���� �Ұ�
sapply(v1, f1)              # ���� ���� ����

# [ ���� ���� ]
# ����ڷκ��� ������ ����� ���޹޾� �����ϴ� ����� �����Լ� ����,
# ������ ������ �����ӿ� ����
df1 <- data.frame(col1=c('1,100','1,200'),
                  col2=c('2,200','2,300'),
                  stringsAsFactors = F)

f2 <- function(data, y) {
  as.numeric(str_remove_all(data, y))
}

f2(data=df1,y=',')            # 2���� ���� �Ұ�
apply(df1, c(1,2), f2, ',')   # 2���� ���� ����
sapply(df1, f2, ',')          # 2���� ���� ����

df1 <- as.data.frame(apply(df1, c(1,2), f2, ','))
df1 <- apply(df1, c(1,2), f2, ',')     # matrix�� ����
df1[,] <- apply(df1, c(1,2), f2, ',')  # ���� data frame ����

# 2. lapply(list, function, ...)
# - ���Һ� ����
# - �ַ� ������ ���Һ� ����
# - ������ ������ ���� �� Ű�� ����
# - ��� ��� �ַ� ����Ʈ
lapply(v1, f1)                 # ����Ʈ ���
as.data.frame(lapply(v1, f1))  # ������������ ���(�� -> �÷�)
as.vector(lapply(v1, f1))      # ���� ��� �Ұ�(key ���� ����)
unlist(lapply(v1, f1))         # ���� ��� ����(key ���� ����)

lapply(iris[,-5], mean)        # �÷���(key) ����

# 3. sapply(list, function, ...)
# - ���Һ� ����
# - �ַ� ������ ���Һ� ����
# - 2���� ������ ���� ���Һ� ���뵵 ����
# - �ַ� ����, ��ķ� ����
sapply(v1, f1) 

# 4. mapply(function, ...)
# - ���Һ� ����
# - lapply, sapply�ʹ� �ٸ��� ���� �Լ��� ù ��° ���ڷ� ����
# - �ַ� ����, ��� ����
mapply(f1, v1)
mapply(f2, v1, ',')

# 5. tapply(vector, index, function)
# tapply(vector,    # �׷쿬�� ���� ���(����)
#        index,     # �׷캤��(group by �÷�)
#        function)  # �����Լ�(�ַ� �׷��Լ�)

# - �׷캰 ����
# - vector �ڸ����� 2���� ���� �Ұ�
# - sql�� group by ����� ���
# - �ַ� ���� ����

tapply(iris[,-5], iris$Species, mean)  # 2���� ������ �� ���� �Ұ�

# ����) googleVis ��Ű���� �ִ� Fruits �����Ϳ���
# ���Ϻ� sales�� �� �� ���

install.packages('googleVis')
library(googleVis)
Fruits

tapply(Fruits$Sales, Fruits$Fruit, sum)

# ����) profit�� 15�̻��� ���� �̸��� ��� �� �׷��� sales�� ����
g1 <- ifelse(Fruits$Profit>=15,'15�̻�','15�̸�')
tapply(Fruits$Sales, g1, sum)

vsum <- tapply(Fruits$Sales, Fruits$Profit>=15, sum)
names(vsum) <- c('15����','15�̻�')

# [ ���� - in sql ]
# select sum(Sales)
#   from Fruits
#  group by Fruit


# [ �������� ]
# 1. emp.csv ������ �а� ��/�Ϲݱ� �Ի����� ��տ���
df1 <- read.csv('emp.csv', stringsAsFactors = F)
vmonth <- as.numeric(str_sub(df1$HIREDATE, 6,7))
vgroup <- ifelse(vmonth < 7, '��ݱ�','�Ϲݱ�')

tapply(df1$SAL, vgroup, mean, na.rm=T)

# 2. 2000-2013��_���ɺ��Ǿ���_40-49��.csv ������ �а�
# 2005��~2009�⿡ ���� �� ����, �⵵�� �Ǿ��� ���
# ��, �⵵ ������ �⵵�� ����Ͽ� ǥ��, ��) year >= 2005
df2 <- read.csv('2000-2013��_���ɺ��Ǿ���_40-49��.csv')

rownames(df2) <- str_c(df2$��,'��')
df2 <- df2[,-1]

# 1) ���� �Ǿ��� ���
apply(df2, 1, mean)

# 2) �⵵�� �Ǿ��� ���
colnames(df2) <- as.numeric(str_remove_all(colnames(df2), '[X��]'))
apply(df2,2,mean)


# [ ���� ���� ]
# apply_test.csv ������ �а�
# �μ��� �Ǹŷ��� �� ���� ���ϼ���.
# ��, �� ���� -�� ���� 0���� ġȯ �� ���
# (ġȯ�Լ��� �������� Ǯ��)

df3 <- read.csv('apply_test.csv', stringsAsFactors = F)

# 1) NA �Ǵ� '-'�� 0���� ����
ifelse(is.na(df3), 0, df3)     # 2���� ���� �Ұ�
df3[is.na(df3)] <- 0           # �������� ���� �� ���� ����
str_replace_na(df3,0)          # 2���� ���� �Ұ�

f_na <- function(x) {
  if ((x=='-') | is.na(x)) {
    return(0)
  } else {
    return(x)
  }
}

f_na('-')
f_na(NA)
f_na(3)

df3[,] <- apply(df3, c(1,2), f_na)   # 2���� ������ �� ���� �Ұ�
sapply(df3, f_na)                    # 2���� ������ �� ���� �Ұ�

# 2���� ������ ���� ���Һ� ���� : apply
# 1���� ������ ���� ���Һ� ���� : sapply

# 2) 2~5��° �÷� �����÷����� ����
as.numeric(df3[,-1])
df3[,-1] <- apply(df3[,-1], c(1,2), as.numeric)  # ����
sapply(df3[,-1], as.numeric)                     # ����

# 3) �⵵�� ������� �� ���� �� ��
df3$total <- apply(df3[,-1], 1, sum)

# 4) �μ���ȣ ����
str_split(df3$deptno.name,'-')

f_split <- function(x) {
  str_split(x,'-')[[1]][1]
}

f_split(df3$deptno.name)                       # ���� ���� �Ұ�
df3$deptno <- sapply(df3$deptno.name, f_split) # ���� ���� ����(���Һ� ����)

# 5) �μ��� �� ��
tapply(df3$total, df3$deptno, sum)

# sapply, lapply, mapply�� 2���� ������ �� ���� ����
# 1. 2���� ������ ���� key�� ������ �и�
# 2. �� key�� �����͸� ���� ���·� �Լ��� ����
#    �� �������� �Լ��� ���͸� input ���ڷ� �ް� ��
# => �Լ� ��ü�� ���Ϳ����� ������ ��츸 �����Լ��� ���� ���� ����

df4 <- data.frame(col1=1:5, col2=6:10)

f1 <- function(x) {
  x * 10                # ���Ϳ��� ����(x�� ���� input ����)
}

f2 <- function(x) {
  if (x%%2 == 0) {      # ���Ϳ��� �Ұ�(x�� scalar�� input ����)
    x * 10
  } else {
    x * 20
  }
}

apply(df4, c(1,2), f1)
apply(df4, 1, f1)
sapply(df4, f1)         # key�� ����, 2���� ������ �� ���� ����
sapply(df4, f2)         # key�� ����, 2���� ������ �� ���� �Ұ�
                        # f2�� ���͸� input ���ڷ� ��� X 








