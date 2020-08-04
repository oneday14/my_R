# 1. card_history.csv ������ �а�
# �� �Ϻ� ǰ�� ���� ������ ���

# NUM   �ķ�ǰ     �Ǻ�    �ܽĺ�      å�� �¶��μҾװ���    �Ƿ��
# 1   8.629893 63.61210  3.825623 12.900356       2.491103  8.540925
# 2  11.568525 62.74101  3.647733 13.548723       1.719646  6.774362
# 3  14.757049 53.08938  4.499100 13.197361       4.499100  9.958008
df1 <- read.csv('card_history.csv', stringsAsFactors = F)
library(stringr)

df1 <- df1[,-1]

f1 <- function(x) {
  as.numeric(str_remove_all(x,','))
}

# step1) õ���� ���б�ȣ ���� �� ���� ����
df1[,] <- apply(df1, c(1,2), f1)

# step2) ������ ���� ã��
df1[1,] / sum(df1[1,]) * 100
df1[2,] / sum(df1[2,]) * 100
...
df1[30,] / sum(df1[30,]) * 100

# step3) ���� ���� ���� �� ��ü ����
f2 <- function(x) {
  x / sum(x) * 100
}

df1[,] <- t(apply(df1, 1, f2))

# 2. kimchi_test.csv ������ �а�
# 1) ��ġ�� �Ǹŷ��� ���� ���� �� ���
df2 <- read.csv('kimchi_test.csv', stringsAsFactors = F)

# step1) ��ġ�� ���� �Ǹŷ��� �� �� ������ ����
# �Ѱ���ġ 1   9865
# �Ѱ���ġ 2   9754
# �Ѱ���ġ 3   8755
library(plyr)
df2_1 <- ddply(df2, .(��ǰ, �Ǹſ�), summarise, v1=sum(����))

# step2) ��ġ�� �Ǹŷ��� �ִ밪�� ���� �� ����(��)
ddply(df2_1, .(��ǰ), subset, v1==max(v1))

# 3. �ε���_�Ÿ�������Ȳ.csv ������ �а�
df3 <- read.csv('�ε���_�Ÿ�������Ȳ.csv', stringsAsFactors = F, 
                skip=1)

colnames(df3)
head(df3)

# 1) ���� �� ������ Ȱ���� ������ ��� ���
# step1) ���ʿ��� ��� �÷� ���� �� ���̸� ����
df3_1 <- df3[-c(1,2), ]
rownames(df3_1) <- df3_1[,1]
df3_1 <- df3_1[,-1]
head(df3_1)

# step2) Ȱ���� ������ �ѻ��� ���� �и�
df3_2 <- df3_1[ , df3[1,-1] == 'Ȱ����']
df3_3 <- df3_1[ , df3[1,-1] == '�ѻ���']

# step3) �÷��̸� ����
colnames(df3_2) <- str_sub(colnames(df3_2),1,2)
colnames(df3_3) <- str_sub(colnames(df3_2),1,2)

# step4) ���ڷ� ����
df3_2[,] <- apply(df3_2, 2, as.numeric)

# step5) �� ����, ���� Ȱ���� ���� ��� ���
df3_2$month <- str_sub(rownames(df3_2), 6,7)
ddply(df3_2, .(month), summarise, v1=mean(����),
      v2=mean(�λ�),
      v3=mean(�뱸))

str(df3_2)

# 2) �⵵�� Ȱ���� ������ �ѻ��� ������ ��� ���
df3_2$year <- str_sub(rownames(df3_2), 1,4)
df3_2_mean <- as.data.frame(apply(df3_2[,1:7],1,mean))
colnames(df3_2_mean) <- 'mean'

ddply(df3_2_mean, .(df3_2$year), summarise, v1=mean(mean))

########## ��������� �����Դϴ�. ##########

# ������ ����
# 1. long data(tidy data)
# - ������ �����ͺ��̽��� ���̺� ����
# - �ϳ��� ���� ����� �ϳ��� �÷��� �����ϴ� ���
# - ���� ����
# - group by ���� ����
# - ���� ������ ���� ����

# 2. wide data(cross table)
# - �ַ� ������� ������ ǥ�� ����(����ǥ)
# - �ະ, ���� ������ ����(group by ������ ��ü)
# - �ð�ȭ �� �ʿ�
# - ���� ���� �Ұ�
# - �÷��� ���� �߰�/���� �߻�

# ������ ���� ����
# 1. stack : wide -> long
stack(x,      # data frame
      ...)    # ��Ÿ �ɼ�

# 2. unstack : long -> wide
unstack(x,    # data frame
        ...)  # formular : value column ~ index column

df1 <- data.frame(apple=c(10,20,30),
                  banana=c(20,25,30),
                  mango=c(5,6,7))

df2 <- data.frame(month=c(1,2,3),
                  apple=c(10,20,30),
                  banana=c(20,25,30),
                  mango=c(5,6,7))

df3 <- stack(df1)
stack(df2)  #  month �÷��� stack ó����(���� ǥ�� X)

unstack(df3, values ~ ind)

# [ ���� ���� ]
# melt_ex.csv ������ �а� ���� ������ ���� �Ʒ��� ���� ���̺� �ϼ�
#         1   2   3  4  5 ....   12(��)
# 2000  400 401 402  .
# 2001  412

df1 <- read.csv('melt_ex.csv', stringsAsFactors = F)
df2 <- unstack(df1, latte ~ mon)
rownames(df2) <- c(2000,2001)
colnames(df2) <- str_c(1:12,'��')

stack(df1)



# reshape2 ��Ű��
# - stack�� unstack ó���� ���� �� ����ϰ� ǥ��
# - stack������ stack�� �÷��� stack���� ���ƾ� �� �÷� ���� ����
# - unstack������ ���� �����, �÷����⿡ ǥ���ؾ��� ��� ���� ����

install.packages('reshape2')
library(reshape2)

# 1. melt
# - stack ó�� �Լ�(wide -> long)
# - stack �÷� ���� ����
# 
# melt(data,             # ���� ������ ������
#      id.vars=,         # stack�� ���� �÷�
#      measure.vars=,    # stack ó�� �� �÷�(������ id.vars ���� ���)
#      na.rm = F,        # NA ǥ�� ����
#      value.name = ,    # value column �̸� ����
#      variable.name = ) # index column �̸� ����

df2 <- data.frame(month=c(1,2,3),
                  apple=c(10,20,30),
                  banana=c(20,25,30),
                  mango=c(5,6,7))
stack(df2)
melt(df2, id.vars = 'month')
melt(df2, id.vars = 'month', variable.name='�����̸�', 
                             value.name='����')


# ����) melt_ex.csv ������ �ǹ��ִ� tidy data�� ����
df1 <- read.csv('melt_ex.csv', stringsAsFactors = F)
melt(df1, id.vars = c('year','mon'), variable.name = '�̸�',
                                     value.name = '����')


# [ ���� ���� ] 
# 2000-2013��_���ɺ��Ǿ���_40-49��.csv ������ �а�
# �ش� �����͸� �⵵�� ���� ������ ����(tidy)�� ���
library(reshape2)
library(stringr)

df1 <- read.csv('2000-2013��_���ɺ��Ǿ���_40-49��.csv')
df2 <- melt(df1, id.vars = '��', variable.name = '�⵵',
                                 value.name='�Ǿ���')

df2$�⵵ <- as.numeric(str_remove_all(df2$�⵵, '[X��]'))

# 2. dcast
# - unstack ó��(long -> wide)
# - ���� ���̺� ����
# - �����(index column), �÷� ����, value ǥ�� �÷� �ʿ�

dcast(data,              # data frame
      formula = ,        # �� ���� ~ �÷� ����
      fun.aggregate = ,  # ����Լ�(�ʿ�� ����, ������ ����)
      ...,               # �Լ� �߰� ����
      drop = T,          # ���� ���
      value.var = )      # value column(���� �� �� �� �÷�)

# ����1) dcast_ex1.csv �� �а� ������ ���� �������� ��ġ
#       qty	price
# latte	100	2200
# mocha	80	2500
dcast1 <- read.csv('dcast_ex1.csv')
dcast(dcast1, name ~ info)

# ����2) dcast_ex2.csv�� �а� �⵵�� ǰ�� ���� ���̺� ����
dcast2 <- read.csv('dcast_ex2.csv')
dcast(dcast2, year ~ name)                    # price�� ���� �������̺�
dcast(dcast2, year ~ name, value.var = 'qty') # qty�� ���� �������̺�

# ����3) dcast_ex3.csv�� �а� �⵵�� ������ ���� ���̺� ����
dcast3 <- read.csv('dcast_ex3.csv')
dcast(dcast3, �⵵ ~ ����, sum)

# [ ���� ���� ]
# ��ݱ���������������Ȳ_new.csv ������ �а�
df_data <- read.csv('��ݱ���������������Ȳ_new.csv')
library(reshape2)

# 1) ������ ���� �������̺��� ǥ��
#       1     2     3     4     5     6
#�ڵ��� 1     0.85 0.75  0.98  0.92  0.97
#�ְ�� 0.90 0.92  0.68  0.87  0.89  0.89

df_data2 <- dcast(df_data, �̸� ~ ��)

# 2) ����� ���뵵 ���
rownames(df_data2) <- df_data2$�̸�
df_data2 <- df_data2[,-1]

apply(df_data2, 1, mean)

rowSums(df_data2)
rowMeans(df_data2)
colSums(df_data2)
colMeans(df_data2)
