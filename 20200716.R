# R ��ġ�� ���� ����
# 1. R ��ġ
# - ��ġ ��ġ : Program Files ����
# - 32 bit�� ��ġ���� ���� ����
# 
# 2. java ��ġ 
# - ��ġ ��ġ : Program Files ����
# 
# 3. R ���� �� rJava ��Űġ ��ġ �� �ε� Ȯ��
# (������ �ڹ� �н� ���)
# install.packages('rJava')
# library('rJava')

# 4. cmd���� R ���� �����ϵ��� R��ġ��ġ PATH ���
# 
# 5. RStudio ��ġ �� ȯ�漳��

# ���� : ���� ���� �ּ�ó�� �� ����(ctrl + shift + c)

# ȯ�漳��
# - �۾� ���丮(Ȩ���丮) : C:/Users/KITCOOP/Documents
getwd()                      # Ȯ��
setwd('C:/Users/KITCOOP/')   # ����(���� ���ǿ����� ��ȿ)
                             # rstudio ȯ�漳������ ���� ���� ����

# ���� : �������� ����� �����ϱ� ���� ��ü
������ <- ��� Ȥ�� ������
a1 = 1
1 -> a1

a1 <- (1 + 10) * 101 / 200
a1 * 100
b1 <- 100
a1 + b1

c1 <- 'abc'
d1 <- "abc"

# ������ Ÿ���� Ȯ���ϴ� �Լ� : class
class(a1)  # numeric
class(c1)  # character

# ������ ���� : sum, mean, c
sum1 <- 1 + 1
vsum <- 1 + 1

c(1,2,3)


# �������
a1 + 1        # ���� + ���� ����
a1 + b1       # ���ں��� + ���ں��� ����
e1 <- '100'
e1 + 1        # ���� + ���� ����, ����ó�� ���� ������ �� ��ȯ �߻� X

d1 <- Sys.Date()  # sysdate
class(d1)    
d1 + 100          # ��¥ + ���� ���� ����(�⺻���� day)

# �� ��ȯ �Լ�
as.numeric()     # to_number
as.character()   # to_char
as.Date()        # to_date

as.numeric(e1) + 1

# ������ �������� �� ����
1:10     # ������ ���� �迭 ���� ����
'a':'f'  # ������ ���� �迭 ���� �Ұ�

seq()
help(seq)

seq(from = 1,  # ���۰� 
    to = 1,    # ����
    by = 1)    # ������

seq(from=1, to=10, by=2)
seq(1, 10, 2)
seq(to=20,from=10,by=5)

# ���� -> ��¥ �Ľ�
2020/01/01 ~ 2020/01/31
seq(as.Date('2020/01/01'), as.Date('2020/01/31'), by=1)

as.Date('2020/01/01')
as.Date(x='06/30/2020', format='%m/%d/%Y')
as.Date('2020/01/01','%Y/%m/%d')

# ��¥ -> ����(��¥�� ���� ����)
d1 <- Sys.Date()
d2 <- as.character(d1, '%m-%d,%Y')
class(d2)

# [ ���� : ��¥ ���� Ȯ�� ��� ]
help(as.Date)   # �Ұ�
help(strptime)  # ����

# %Y : 4�ڸ� �⵵
# %y : 2�ڸ� �⵵
# %m : ��
# %d : ��
# %H : ��
# %M : �� 
# %S : ��
# %w : ���ڿ���(�Ͽ���:0)
# %A : ���ڿ���

# �Լ��� ��� ���
# - ������ ������� ���� ����, �� ������ �̸� ���� ����
# - ������ ������ �ٸ� ������ ���� ���� �� �ݵ�� ���� �̸� ����


# [ ���� ���� ]
# 1. 2020�� 8�� 1�Ϻ��� 2020�� 8�� 31�ϱ����� ��¥�� ��� ���,
# �ش� ��¥�� ������ ���
v1 <- seq(as.Date('2020/08/01'), as.Date('2020/08/31'), 1)
class(v1)  # ��¥ ����

as.character(v1, '%A')

# 2. 2020�� 8�� 15�Ϸκ��� ���� ��¥���� ���� �ϼ� ���
v3 <- as.Date('2020/08/15') - Sys.Date()
as.numeric(v3 + 100)
class(v3)


# package : �Լ��� ����, �ܺ� package�� �ٿ�ް� �Ǹ�
# ���Ǹ��� �ش� package�� �ε��ؼ� ���

# ��¥ ���� �ܺ� ��Ű�� : lubridate
install.packages('lubridate')
library(lubridate)

todate <- Sys.Date()
as.character(todate, '%Y')  # as.character�� ����� ��¥ ��ȯ

year(todate)   # �⵵
month(todate)             # ��, ��������
month(todate, label = T)  # ��, ������¥�� ��� �������� ���

day(todate)    # ��

wday(todate)              # ����, ��������
wday(todate, label = T)   # ����, ��������

hour(todate)   # �ð�
minute(todate) # ��
second(todate) # ��

todate + 7         # 7�� ��
todate + days(7)   # 7�� ��
todate + hours(7)  # 7�ð� ��
todate + months(7) # 7���� ��
todate + years(7)  # 7�� ��

lubridate::

# [ ���� : ��¥ ��� ���� ]  
Sys.setlocale('LC_TIME','C')       # ����
Sys.setlocale('LC_TIME','KOREAN')  # �ѱ�

# [��������]
# 2020�� 7���� �Ϻ� �����͸� ���,
# ���� v_year��� �÷�(����)�� �⵵��,
# v_month��� �÷�(����)�� ����, 
# v_day��� �÷�(����)�� �ϸ� �и�����
# v_bonus_date �÷��� 6���� �� �����͸� �Է�
s1 <- seq(as.Date('2020/07/01'), as.Date('2020/07/31'),1)

v_year <- year(s1)
v_month <- month(s1)
v_day <- day(s1)

v_bonus_date <- s1 + months(6)

# [ ���� Ȯ�� �� ���� ]
objects()   # ���� ���ǿ� ���ǵ� ���� ��� Ȯ��
ls()        # ���� ���ǿ� ���ǵ� ���� ��� Ȯ��

sum <- 10
rm(sum)                 # sum ���� �ϳ� ����
rm(list = c('v1','v3')) # ���� ������ ����(v1,v3)

rm(list = objects()) # ���� ������ ��� ���� ����
rm(list = ls())      # ���� ������ ��� ���� ����


# ��� ������
7 %/% 3   # ��
7 %% 3    # ������
2 ^ 3     # �ŵ�����(2�� 3��)
2 ** 3    # �ŵ�����(2�� 3��)
1e1       # 10
2e1       # 20
1e2       # 100
1e-1      # 0.1


# �ڷᱸ��
# 0. ��Į�� : �� �ϳ��� ���
# 1. ����(vector) : ������, ���� ���� ���ÿ� ����, ���� ���� Ÿ��
# 2. ����Ʈ(list) : ������, ���� ���� ���ÿ� ����, ���� �ٸ� Ÿ��
# 3. ���(matrix) : 2����, ���� ���� Ÿ��
# 4. �迭(array) : 3���� �̻�, ���� ���� Ÿ��
# 5. ������������(data.frame) : 2����, ���� �ٸ� Ÿ��

a1 <- c(1,2,3,10)
b1 <- c(1,2,3,'a')




