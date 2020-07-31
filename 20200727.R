# ������ ���� ���ڸ� ���� ����� ���� �Լ�
f1 <- function(...) {
  var1 <- list(...)
  for (i in var1) {
    �ݺ�����
  }
}

# ����) �Էµ� ���� �� ���� ����ϴ� ����� �����Լ� ����
# fsum(2,6,8,43)
fsum <- function(...) {
  vsum <- c(...)
  vhap <- 0
  for (i in vsum) {
    vhap <- vhap + i
  }
  return(vhap)
}

fsum(2,6,8,43,1)


# ���������� ��������
# �������� : ������ ���ǰ� ���� ��ü ����
# �������� : ������ ���ǰ� �Ϻ� �Լ����� ��ȿ

# 1)
v1 <- 1               # �Լ� �ۿ��� ����, ��������

f1 <- function(x) {
  print(v1)
}

f1()

# 2)
v1 <- 1               # ��������

f2 <- function(x) {
  v1 <- 10            # �Լ� �� ����, ��������
  print(v1)
}

f2()

# 3) 
f3 <- function(x) {
  v2 <- 10            # ��������, f3 �Լ������� ��� ����
  print(v2)
}

f4 <- function(x) {
  print(v2)
}

f3()
f4()                  # object not found error
v2                    # object not found error


# 4) ���������� �������� ���� 
f3 <- function(x) {
  v2 <<- 10            # ��������, f3 �Լ������� ��� ����
  print(v2)
}

f4 <- function(x) {
  print(v2)
}

f3()                  # 10
f4()                  # 10
v2                    # 10

# �� ���ǿ� ���ǵ� ���� ���� �� ����
ls()
rm('v2')
ls()

# iris data set : ���� ǰ��(3��)�� �����ϴ� �з��� �׽�Ʈ�� data
iris
str(iris)

# [ �����ͺм� ]
# 1. �����н� : Y(����, ������)�� �˷��� ����� �м� ����
# 1) ȸ�� ��� �м� : Y(����, ������)�� �������� ���
# 2) �з� ��� �м� : Y(����, ������)�� factor�� ���

# 2. �������н� : Y(����, ������)�� �˷����� ���� ����� �м� ����


# ���� �����
# 1. read.csv : csv(�ĸ��� �и����е�) ������ �ҷ����� �Լ�
# - header = T : ù ���� �÷�ȭ ��ų�� ����
read.csv('read_test.csv', header = F)  # header=T
read.table('read_test.csv')            # header=F

# - sep = "," : �� ������ �и� ���б�ȣ
read.csv('read_test.csv', sep=',')     # sep=','
read.table('read_test.csv', sep=',')   # sep=''

read.csv('test1.txt', sep=':', header = F)        
read.table('test1.txt', sep=':') 

# - row.names : �ҷ��ö� �� ���� �̸� �ο�
# - col.names : �ҷ��ö� �� �÷� �̸� �ο�
read.csv('test1.txt', sep=':', header = F,
         col.names = c('name','deptno','sal'))

# - na.strings = "NA" : NA�� ó���� ���ڿ�
df1 <- read.csv('read_test.csv', 
                na.strings =c('.','-','?','!','null','nan'))
str(df1)

# - nrows = -1, : �ҷ��� ���� ��
read.csv('emp.csv', nrows = 5)

# - skip = 0 : ��ŵ�� ���� ����
read.csv('emp.csv', skip = 1, header = F)

# - stringsAsFactors = T : ���� �÷��� ����ȭ ����
# - encoding = "unknown" : ���ڵ� �ɼ�
read.csv('emp.csv', encoding='cp949')

 
# 2. write.csv(x,        # ������ ��ü �̸�
#              file = ,  # ������ �ܺ� ���� ��
#              sep = )   # ����� �и� ���б�ȣ

df2 <- data.frame(col1=c('a','b'),
                  col2=c(1,5))
write.csv(df2, 'df2_write_test.csv')

# 3. scan 
# - �ܺ� ������ "����"�� �ҷ�����
# - ���ϸ� ������ ����ڿ��� �� �Է� ���
# - �⺻�� ���� �ε�, ���� �ε��� what='' �ʿ�

scan()           # ���� ���� �Է�, ���� �Էµɶ�����
scan(what = '')  # ���� ���� �Է�, ���� �Էµɶ�����

scan('file1.txt', sep = ',')             # ���� �ε�
scan('file1.txt', sep = ',', what = '')  # ���� �ε�

# 4. readLines 
# - �ܺ� ������ ���ͷ� �ҷ�����
# - �� ������ ������ ���ҷ� �ҷ���
readLines('file1.txt')

# 5. readline
# - ����ڿ��� �Է� ���
# - �ҷ��� ���� ���������� ����
ans1 <- readline('���� �����Ұǰ���? (Y|N) :')
ans1

# if (ans1 == 'Y') {
#   ���� ����
# } else {
#   print('������ �������� �ʰڽ��ϴ�')
# }

# ----
print('�� ���� �Է¹޾� ���ϴ� ���α׷�')

no1 <- as.numeric(readline('ù ��° ���ڸ� �Է��ϼ��� : '))
no2 <- as.numeric(readline('�� ��° ���ڸ� �Է��ϼ��� : '))

no1 + no2



# [ ���� ���� ]
# seoul_new.txt ������ �ҷ��ͼ� ������ ���¸� ���� 
# ������������ ����
# id                       text                  date    cnt
# 305 ���������㿡 ���� ��Ź�� ���� �Դϴ�. 2017-09-27  2 
library(stringr)
data1 <- readLines('seoul_new.txt')

v_str <- str_trim(data1[1], side = 'both')
v_str <- str_split(v_str, ' ')[[1]]
vid <- v_str[1]
vlen <- length(v_str)
vcnt <- v_str[vlen]
vdate <- v_str[vlen-1]
vtext <- str_c(v_str[2:(vlen-2)], collapse = ' ')

vid <- c() ; vcnt <- c() ; vdate <- c() ; vtext <- c()

for (i in data1) {
  v_str <- str_trim(i, side = 'both')
  v_str <- str_split(v_str, ' ')[[1]]
  vlen <- length(v_str)
  vid <- c(vid, v_str[1])
  vcnt <- c(vcnt, v_str[vlen])
  vdate <- c(vdate, v_str[vlen-1])
  vtext <- c(vtext, str_c(v_str[2:(vlen-2)], collapse = ' '))
}

df_data <- data.frame(id=vid, text=vtext, date=vdate, cnt=vcnt)
head(df_data)

df_data$id <- as.numeric(df_data$id)
df_data$cnt <- as.numeric(df_data$cnt)



# 6. �����ͺ��̽� �����(oracle)

oracle
- server : instance(memory) + DB(disk)
- client : instance(memory)









