# 1. data2 �����͸� �а� 
df1 <- read.csv('data2.csv', stringsAsFactors = F)
library(stringr)

# 1) ������ ���� �뼱�� �������� �� ���� ǥ��
#     line_1  line_2  line_3  line_4
#      XXXXX   XXXXX   XXXXX   XXXXX

df1$���� <- as.numeric(str_remove_all(df1$����, ','))
df1$���� <- as.numeric(str_remove_all(df1$����, ','))

f1 <- function(x) {
  as.numeric(str_remove_all(x, ','))
}

df1[,c(3,4)] <- apply(df1[,c(3,4)], c(1,2), f1)

tapply(df1$���� + df1$����, df1$�뼱��ȣ, sum, na.rm=T)

# [ ���� - ��� ��� �÷��� ġȯ�� �ʿ�� ]
mean(c(1,2,3,'-'))           # '-'���� 0 �Ǵ� NA�� ġȯ �ʿ�
mean(c(1,2,3,0))             # 4���� ���
mean(c(1,2,3,NA), na.rm=T)   # 3���� ���

str_replace_all(c(1,2,3,'-'), '-', '0')
str_replace_all(c(1,2,3,'-'), '-', NA)
str_replace_na(NA, 0)
ifelse(c(1,2,3,'-')=='-', NA, c(1,2,3,'-'))


# 2) ���� ������ �������� �� ���� ǥ��
# ���� ����
# XXXX XXXXX

vtime <- as.numeric(str_sub(str_pad(df1$�ð�, 4, 'left', 0), 1, 2))
vtime2 <- ifelse(vtime < 12, '����','����')

tapply(df1$���� + df1$����, vtime2, sum)

# 2. emp2.csv �����͸� �а� 
df2 <- read.csv('emp2.csv', stringsAsFactors = F)

# 1) ���� ������ ���� ������ ������� ġȯ
# sol1) '-'�� NA�� �ҷ��� �� ������� ġȯ
df2 <- read.csv('emp2.csv', stringsAsFactors = F, na.strings = '-')
str_replace_na(df2$POSITION, '���')   # sql nvl�Լ��� ���

# sol2) '-'�� ������� ���� ġȯ(���� ���̽�)
df2 <- read.csv('emp2.csv', stringsAsFactors = F)
df2$POSITION[df2$POSITION=='-'] <- '���'

# sol3) '-'�� ������� ���� ġȯ(ifelse)
df2 <- read.csv('emp2.csv', stringsAsFactors = F)
ifelse(df2$POSITION=='-', '���', df2$POSITION)

# sol4) '-'�� ������� ���� ġȯ(ġȯ�Լ�)
df2$POSITION <- str_replace(df2$POSITION,'-','���')

# 2) ���޺� ��տ����� ���
df2$PAY[df2$PAY=='-'] <- NA
df2$PAY <- as.numeric(df2$PAY)
tapply(df2$PAY, df2$POSITION , mean, na.rm=T)

# 3. emp.csv ������ �а� ������ ���� ǥ��
emp <- read.csv('emp.csv', stringsAsFactors = F)

# deptno  ename 
# 10      CLARK KING MILLER     
# 20      SMITH JONES....    
# 30      ALLEN WARD...
str_c(emp[emp$DEPTNO==10, 'ENAME'], collapse = ' ')

vec1 <- tapply(emp$ENAME, emp$DEPTNO, str_c, collapse = ' ')
df3 <- as.data.frame(vec1)
df3$deptno <- rownames(df3)
colnames(df3)[1] <- 'ename'
df3[,c(2,1)]

# [ ���� - sql ���� �Լ� ]
# select deptno, 
#        listagg(ename,                  -- �����÷�
#                ' ')                    -- ������
#        within group(order by sal desc) -- ���ռ���
#   from emp
#  group by deptno;

########## ��������� �����Դϴ�. ##########

# ����
# 1. order
# - ���͸� ���� ����, ���� ���� ���� ����(1��, 2��,...���� �ʿ��)
# - �� ���͸��� ���� ���� ���� ����
# - ���ڿ� ���� ������ ���� ���� �� ���� ���� ���� ���� �߻�
#   (method ���� �ʿ�)
# - ���� ����� ���� ������� ��ġ�� ����
# - ������������ ���� ���� �Ұ�, ��ġ�� ��� �������� �ذ�

# 2. sort
# - �ϳ��� ���͸� ���� ����
# - ���� ������� ���ĵ� ��� ���� ����
# - ������������ ���� �Ұ�
 
# 3. doBy::orderBy
# - ������ ������ ���� ����
# - formular�� �����ϰ��� �ϴ� ���� �÷� ���� ����
# - formular�� +, - ��ȣ�� �� ���� ���� ���� ����


# sampling
# - ������ �м� �� train/test data�� �и��ϱ� ���� �ַ� ���
# - raw data�� ���� �����ϰ� ����� sample�� ��� ���� ���
 
# 1. sample
# - row number�� ����� ǥ�� ����
# - group name/group number�� ����� ǥ�� ����
# - class�� �յ� ���� �Ұ�(��ü������ raw data ������ ����ϰ� �����)
 
# [ ���� - iris data�� 70%, 30%�� �� �������� �и� ]
# 1) row number�� ����� ǥ�� ����
v_rn <- sample(1:nrow(iris), size = nrow(iris) * 0.7)

iris_train <- iris[v_rn, ]
iris_test <- iris[-v_rn, ]

nrow(iris_train)             # 150 * 0.7 = 105�� ���� Ȯ��
nrow(iris_test)              # 150 * 0.3 =  45�� ���� Ȯ��

table(iris$Species)          # 1 : 1 : 1�� ����
table(iris_train$Species)    # 1 : 1 : 1�� ������ �ƴ����� ���

# 2) group name/group number
v_gname <- sample(c('a','b'),          # a,b�� ������ ǥ�� ����
                  size = nrow(iris),   # ����� ǥ�� ����
                  replace = T,         # �������� ���
                  prob = c(0.7,0.3))   # a�� ���õ� Ȯ�� 70%

iris_train2 <- iris[v_gname=='a', ]
iris_test2 <- iris[v_gname=='b', ]

nrow(iris_train2)          # 112
nrow(iris_test2)           #  38

table(iris_train2$Species) # 37 : 37 : 38

# 2. doBy::sampleBy
# - data frame���� ���� ǥ�� ���� ����
# - ����� ǥ��(train) �̿��� ����(test) ������ �����
# - class�� �յ� ���� ����
library(doBy)
sampleBy(formula = ,     # ~ Y
         frac = ,        # ���� ����
         replace = ,     # ���� ���� ����
         data = )        # raw data

# [ ���� - iris data�� 70%, 30%�� �� �������� �и� ]
# step1) train set ����
iris_train3 <- sampleBy( ~ Species, frac = 0.7, data = iris)

# step2) ����� train set�� row number ȹ��
# sol1) ��ġ��� ����
v_lo <- str_locate(rownames(iris_train3), '\\.')[,1]
v_rn <- as.numeric(str_sub(rownames(iris_train3), v_lo+1))

# sol2) �и���� ����
str_split(rownames(iris_train3), '\\.')   # �� ������ ���� �����

f_split <- function(x) {
  as.numeric(str_split(x, '\\.')[[1]][2])
}

v_rn2 <- sapply(rownames(iris_train3), f_split)
sum(v_rn != v_rn2)

# step3) row number ��� �ݴ� ���� ����
iris_test3 <- iris[-v_rn, ]
iris_test4 <- iris[-v_rn2, ]

nrow(iris_train3)           # 105 * 0.7 = 105�� ����
table(iris_train3$Species)  # 1:1:1�� ��Ȯ�ϰ� �յ� ����

nrow(iris_test3)            # 105 * 0.3 = 45�� ����
table(iris_test3$Species)   # 1:1:1�� ��Ȯ�ϰ� �յ� ����


# ����
# - �� ���̺�(������������)�� ���θ� ����
# - equi join�� ����(non equi join �Ұ�)
# - ������ �÷����� ���� ����
# - �⺻ ������ inner join, outer join ����

merge(x,                       # ������ ù ��° ������������
      y,                       # ������ �� ��° ������������
      by = ,                   # ���� �÷�(������ �̸�)
      by.x = ,                 # ù ��° �������� ���� �÷�
      by.y = ,                 # �� ��° �������� ���� �÷�
      all = ,                  # full outer join ����(T/F)
      all.x = ,                # left outer join ����(T/F)
      all.y =                  # right outer join ����(T/F)
      suffixes = c(".x",".y")  # ���� �÷��� ���̾�
      )

# ����) emp.csv�� dept.csv ������ ���� �ҷ��� �� �����Ͽ�
# �� ������ �̸�, �μ���ȣ, �μ��̸� ���
emp <- read.csv('emp.csv', stringsAsFactors = F)
dept <- read.csv('dept.csv', stringsAsFactors = F)

merge(emp, dept, by='DEPTNO')[ , c('ENAME','DEPTNO','DNAME')]


# ����) student.csv�� professor.csv ������ ���� �ҷ��� �� �����Ͽ�
# �� �л��� �̸�, �г�, �������� �̸� ���
std <- read.csv('student.csv', stringsAsFactors = F)
pro <- read.csv('professor.csv', stringsAsFactors = F)

merge(std, pro, by='PROFNO')                # inner join
merge(std, pro, by='PROFNO', all.x = T)     # left outer join

merge(std, pro, by='PROFNO', all.x = T)[,c('NAME.x','GRADE','NAME.y')]

# [ ���� ���� ]
# emp.csv ������ �а� �� ������ ���������� �̸� ���
# ��, ���������ڰ� ���� ���� ���� �̸����� ġȯ
emp2 <- merge(emp, emp, by.x = 'MGR', by.y = 'EMPNO', all.x = T)
emp2 <- emp2[, c('ENAME.x', 'ENAME.y')]

str_replace_na(emp2$ENAME.y, 'ȫ�浿')
str_replace_na(emp2$ENAME.y, emp2$ENAME.x)              # ġȯ �Ұ�
ifelse(is.na(emp2$ENAME.y), emp2$ENAME.x, emp2$ENAME.y) # ġȯ ����

# [ �������� ]
# gogak.csv, gift.csv ������ �а� 
# �� ������ ���ɻ�ǰ�� ���(������ R ��������)
# 1) data loading
library('RJDBC')
jdbcDriver <- JDBC(driverClass = "oracle.jdbc.OracleDriver", 
                   classPath = "C:/app/KITCOOP/product/11.2.0/client_1/ojdbc6.jar")

con1 <- dbConnect(jdbcDriver, 
                 "jdbc:oracle:thin:@192.168.0.115:1521:orcl",
                 "scott",   # userid
                 "oracle")  # passwd

gogak <- dbGetQuery(con1, 'select * from gogak')
gift  <- dbGetQuery(con1, 'select * from gift')

# 2) for���� ����� ó��

gift[(gift$G_START <= 980000) & (980000 <= gift$G_END), 'GNAME']
gift[(gift$G_START <= 73000) & (73000 <= gift$G_END), 'GNAME']
gift[(gift$G_START <= 320000) & (320000 <= gift$G_END), 'GNAME']

vresult <- c()

for (i in gogak$POINT) {
  vgift <- gift[(gift$G_START <= i) & (i <= gift$G_END), 'GNAME']
  vresult <- c(vresult, vgift)
}

gogak$GIFT <- vresult


# 3) ����� �����Լ� + �����Լ�
f_gift <- function(x) {
  gift[(gift$G_START <= x) & (x <= gift$G_END), 'GNAME']
}

sapply(gogak$POINT, f_gift)


# �׷쿬��
# - �׷캰 Ư�� �Լ��� ����
# - �и� - ���� - ����

# 1. tapply
# - ���ͷ� ����
# - ���Ǻ� �׷쿬�� ����
# - ������������ �Է� �Ұ�
# - ���ÿ� ���� �÷� �׷쿬�� �Ұ�

tapply(emp$SAL, emp$DEPTNO, sum)
tapply(emp$SAL, emp$COMM, emp$DEPTNO, sum)

# 2. aggregate
# - �ΰ��� ����
# - ������������ ����
# - ���� �÷� ���� ����
# - ���� �÷� �׷��� ����
# - ���� ���� �÷��� ���� �ٸ� �Լ� ���� �Ұ�

aggregate(x,       # ������(����, ������������ ����)
          by,      # �׷���(group by �÷�)
          FUN,     # �����Լ�
          ...)     # �����Լ� �ʿ� ����

aggregate(formula, # ������ ~ �׷���
          data,    # ������������
          FUN,     # �����Լ�
          ...)     # �����Լ� �ʿ� ����

# 1) ���� ��� 1��, group  by  �÷� 1��
# ����) student �����Ϳ��� �� �г⺰ Ű�� ���
aggregate(std$HEIGHT, list(std$GRADE), mean, na.rm=T)
aggregate(HEIGHT ~ GRADE, std, mean, na.rm=T)

# 2) ���� ��� 2��, group  by  �÷� 1��
# ����) student �����Ϳ��� �� �г⺰ Ű, �������� ���

aggregate(std[,c('HEIGHT','WEIGHT')], 
          list(std$GRADE), mean, na.rm=T)

aggregate(HEIGHT + WEIGHT ~ GRADE, std, mean, na.rm=T)       # �Ұ�
aggregate(cbind(HEIGHT,WEIGHT) ~ GRADE, std, mean, na.rm=T)  # ����

# 2) ���� ��� 1��, group  by �÷� 2��
# ����) student �����Ϳ��� �� �г⺰, �а��� Ű ���
aggregate(std$HEIGHT, 
          list(std$GRADE, std$DEPTNO1), mean, na.rm=T)

aggregate(HEIGHT ~ GRADE + DEPTNO1, std, mean, na.rm=T)           

aggregate(cbind(HEIGHT,WEIGHT) ~ GRADE + DEPTNO1, 
          data = std, 
          FUN = c('min', 'max'), na.rm=T)         # �Ұ�

# [ ���� ���� ]
# student.csv ���ϰ� exam_01.csv ������ �а�
std <-read.csv('student.csv', stringsAsFactors = F)
exam <-read.csv('exam_01.csv', stringsAsFactors = F)

# 1) �� �г⺰ ���輺���� ����� ���ϼ���.
std2 <- merge(std, exam, 
              by = 'STUDNO')[, c('STUDNO','NAME','GRADE','TOTAL')]
aggregate(TOTAL ~ GRADE, data = std2, FUN=mean)

# 2) �� �г⺰ �ְ������� ���� �л� �̸�, ����, �г� ���
std_max <- aggregate(TOTAL ~ GRADE, data = std2, FUN=max)
merge(std2, std_max, by = c('GRADE', 'TOTAL'))



# 3. plyr::ddply()


# ������ ���� ����

# sprintf
# - ���ڷ� ����

# to_char(1234, '09999')=> '01234')
# to_char(1234, '99999')=> ' 1234')
# to_char(1234, '9999.99')=> '1234.00')

sprintf(fmt = ,  # ������ ����(s:���ڿ�, d:����, f:�Ǽ�)
        ...)     # ���� ���

sprintf('%5d', 1234)     # " 1234"
sprintf('%05d', 1234)    # "01234"
sprintf('%8.2f', 1234)   # " 1234.00"
sprintf('%7.2f', 1234)   # "1234.00"
sprintf('%.2f', 1234)    # "1234.00"
sprintf('%5s', 'abc')    # "  abc"















