# 1. movie_ex1.csv ������ �а� 
df1 <- read.csv('movie_ex1.csv', stringsAsFactors = F)
head(df1)
str(df1)
# 1) ���ɴ뺰 ���� �̿������ ����� ���Ͽ���
aggregate(df1$�̿�_����... , list(df1$���� ,df1$���ɴ�), FUN=mean)
aggregate(�̿�_����... ~ ���� + ���ɴ�, data = df1, FUN = mean)

# 2) ���Ϻ� �̿������ ����� ���Ͽ���.
library(stringr)
vdate <- str_c(df1$��, df1$��, df1$��, sep='/')

# sol1) ��¥ ���� �� �и� ���б�ȣ ����
as.Date("2018/2/1", '%Y/%m/%d')

df1$DAY <- as.character(as.Date(vdate, '%Y/%m/%d'), '%A')

aggregate(�̿�_����... ~ DAY, data = df1, FUN = mean)[c(4,7,3,2,1,6,5),]

# sol2) 2�ڸ� ��, �� �������� ����
vdate2 <- str_c(df1$��, 
                sprintf('%02d', df1$��), 
                sprintf('%02d', df1$��))

as.Date(vdate2, '%Y%m%d')

# 2. delivery.csv ������ �а�
df2 <- read.csv('delivery.csv', stringsAsFactors = F)
head(df2)

# 1) ���ں� �� ��ȭ�Ǽ��� ���Ͽ���
df2_day <- aggregate(��ȭ�Ǽ�~����, data=df2, FUN=sum)

# 2) �������� �ֹ����� ���� �ð��븦 ���
# �߱����� 1  6
#          2  10
#          3  30
#          ...
#          24 30

# step1) ������ �ð��뺰 ��ȭ�Ǽ� �� ��
df2_1 <- aggregate(��ȭ�Ǽ� ~�ð���+����, data=df2, FUN=sum)

# step2) ������ ��ȭ�Ǽ� �ִ밪
df2_max <- aggregate(��ȭ�Ǽ� ~����, data=df2_1, FUN=max)

# step3) 1,2 ������ ����
merge(df2_1, df2_max, by=c('����','��ȭ�Ǽ�'))

# 3) ���ں� ���ϴ�� �������� ���Ͽ���
df2_day

# 20180201  39653  39653   
# 20180202  46081  39653
# 20180203  54124  46081
# 
# (46081 - 39653) / 39653 * 100

# step1) ���� �� ���� �������� ����� ���� �Լ� ����
f_shift <- function(x) {
  vshift <- c()
  for (i in 1:length(x)) {
    if (i==1) {
      vshift <- c(vshift, x[1])
    } else {
      vshift <- c(vshift, x[i-1])
    }
  }
  return(vshift)
}

f_shift <- function(x) {
  vshift <- c()
  for (i in 1:length(x)) {
    vshift <- c(vshift, x[max(i-1,1)])
  }
  return(vshift)
}

# [ ���� - �ܺ� ��Ű�� ��� ]
install.packages('data.table')
library(data.table)

v1 <- 1:10
shift(v1,n=1, type='lag')           # NA  1  2  3  4  5  6  7  8  9
shift(v1,n=1, fill=0, type='lag')   # 0  1  2  3  4  5  6  7  8  9
shift(v1,n=1, type='lead')          # 2  3  4  5  6  7  8  9 10 NA

# step2) ������ ��ȭ�Ǽ� �÷� ����
df2_day$before <- f_shift(df2_day$��ȭ�Ǽ�)

# step3) ������ ��� ������ ���
# (46081 - 39653) / 39653 * 100

df2_day$rate <- (df2_day$��ȭ�Ǽ�-df2_day$before)/df2_day$before * 100

# 3. get_query ����� ���� �Լ� ����
get_query <- function(sql, ip='localhost', port='1521',
                      sid='orcl', user='scott', pwd='oracle') {
  library(RJDBC)
  library(stringr)
  
  jdbcDriver <- JDBC(driverClass = "oracle.jdbc.OracleDriver", 
                     classPath = "C:/app/KITCOOP/product/11.2.0/client_1/ojdbc6.jar")
  
  vaddr <- str_c('jdbc:oracle:thin:@', ip, ':', port, ':', sid )
  
  con <- dbConnect(jdbcDriver, 
                   vaddr,
                   user,
                   pwd)
  
  df1 <- dbGetQuery(con, sql)
  
  return(df1)
}

save(list=ls(), file='my_function')
get_query('select * from employees', user='hr', port='1521')

########## ��������� �����Դϴ�. ##########

# sqldf 
# - R ���α׷����� sql�������� ó�� �����ϵ��� ���� ��Ű��
# - sql ������ ������ ���������� ���� �� ����
# - Ư�� ����(non equi join) ���� �� ����

install.packages('sqldf')
library(sqldf)

emp <- read.csv('emp.csv', stringsAsFactors = F)

# 1) ENAME, EMPNO �÷� ����
emp[,c('ENAME', 'EMPNO')]
sqldf('select ENAME, EMPNO from emp')

# 2) ALLEN�� �̸��� SAL ��� 
emp[emp$ENAME=='ALLEN',c('ENAME', 'SAL')]
sqldf('select ENAME, SAL from emp where ENAME='ALLEN'')    # X
sqldf('select ENAME, SAL from emp where ENAME=\'ALLEN\'')  # O
sqldf("select ENAME, SAL from emp where ENAME='ALLEN'")    # O


# [ �������� ]
# 1) ALLEN�� FORD�� �̸�, �μ���ȣ, ���� ���
emp[emp$ENAME %in% c('ALLEN','FORD'), c('ENAME','DEPTNO','SAL')]

v_sql1 <- "select ENAME, DEPTNO, SAL
             from emp
            where ENAME in ('ALLEN','FORD')"

sqldf(v_sql1)

# 2) emp �������� �� �μ��� �ִ뿬���� �̸�, �μ���ȣ, �������
emp_max <- aggregate(SAL~DEPTNO, data=emp, FUN=max)
merge(emp, emp_max, by=c('DEPTNO','SAL'))[, c('ENAME','DEPTNO','SAL')]

v_sql2 <- "select ENAME, DEPTNO, SAL
             from emp
            where (DEPTNO, SAL) in (select DEPTNO, max(SAL)
                                      from emp
                                     group by DEPTNO)"

sqldf(v_sql2)

v_sql3 <- "select e.ENAME, e.DEPTNO, e.SAL
             from emp e, (select DEPTNO, max(SAL) as max_sal
                            from emp
                           group by DEPTNO) i
            where e.DEPTNO = i.DEPTNO
              and e.SAL    = i.max_sal"

sqldf(v_sql3)


# [ sqldf�� outer join ���� ]
student <- read.csv('student.csv', stringsAsFactors = F)
professor <- read.csv('professor.csv', stringsAsFactors = F)

v_sql4 <- "select s.name, p.name
             from student s, professor p
            where s.profno = p.profno(+)"

sqldf(v_sql4)   # ���� �߻�

v_sql5 <- "select s.name, p.name
             from student s left outer join professor p
               on s.profno = p.profno"

sqldf(v_sql5) 

v_sql6 <- "select s.name, p.name
             from professor p right outer join student s
               on s.profno = p.profno"

sqldf(v_sql6)

# plyr
- apply �迭 �Լ��� ���� ����
- data frame������ ��� ����
- {}{}ply ������ �پ��� �Լ� ����
 input      output
����������  ����������

install.packages('plyr')
library(plyr)

# 1. plyr::adply
# - array input(matrix, data frame), data frame output
# - apply �Լ��� ���

# ����) iris data�� �ະ ���� �� ��
apply(iris[, -5], 1, sum)
apply(iris[, -5], 2, sum)

adply(iris[, -5], 1, sum)  # �ະ ����� ���� �����������ӿ� �߰�
adply(iris[, -5], 2, sum)  # ���� �����������Ӱ� �Բ� ��� �Ұ�

# [ ���� - adply�� mean �Լ� ���� ���� ]
apply(iris[, -5], 1, mean) # �ϳ��� �� ���޽� ���ͷ� ����
adply(iris[, -5], 1, mean) # �ϳ��� �� ���޽� ���������������� ����
adply(as.matrix(iris[, -5]), 1, mean) # ����

# [ ���� - sum�� mean�� ������ ���� ��Ŀ� ���� ���� ]
v1 <- 1:10
v2 <- data.frame(v1)

sum(v1)  # ����
sum(v2)  # ����
 
mean(v1) # ����  
mean(v2) # �Ұ�, mean�Լ��� �������������� ���� �Ұ�

# 2. plyr::ddply
# - �׷쿬�� ����(group by ����)
# - ���� ���� group by �÷� ����
# - ���� �÷��� �׷� ���� ����
# - �� �÷����� ���� �ٸ� �׷��Լ� ���� ����
# - �׷캰 �� ����

# ddply(.data = ,        # ������������
#       .variables = ,   # �׷� ���� �÷�
#       .fun = ,         # ���� �Լ�
#       ...)             # �������
# 

# ddply ���� �Լ�
# 1. summarise : �׷캰 ������ ���(sql group by ����� ���)
# 2. transform : ���� �����Ϳ� �׷� ���� ��� ���� ����
# 3. mutate : transform�� ����, ���� �׷� ���� ��� ���� ����
# 4. subset : �׷캰 ���� ���� ����(���� ����)

# ����) emp���� �μ��� ��� ����
ddply(emp, .(DEPTNO), summarise, avg_mean=mean(SAL))
ddply(emp, .(DEPTNO), transform, avg_mean=mean(SAL))

# ����) emp���� �μ��� ��� �������� ���� ������ �޴� ���� ���
emp_1 <- ddply(emp, .(DEPTNO), transform, avg_mean=mean(SAL))
emp_1[emp_1$SAL > emp_1$avg_mean, c('ENAME','DEPTNO','SAL','avg_mean')]

# ����) student���� �г⺰ Ű�� ���, �������� �ִ밪 ���
ddply(student, .(GRADE), summarize, v1=mean(HEIGHT), v2=max(WEIGHT))

# ����) student���� �г⺰, �а��� Ű�� ���
ddply(student, .(GRADE, DEPTNO1), summarize, v1=mean(HEIGHT))

# [ �������� ] 
# delivery.csv ������ �а�
# �� ���鵿�� ��ȭ�Ǽ��� �� ���� ���ϵ�, 
# (��, �� ���� ���ڸ� �����ϰ� �ִ� ��� 
# ���ڸ� ������ ������ ǥ���ϵ��� �� (ex ������6�� => ������))
de <- read.csv('delivery.csv', stringsAsFactors = F)

unique(de$���鵿)

# 1) �ּ� ����
str_split('�Ź���2��','[0-9]')[[1]][1]
str_remove_all('�Ź�����2��','[0-9��]')
str_remove_all('�Ź�����2����','[0-9].{1,}')

de$�� <- str_remove_all(de$���鵿,'[0-9].{1,}')

# 2) �׷� ����
ddply(de, .(��), summarise, CNT=sum(��ȭ�Ǽ�))


# ����) subset�� ����� �׷� ���� ����
#       student �����Ϳ��� �� �г⺰ Ű�� ���� ū �л� ���
ddply(student, .(GRADE), subset, HEIGHT==max(HEIGHT))


# mutate ��� ��
ddply(student, .(GRADE), mutate, v1=mean(HEIGHT), v2=log(v1))



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
# 2. unstack : long -> wide



