# [ ���� ���� ]
# subway2.csv ������ �а� ����, �ð��뺰 �������� �� ���� ���
sub <- read.csv('subway2.csv', stringsAsFactors = F, skip=1,
                na.strings = '')
head(sub)

# 1) ���̸� ��ĭ ä���(���� ���̸� ��������)
library(zoo)
sub$��ü <- na.locf(sub$��ü)

# 2) ���̸����� ȣ�� ���� �����
library(stringr)
str_remove_all('��9��(4)','[(0-9)]')     #[] �ȿ� ���� �Ϲݱ�ȣ
str_remove_all('����(4)','([0-9])')      # ()�� Ư����ȣ�� �ؼ�
str_remove_all('��9��(4)','\\([0-9]\\)')  # \\�� ����ؾ� �Ϲݱ�ȣȭ

unique(str_remove_all(sub$��ü, '\\([0-9]\\)'))
sub$��ü <- str_remove_all(sub$��ü, '\\([0-9]\\)')

# 3) ���� �������� �ð��뺰 �׷���(ddply)
library(plyr)
ddply(sub, .(��ü,����), summarise, v5=sum(X05.06),
                                    v6=sum(X06.07), 
                                    ........) # ��� �÷� ���� ����

# 4) �ð��� �÷��� stack ó��
library(reshape2)
sub2 <- melt(sub, id.vars = c('��ü','����'), 
             variable.name = '�ð���', value.name = '��������')

sub2$�ð��� <- as.numeric(str_sub(sub2$�ð���, 2,3))

# 5) ����, ��������, �ð��뺰 �� ��
ddply(sub2, .(��ü,����,�ð���), summarise, cnt=sum(��������))

# [ ���� - ����, �ð��뺰 �� �� ]
ddply(sub2, .(��ü, �ð���), summarise, cnt=sum(��������)) # long
dcast(sub2, ��ü ~ �ð���, sum)                            # wide


# doBy     : ~by (order, sample) ����
# plyr     : apply �迭 �Լ�(�����Լ�) ���� 
# reshape2 : stack/unstack ����
# dplyr    : ����ȭ�� R ���� ����(sqló��)

install.packages('dplyr')
library(dplyr)

# dplyr�� ����ȭ�� ����
# 1. select : �÷��� ����
# 2. mudate : �÷� ����
# 3. filter : �� ����
# 4. group_by : �׷쿬��
# 5. arrange : ����
# 6. summarise_each : �׷쿬���� ���� ���� ����

# ����1) emp ���̺����� �̸�, ���, ���� ����
emp <- read.csv('emp.csv', stringsAsFactors = F)

emp %>%                     # sql from�� ó�� ���� ������ ���� �� ����
  select(ENAME, EMPNO, SAL)

# ����2) emp ���̺����� �̸�, ���, ����, 10% �λ�� ���� ���
emp %>%                     # sql from�� ó�� ���� ������ ���� �� ����
  select(ENAME, EMPNO, SAL) %>%
  mutate(new_sal=SAL*1.1)

# ���� : ������ ������ ���� �Ľ� ���� ����(�÷� ���� ���� �޶���)
emp %>%                     
  select(ENAME, EMPNO) %>%   # ENAME, EMPNO�� ���� �������� ����
  mutate(new_sal=SAL*1.1)    # mutate������ SAL�� �� �� ����

emp %>%
  mutate(new_sal=SAL*1.1) %>%
  select(ENAME, EMPNO, new_sal) 
   

# ����3) emp ���̺����� 10�� �μ����� ���� �̸�, �μ���ȣ, ���� ���
emp %>%
  select(ENAME, DEPTNO, SAL) %>%
  filter(DEPTNO==10)

# ����4) emp ���̺����� 10�� �μ����� ���� �̸�, �μ���ȣ, ���� ���
# ��, ���������� ����
emp %>%
  select(ENAME, DEPTNO, SAL) %>%
  filter(DEPTNO==10) %>%
  arrange(desc(SAL))

# ����5) emp ���̺����� �� �μ��� ��� ���� ���
emp %>%
  select(DEPTNO, SAL) %>%
  group_by(DEPTNO) %>%
  summarise_each(mean, SAL)


# ����6) emp ���̺����� HIREDATE �÷� ���� ��ü ����
emp[ , c('EMPNO','ENAME','JOB')]
emp[ , -5]
emp[ , colnames(emp) != 'HIREDATE']

emp %>%
  select(-HIREDATE) 

# [ ���� ���� ]
# student.csv ������ �а�
std <- read.csv('student.csv', stringsAsFactors = F)

# 1. �� �л��� �̸�, �г�, ������ȣ ���
std %>%
  select(NAME, GRADE, PROFNO)

# 2. �� ������ ������ȣ�� ���� �л��� ����
std %>%
  select(NAME, GRADE, PROFNO) %>%
  filter(!is.na(PROFNO))

# 3. �� ������ �� �л��� ���� �÷� �߰��Ͽ� ���
std %>%
  select(NAME, GRADE, PROFNO, JUMIN) %>%
  filter(!is.na(PROFNO)) %>%
  mutate(v1=ifelse(str_sub(JUMIN,7,7)=='1','��','��'))
  
std %>%
  filter(!is.na(PROFNO)) %>%
  mutate(v1=ifelse(str_sub(JUMIN,7,7)=='1','��','��')) %>%
  select(NAME, GRADE, PROFNO, v1) 

# 4. �� �����Ϳ��� �г⺰ ���� 
std %>%
  filter(!is.na(PROFNO)) %>%
  mutate(v1=ifelse(str_sub(JUMIN,7,7)=='1','��','��')) %>%
  select(NAME, GRADE, PROFNO, v1) %>%
  arrange(GRADE, v1)
  
# 5. �г⺰ Ű ���
std %>%
  select(GRADE, HEIGHT) %>%
  group_by(GRADE) %>%
  summarise_each(mean, HEIGHT)

std %>%
  select(GRADE, HEIGHT, WEIGHT) %>%
  group_by(GRADE) %>%
  summarise_each(mean, c(HEIGHT, WEIGHT))

std %>%
  select(GRADE, HEIGHT, WEIGHT) %>%
  group_by(GRADE) %>%
  summarise_each(c(mean,max), c(HEIGHT, WEIGHT))

# summarise_each�� across ��ü
std %>%
  select(GRADE, HEIGHT, WEIGHT) %>%
  group_by(GRADE) %>%
  summarise(across(c(HEIGHT, WEIGHT), c(mean,max)))


# which.min, which.max 
# - �ִ밪�� �ּҰ��� ���� index ����(��ġ)

library(googleVis)
Fruits

df1 <- dcast(Fruits, Fruit ~ Year, value.var = 'Sales')
vord <- which.max(df1$`2008`)

df1[vord, 'Fruit']

# kimchi_test.csv ������ �а�
df2 <- read.csv('kimchi_test.csv', stringsAsFactors = F)

# 1) 1�� �Ѱ���ġ�� ������Ʈ �Ǹŷ��� �Ǹűݾ� ���(dplyr)
df2 %>%
  filter((�Ǹſ�==1) & (��ǰ=='�Ѱ���ġ') & (�Ǹ�ó=='������Ʈ')) %>%
  select(����, �Ǹűݾ�)

# 2) �⵵�� ���� ��ü �Ǹŷ��� �� �� ���(dplyr)
df2 %>%
  select(�Ǹų⵵,�Ǹſ�,����) %>%
  group_by(�Ǹų⵵,�Ǹſ�) %>%
  summarise_each(sum, ����)

# 3) �⵵�� �Ǹŷ��� ���� ���� ��ġ ���
# sol1) ddply - long data
df3 <- ddply(df2, .(�Ǹų⵵, ��ǰ), summarise, total=sum(����))
ddply(df3, .(�Ǹų⵵), subset, total==max(total))

# sol2) which.max - wide data
df4 <- dcast(df2, ��ǰ ~ �Ǹų⵵, sum, value.var = '����')


f1 <- function(x) {
  vord <- which.max(x)
  df4[vord,1]
}

f1(df4$`2013`)
f1(df4$`2014`)
f1(df4[,-1])             # 2���� ���� �Ұ� 
apply(df4[,-1], 2, f1)

# str_extract_all(���ڿ�, ����)
# - ���ϴ� ������ ���ڿ� ����
# - ���Ͽ� ���Խ� ǥ�� ����
# - ����Ʈ ���

# ����) ������ ���̸����� �� �̸��� ����
str_extract_all('������2��(2)', '[��-�R]')
str_c(str_extract_all('������2��(2)', '[:alpha:]')[[1]], collapse='')


