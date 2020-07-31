# 1. subway2.csv ������ �а�
library(stringr)
df1 <- read.csv('subway2.csv ', stringsAsFactors = F, skip = 1)
head(df1)
str(df1)

# step1) �÷��� �ð������� ǥ��(X05.06 => 5)
colnames(df1)[-c(1,2)] <- as.numeric(str_sub(colnames(df1)[-c(1,2)], 
                                             2,3))

# step2) ����/���� ������ �и�
df1_1 <- df1[df1$����=='����', -2]
rownames(df1_1) <- df1_1$��ü
df1_1 <- df1_1[,-1]

df1_2 <- df1[df1$����=='����', -2]
rownames(df1_2) <- rownames(df1_1)
df1_2 <- df1_2[,-1]

# 1) ���� ������ �� ��
apply(df1_1, 1, sum)

# 2) ���� ������ �� ��
apply(df1_2, 1, sum)

# 3) �ð��뺰 �� ��
apply(df1[, -c(1,2)], 2, sum)

# 2.employment.csv ������ �а�
df2 <- read.csv('employment.csv', stringsAsFactors = F)

# 1) �⵵�� �ѱٷ��ϼ��� ���
# step1) �ѱٷ��ϼ� �÷� ����
head(df2)
df2_1 <- df2[ , df2[1,] == '�ѱٷ��ϼ� (��)']
df2_1 <- df2_1[-1,]
rownames(df2_1) <- df2$��������[-1]

# step2) '-'�� 0���� ġȯ
str_replace_all(df2_1,'-','0')                   # 2���� ���� �Ұ�
df2_1[,] <- apply(df2_1, c(1,2), str_replace_all, '-', '0')
df2_1[,] <- apply(df2_1, c(1,2), as.numeric)

# step3) �⵵�� ���
apply(df2_1, 2, mean)


# 2) �������º� ���޿��� ���
# (��ü�ٷ��ڿ� ��ü�ٷ���(Ư����������)�� ���� �׷��� �ǵ���)
# step1) ���޿��� �÷� ����
df2_2 <- df2[ , df2[1,] == '���޿��� (õ��)']
df2_2 <- df2_2[-1,]
rownames(df2_2) <- df2$��������[-1]

# step2) �÷� ����(X2007.4 => 2007)
colnames(df2_2) <- str_sub(colnames(df2_2), 2,5)

# step3) õ���� ���б�ȣ ���� �� ���ں���
f1 <- function(x) {
  as.numeric(str_remove_all(x, ','))
}

df2_2[,] <- apply(df2_2, c(1,2), f1)

# step4) �������º� ���(�ະ ���)
df2_2$total <- apply(df2_2, 1, mean)

# step5) ��ü�ٷ��ڿ� ��ü�ٷ���(Ư����������)�� �׷���� ���ϱ�
str_split(rownames(df2_2), '\\(')   # �� ������ ���� ���� �Ұ�

f2 <- function(x) {
  str_split(x, '\\(')[[1]][1]
}

vgroup <- sapply(rownames(df2_2), f2)

tapply(df2_2$total, vgroup, mean)


# ----------- �ٸ� Ǯ�� : �÷��� + ù��° �� ���հ����� �÷� ����
# step1) �÷� �� ù��° �� �� ����
v1 <- str_sub(colnames(df2)[-1], 2, 5)

f3 <- function(x) {
  str_split(x, ' ')[[1]][1]
}

v2 <- sapply(df2[1,][-1], f3)

# step2) ������ ������ �÷� ����
colnames(df2)[-1] <- str_c(v1,v2,sep='_')
df2 <- df2[-1,]

# step3) '-'�� NA�� ����
f_na <- function(x) {
  if (x=='-') {
    NA
  } else {
    x
  }
}

df2[,] <- apply(df2, c(1,2), f_na)

# step4) õ���� ���б�ȣ ���� �� ���� ����
df2[,-1] <- apply(df2[,-1], c(1,2), f1)

# step5) ���޿��� �÷� ����
df2[, str_detect(colnames(df2), '���޿���')]



vbool <- str_detect(colnames(df3), '2007') & 
  str_detect(df3[1,], '���޿���')

df3[,vbool, drop=F]


# ���� : f_shift �Լ� ����
# f_shift(vector, n) : ���ڿ��� ��� n��° ������ ��������
df1

f_shift <- function(vector, n=1) {
  v_vector <- vector
  for (i in 1:length(vector)) {
    if (v_vector[i] =='' | is.na(v_vector[i])) {
      v_vector[i] <- v_vector[i-n]
    }
  }
  return(v_vector)
}

f_shift(df1$��ü)
v1 <- c('a','','','b','','')
f_shift(v1)


# zoo::na.locf : NA���� ���� Ȥ�� ���� ������ ġȯ

install.packages('zoo')
library(zoo)

v2 <- c(1,NA,2,NA,3)
na.locf(v2)                # ������(ffill)���� ġȯ
na.locf(v2, fromLast = T)  # ���İ�(bfill)���� ġȯ

########## ��������� �����Դϴ�. ##########

# [ apply �ǽ� ���� ]
# ����) �� ������ 1�б� ������ �� ��
df1 <- read.csv('apply_test2.csv', stringsAsFactors = F)

# step1) '-', '.', '?' 0���� ġȯ
# sol1) str_replace_all + apply
str_replace_all(df1, '[-.?]', '0')  # ������������ ���� �Ұ�
apply(df1, c(1,2), str_replace_all, '[-.?]', '0')

# sol2) ����� ���� �Լ� + apply
f_rep <- function(x)  {
  if ((x=='-') | (x=='.') | (x=='?')) {     # x %in% c('-', '.', '?')
    '0'
  } else {
    x
  }
}

f_rep(0)
f_rep('-')
f_rep('?')

f_rep(df1)                  # 2���� ���� �Ұ�
apply(df1, c(1,2), f_rep)   # 2���� ���� ����, ġȯ �Ұ�(�յ� ����)

df1[,] <- apply(df1, c(1,2), str_trim, 'both')
df1[,] <- apply(df1, c(1,2), f_rep)

# step2) �����÷� ����
f2 <- function(x) {
  as.numeric(str_remove_all(x, ','))
}

df1[,-1] <- apply(df1[,-1], c(1,2), f2)
apply(df1[,-1], 2, f2)
sapply(df1[,-1], f2)

# step3) 1�б� �÷� ����
str_detect(colnames(df1), '1$')
df1_1 <- df1[ , str_detect(colnames(df1), '1$')]
rownames(df1_1) <- df1$name

# step4) ������ 1�б� ���� �� ��
apply(df1_1, 1, sum)

# ����
# 1. order(...,            # ���Ĵ��(����, ������������ �Ұ�)
#          na.last = T,    # NA ��ġ ����
#          decreasing = T) # ���� ����

v1 <- c(1,10,2,9,4)
order(v1)                  # ��ġ�� ����(1 3 5 4 2)
v1[order(v1)]              # ���İ��(������� �� ���ġ)

# ����) emp �����Ϳ��� sal�� ū ������� ����
emp <- read.csv('emp.csv', stringsAsFactors = F)
order(emp, decreasing = T)
vord <- order(emp$SAL, decreasing = T)
emp[vord, ]

# ����) emp �����Ϳ��� deptno�� ������ ����, 
# ���� deptno�������� sal�� ū ������� ����
vord <- order(emp$DEPTNO, emp$SAL, decreasing = c(F,T))
emp[vord, ]

# 2. sort
# sort(x,               # ���Ĵ��
#      decreasing = ,   # ���ļ���
#      ...)             # ��Ÿ�ɼ�

sort(v1)                # ���İ�� ���� ���
sort(emp$SAL)           # sort�� ����δ� ������������ ���� �Ұ�


# 3. doBy::orderBy
install.packages('doBy')
library(doBy)

doBy::orderBy(formula = ,   # Y ~ X1 + X2 + ...
              data = )      # ������ ������������

# ����) emp �����Ϳ��� sal�� ���� ������� ����
orderBy( ~ SAL, emp)

# ����) emp �����Ϳ��� deptno, sal�� ���� ������� ����
orderBy( ~ DEPTNO + SAL, emp)

# ����) emp �����Ϳ��� deptno�� ������, sal�� ���� ������� ����
orderBy( ~ DEPTNO - SAL, emp)

# [ ���� ���� ] 
# student.csv ������ �а�
# ��,�� ������� �����͸� �����ϰ�, ���� ������������ Ű�� ������
std <- read.csv('student.csv', stringsAsFactors = F)

# step1) �����÷� ����
std$g1 <- as.numeric(str_sub(std$JUMIN, 7,7))
std$g2 <- ifelse(std$g1==1,'M','F')
std$g3 <- ifelse(std$g1==1,'����','����')

# step2) order�� ���� ����
vord1 <- order(std$g1, std$HEIGHT, decreasing = c(F, T))
std[vord1, ]

vord2 <- order(std$g2, std$HEIGHT, decreasing = c(F, T),
               method = 'radix')  # ����, �����÷� ���� ���ļ��� ����
std[vord2, ]

vord3 <- order(std$g3, std$HEIGHT, decreasing = c(F, T),
               method = 'radix')  # �ѱ� ���� �÷� ��� �Ұ�
std[vord3, ]

# step3) orderBy�� ���� ����
orderBy( ~ g3 -HEIGHT, data = std)



# sampling
# 1. sample(x,          # ������ ���� ������(����)
#           size = ,    # ���� ����
#           replace = , # ���� ���� ����
#           prob = )    # ���� ����

sample(c(1,3,5,13,5,7), size = 1)
sample(c(1,3,5,13,5,7), size = 10)  # �����ܺ��� �� ū ǥ�� ���� �Ұ�
sample(c(1,3,5,13,5,7), size = 10, 
       replace = T)                 # �����ܺ��� �� ū ǥ�� ���� ����

sample(1:2, size=150, replace = T, prob = c(0.7, 0.3))

# [ ���� ���� ]
# iris �����͸� �����ϰ� ���ø� �Ͽ� ���� 70%�� 30% �����ͷ� �и�,
# df_train, df_test�� ����(�����ϰ� ���õ� �ο�ѹ��� Ǯ��)

v_rn <- sample(1:nrow(iris), size = nrow(iris) * 0.7)
iris[v_rn, ]

df_train <- iris[v_rn, ]
df_test <- iris[-v_rn, ]

nrow(df_train)   # 105
nrow(df_test)    # 45
