# 1. kimchi_test.csv ������ �а�
kimchi <- read.csv('kimchi_test.csv', stringsAsFactors = F)
head(kimchi)
library(plyr)

# 1) �� (�⵵�� ��ǰ��) �Ǹŷ��� �Ǹűݾ��� ����� ���ϰ�
#    �⵵�� �� ��ǰ�� �Ǹŷ��� �������̸� plot �׷����� ǥ��
kimchi2 <- ddply(kimchi, .(�Ǹų⵵, ��ǰ), summarise, 
                 v1=mean(����), v2=mean(�Ǹűݾ�))

kimchi3 <- dcast(kimchi, �Ǹų⵵ ~ ��ǰ, sum, value.var='����')

min(kimchi3[,-1]) 
max(kimchi3[,-1]) 

plot(kimchi3$����ġ/10000, type='o', col=2, ylim=c(40, 80),
     ann=F, axes=F)
lines(kimchi3$������ġ/10000, type='o', col=3)
lines(kimchi3$�Ѱ���ġ/10000, type='o', col=4)

axis(1, at=1:nrow(kimchi3), kimchi3$�Ǹų⵵)
axis(2, ylim=c(40, 80))

title(main='��ġ�� �Ǹŷ� ��������', 
      xlab='�Ǹų⵵',
      ylab='�Ǹŷ�(��)')

legend(1,80, colnames(kimchi3)[-1], lty = 1, col = 2:4)

# 2) �� �⵵���� ��ǰ�� �Ǹŷ��� ���Ҽ� �ֵ��� ����׷����� ǥ��
rownames(kimchi3) <- kimchi3$�Ǹų⵵
kimchi3$�Ǹų⵵ <- NULL

dev.new()
barplot(as.matrix(t(kimchi3))/10000, beside = T, col = rainbow(3),
        ylim=c(0, 80), legend = colnames(kimchi3))

# 2. ������Ȳ.csv ������ �а�
df_data <- read.csv('������Ȳ.csv', stringsAsFactors=F, skip=1)
head(df_data)

df_data <- df_data[, -c(3,4)]

# ǥ�ð����� '��' �������� �и�
df_data_tt <- df_data[df_data$ǥ�ð��� == '��', -2]
df_data <- df_data[df_data$ǥ�ð��� != '��', ]

head(df_data)

# 1) �������� ���� ���� 5�� ���� ���� �⵵�� ������ �������̸� �ð�ȭ
# step1) �⵵/�б� �÷��� stack ó��
df_data2 <- melt(df_data, id.vars = c('�ñ�����Ī','ǥ�ð���'), 
                 variable.name='��¥', value.name='������')

# step2) �⵵�� �б� ���� �и� �� �÷� ����
library(stringr)
df_data2$�⵵ <- as.numeric(str_sub(df_data2$��¥, 2, 5))
df_data2$�б� <- as.numeric(str_sub(df_data2$��¥, 8, 8))

# step3) ǥ�ð��� ������ ����
df_data3 <- ddply(df_data2, .(ǥ�ð���), summarise, cnt=sum(������))
str(df_data2)

vrn <- order(df_data3$cnt, decreasing = T)[1:5]
vname <- df_data3$ǥ�ð���[vrn]

# step4) ���õ� 5�� ���� ���� ������ ����
df_data4 <- df_data2[df_data2$ǥ�ð��� %in% vname, ]
df_data5 <- dcast(df_data4, �⵵ ~ ǥ�ð���, sum, value.var='������')

# step5) �ð�ȭ
dev.new()

plot(df_data5$����, type='o', col=2, ylim=c(1000, 8000),
     ann=F, axes=F)

lines(df_data5$�Ҿ�û�ҳ�� , type='o', col=3)
lines(df_data5$�̺����İ�, type='o', col=4)
lines(df_data5$�Ϲ���, type='o', col=5)
lines(df_data5$���������ǥ��������, type='o', col=6)

axis(1, at=1:5, df_data5$�⵵)
axis(2, ylim=c(1000, 8000))

legend(1,8000, colnames(df_data5)[-1], col=2:6, lty=1)

# 2) �������� ���� ���� 5�� ���� ���� �б⺰ �� ������ ��������
#    ���ϴ� ����׷��� ����

# step1) ���õ� 5�� ���� ���� ������ ����
df_data4 <- df_data2[df_data2$ǥ�ð��� %in% vname, ]

# step2) ���õ� 5�� ���� ���� �б⺰ ������ ����
total <- dcast(df_data4, ǥ�ð��� ~ �б�, sum, value.var='������')
rownames(total) <- total$ǥ�ð���
total$ǥ�ð��� <- NULL

# step3) �ð�ȭ
barplot(as.matrix(total), beside = T, col = rainbow(5),
        ylim = c(0,10000), legend=rownames(total))

# 3. ��ȭ�̿���Ȳ.csv ������ �а�,
# ����.�õ��� ���� �̿������ ����� ���ϱ� ���� ����׷��� ���
df1 <- read.csv('��ȭ�̿���Ȳ.csv', stringsAsFactors=F)
head(df1)

library(reshape2)

total <- dcast(df1, ���� ~ ����.�õ�, sum)
rownames(total) <- total$����
total$���� <- NULL
total

dev.new()
barplot(as.matrix(total), beside = T, col = rainbow(2),
        legend = rownames(total))

########## ��������� �����Դϴ�. ##########

# 3. hist : ������׷�
hist(x,                  # ����
     breaks = ,          # ���뺰 ����
     include.lowest = T, # �ּҰ� ���� ����
     right = T)          # ������ ���� ����

# [ ���� : �������� ���� ���� �� �����ִ��� �ǹ� ]
# a <= x <  b   => [a,b) => ������ �����ִ�
# a <  x <= b   => (a,b] => �������� �����ִ�

# ����) student.csv ������ �а� Ű�� ���� ������׷� ���
std <- read.csv('student.csv', stringsAsFactors = F)
dev.new()
par(mfrow=c(1,2))
hist(std$HEIGHT)
hist(std$HEIGHT, breaks = c(160,170,180,190))
hist(std$HEIGHT, angle = c(0,90,45,10,60), density = 30, col = 2:6,
     border = 1)

# ����) ������ ���Ϳ� ���� ������׷��� ���
v1 <- c(160, 161, 163, 165, 166, 168, 171, 174)
hist(v1)  # right=T, include.lowest = T

# 160 �ʰ� 165 ���� : 3�� ������ �ּҰ��� 160�� ���ԵǾ� ������ 4�� ��
# 165 �ʰ� 170 ���� : 2  

dev.new()
hist(v1, breaks = c(160,165,170,175), 
     include.lowest = F)               # right=T, include.lowest = F
                                       # 160�� ���Խ�ų ������ ����
                                       # ���� �߻�


# ���� ����) ��ȭ�̿���Ȳ.csv ������ �а�,
movie <- read.csv('��ȭ�̿���Ȳ.csv', stringsAsFactors=F)
head(movie)

# 1) 20���̸�, 20��, 30��, 40��, 50��, 60���̻� �� �̿������ ���
unique(str_c(str_sub(movie$���ɴ�,1,1),'0��'))
v1 <- str_c(str_sub(movie$���ɴ�,1,1),'0��')
v1 <- str_replace_all(v1,'10��','20���̸�')
v1 <- str_replace_all(v1,'60��','60���̻�')
v1 <- str_replace_all(v1,'70��','60���̻�')

movie2 <- ddply(movie, .(v1), 
                summarise, vsum=sum(�̿�_����...))[c(2,1,3:6),]

barplot(movie2$vsum, col = 2:7, 
        names.arg = movie2$v1,
        angle = c(0,90,45,10,80), density = 30)

# 2) �� ����.�õ��� �̿���� ���� ���� ���ɴ� ���
movie3 <- ddply(movie, .(����.�õ�, v1), 
                summarise, vsum=sum(�̿�_����...))

ddply(movie3, .(����.�õ�), subset, vsum==max(vsum))

# 3) ���� �̿������ �� ���� ���ѵ� ������׷����� ���
movie4 <- ddply(movie, .(����.���鵿), summarise, vsum=sum(�̿�_����...))
hist(movie4$vsum)

movie4[movie4$vsum > 3.5, ]

# 4. pie : ���� ��Ʈ
pie(x,              # ����
    labels = ,      # �� ���� �̸�
    radius = ,      # ���� ũ��
    clockwise = F,  # ���� �������(�ð���� ����)
    init.angle = ,  # ���� ������
    col = ,         # �� ���� ��
    ...)            # ��Ÿ �ɼ�

vec1 <- c(10,11,14,15,2)
dev.new()

vrate <- vec1 / sum(vec1) * 100
vlabel <- str_c(c('��','ȭ','��','��','��'), '\n', 
                round(vrate,1), '%')

par(mfrow=c(1,2))
pie(vec1, labels = c('��','ȭ','��','��','��'))
pie(vec1, labels = vlabel, init.angle = 90)

# plotrix::pie3D
install.packages('plotrix')
library(plotrix)

pie3D(x,            # ����
      radius = ,    # ���� �� ũ��
      height = ,    # ���� ����
      labels = ,    # �� ���� �̸�
      labelcex = ,  # �� ���� ũ��
      labelcol = ,  # �� ���� ��
      explode = ,   # ���̰� ����
      ...)

pie(vec1, labels = c('��','ȭ','��','��','��'))
pie3D(vec1, labels = c('��','ȭ','��','��','��'), explode=0.1)

# [ ���� ���� ] ��ȭ �����͸� ����Ͽ�
# �� �ñ����� ��ȭ�̿���� ���� ���� ���� 3�� �ñ����� ����
# ���� ���� �̿������ ���� �� �ִ� pie��Ʈ �ð�ȭ
movie2 <- ddply(movie, .(����.�ñ���), 
                summarise, v1=sum(�̿�_����...))

library(doBy)
vname <- doBy::orderBy( ~ -v1, movie2)[1:3, 1]

movie3 <- movie[movie$����.�ñ��� %in% vname, ]
total <- dcast(movie3, ���� ~ ����.�ñ���, sum)

dev.new()
par(mfrow=c(1,3))

# �� ���� ���� ���� ���
f1 <- function(x) {
  round(x / sum(x) * 100, 2)
}

vrate <- as.data.frame(apply(total[,-1], 2, f1))

# �ð�ȭ
pie3D(total$������, col=c(2,4), explode=0.1, main = '������',
      cex.main=3, labels = str_c(vrate$������,'%'))
pie3D(total$����, col=c(2,4), explode=0.1, main = '����',
      cex.main=3, labels = str_c(vrate$����,'%'))
pie3D(total$�߱�, col=c(2,4), explode=0.1, main = '�߱�',
      cex.main=3, labels = str_c(vrate$�߱�,'%'))

legend(0.5,1, total$����, fill = c(2,4), cex=1.5)

