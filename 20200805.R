# �׷��� 
# figure : �׷����� �׸� ����
dev.new() # ���ο� �ϳ��� figure ����

# 1. plot : ���׷���, ������, ����������, ...
plot(x,     # x�� ��ǥ
     y,     # y�� ��ǥ
     ...)   # �׷��� �ɼ�

plot(c(1,2,3), c(10,11,12))

# 1) type : ���׷��� ��Ÿ��
plot(c(1,2,3), c(10,11,12), type='o')  # ���� �� ���� ���
plot(c(1,2,3), c(10,11,12), type='l')  # ���� ���

# 2) col : ���� ��
plot(c(1,2,3), c(10,11,12), type='l', col='red')  # ���� ���
plot(c(1,2,3), c(10,11,12), type='l', col=1)  # ���� ���

# 3) lty : �� ��Ÿ��(����, �Ǽ�, ...)
par(mfrow=c(1,3))                            # ���� �׷��� ���� ���
plot(c(1,2,3), c(10,11,12), type='l', lty=1) # �Ǽ�
plot(c(1,2,3), c(10,11,12), type='l', lty=2) # �뽬��
plot(c(1,2,3), c(10,11,12), type='l', lty=3) # ����

# 4) xlab, ylab, main : �� �� �̸�, ���� ����
dev.new()
plot(c(1,2,3), c(10,11,12), type='l',
     xlab = 'x�� �̸�', ylab = 'y�� �̸�')

# 5) xlim, ylim : �� �� ���� ����
plot(c(1,2,3), c(10,11,12), type='l',
     xlab = 'x�� �̸�', ylab = 'y�� �̸�', ylim = c(0,12))

# 6) axis : x��, y�� ���� ����(�Լ�)
axis(side=,       # ���� ���� ����(x��:1, y��:2)
     at=,         # ���� ����
     labels = ,   # �� ������ �̸�
     ...)         # ��Ÿ�ɼ�

# 7) axes : x��, y�� ���� ��� ����
plot(1:5, c(10,9,11,7,13), type = 'o', col = 4, ylim = c(0,15),
     axes = F)

# 8) ann : x��, y�� �̸� ��� ����

# 9) title : �� ��, ���� ������ ���� �����ϴ� �Լ�
title(main = ,  # ��������
      sub = ,   # ��������
      xlab = ,  # x�� �̸�
      ylab = ,  # y�� �̸�
      ...)      # ��Ÿ�ɼ�

# 10) legend : ���ʸ� ����ϴ� �Լ�
legend(x,          # ���� ��� x ��ġ
       y,          # ���� ��� y ��ġ
       legend = ,  # ���� ǥ�� ��
       fill = ,    # ���� �� ǥ��(����׷��� ��½� �ַ� ���)
       col = ,     # ���� �� ǥ��(���׷��� ��½� �ַ� ���)
       ...)

# ����) �� �׷����� x�� ������ ��ȭ����� ����
axis(1, 1:5, c('��','ȭ','��','��','��'))
axis(2, ylim=c(0,10)) # y�� ���� ��¸� ����, ylim ���� �Ұ�

# �������������� �� �׷��� ���
# - plot�� ������ ������ ���� �� �� �÷���(�ο캰) ���׷��� ��ºҰ�
#   (���� ������ ���)
# - �� �÷���, �ະ �и�, �ϳ��� ���� �ʿ�(���̽㿡�� ���� ��� ����)
# - ���� �׷����� �ϳ��� figure�� ���� �׸� �� plot -> lines ��ü

# ����) ������ ������ �����ӿ��� �� ���Ϻ� �Ǹŷ� ���� ���̸�
# �� �׷����� ���
df1 <- data.frame(apple=c(100,120,150),
                  banana=c(200,210,250),
                  mango=c(90,80,110))

rownames(df1) <- 2010:2012
plot(df1)

dev.new()
plot(df1$apple, type = 'o', col=2, ylim = c(50, 300), 
     axes = F, ann = F)
lines(df1$banana, type = 'o', col=3)
lines(df1$mango, type = 'o', col=4)

axis(1, at=1:3, labels = rownames(df1))
axis(2, ylim = c(50, 300))


title(main = '���Ϻ� �Ǹŷ� ��������', col.main = 'red',
      xlab = '�⵵', cex.lab = 2,
      ylab = '�Ǹŷ�', font.lab = 4)

legend(1,300, colnames(df1), col = 2:4, lty=1)

# [ ���� : ���� ������ ]
# - plot �Լ��� ������������(�����÷����� ������)�� �����ϸ� �ڵ� ���
# - �� ������ ������� �ľ� �� ���
# - �з��м��� ���Ӻ����� �з��� ������� ū �������� ã�� ������ ���

# ����) iris �������� 4�� ���������� ���� ������ ���
plot(iris[,-5])

# ����) iris �������� 4�� ���������� ���� ������ ���
#       ��½� species�� ������ ���� �ٸ� �� �ο�
plot(iris[,-5], col=iris$Species)


# [ ���� ���� ]
# data.csv ���Ͽ��� �⵵�� �ѱ����ڼ��� ��ȭ���� ���׷��� ���
# (�� �⵵���� ���� �����ڼ��� ��ȭ�� Ȯ���� �� �ִ�...)

df1 <- read.csv('data.csv', stringsAsFactors = F)

library(reshape2)

total <- dcast(df1, �� ~ �⵵)

dev.new()

plot(total$`2014`, type = 'o', col = 1, ylim = c(4000,13000),
     ann = F, axes = F)
lines(total$`2015`, type = 'o', col = 2)
lines(total$`2016`, type = 'o', col = 3)
lines(total$`2017`, type = 'o', col = 4)
lines(total$`2018`, type = 'o', col = 5)

library(stringr)
axis(1, at=1:12, labels = str_c(total$��,'��'))
axis(2, ylim = c(4000,13000))

title(main='�⵵�� ���� �����ڼ� ��ȭ', col.main=4, cex.main=2,
      xlab='��', cex.lab=1.3,
      ylab='�����ڼ�', font.lab=2)

legend(11, 13000, colnames(total)[-1], col=1:5, lty=1, cex = 1.5)


# [ ���� : plot ��ǥ�� Ư�� �� �ϳ� ���� �� ]
# - ������ �����ӿ��� Ư�� �� �ϳ� ������ ������������ ���
# - plot�� ������������ �Է� => ���� ������ ���
# - ��,�� ��ġ Ȥ�� unlist�� ���ͷ� ���� �� plot�� ���� �ʿ�

total <- dcast(df1, �⵵ ~ ��)

dev.new()
plot(total[1,-1], type='o')

total[1,-1]             # ������������ ���
as.vector(total[1,-1])  # key���� ����, ������������ ���
unlist(total[1,-1])     # key���� ����, ���� ���

plot(unlist(total[1,-1]), type='o')

# [ ���� ���� ]
# subway2.csv ������ �����͸� �������
# ������ ���� ���� top 5���� ���� ���ϰ� 
# �� ���� �ð��뺰 ������ �����߼��� ��ǥȭ
sub <- read.csv('subway2.csv', stringsAsFactors = F, skip = 1)
head(sub)

# 1) ���� ������ ����
sub2 <- sub[sub$���� == '����', -2]
sub2

# 2) ���̸��� �ο��̸����� ����
rownames(sub2) <- sub2$��ü
sub2$��ü <- NULL   # sub2 <- sub2[,-1]  (Ư�� �÷�(KEY) ����)

# 3) ���� ���� �� ��
str(sub2)
vsum <- apply(sub2, 1, sum)

# 4) ������ �� ������ ����
sort(vsum, decreasing = T)[1:5]
vname <- names(sort(vsum, decreasing = T)[1:5])

# 5) top5 ���� ���� �ð��� ���� �� ����
total <- t(sub2[rownames(sub2) %in% vname, ])

# 6) �׷��� �׸���
dev.new()
rownames(total) <- as.numeric(str_sub(rownames(total),2,3))

min(total)
max(total)

plot(total[,1]/1000, type = 'o', col = 1, ylim = c(1,400),
     ann = F, axes = F)
lines(total[,2]/1000, type = 'o', col = 2)
lines(total[,3]/1000, type = 'o', col = 3)
lines(total[,4]/1000, type = 'o', col = 4)
lines(total[,5]/1000, type = 'o', col = 5)

axis(1, at=1:nrow(total), labels = rownames(total))
axis(2, ylim = c(1,380))

title(main='�ð��뺰 ���� ��ȭ��', 
      xlab='�ð���',
      ylab='������/(õ)')

legend(18, 400, colnames(total), col = 1:5, lty = 1)


# 2. barplot
# - ���� �׷���
# - �÷��� ���� �ٸ� �׷��� ���� ����
# - �ϳ��� �÷� �� ���� �ٸ� �� �����Ͱ� stack�� ���� ����� �⺻

barplot(height = ,    # 2���� ������
        ...)


# ����) ������ �����Ϳ� ���� �� ���Ϻ� �Ǹŷ��� ���ϴ�
# ����׷��� ��� 
fruit <- data.frame(apple=c(100,120,150),
                  banana=c(200,210,250),
                  mango=c(90,80,110))

rownames(fruit) <- 2010:2012

dev.new()
barplot(fruit)
barplot(as.matrix(fruit))               # stack�� ����
barplot(as.matrix(fruit), beside = T)   # �ະ ���� �ٸ� ����


barplot(as.matrix(t(fruit)), beside = T, col = rainbow(3), 
        ylim = c(0,300))

# legend(1,300, colnames(fruit), col = rainbow(3), lty=1 ) # ����Ÿ��
legend(1,300, colnames(fruit), fill = rainbow(3))        # �ڽ���Ÿ��

# [ ���� ���� ]
# ��ݱ���������������Ȳ_new.csv�� �а�,
# ���� �� ������ ���뵵�� ���ϱ� ���� ����׷��� ���
df1 <- read.csv('��ݱ���������������Ȳ_new.csv', stringsAsFactors=F)

df2 <- dcast(df1, �̸� ~ ��)
rownames(df2) <- df2$�̸�
df2$�̸� <- NULL

dev.new()
barplot(as.matrix(df2), beside = T, col = rainbow(nrow(df2)), 
        ylim = c(0, 1.5),
        angle = 45,        # ������ ���� ����
        density = 80,      # ������ ���� ��
        names.arg = str_c(1:6, '��'),
        legend = rownames(df2), 
        args.legend = list(cex=1, x='topleft'))
     

# ���Ǻ� ���� �ٸ� �� ����(����� ���� �ȷ�Ʈ ����)
barplot(as.matrix(fruit[,1]), col = vcol, beside = T)
v1 <- fruit[,1]

vcol <- c()

for (i in v1) {
  if (i <= 110) {
    vcol <- c(vcol, 'green')
  } else if (i <= 130) {
    vcol <- c(vcol, 'yellow')
  } else {
    vcol <- c(vcol, 'red')
  }
}


# [ ���� ���� ]
# ��ȭ�̿���Ȳ.csv ������ �а�,
# ����.�õ��� ���� �̿������ ����� ���ϱ� ���� ����׷��� ���








