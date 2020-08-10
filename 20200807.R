# 1. ������Ȳ.csv ������ �а�
df1 <- read.csv('������Ȳ.csv', stringsAsFactors=F, skip=1)
df1 <- df1[,-c(3,4)]
str(df1)

library(plyr) ; library(reshape2) ; library(stringr)

# 1) ���� ���������� �� �ݾ��� ����׷����� �ð�ȭ
# step1) �� ����
str_extract_all(df1$�������ּ�[1], '..��')
str_extract_all(df1$�������ּ�[1], '[��-�R]{1,}��')
str_extract_all(df1$�������ּ�[1], '[:alpha:]{1,}��')[[1]]

f1 <- function(x) {
  str_extract_all(x, '..��')[[1]][1]
}

df1$�� <- sapply(df1$��������, f1)
unique(df1$��)

# step2) �����ݾ� �÷� ����
df2 <- df1[, str_detect(colnames(df1), '^X')]

# step3) �п��� �� �����ݾ� ���(�ະ ��)
f_num <- function(x) {
  as.numeric(str_remove_all(x, ','))
}

df2[,] <- apply(df2, c(1,2), f_num)

df1$total <- apply(df2, 1, sum)

# step4) ����, ���������� �����ݾ� �� ���
str_remove_all('�ǿ�ܱ���(����/�ʡ��ߡ���)','\\(.{1,}\\)')
str_remove_all('��ǻ��(��)','\\(.{1,}\\)')

unique(str_remove_all(df1$��������,'\\(.{1,}\\)'))
df1$�������� <- str_remove_all(df1$��������,'\\(.{1,}\\)')

df1_total <- dcast(df1, �������� ~ ��, sum, value.var='total')

# step5) �ð�ȭ
dev.new()
barplot(as.matrix(df1_total[,-1])/1000000, beside = T, 
        ylim = c(0,60000), col=1:nrow(df1_total))

legend(1,60000,df1_total$��������, fill = 1:nrow(df1_total))

# 2) �⵵�� ���� ���������� ����ݾ��� ���� ū ���̸�, ����ݾ� ���
# step1) ���̸� ����
# --
str_extract_all(df1$�������ּ�[1], '[��-�R]{1,}��')[[1]][1]

f2 <- function(x) {
  str_extract_all(x, '[��-�R]{1,}��')[[1]][1]
}

unique(sapply(df1$�������ּ�, f2))
df1$��������[sapply(df1$�������ּ�, f2) == '����Ǫ�������󰡵�']

# --
str_extract_all(df1$�������ּ�[1], '\\([��-�R]{1,}��')[[1]][1]

f2 <- function(x) {
  str_extract_all(x, '\\([��-�R]{1,}��')[[1]][1]
}

unique(sapply(df1$�������ּ�, f2))
df1$�������ּ�[is.na(sapply(df1$�������ּ�, f2))]

# --
str_extract_all(df1$�������ּ�[1], '\\([��-�R0-9]{1,}��')[[1]][1]

f2 <- function(x) {
  str_extract_all(x, '\\([��-�R0-9]{1,}��')[[1]][1]
}

unique(sapply(df1$�������ּ�, f2))
df1$�� <- str_remove_all(sapply(df1$�������ּ�, f2), '[(0-9]')

# step2) �ʿ䵥���� ����
df2$�� <- df1$��
df2$�������� <- df1$��������

# step3) �⵵ �÷� stack
df3 <- melt(df2, id.vars = c('��������','��'), 
            variable.name='�⵵', value.name='�ݾ�')
df3$�⵵ <- as.numeric(str_sub(df3$�⵵,2,5))

# step4) �⵵�� ���� ���������� �ݾ��� ����
df4 <- ddply(df3, .(�⵵,��,��������), summarise, vsum=sum(�ݾ�))

# step5) �⵵�� ���������� �ݾ��� �ִ븦 ���� �� ���� 
ddply(df4, .(�⵵,��������), subset, vsum==max(vsum))

# 3) �� ���������� ������ ���� ���� �����Ҹ��� ����ѵ�
#    �� �����Ҹ�(��������) �� ������� ���� �� �ִ� ����׷��� ���
# step1) ���������� �����Ҹ��� �����
df2$�����Ҹ� <- df1$�����Ҹ�
df2$total <- df1$total

df_total <- ddply(df2, .(��������, �����Ҹ�), summarise, vsum=sum(total))

# step2) ���������� �ִ�ݾ��� ���� �� ����
df_total2 <- ddply(df_total, .(��������), subset, vsum==max(vsum))

# step3) �ð�ȭ
dev.new()
par(oma=c(5,0,0,0))  # ��,��,��,��
vname <- str_c(df_total2$�����Ҹ�,'\n',df_total2$��������)
barplot(df_total2$vsum/100000, col = rainbow(nrow(df_total2)),
        ylim = c(0,30000), names.arg = vname, las=2)

# 2. total.csv ������ �а�
data1 <- read.csv('total.csv', stringsAsFactors = F)

# 1) �⵵�� �� ǰ�� ���� ������ ����׷����� �ð�ȭ
# step1) �⵵�� ������ ������ ���·� �÷��̸� ����(stack�� ���� ó��)
colnames(data1) <- str_c(str_sub(colnames(data1),2,5), '_', data1[1,])
data1 <- data1[-1,]

# step2) stack
colnames(data1)[1] <- 'name'
data2 <- melt(data1, id.vars = 'name',
              variable.name = '�⵵', value.name='�ݾ�')

# step3) �⵵�� ���� �÷� �и�
data2$���� <- str_sub(data2$�⵵,6,6)
data2$�⵵ <- str_sub(data2$�⵵,1,4)

# step4) �ݾ��÷� ���� ����
data2$�ݾ� <- sapply(data2$�ݾ�, f_num)

# step5) �⵵�� ��ǰ�� ���⿡ ���� �������̺� ����
data_total <- dcast(data2, name ~ �⵵, sum, value.var='�ݾ�')

# step6) �ð�ȭ
dev.new()
barplot(as.matrix(data_total[,-1])/1000, beside = T, col = 2:4,
        ylim = c(0,100), legend = data_total$name,
        args.legend = list(cex=0.7))

# 2) �������� ���� ������ ���� ǰ��� �� ������� �Բ� ���
# step1) ������ ǰ�� ����� �� ��
data3 <- ddply(data2, .(����, name), summarise, vsum=sum(�ݾ�))

# step2) ������ �ִ밪 ���� �� ����
ddply(data3, .(����), subset, vsum==max(vsum))

########## ��������� �����Դϴ�. ##########

# [ ���� ���� ]
# taxi_call.csv ������ �а�
# �� ���Ϻ��� �ð��뺰 �ý� �̿���� ������Ʈ�� ���(7������)
# 1) ��¥ �Ľ�
taxi <- read.csv('taxi_call.csv', stringsAsFactors = F)
taxi$���س���� <- as.Date(as.character(taxi$���س����), '%Y%m%d')

# 2) ���� ����
taxi$���� <- as.character(taxi$���س����, '%A')

# 3) �ð��뺰 ���Ϻ� �������̺� ����
taxi_total <- dcast(taxi, �ð��� ~ ����, sum, value.var='��ȭ�Ǽ�')

# 4) �� ���Ϻ� �ð����� ��ȭ�Ǽ� ����
f_rate <- function(x) {
  round(x / sum(x) * 100, 1)
}

taxi_total[,-1] <- apply(taxi_total[,-1],2,f_rate)

# 5) �� ������ label �� ����
# 0��(8.9%)
str_c(taxi_total$�ð���,'��(',taxi_total$������,'%)')

dev.new()
par(mfrow=c(2,4))
library(plotrix)
pie3D(taxi_total$������, 
      labels=str_c(taxi_total$�ð���,'��(',taxi_total$������,'%)'),
      labelcex=0.5, main='������')

pie3D(taxi_total$ȭ����, 
      labels=str_c(taxi_total$�ð���,'��(',taxi_total$ȭ����,'%)'),
      labelcex=0.5, main='ȭ����')

pie3D(taxi_total$������, 
      labels=str_c(taxi_total$�ð���,'��(',taxi_total$������,'%)'),
      labelcex=0.5, main='������')

pie3D(taxi_total$�����, 
      labels=str_c(taxi_total$�ð���,'��(',taxi_total$�����,'%)'),
      labelcex=0.5, main='�����')

pie3D(taxi_total$�ݿ���, 
      labels=str_c(taxi_total$�ð���,'��(',taxi_total$�ݿ���,'%)'),
      labelcex=0.5, main='�ݿ���')


# ������ �м�
# - ������ ���̴�
# - �̷� ����
# 1. �ӽŷ���(����������)
#   - Ʈ����� ��
#   - Ȯ��/��� ��
#     ...
#   - �Ű�� ��(������)
# 2. ������(������������)

