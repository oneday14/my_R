# �������ù�(����,����,stepwise)
# 1. �������ù�(forward selection)
Y = X1
Y = X1 + X2 
Y = X1 + X2 + X3 

# 2. �������ù�(backward selection)
Y = X1 + X2 + X3 + X4 + X5
Y = X1 + X2 + X3 + X4
Y = X1 + X2 + X3 

# 3. �ܰ������ù�(stepwise selection)
Y = X1 + X2 + X3 + X4 + X5 
Y = X1 + X2 + X3 + X4
Y = X1 + X2 + X3             # ���ŵ� ������ �߰��� ���� �ִ���
Y = X1 + X2 + X3 + X5


# [ ���� - ������ ���� ���ݼ��� ����� �������� ��� ]
install.packages('mlbench')  #BostonHousing data loading �� �ʿ�
library(mlbench)
data(BostonHousing)
str(BostonHousing)

lm1 <- lm(medv ~ ., data = BostonHousing)
step(lm1, direction = 'forward')
step(lm1, direction = 'backward')
step(lm1, direction = 'both')

# [ ���� - iris���� ����� �������� ��� ]
m_dt1 <- rpart(Species ~ ., data=iris)
step(m_dt1, direction = 'both')           # �Ұ�

install.packages('MASS')
library(MASS)
install.packages('klaR')
library(klaR)

stepclass(x,               # x ������
          y,               # y ������
          method = 'lda',  # �з����
          direction = ,    # forward, backward, both
          start.vars = )   # ���ۺ���

stepclass(iris[,-5], iris[,5], 'lda',
          direction = 'both')

# final model : iris[, 5] ~ Petal.Width + 
#   correctness rate = 0.96 

stepclass(iris[,-5], iris[,5], 'lda',
          direction = 'both', start.vars = 'Petal.Width')

stepclass(iris[,-5], iris[,5], 'lda',
          direction = 'both', start.vars = 'Petal.Length')

# [ ���� - cancer data�� ����� �����м� ]
library(rpart)
library(randomForest)
library(NbClust)

# 1. data loading
cancer <- read.csv('cancer.csv')
cancer$id <- NULL

# 2. ������ �����м� ����(ǥ��ȭ X, ��������X)
# 1) �� ����
d1 <- dist(cancer[,-1])
m_cl1 <- hclust(d1, 'single')
m_cl2 <- hclust(d1, 'complete')
m_cl3 <- hclust(d1, 'average')

# 2) �� �ð�ȭ �� ��
dev.new()
par(mfrow=c(1,3))

plot(m_cl1, hang = -1, main='single')
rect.hclust(m_cl1, 2)

plot(m_cl2, hang = -1, main='complete')
rect.hclust(m_cl2, 2)

plot(m_cl3, hang = -1, main='average')
rect.hclust(m_cl3, 2)

table(cancer$diagnosis)


# 3. ������ �����м� ����(ǥ��ȭ O, ��������X)
# 1) �� ����
d2 <- dist(scale(cancer[,-1]))
m_cl_sc1 <- hclust(d2, 'single')
m_cl_sc2 <- hclust(d2, 'complete')
m_cl_sc3 <- hclust(d2, 'average')

# 2) �� �ð�ȭ �� ��
dev.new()
par(mfrow=c(1,3))

plot(m_cl_sc1, hang = -1, main='single')
rect.hclust(m_cl_sc1, 2)

plot(m_cl_sc2, hang = -1, main='complete')
rect.hclust(m_cl_sc2, 2)

plot(m_cl_sc3, hang = -1, main='average')
rect.hclust(m_cl_sc3, 2)


# 4. ������ �����м� ����(ǥ��ȭ O, ��������O)
# 1) tree��� �𵨸��� ���� ���� ���� Ȥ�� ����
cancer_dt1 <- rpart( diagnosis ~ ., data = cancer)
var_imp1 <- cancer_dt1$variable.importance

feature1 <- names(sort(var_imp1)[sort(var_imp1) > 10])

cancer_sel <- cancer[  , colnames(cancer) %in% feature1 ]


# 2) �������ù�(����,����,stepwise)�� ���� �������� ����
# step : Y�� ������
# regsebsets : Y�� ������
# stepclass : Y�� ������

stepclass(cancer[,-1], cancer[,1], 'lda', 
          direction = 'both',
          improvement = 0.01, fold = 10)

df_caner <- cancer[, c('concave_points_worst',
                       'radius_worst',
                       'texture_mean')]

# 3) �����м� ����
d3 <- dist(scale(cancer_sel))

m_cl_sel_sc1 <- hclust(d3, 'single')
m_cl_sel_sc2 <- hclust(d3, 'complete')
m_cl_sel_sc3 <- hclust(d3, 'average')

dev.new()
par(mfrow=c(1,3))
plot(m_cl_sel_sc1, hang = -1, main='single')
plot(m_cl_sel_sc2, hang = -1, main='complete')
plot(m_cl_sel_sc3, hang = -1, main='average')

#-- stepwise
m_c1_f1 <- hclust(dist(scale(df_caner)), 'single')
m_c1_f2 <- hclust(dist(scale(df_caner)), 'complete')
m_c1_f3 <- hclust(dist(scale(df_caner)), 'average')

dev.new()
par(mfrow=c(1,3))
plot(m_c1_f1, hang = -1, main='single')
rect.hclust(m_c1_f1,2)

plot(m_c1_f2, hang = -1, main='complete')
rect.hclust(m_c1_f2,2)

plot(m_c1_f3, hang = -1, main='average')
rect.hclust(m_c1_f3,2)
  
  
# 5. ������� �����м� ����(ǥ��ȭ O, ��������O)  
df_cancer <- cancer[ , c('radius_worst',
                         'concave_points_worst', 
                         'texture_mean')]

m1 <- hclust(dist(scale(df_cancer)), 'complete')
c1 <- cutree(m1,2)
g1 <- cancer$diagnosis
levels(g1) <- c(2,1)

sum(c1 == g1) / nrow(cancer)   # 93.85%

m_kmeans1 <- kmeans(scale(df_cancer),2)
# between_SS / total_SS =  48.6 %

vscore <- c()
for (i in 1:10) {
  m_kmeans1 <- kmeans(scale(df_cancer),i)
  vscore <- c(vscore, m_kmeans1$betweenss / m_kmeans1$totss)
}

plot(1:10, vscore, type='o')


# [ �̻�ġ ���� ]
# 1. Y�� �������ΰ��
# - ȸ�͸� ���� �� �̻�ġ ���� ����
# - ����� ���� ���� ���Ǽ� ���� ����(p-value)
# - car::outlierTest�� Ȯ�� ����

# [ ���� - ������ ���� ���� �������� �̻�ġ ���� ]
install.packages('car')
library(car)

m1 <- lm(medv ~ ., Boston)
outlierTest(m1)

# 2. Y�� �������ΰ��
# - randomForest �� ���� �� �̻�ġ ���� ����
# - �� ����������Ʈ�� �Ÿ�(���缺) ������� �̻�ġ Ȯ��
# - ���Ǽ� ���� �Ұ�(p-value Ȯ�� �Ұ�)
# - outlier�� Ȯ�� ����


# [ ���� - iris �������� �̻�ġ ���� ]
m2 <- randomForest(Species ~ ., iris, proximity=T)
m2$proximity

outlier(m2)

dev.new()
plot(outlier(m2), type = 'h', col = col2)

col1 <- ifelse(iris$Species=='setosa','red',
              (ifelse(iris$Species=='versicolor','blue','green')))

col2 <- c('red','blue','green')[as.numeric(iris$Species)]





