# [ Y�� �������� ��� �м� ��� ]
# 1. ���� ȸ�� �м�
# - �������� ����� ���� �ʿ�
# - ������ �������� ������ �������� ������
# - �ΰ����� �ľ��� ����
# 
# 2. �з����� �����ϴ� ȸ�� ��
#    (ex. randomForest-classification)
# - �� ����� ��
# - Ư���� Ʃ�� ���̵� ����� ������ ����
# - �ΰ����� �ľ��� �Ұ�(������ ����)


# [ �з� �м� ]
# 1. Ʈ����� ��
# - DT -> RF > GB > XGB
# - outlier �ΰ� X
# - ����, ������ ���� ���� ��� ���� ����
# - �н��ð��� ���� �����ð��� ����
# - ������ �����Ϳ� ���� scaling �ʿ� X
# 
# 2. �Ÿ���� ��
# - outlier �ſ� �ΰ� => ����/����
# - ������ ���� ������ ���� ���Եɼ��� ������ ������
# - ������ �����Ϳ� ������
# - ���� �ð��� �����ɸ�(�н����� ����)
# - ������ �����Ϳ� ���� scaling �ʿ�(ǥ��ȭ)
# - �𵨿� �н��Ǵ� ���������� ������ �ſ� �߿��� ��(�߿��� ���������θ� ���� �ɼ��� ����)

# ��) �Ÿ� ��� �𵨸� ����
#   �ҵ� ����  ���� A��ǰ���ſ���
# 1 400 ȸ���(1) 200   X
# 2 410 ȸ���    220   X
# 3 500 �ڿ���(2) 400   O
# 
# 4 420 ȸ���(1) 180   ?
# 
# d41 <- sqrt((420-400)^2 + (1-1)^2 + (180-200)^2) ; d41  # 28.28
# d42 <- sqrt((420-410)^2 + (1-1)^2 + (180-220)^2) ; d42  # 41.23
# d43 <- sqrt((420-500)^2 + (1-2)^2 + (180-400)^2) ; d43  # 234.1
# 
# ���) 4�� ����ġ�� ���� ������(�Ÿ��� �����) ����ġ�� 1���̹Ƿ�
# 1���� �ൿ�� ���� �ൿ�� �� ������ ����, �� �������� ������!!
  
  
# [ �Ÿ� ��� �� ǥ��ȭ �ʿ� ���� ]
    x1    x2
p1  5     100
p2  6     200
p3  10    150

# 1) ǥ��ȭ ���� �Ϲ� �Ÿ� ���
d12 <- sqrt((5-6)^2  + (100-200)^2) ; d12  # 100.005
d13 <- sqrt((5-10)^2 + (100-150)^2) ; d13  # 50.23

d12 > d13

# 2) ǥ��ȭ �� �Ÿ� ���
# (x1�� ��� 6, ǥ������ 0.01)
# (x2�� ��� 150, ǥ������ 30)
    
    x1             x2
p1  (5-6)/0.01     (100-150)/30
p2  (6-6)/0.01     (200-150)/30
p3  (10-6)/0.01    (150-150)/30

     x1    x2
p1  -100  -1.67
p2   0     1.67
p3  400    0

d12 <- sqrt((-100-0)^2  + (-1.67-1.67)^2) ; d12  # 100.05
d13 <- sqrt((-100-400)^2 + (-1.67-0)^2) ; d13    # 500.00

d12 < d13



# knn 
# - �����ϰ��� �ϴ� ����ġ�κ��� ��������ġ���� �Ÿ� ���
# - ���� �Ÿ��� k���� ���� ����� �̿� ����ġ Ȯ��
# - k���� �̿��� ���� ����(Y)�� ��� Ȥ�� �ټ���� ���� ��� ����
# - ���������� �����ϸ� �ʿ�
# - Y class�� ������ ����� �Ǵ� k���� ����ġ ����
# - �𵨿� �н��Ǵ� ���������� ������ �ſ� �߿��� ��

install.packages('class')
library(class)

knn(train = ,   # ���� ������ X
    test = ,    # ������ ������ X 
    cl = ,      # ���� ������ Y
    k = ,       # �̿��� ��
    prob = )    # Ȯ�� ���� ����

# [ ���� - knn ���� ����� iris data ǰ�� ���� ]
# 1. sampling
vrn <- sample(1:nrow(iris), size = nrow(iris)*0.7)

iris_tr_y <- iris[vrn, 5]
iris_tr_x <- iris[vrn, -5]

iris_te_y <- iris[-vrn, 5]
iris_te_x <- iris[-vrn, -5]

# 2. �𵨸� �� ����
pre_te <- knn(iris_tr_x, iris_te_x, iris_tr_y, k=3, prob = T)

# 3. ��
sum(iris_te_y == pre_te) / nrow(iris_te_x) * 100  # 97.8

# 4. ����
new_data <- data.frame(Sepal.Length=7, 
                       Sepal.Width=3, 
                       Petal.Length=1, 
                       Petal.Width=1.0)

knn(iris_tr_x, new_data, iris_tr_y, k=3, prob = T)

# 5. �Ű����� Ʃ��
# k�� ��ȭ�� ���� train/test data set score Ȯ��
score_tr <- c() ; score_te <- c()

for ( i in 1:10) {
  pre_tr <- knn(iris_tr_x, iris_tr_x, iris_tr_y, k=i, prob = T)
  pre_te <- knn(iris_tr_x, iris_te_x, iris_tr_y, k=i, prob = T)
  
  vscore_tr <- sum(iris_tr_y == pre_tr) / nrow(iris_tr_x) * 100 
  vscore_te <- sum(iris_te_y == pre_te) / nrow(iris_te_x) * 100 
  
  score_tr <- c(score_tr, vscore_tr)
  score_te <- c(score_te, vscore_te)
}

dev.new()
plot(1:10, score_tr, type='o', col='blue', ylim = c(80,100),
     xlab = 'k�� ��', ylab = 'score', 
     main = 'k�� ��ȭ�� ���� score ��ȭ')
lines(1:10, score_te, type='o', col='red')

axis(1, at=1:10)
legend(9,90, c('train','test'), col = c('blue','red'), lty=1)

# [ ���� ���� - knn�� ����� cancer data�� ������ �缺 ���� ���� ]
# 1. ������ �ε� �� sampling
cancer <- read.csv('cancer.csv', stringsAsFactors = F)
cancer$id <- NULL

vrn <- sample(1:nrow(cancer), size = nrow(cancer)* 0.7)

cancer_tr_y <- cancer[vrn, 1]
cancer_tr_x <- cancer[vrn, -1]

cancer_te_y <- cancer[-vrn, 1]
cancer_te_x <- cancer[-vrn, -1]

# 2. �𵨸� �� ����
pre_cancer_te <- knn(cancer_tr_x, cancer_te_x, cancer_tr_y, k=3)

# 3. ��
sum(cancer_te_y == pre_cancer_te) / nrow(cancer_te_x) * 100

# 4. ������ k�� �� ã��
score_tr <- c() ; score_te <- c()

for ( i in 1:10) {
  pre_tr <- knn(cancer_tr_x, cancer_tr_x, cancer_tr_y, k=i, prob = T)
  pre_te <- knn(cancer_tr_x, cancer_te_x, cancer_tr_y, k=i, prob = T)
  
  vscore_tr <- sum(cancer_tr_y == pre_tr) / nrow(cancer_tr_x) * 100 
  vscore_te <- sum(cancer_te_y == pre_te) / nrow(cancer_te_x) * 100 
  
  score_tr <- c(score_tr, vscore_tr)
  score_te <- c(score_te, vscore_te)
}

dev.new()
plot(1:10, score_tr, type='o', col='blue', ylim = c(80,100),
     xlab = 'k�� ��', ylab = 'score', 
     main = 'k�� ��ȭ�� ���� score ��ȭ')
lines(1:10, score_te, type='o', col='red')

axis(1, at=1:10)
legend(8,90, c('train','test'), col = c('blue','red'), lty=1)

# 5. ���������� ������ ���� �� �� �н�(k=5)

df1 <- data.frame(col1=c(1,2,4,7,10), col2 = c(10,32,45,35,35))
scale(df1)

mean(df1$col1)  # 4.8
sd(df1$col1)    # 3.7

# cancer �������� scaling
cancer_tr_x_sc <- scale(cancer_tr_x)
cancer_te_x_sc <- scale(cancer_te_x)

# scaling�� data�� �𵨸� �� ��
pre_cancer_te_sc <- knn(cancer_tr_x_sc, cancer_te_x_sc, 
                        cancer_tr_y, k=5, prob = T)

sum(cancer_te_y == pre_cancer_te_sc) / nrow(cancer_te_x_sc) * 100

# ��� : scaling�� �� �������� ���� Ȯ�� ����
# ���� ���������� �ǹ̾���, �����¿� ���صǴ� ������ �ִٸ�
# �ش� ������ ������ �� �ٽ� �𵨸� �ʿ�


# ������ �м�
# 1. �����н�(Y����)
#  1) ȸ�ͺм�
#  2) �з��м�
#   - Ʈ����� ��
#   - �Ÿ���� ��
#     
# 2. �������н�(Y����X)
#  1) �����м�(�Ÿ���� ��)
#  2) �����м�  

# ���� �м�
# - ������ ���� �������н�
# - �־��� �����͵��� ���缺�� �ٰ��� ����� �����ͳ��� ���� �м����
# - �ʱ� �����Ϳ� ���� ������ ���� ���ǰų�
#   Ŭ�����͸��� ������ ���� ������ �߰��� Ȱ���ϱ� ���� �ַ� ���
# - ������ ��� ��ũ��
# - �Ÿ���� ��

# 1. ������ �����м�
# - �Ÿ��� ����� ����������Ʈ�鳢�� ���������� ���� ����
# - �ϳ��� �����Ͱ� �� ������ ���ϸ� ������ ������� ����
# - ������ �����ϴ� ���ؿ� ���� �������� �� ����

# ** ���� ���� ����(������ ����������Ʈ���� �Ÿ� ���� ��Ŀ� ����)
# 1. �ִܰŸ���(single, min)
# 2. ����Ÿ���(complete, max)
# 3. ��հŸ���(average)
# 4. �߾ӰŸ���(median)

# [ ���� - �� �����͵鳢���� ���� ���� ���� ]
library(stringr)
v1 <- c(1,3,6,10,18)
names(v1) <- str_c('p',1:5)

step1) �� ���� ����ġ�⸮�� �Ÿ� ���(�Ÿ����)
dist(v1)

    p1 p2 p3 p4
p2  2         
p3  5  3      
p4  9  7  4   
p5 17 15 12  8

step2) �� �Ÿ��� ���� ����� ����������Ʈ�� �ϳ��� �������� ����
       => c1(p1,p2)
    
step3) c1�� ������ ������ ����Ʈ������ �Ÿ� ���
1) �ִܰŸ���
dc1p3 = min(dp1p3, dp2p3) = min(5,3) = 3
dc1p4 = min(dp1p4, dp2p4) = min(9,7) = 7
dc1p5 = min(dp1p5, dp2p5) = min(17,15) = 15

2) ����Ÿ���
dc1p3 = max(dp1p3, dp2p3) = max(5,3) = 5
dc1p4 = max(dp1p4, dp2p4) = max(9,7) = 9
dc1p5 = max(dp1p5, dp2p5) = max(17,15) = 17

3) ��հŸ���
dc1p3 = d(p1p2)p3 = d(2,6) = 4    
...

step4) ���ؼ� ������ �������� �Ÿ��� ��� ���,
       ���� ����� �Ÿ� �ľ�(�ִܰŸ���)
    
       c1 p3 p4 p5
    c1  . 
    p3  3  .    
    p4  7  4  .
    p5  15 12 8  .

    => c1(p1,p2,p3)

step5) ����� c1�� ������ ����ġ(p4,p5)�� �Ÿ� ���(�ִ�)
dc1p4 = min(p1p4,p2p4,p3p4) = min(9,7,4) = 4
dc1p5 = min(p1p5,p2p5,p3p5) = min(17,15,12) = 12

   c1 p4 p5
c1  .
p4 4   .
p5 12  8  .

step6) �� �Ÿ��� ���� ����� �Ÿ� Ȯ�� 
       => c1(p1,p2,p3,p4)

# �� ����� ���� ������ �����м��� ���� �ð�ȭ
hclust(d,                    # �Ÿ���� 
       method = 'single',    
                'complete',
                'average',
                'median')

# 1) �� ����(��������) 
d1 <- dist(v1)
m_clust1 <- hclust(d1, method = 'single')

# 2) �ð�ȭ
dev.new()
par(mfrow=c(1,2))
plot(m_clust1, main = '�ִܰŸ����� ���� ������������')

plot(m_clust1, 
     hang = -1)  # ����������Ʈ���� ������ �� ������ �ϴ� �ɼ� 

dev.new()
plot(m_clust1, hang = -1)
rect.hclust(tree = m_clust1, k = 2)

# 2. ������� �����м�
