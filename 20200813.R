# ���� �м�
# 1. ������ �����м�
# - �Ÿ��� ª�� �����ͳ��� ���������� ������ �����ϴ� ����
# - �ѹ� ���� ������ ������� X
# - ������ �����Ϳ��� �Ÿ��� �����ϴ� ��Ŀ� ����
#   �ִܰŸ���, ����Ÿ���, ��հŸ���, �߾ӰŸ������� ����

# [ ���� - iris data�� �������������� �����м� ���� �� ��� Ȯ�� ]
iris[ 1:2,-5]

# 1. �Ÿ���� ���ϱ�
d1 <- dist(iris[,-5])

# 2. �����м� ����
# 1) �ִܰŸ���
m1 <- hclust(d1, 'single')

# 2) ����Ÿ���
m2 <- hclust(d1, 'complete')

# 3) ��չ�
m3 <- hclust(d1, 'average')


# 3. �ð�ȭ
dev.new()
par(mfrow=c(1,3))

plot(m1, hang = -1, main = 'single')
rect.hclust(m1, k=3)

plot(m2, hang = -1, main = 'complete')
rect.hclust(m2, k=3)

plot(m3, hang = -1, main = 'average')
rect.hclust(m3, k=3)


# 4. ��
# iris �������� ����(Y)�� �˰��ֱ� ������
# �з��м��� �� metric�� ���� �� �õ�
# 1) single
g1 <- ifelse(iris$Species=='setosa',1,
             ifelse(iris$Species=='versicolor',3,2))

sum(cutree(m1, k=3) == g1) / nrow(iris) * 100       # 65

# 2) complete
g2 <- ifelse(iris$Species=='setosa',1,
             ifelse(iris$Species=='versicolor',3,2))

sum(cutree(m2, k=3) == g2) / nrow(iris) * 100       # 84

# 3) average
g3 <- ifelse(iris$Species=='setosa',1,
             ifelse(iris$Species=='versicolor',2,3))

sum(cutree(m3, k=3) == g3) / nrow(iris) * 100       # 90


# 5. ������ k�� �� ã��
install.packages('NbClust')
library(NbClust)

NbClust(data = ,                # �����м� ���� ������(�Ÿ����X)
        distance = 'euclidean', # �Ÿ� ���� ��� 
        min.nc = ,              # ���� �ּ� ����
        max.nc = ,              # ���� �ִ� ����
        method = )              # �������� �Ÿ� ���

nc1 <- NbClust(iris[,-5], min.nc = 2, max.nc = 10, method = 'single')
nc1 <- NbClust(iris[,-5], min.nc = 2, max.nc = 10, method = 'complete')
nc1 <- NbClust(iris[,-5], min.nc = 2, max.nc = 10, method = 'average')


# [ �������� ]
# iris data�� �����м� ȿ���� ���̱� ����(������ �˰��ִٴ� ����)
# ��� ���

# 1. �߿� ���� ����
dev.new()

plot(iris[,-5], col = iris$Species) # Petal ���� ���� �߿伺 Ȯ��
iris_sel <- iris[,3:4]

# 2. ������ ǥ��ȭ
iris_sel_sc <- scale(iris_sel)
iris_sc <- scale(iris[,-5])

# 3. �Ÿ� ���
d_sel_sc <- dist(iris_sel_sc)
d_sc <- dist(iris_sc)

# 4. �����м� ����
iris_cl_sel1 <- hclust(d_sel_sc, 'single')
iris_cl_sel2 <- hclust(d_sel_sc, 'complete')
iris_cl_sel3 <- hclust(d_sel_sc, 'average')

iris_cl1 <- hclust(d_sc, 'single')
iris_cl2 <- hclust(d_sc, 'complete')
iris_cl3 <- hclust(d_sc, 'average')

# 5. �ð�ȭ
# 1) 4�� ������ �𵨸��� ���
dev.new()
par(mfrow=c(1,3))

plot(iris_cl1, hang = -1, main = 'single')
rect.hclust(iris_cl1,3)

plot(iris_cl2, hang = -1, main = 'complete')
rect.hclust(iris_cl2,3)

plot(iris_cl3, hang = -1, main = 'average')
rect.hclust(iris_cl3,3)

# 2) 2�� ������ �𵨸��� ���
dev.new()
par(mfrow=c(1,3))

plot(iris_cl_sel1, hang = -1, main = 'single')
rect.hclust(iris_cl_sel1,3)

plot(iris_cl_sel2, hang = -1, main = 'complete')
rect.hclust(iris_cl_sel2,3)

plot(iris_cl_sel3, hang = -1, main = 'average')
rect.hclust(iris_cl_sel3,3)

# 6. ��
as.numeric(c('a','b','c'))
as.numeric(factor(c('a','b','c')))

f1 <- factor(c('a','b','c'))
levels(f1) <- 1:3
f1

# 1) iris$Species �÷��� ���� cluster number�� ����
g1 <- as.numeric(iris$Species)

g2 <- iris$Species
levels(g2) <- c(1,3,2)

# [ ���� - ������ ������ ������ ���� ���� ���� ]
#                            (dummy variable)
# name name_n  name_a name_b name_c       name_a  name_b      
# a    1            1      0      0            1       0
# b    2            0      1      0            0       1
# c    3            0      0      1            0       0
# 
#    G_M G_F
# F    0   1
# M    1   0
# M    1   0


# 2) 4�� ���������� ���� �����ͼ��� ���� ��
iris_cl_n1 <- cutree(iris_cl1,3)
iris_cl_n2 <- cutree(iris_cl2,3)
iris_cl_n3 <- cutree(iris_cl3,3)

sum(iris_cl_n1 == g1) / nrow(iris) * 100   # 66
sum(iris_cl_n2 == g1) / nrow(iris) * 100   # 78.67
sum(iris_cl_n3 == g2) / nrow(iris) * 100   # 64.67

# 3) 2�� ���������� ���� �����ͼ��� ���� ��
iris_cl_sel_n1 <- cutree(iris_cl_sel1,3)
iris_cl_sel_n2 <- cutree(iris_cl_sel2,3)
iris_cl_sel_n3 <- cutree(iris_cl_sel3,3)

sum(iris_cl_sel_n1 == g2) / nrow(iris) * 100   # 66
sum(iris_cl_sel_n2 == g2) / nrow(iris) * 100   # 83.33
sum(iris_cl_sel_n3 == g1) / nrow(iris) * 100   # 98


# 7. ������ ������ �� ���ϱ�
iris_nb1 <- NbClust(iris_sel_sc, min.nc = 2, max.nc = 10,
                    method = 'average')

# * Among all indices:                                                
# * 8 proposed 2 as the best number of clusters 
# * 9 proposed 3 as the best number of clusters 
# * 1 proposed 4 as the best number of clusters 
# * 1 proposed 7 as the best number of clusters 
# * 2 proposed 8 as the best number of clusters 
# * 1 proposed 9 as the best number of clusters 
# * 1 proposed 10 as the best number of clusters 
# 
# ***** Conclusion *****                            
#   
#   * According to the majority rule, the best number of clusters is  3 

# 26�� �� ��ǥ�� Ȯ��
iris_nb1$All.index

# Dindex ��ǥ : �׷쳻 �л꿡 ���� ���� ��ǥ
# SDindex ��ǥ : �׷찣 �л� �� �� �߽��� ������κ��� ���� ��ǥ

# �����м������� �л�
# 1. �Ѻл�(total_ss)
# 2. �׷쳻 �л�(within_ss)
# - �� Ŭ�������� �߽����κ��� �� Ŭ������ ����ġ�� ������ ����
# - �׷쳻 �л��� �������� ���� Ŭ�����͸� ���
# - �׷��� ������ Ŀ������ �׷쳻 �л굵 �۾���

# 3. �׷찣 �л�(between_ss)
# - �� Ŭ�������� �߽��� ��ü�߽����κ��� ����� ����
# - �׷찣 �л��� Ŭ���� �� Ŭ�������� �������� �뺯�ϹǷ� ���� ���
# - �׷��� ������ Ŀ������ �׷쳻 �л굵 Ŀ��

# total_ss = between_ss + within_ss

# 2. ������� �����м�
# - �Ÿ��� ª�� ����������Ʈ���� ���� ����� ����
# - ������ �����м����� �ٸ��� �ѹ� Ŭ�����Ϳ� �Ҽӵ� ����ġ��
#   �Ÿ� ���� ��� ���Խ��� ���� �ٸ� Ŭ�����Ϳ��� �Ÿ��� �� ª�ٸ�
#   �ٸ� Ŭ�����ͷ� �̵��Ǵ� ���
# - �� metric����(�׷쳻/�׷찣 �л꿡 ���� score)
# - ����ڰ� ���� ������ ���� ���ϸ� �ش� ������ �°� �з�

# [ ���� - iris �����͸� ����� ������� �����м�(k-means) ]
# 1. �����м� ����(�𵨸�)

# knn - �з��м� => Y ����
# kmeans - �����м� => ������ ����ȭ

kmeans(x,           # ���� data(�Ÿ���� X)
       centers = )  # ������

kmean1 <- kmeans(iris[,-5], 3)

# 2. �� Ȯ�� �� ��
kmean1

# Within cluster sum of squares by cluster:
#   [1] 15.15100 23.87947 39.82097
# (between_SS / total_SS =  88.4 %)

# 1) totss
kmean1$totss          # 681.3706

# 2) withinss
sum(kmean1$withinss)  # 78.85144

# 3) betweenss
kmean1$betweenss      # 602.5192

# 78.85144 + 602.5192 = 681.3706

# 3. k�� ��ȭ�� ���� between_SS / total_SS �� ��ȭ Ȯ��
vscore <- c()

for ( i in 1:10) {
  m_kmean <- kmeans(iris[,-5], i)
  vscore <- c(vscore, round((m_kmean$betweenss/m_kmean$totss)*100,2))
}

dev.new()
plot(1:10, vscore, type = 'o', xlab = 'k�� ��',
     ylab = 'score', main = '������ ��ȭ�� ���� ������')



# �� ������ �����м� ���� ���
# 1. ����ڰ� ������ k�� ����ŭ �����ϰ� �ʱ� �߽�(seed)�� ����
# 2. �� seed�κ��� �� ����ġ���� �Ÿ� ���
# 3. �Ÿ��� ���� ����� ����ġ���� �� Ŭ�����Ϳ� �Ҵ�
# 4. ����� Ŭ�������� �߽��� ����
# 5. ����� Ŭ�������� �߽����� ��ü �������� �Ÿ� ��� ���
# 6. �� Ŭ�������� �߽����κ��� ����� ����������Ʈ �Ҽ�/�̵�
# 7. �� ������ ���̻��� �߽��� �̵����� ���������� ��� �ݺ�
# 8. �׷� ����


# [ �������� ]
# iris data�� �� ȿ������ ������� �����м��� ���� ��� ���
# 1. �߿� ���� ����
iris_sel <- iris[,3:4]

# 2. ������ ǥ��ȭ
iris_sel_sc <- scale(iris_sel)
iris_sc <- scale(iris[,-5])

# 3. clustering
# 1) 4�� ��������, ǥ��ȭ ó��
iris_kmean <- kmeans(iris_sc, 3)
iris_kmean$betweenss / iris_kmean$totss * 100  # 76.69658

# 2) 2�� ��������, ǥ��ȭ ó��
iris_kmean_sel <- kmeans(iris_sel_sc, 3)

iris_kmean_sel$betweenss / iris_kmean_sel$totss * 100 # 93

levels(g2) <- c(3,1,2)
sum(iris_kmean_sel$cluster == g2) / nrow(iris) * 100  # 96

# 4. k�� ��ȭ�� ���� between_SS / total_SS �� ��ȭ Ȯ��
vscore <- c()

for ( i in 1:10) {
  m_kmean <- kmeans(iris_sel_sc, i)
  vscore <- c(vscore, round((m_kmean$betweenss/m_kmean$totss)*100,2))
}

dev.new()

plot(1:10, vscore, type = 'o', xlab = 'k�� ��',
     ylab = 'score', main = '������ ��ȭ�� ���� ������')

