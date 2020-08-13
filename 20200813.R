# 군집 분석
# 1. 계층적 군집분석
# - 거리가 짧은 데이터끼리 순차적으로 군집을 형성하는 과정
# - 한번 속한 군집은 변경되지 X
# - 군집과 데이터와의 거리를 정의하는 방식에 따라
#   최단거리법, 최장거리법, 평균거리법, 중앙거리법으로 나뉨

# [ 예제 - iris data의 설명변수만으로 군집분석 수행 및 결과 확인 ]
iris[ 1:2,-5]

# 1. 거리행렬 구하기
d1 <- dist(iris[,-5])

# 2. 군집분석 수행
# 1) 최단거리법
m1 <- hclust(d1, 'single')

# 2) 최장거리법
m2 <- hclust(d1, 'complete')

# 3) 평균법
m3 <- hclust(d1, 'average')


# 3. 시각화
dev.new()
par(mfrow=c(1,3))

plot(m1, hang = -1, main = 'single')
rect.hclust(m1, k=3)

plot(m2, hang = -1, main = 'complete')
rect.hclust(m2, k=3)

plot(m3, hang = -1, main = 'average')
rect.hclust(m3, k=3)


# 4. 평가
# iris 데이터의 정답(Y)을 알고있기 때문에
# 분류분석의 평가 metric에 의해 평가 시도
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


# 5. 적절한 k의 수 찾기
install.packages('NbClust')
library(NbClust)

NbClust(data = ,                # 군집분석 수행 데이터(거리행렬X)
        distance = 'euclidean', # 거리 측정 방식 
        min.nc = ,              # 평가할 최소 군집
        max.nc = ,              # 평가할 최대 군집
        method = )              # 군집과의 거리 방식

nc1 <- NbClust(iris[,-5], min.nc = 2, max.nc = 10, method = 'single')
nc1 <- NbClust(iris[,-5], min.nc = 2, max.nc = 10, method = 'complete')
nc1 <- NbClust(iris[,-5], min.nc = 2, max.nc = 10, method = 'average')


# [ 연습문제 ]
# iris data의 군집분석 효과를 높이기 위한(정답을 알고있다는 가정)
# 방법 모색

# 1. 중요 변수 선택
dev.new()

plot(iris[,-5], col = iris$Species) # Petal 관련 변수 중요성 확인
iris_sel <- iris[,3:4]

# 2. 데이터 표준화
iris_sel_sc <- scale(iris_sel)
iris_sc <- scale(iris[,-5])

# 3. 거리 행렬
d_sel_sc <- dist(iris_sel_sc)
d_sc <- dist(iris_sc)

# 4. 군집분석 수행
iris_cl_sel1 <- hclust(d_sel_sc, 'single')
iris_cl_sel2 <- hclust(d_sel_sc, 'complete')
iris_cl_sel3 <- hclust(d_sel_sc, 'average')

iris_cl1 <- hclust(d_sc, 'single')
iris_cl2 <- hclust(d_sc, 'complete')
iris_cl3 <- hclust(d_sc, 'average')

# 5. 시각화
# 1) 4개 변수로 모델링한 경우
dev.new()
par(mfrow=c(1,3))

plot(iris_cl1, hang = -1, main = 'single')
rect.hclust(iris_cl1,3)

plot(iris_cl2, hang = -1, main = 'complete')
rect.hclust(iris_cl2,3)

plot(iris_cl3, hang = -1, main = 'average')
rect.hclust(iris_cl3,3)

# 2) 2개 변수로 모델링한 경우
dev.new()
par(mfrow=c(1,3))

plot(iris_cl_sel1, hang = -1, main = 'single')
rect.hclust(iris_cl_sel1,3)

plot(iris_cl_sel2, hang = -1, main = 'complete')
rect.hclust(iris_cl_sel2,3)

plot(iris_cl_sel3, hang = -1, main = 'average')
rect.hclust(iris_cl_sel3,3)

# 6. 평가
as.numeric(c('a','b','c'))
as.numeric(factor(c('a','b','c')))

f1 <- factor(c('a','b','c'))
levels(f1) <- 1:3
f1

# 1) iris$Species 컬럼의 값을 cluster number로 변경
g1 <- as.numeric(iris$Species)

g2 <- iris$Species
levels(g2) <- c(1,3,2)

# [ 참고 - 문자형 변수의 숫자형 변경 가능 형태 ]
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


# 2) 4개 설명변수를 갖는 데이터셋의 군집 평가
iris_cl_n1 <- cutree(iris_cl1,3)
iris_cl_n2 <- cutree(iris_cl2,3)
iris_cl_n3 <- cutree(iris_cl3,3)

sum(iris_cl_n1 == g1) / nrow(iris) * 100   # 66
sum(iris_cl_n2 == g1) / nrow(iris) * 100   # 78.67
sum(iris_cl_n3 == g2) / nrow(iris) * 100   # 64.67

# 3) 2개 설명변수를 갖는 데이터셋의 군집 평가
iris_cl_sel_n1 <- cutree(iris_cl_sel1,3)
iris_cl_sel_n2 <- cutree(iris_cl_sel2,3)
iris_cl_sel_n3 <- cutree(iris_cl_sel3,3)

sum(iris_cl_sel_n1 == g2) / nrow(iris) * 100   # 66
sum(iris_cl_sel_n2 == g2) / nrow(iris) * 100   # 83.33
sum(iris_cl_sel_n3 == g1) / nrow(iris) * 100   # 98


# 7. 적절한 군집의 수 정하기
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

# 26개 각 지표값 확인
iris_nb1$All.index

# Dindex 지표 : 그룹내 분산에 의해 계산된 지표
# SDindex 지표 : 그룹간 분산 및 각 중심의 평균으로부터 계산된 지표

# 군집분석에서의 분산
# 1. 총분산(total_ss)
# 2. 그룹내 분산(within_ss)
# - 각 클러스터의 중심으로부터 각 클러스터 관측치가 떨어진 정도
# - 그룹내 분산은 작을수록 좋은 클러스터링 결과
# - 그룹의 개수가 커질수록 그룹내 분산도 작아짐

# 3. 그룹간 분산(between_ss)
# - 각 클러스터의 중심이 전체중심으로부터 흩어진 정도
# - 그룹간 분산은 클수록 각 클러스터의 이질성을 대변하므로 좋은 결과
# - 그룹의 개수가 커질수록 그룹내 분산도 커짐

# total_ss = between_ss + within_ss

# 2. 비계층적 군집분석
# - 거리가 짧은 데이터포인트끼리 묶는 방식은 동일
# - 계층적 군집분석과는 다르게 한번 클러스터에 소속된 관측치도
#   거리 측정 대상에 포함시켜 만약 다른 클러스터와의 거리가 더 짧다면
#   다른 클러스터로 이동되는 방식
# - 평가 metric존재(그룹내/그룹간 분산에 의한 score)
# - 사용자가 직접 군집의 수를 정하면 해당 군집에 맞게 분류

# [ 예제 - iris 데이터를 사용한 비계층적 군집분석(k-means) ]
# 1. 군집분석 수행(모델링)

# knn - 분류분석 => Y 예측
# kmeans - 군집분석 => 데이터 세분화

kmeans(x,           # 원본 data(거리행렬 X)
       centers = )  # 군집수

kmean1 <- kmeans(iris[,-5], 3)

# 2. 모델 확인 및 평가
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

# 3. k수 변화에 따른 between_SS / total_SS 의 변화 확인
vscore <- c()

for ( i in 1:10) {
  m_kmean <- kmeans(iris[,-5], i)
  vscore <- c(vscore, round((m_kmean$betweenss/m_kmean$totss)*100,2))
}

dev.new()
plot(1:10, vscore, type = 'o', xlab = 'k의 수',
     ylab = 'score', main = '군집수 변화에 따른 설명력')



# 비 계층적 군집분석 수행 방식
# 1. 사용자가 지정한 k의 수만큼 랜덤하게 초기 중심(seed)값 추출
# 2. 위 seed로부터 각 관측치와의 거리 계산
# 3. 거리가 가장 가까운 관측치들을 각 클러스터에 할당
# 4. 변경된 클러스터의 중심을 재계산
# 5. 재계산된 클러스터의 중심으로 전체 데이터의 거리 모두 계산
# 6. 각 클러스터의 중심으로부터 가까운 데이터포인트 소속/이동
# 7. 위 과정을 더이상의 중심이 이동되지 않을때가지 계속 반복
# 8. 그룹 고정


# [ 연습문제 ]
# iris data의 더 효과적인 비계층적 군집분석을 위한 방안 모색
# 1. 중요 변수 선택
iris_sel <- iris[,3:4]

# 2. 데이터 표준화
iris_sel_sc <- scale(iris_sel)
iris_sc <- scale(iris[,-5])

# 3. clustering
# 1) 4개 변수선택, 표준화 처리
iris_kmean <- kmeans(iris_sc, 3)
iris_kmean$betweenss / iris_kmean$totss * 100  # 76.69658

# 2) 2개 변수선택, 표준화 처리
iris_kmean_sel <- kmeans(iris_sel_sc, 3)

iris_kmean_sel$betweenss / iris_kmean_sel$totss * 100 # 93

levels(g2) <- c(3,1,2)
sum(iris_kmean_sel$cluster == g2) / nrow(iris) * 100  # 96

# 4. k수 변화에 따른 between_SS / total_SS 의 변화 확인
vscore <- c()

for ( i in 1:10) {
  m_kmean <- kmeans(iris_sel_sc, i)
  vscore <- c(vscore, round((m_kmean$betweenss/m_kmean$totss)*100,2))
}

dev.new()

plot(1:10, vscore, type = 'o', xlab = 'k의 수',
     ylab = 'score', main = '군집수 변화에 따른 설명력')


