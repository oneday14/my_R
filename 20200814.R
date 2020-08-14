# 변수선택법(전진,후진,stepwise)
# 1. 전진선택법(forward selection)
Y = X1
Y = X1 + X2 
Y = X1 + X2 + X3 

# 2. 후진선택법(backward selection)
Y = X1 + X2 + X3 + X4 + X5
Y = X1 + X2 + X3 + X4
Y = X1 + X2 + X3 

# 3. 단계적선택법(stepwise selection)
Y = X1 + X2 + X3 + X4 + X5 
Y = X1 + X2 + X3 + X4
Y = X1 + X2 + X3             # 제거된 변수중 추가할 변수 있는지
Y = X1 + X2 + X3 + X5


# [ 예제 - 보스턴 주택 가격셋을 사용한 변수선택 방법 ]
install.packages('mlbench')  #BostonHousing data loading 시 필요
library(mlbench)
data(BostonHousing)
str(BostonHousing)

lm1 <- lm(medv ~ ., data = BostonHousing)
step(lm1, direction = 'forward')
step(lm1, direction = 'backward')
step(lm1, direction = 'both')

# [ 예제 - iris셋을 사용한 변수선택 방법 ]
m_dt1 <- rpart(Species ~ ., data=iris)
step(m_dt1, direction = 'both')           # 불가

install.packages('MASS')
library(MASS)
install.packages('klaR')
library(klaR)

stepclass(x,               # x 데이터
          y,               # y 데이터
          method = 'lda',  # 분류방법
          direction = ,    # forward, backward, both
          start.vars = )   # 시작변수

stepclass(iris[,-5], iris[,5], 'lda',
          direction = 'both')

# final model : iris[, 5] ~ Petal.Width + 
#   correctness rate = 0.96 

stepclass(iris[,-5], iris[,5], 'lda',
          direction = 'both', start.vars = 'Petal.Width')

stepclass(iris[,-5], iris[,5], 'lda',
          direction = 'both', start.vars = 'Petal.Length')

# [ 예제 - cancer data를 사용한 군집분석 ]
library(rpart)
library(randomForest)
library(NbClust)

# 1. data loading
cancer <- read.csv('cancer.csv')
cancer$id <- NULL

# 2. 계층적 군집분석 수행(표준화 X, 변수선택X)
# 1) 모델 생성
d1 <- dist(cancer[,-1])
m_cl1 <- hclust(d1, 'single')
m_cl2 <- hclust(d1, 'complete')
m_cl3 <- hclust(d1, 'average')

# 2) 모델 시각화 및 평가
dev.new()
par(mfrow=c(1,3))

plot(m_cl1, hang = -1, main='single')
rect.hclust(m_cl1, 2)

plot(m_cl2, hang = -1, main='complete')
rect.hclust(m_cl2, 2)

plot(m_cl3, hang = -1, main='average')
rect.hclust(m_cl3, 2)

table(cancer$diagnosis)


# 3. 계층적 군집분석 수행(표준화 O, 변수선택X)
# 1) 모델 생성
d2 <- dist(scale(cancer[,-1]))
m_cl_sc1 <- hclust(d2, 'single')
m_cl_sc2 <- hclust(d2, 'complete')
m_cl_sc3 <- hclust(d2, 'average')

# 2) 모델 시각화 및 평가
dev.new()
par(mfrow=c(1,3))

plot(m_cl_sc1, hang = -1, main='single')
rect.hclust(m_cl_sc1, 2)

plot(m_cl_sc2, hang = -1, main='complete')
rect.hclust(m_cl_sc2, 2)

plot(m_cl_sc3, hang = -1, main='average')
rect.hclust(m_cl_sc3, 2)


# 4. 계층적 군집분석 수행(표준화 O, 변수선택O)
# 1) tree기반 모델링을 통한 변수 선택 혹은 제거
cancer_dt1 <- rpart( diagnosis ~ ., data = cancer)
var_imp1 <- cancer_dt1$variable.importance

feature1 <- names(sort(var_imp1)[sort(var_imp1) > 10])

cancer_sel <- cancer[  , colnames(cancer) %in% feature1 ]


# 2) 변수선택법(전진,후진,stepwise)에 의한 데이터의 선택
# step : Y가 연속형
# regsebsets : Y가 연속형
# stepclass : Y가 팩터형

stepclass(cancer[,-1], cancer[,1], 'lda', 
          direction = 'both',
          improvement = 0.01, fold = 10)

df_caner <- cancer[, c('concave_points_worst',
                       'radius_worst',
                       'texture_mean')]

# 3) 군집분석 수행
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
  
  
# 5. 비계층적 군집분석 수행(표준화 O, 변수선택O)  
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


# [ 이상치 검정 ]
# 1. Y가 연속형인경우
# - 회귀모델 적용 후 이상치 검정 수행
# - 통계적 모델을 통한 유의성 검정 가능(p-value)
# - car::outlierTest로 확인 가능

# [ 예제 - 보스턴 주택 가격 데이터의 이상치 검정 ]
install.packages('car')
library(car)

m1 <- lm(medv ~ ., Boston)
outlierTest(m1)

# 2. Y가 범주형인경우
# - randomForest 모델 적용 후 이상치 검정 수행
# - 각 데이터포인트의 거리(유사성) 기반으로 이상치 확인
# - 유의성 검정 불가(p-value 확인 불가)
# - outlier로 확인 가능


# [ 예제 - iris 데이터의 이상치 검정 ]
m2 <- randomForest(Species ~ ., iris, proximity=T)
m2$proximity

outlier(m2)

dev.new()
plot(outlier(m2), type = 'h', col = col2)

col1 <- ifelse(iris$Species=='setosa','red',
              (ifelse(iris$Species=='versicolor','blue','green')))

col2 <- c('red','blue','green')[as.numeric(iris$Species)]






