# [ Y가 연속형인 경우 분석 방법 ]
# 1. 전통 회귀 분석
# - 여러가지 통계적 가정 필요
# - 가정이 성립되지 않으면 예측력이 떨어짐
# - 인과관계 파악이 용이
# 
# 2. 분류모델이 제공하는 회귀 모델
#    (ex. randomForest-classification)
# - 비 통계적 모델
# - 특별한 튜닝 없이도 우수한 예측력 갖음
# - 인과관계 파악은 불가(예측에 집중)


# [ 분류 분석 ]
# 1. 트리기반 모델
# - DT -> RF > GB > XGB
# - outlier 민감 X
# - 범주, 연속형 설명 변수 모두 포함 가능
# - 학습시간에 비해 예측시간이 빠름
# - 연속형 데이터에 대한 scaling 필요 X
# 
# 2. 거리기반 모델
# - outlier 매우 민감 => 제거/수정
# - 범주형 설명 변수가 많이 포함될수록 예측력 떨어짐
# - 고차원 데이터에 비적합
# - 예측 시간이 오래걸림(학습과정 생략)
# - 연속형 데이터에 대한 scaling 필요(표준화)
# - 모델에 학습되는 설명변수의 조합이 매우 중요한 모델(중요한 설명변수로만 구성 될수록 좋음)

# 예) 거리 기반 모델링 과정
#   소득 직업  지출 A상품구매여부
# 1 400 회사원(1) 200   X
# 2 410 회사원    220   X
# 3 500 자영업(2) 400   O
# 
# 4 420 회사원(1) 180   ?
# 
# d41 <- sqrt((420-400)^2 + (1-1)^2 + (180-200)^2) ; d41  # 28.28
# d42 <- sqrt((420-410)^2 + (1-1)^2 + (180-220)^2) ; d42  # 41.23
# d43 <- sqrt((420-500)^2 + (1-2)^2 + (180-400)^2) ; d43  # 234.1
# 
# 결론) 4번 관측치와 가장 유사한(거리가 가까운) 관측치는 1번이므로
# 1번의 행동과 같은 행동을 할 것으로 예상, 즉 구매하지 않을것!!
  
  
# [ 거리 계산 시 표준화 필요 이유 ]
    x1    x2
p1  5     100
p2  6     200
p3  10    150

# 1) 표준화 없이 일반 거리 계산
d12 <- sqrt((5-6)^2  + (100-200)^2) ; d12  # 100.005
d13 <- sqrt((5-10)^2 + (100-150)^2) ; d13  # 50.23

d12 > d13

# 2) 표준화 후 거리 계산
# (x1의 평균 6, 표준편차 0.01)
# (x2의 평균 150, 표준편차 30)
    
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
# - 예측하고자 하는 관측치로부터 기존관측치와의 거리 계산
# - 계산된 거리중 k개의 가장 가까운 이웃 관측치 확인
# - k개의 이웃이 갖는 정답(Y)의 평균 혹은 다수결로 최종 결론 내림
# - 설명변수의 스케일링 필요
# - Y class의 개수의 배수가 되는 k수는 적절치 않음
# - 모델에 학습되는 설명변수의 조합이 매우 중요한 모델

install.packages('class')
library(class)

knn(train = ,   # 기존 데이터 X
    test = ,    # 예측할 데이터 X 
    cl = ,      # 기존 데이터 Y
    k = ,       # 이웃의 수
    prob = )    # 확률 추출 여부

# [ 예제 - knn 모델을 사용한 iris data 품종 예측 ]
# 1. sampling
vrn <- sample(1:nrow(iris), size = nrow(iris)*0.7)

iris_tr_y <- iris[vrn, 5]
iris_tr_x <- iris[vrn, -5]

iris_te_y <- iris[-vrn, 5]
iris_te_x <- iris[-vrn, -5]

# 2. 모델링 및 예측
pre_te <- knn(iris_tr_x, iris_te_x, iris_tr_y, k=3, prob = T)

# 3. 평가
sum(iris_te_y == pre_te) / nrow(iris_te_x) * 100  # 97.8

# 4. 예측
new_data <- data.frame(Sepal.Length=7, 
                       Sepal.Width=3, 
                       Petal.Length=1, 
                       Petal.Width=1.0)

knn(iris_tr_x, new_data, iris_tr_y, k=3, prob = T)

# 5. 매개변수 튜닝
# k수 변화에 따른 train/test data set score 확인
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
     xlab = 'k의 수', ylab = 'score', 
     main = 'k수 변화에 따른 score 변화')
lines(1:10, score_te, type='o', col='red')

axis(1, at=1:10)
legend(9,90, c('train','test'), col = c('blue','red'), lty=1)

# [ 연습 문제 - knn을 사용한 cancer data의 종양의 양성 여부 예측 ]
# 1. 데이터 로딩 및 sampling
cancer <- read.csv('cancer.csv', stringsAsFactors = F)
cancer$id <- NULL

vrn <- sample(1:nrow(cancer), size = nrow(cancer)* 0.7)

cancer_tr_y <- cancer[vrn, 1]
cancer_tr_x <- cancer[vrn, -1]

cancer_te_y <- cancer[-vrn, 1]
cancer_te_x <- cancer[-vrn, -1]

# 2. 모델링 및 예측
pre_cancer_te <- knn(cancer_tr_x, cancer_te_x, cancer_tr_y, k=3)

# 3. 평가
sum(cancer_te_y == pre_cancer_te) / nrow(cancer_te_x) * 100

# 4. 적절한 k의 수 찾기
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
     xlab = 'k의 수', ylab = 'score', 
     main = 'k수 변화에 따른 score 변화')
lines(1:10, score_te, type='o', col='red')

axis(1, at=1:10)
legend(8,90, c('train','test'), col = c('blue','red'), lty=1)

# 5. 설명변수의 스케일 조정 후 모델 학습(k=5)

df1 <- data.frame(col1=c(1,2,4,7,10), col2 = c(10,32,45,35,35))
scale(df1)

mean(df1$col1)  # 4.8
sd(df1$col1)    # 3.7

# cancer 설명변수 scaling
cancer_tr_x_sc <- scale(cancer_tr_x)
cancer_te_x_sc <- scale(cancer_te_x)

# scaling된 data로 모델링 및 평가
pre_cancer_te_sc <- knn(cancer_tr_x_sc, cancer_te_x_sc, 
                        cancer_tr_y, k=5, prob = T)

sum(cancer_te_y == pre_cancer_te_sc) / nrow(cancer_te_x_sc) * 100

# 결론 : scaling한 후 예측력이 향상될 확률 존재
# 만약 설명변수중 의미없는, 예측력에 방해되는 변수가 있다면
# 해당 변수를 제거한 후 다시 모델링 필요


# 데이터 분석
# 1. 지도학습(Y존재)
#  1) 회귀분석
#  2) 분류분석
#   - 트리기반 모델
#   - 거리기반 모델
#     
# 2. 비지도학습(Y존재X)
#  1) 군집분석(거리기반 모델)
#  2) 연관분석  

# 군집 분석
# - 정답이 없는 비지도학습
# - 주어진 데이터들을 유사성에 근거해 비슷한 데이터끼리 묶는 분석기법
# - 초기 데이터에 대한 연구를 위해 사용되거나
#   클러스터링된 군집에 대한 정보를 추가로 활용하기 위해 주로 사용
# - 데이터 축소 테크닉
# - 거리기반 모델

# 1. 계층적 군집분석
# - 거리가 가까운 데이터포인트들끼리 순차적으로 묶는 형식
# - 하나의 데이터가 한 군집에 속하면 군집은 변경되지 않음
# - 군집을 형성하는 기준에 따라 여러가지 모델 제공

# ** 군집 형성 과정(군집과 데이터포인트와의 거리 측정 방식에 따라)
# 1. 최단거리법(single, min)
# 2. 최장거리법(complete, max)
# 3. 평균거리법(average)
# 4. 중앙거리법(median)

# [ 예제 - 각 데이터들끼리의 군집 형성 과정 ]
library(stringr)
v1 <- c(1,3,6,10,18)
names(v1) <- str_c('p',1:5)

step1) 각 개별 관측치기리의 거리 계산(거리행렬)
dist(v1)

    p1 p2 p3 p4
p2  2         
p3  5  3      
p4  9  7  4   
p5 17 15 12  8

step2) 위 거리중 가장 가까운 데이터포인트를 하나의 군집으로 묶음
       => c1(p1,p2)
    
step3) c1과 나머지 데이터 포인트끼리의 거리 계산
1) 최단거리법
dc1p3 = min(dp1p3, dp2p3) = min(5,3) = 3
dc1p4 = min(dp1p4, dp2p4) = min(9,7) = 7
dc1p5 = min(dp1p5, dp2p5) = min(17,15) = 15

2) 최장거리법
dc1p3 = max(dp1p3, dp2p3) = max(5,3) = 5
dc1p4 = max(dp1p4, dp2p4) = max(9,7) = 9
dc1p5 = max(dp1p5, dp2p5) = max(17,15) = 17

3) 평균거리법
dc1p3 = d(p1p2)p3 = d(2,6) = 4    
...

step4) 위해서 구해진 군집과의 거리를 모두 계산,
       가장 가까운 거리 파악(최단거리법)
    
       c1 p3 p4 p5
    c1  . 
    p3  3  .    
    p4  7  4  .
    p5  15 12 8  .

    => c1(p1,p2,p3)

step5) 변경된 c1과 나머지 관측치(p4,p5)의 거리 계산(최단)
dc1p4 = min(p1p4,p2p4,p3p4) = min(9,7,4) = 4
dc1p5 = min(p1p5,p2p5,p3p5) = min(17,15,12) = 12

   c1 p4 p5
c1  .
p4 4   .
p5 12  8  .

step6) 위 거리중 가장 가까운 거리 확인 
       => c1(p1,p2,p3,p4)

# 위 결과를 실제 계층적 군집분석에 의해 시각화
hclust(d,                    # 거리행렬 
       method = 'single',    
                'complete',
                'average',
                'median')

# 1) 모델 생성(군집형성) 
d1 <- dist(v1)
m_clust1 <- hclust(d1, method = 'single')

# 2) 시각화
dev.new()
par(mfrow=c(1,2))
plot(m_clust1, main = '최단거리법에 의한 군집형성과정')

plot(m_clust1, 
     hang = -1)  # 데이터포인트들의 시작을 맨 밑으로 하는 옵션 

dev.new()
plot(m_clust1, hang = -1)
rect.hclust(tree = m_clust1, k = 2)

# 2. 비계층적 군집분석

