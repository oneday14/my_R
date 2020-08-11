# 4. 모델 튜닝
iris_dt2$control

# 1) minsplit = minbucket
#    minbucket = round(minsplit / 3)
# - 추가 가지치기 매개변수
# - 오분류 개수 > minbucket 인 경우 추가 가지치기 진행
# - minbucket값 보다 오분류개수가 작아질때까지 가지치기 계속 시도
# - 하지만 추가로 가지치기할 분류 기준이 더이상 없는경우 stop
# - minbucket값이 작을수록 더 복잡한 모델의 생성 확률 높아짐
# - minbucket값이 작을수록 모델이 복잡해져 overfit 현상 발생 확률 증가


# 예제) minbucket값의 변화에 따른 모델의 예측점수 변화 확인
score_tr <- c() ; score_te <- c()

for (i in 2:10) {
  iris_dt <- rpart(Species ~ . , data = iris_train, 
                   control = rpart.control(minbucket=i))
  
  v_pre_tr <- predict(iris_dt, newdata = iris_train, type = 'class')
  v_pre_te <- predict(iris_dt, newdata = iris_test, type = 'class')
  
  vscore_tr <- sum(iris_train$Species == v_pre_tr) / nrow(iris_train)
  vscore_te <- sum(iris_test$Species == v_pre_te) / nrow(iris_test)
  
  
  score_tr <- c(score_tr, vscore_tr)
  score_te <- c(score_te, vscore_te)
}

dev.new()
plot(2:10, score_tr, type = 'o', col = 'blue', ylim = c(0.9,1))
lines(2:10, score_te, type = 'o', col = 'red')

# 과대적합 : 새로운 데이터에서는 예측률이 낮고, 기존의 데이터에서는 예측력이 높은것
# 결론 : overfit(과대적합)이 발생하지 않으면서 test score가 높은
# 매개변수를 선택

# 2) maxdepth 
# - 각 트리의 노드의 분기 시 설명변수의 재사용 횟수 제한
# - maxdepth 값이 작을수록 단순한 트리구조를 갖을 확률 높아짐

# [ 참고 : minbucket이 작아짐에 따른 추가 split 생성 확인 ]
iris_dt2 <- rpart(Species ~ ., data = iris_train,
                  control = rpart.control(minbucket = 2))
dev.new()
rpart.plot(iris_dt2, type = 4)

# 3) cp
# - 직접적으로 트리의 size를 조절하는 매개변수
# - 모델 훈련 후 cptable의 cp값과 tree size, error를 비교하면서
#   적절한 cp값으로 튜닝
# - cp값을 시각화하면 모델의 기준 error와 함께 적절한 cp값 찾기 용이

# cptable : 각 split개수마다의 표준오차 확인
# nsplit + 1 = tree size
iris_dt2$cptable  

# plotcp : cp값 시각화(기준 오차 확인)
dev.new()
plotcp(iris_dt2)

# rpart.plot : 모델의 시각화
dev.new()
rpart.plot(iris_dt2, type = 4)


# [ 연습 문제 : cancer data의 종양의 양/악성 예측(Decision Tree) ]

# 1. data loading
cancer <- read.csv('cancer.csv', stringsAsFactors = F)
cancer$id <- NULL

# 2. 교차 산점도 시각화
# 종양의 양/악성에 영향을 미치는 주요 변수 확인
dev.new()
cancer$diagnosis <- as.factor(cancer$diagnosis)
plot(cancer[, 2:10], col= cancer$diagnosis)

# 3. sampling
v_rn <- sample(1:nrow(cancer), size = nrow(cancer) * 0.7)
cancer_tr <- cancer[v_rn, ]
cancer_te <- cancer[-v_rn, ]

# 4. modeling
cancer_dt1 <- rpart(diagnosis ~ ., data = cancer_tr)

# 5. 시각화
dev.new()
rpart.plot(cancer_dt1)

# 6. 모델 평가
pre_te <- predict(cancer_dt1, newdata = cancer_te, type = 'class')
sum(cancer_te$diagnosis == pre_te) / nrow(cancer_te) * 100

# 7. 모델 튜닝(minbucket)
score_tr <- c() ; score_te <- c()

for (i in 1:10) {
  cancer_dt <- rpart(diagnosis ~ . , data = cancer_tr, 
                     control = rpart.control(minbucket=i))
  
  v_pre_tr <- predict(cancer_dt, newdata = cancer_tr, type = 'class')
  v_pre_te <- predict(cancer_dt, newdata = cancer_te, type = 'class')
  
  vscore_tr <- sum(cancer_tr$diagnosis == v_pre_tr) / nrow(cancer_tr)
  vscore_te <- sum(cancer_te$diagnosis == v_pre_te) / nrow(cancer_te)
  
  score_tr <- c(score_tr, vscore_tr)
  score_te <- c(score_te, vscore_te)
}

cancer_dt1$control
dev.new()
plot(1:10, score_tr, type = 'o', col = 'blue', ylim = c(0.9,1))
lines(1:10, score_te, type = 'o', col = 'red')


# train score >>> test score  : 과대적합
# train score <<< test score  : 과소적합



# 새로운 데이터에 대한 예측
new_data <- cancer[10, -1] + 0.01
predict(cancer_dt1, newdata = new_data, type = 'class')


# [ 트리 기반 모델의 변수 선택 기능 ]
# - 트리 모델은 각 변수의 중요도를 자식 노드의 불순도를 통해 계산
# - 중요도가 높은 변수를 상위 split 조건으로 사용
# - 변수간 중요도 차이가 큰 경우 특정 변수만 재사용 가능성 있음
# - 트리 모델의 변수 중요도는 분석시 중요 변수 파악이 용이하게 사용

# 불순도
# - 특정 조건으로 인해 분리된 자식노드의 클래스의 혼합 정도
# - 주로 지니계수로 측정
# - 2개의 class를 갖는경우 f(p) = p(1-p)로 계산

# 예제) 
# - AAAAAA : p(A가 속할 확률) = 1, f(p) = 1 * (1-1) = 0
# - AAAABB : p = 4/6, f(p) = 4/6 * 2/6 = 0.22
# - AAABBB : p = 1/2, f(p) = 1/2 * 1/2 = 0.25

# decision tree에서 변수 중요도 확인
iris_dt2$variable.importance

# Petal.Width Petal.Length Sepal.Length  Sepal.Width 
# 62.64908     61.22238     39.67210     26.34410 


# 2) 조건부 추론 나무
# - 기존 decision tree를 보안
# - 분류 기준에 따른 적합성을 통계적으로 해석하는 장치 추가
#   (변수의 유의성 검정)

install.packages('party')
library(party)

ctree(formula = ,  # Y ~ X
      data = )     # data set

iris_dt3 <- ctree(Species ~ ., data = iris_train)

dev.new()
rpart.plot(iris_dt2)

dev.new()
plot(iris_dt3)



# 3) Ramdom Forest
# - Decision Tree의 과대적합 현상을 해결하기 위해 여러개의 서로 다른
#   모양의 tree를 구성, 종합하여 최종 결론을 내는 방식
# - Random Forest Classifier와 Random Forest Regressor 존재
# - 분류모델인 경우는 다수결로, 회귀모델인 경우는 평균으로 최종결론

install.packages('randomForest')
library(randomForest)

# 1. 데이터 수집 및 sampling
v_rn <- sample(1:nrow(iris), size = nrow(iris)*0.7)

iris_tr <-iris[v_rn, ]
iris_te <-iris[-v_rn, ]

# 2. modeling
randomForest(fomular = ,
             data = )

iris_rf1 <- randomForest(Species ~ . , iris_tr)

# 3. 평가
pre_te <- predict(iris_rf1, newdata = iris_te, type='class')
sum(iris_te$Species == pre_te) / nrow(iris_te) 

iris_rf1$importance

# 4. 예측
new_data <- data.frame(Sepal.Length=6, 
                       Sepal.Width=0.1, 
                       Petal.Length=5, 
                       Petal.Width=2)

predict(iris_rf1, newdata = new_data, type='class')

# 5. 매개변수 튜닝
# 5-1) ntree : 트리의 개수(기본 500개)
randomForest( ~, , ntree=10)
help(randomForest)

# [ 연습문제 : ntree의 elbow point 확인 ]
score_te <- c()

for (i in 1:1000) {
  iris_rf <- randomForest(Species ~ ., iris_tr, ntree=i)
  pre_te <- predict(iris_rf, newdata = iris_te, type='class')
  vscore_te <- sum(iris_te$Species == pre_te) / nrow(iris_te)
  score_te <- c(score_te, vscore_te)
}

dev.new()
plot(1:1000, score_te, type='o', ylim = c(0.7,1),
     ylab = 'score', xlab='ntree',
     main='tree수 변화에 따른 예측점수 변화')

score_te

# 5-2) mtry 
# - 각 split시 고려되어지는 설명변수의 후보의 개수
# - 서로 다른 트리를 구성하기 위해 생긴 매개변수 형태
# - 값이 작을수록 서로 다른 트리가 형성

# mtry=1 => 매번 랜덤하게 선택된 변수 선택 => 서로 다른 트리 구성
#        => 트리가 복잡해지거나 예측력이 낮아질 확률 존재
#   
# mtry=4 => 똑같은 설명변수 선택 => 똑같은 트리(학습되는 데이터 동일 가정)

# [ 실습 : mtry 변화에 따른 예측력과 과대적합 여부 확인 ]
score_tr <- c(); score_te <- c()

for (i in 1:4) {
  iris_rf <- randomForest(Species ~ ., iris_tr, mtry=i)
  
  pre_tr <- predict(iris_rf, newdata = iris_tr, type='class')
  pre_te <- predict(iris_rf, newdata = iris_te, type='class')
  
  vscore_tr <- sum(iris_tr$Species == pre_tr) / nrow(iris_tr)
  vscore_te <- sum(iris_te$Species == pre_te) / nrow(iris_te)
  
  score_tr <- c(score_tr, vscore_tr)
  score_te <- c(score_te, vscore_te)
}

dev.new()
plot(1:4, score_tr, type='o', ylim = c(0.7,1), col='blue',
     ylab = 'score', xlab='mtry',
     main='mtry 변화에 따른 예측점수 변화')
lines(1:4, score_te, type='o', col = 'red')


# Random Forest에서의 회귀
data('airquality')
airquality

randomForest(Ozone ~ . , airquality,
             mtry = 3, ntree=100, na.action = na.omit )


