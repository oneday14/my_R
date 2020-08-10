# [ 데이터 분석 ]
# - 데이터마이닝 : 데이터로부터 의미있는 정보(현상)를 찾는 분석 기법
  # ex) 비오는날 빨간립스틱의 수요가 증가함을 발견

# - 머신러닝(기계학습) 
# 사용자가 데이터로부터 직접 패턴을 찾는게 아닌
# 기계(모델)가 데이터의 학습을 통해 규칙을 찾아주는 분석 기법


# 1. 지도학습 : 예측값(Y)이 존재
#  X ~ Y
#  1) 회귀기반(Y가 연속형) : 수요예측, 부동산가격예측...
#  2) 분류기반(Y가 범주형) : 이탈예측, 생존여부예측, ... 

# 2. 비지도학습 : 예측값(Y)이 존재 X
#    군집분석, 연관분석...

# - 딥러닝
# 인공지능 > 머신러닝 > 딥러닝
# 신경망 구조의 머신러닝 기법을 딥러닝이라 표현
# 주로 비정형 데이터의 분석시 사용
 


# [ 분류 분석 ]
# - 지도학습의 일부(Y가 존재)
# - Y가 범주형인 경우
# - 대표적 모델로 트리기반 모델, 거리기반 모델, 확률통계기반 모델 존재

# 1. 분류분석 과정
# - 데이터 수집(Y의 분류에 영향을 미칠것 같은 X들을 수집)
# - 전처리(이상치/결측치 제거 및 수정)
# - 모델 선택(데이터에 맞게)
# - 데이터 분리(train/test)
# - 모델 학습(train data set)
# - 모델 평가(test data set)  --- ****
# - 모델 튜닝
# - 결과 해석 및 실무 적용

# 1) Decision Tree(트리기반 모델)
# - 분류 분석을 수행하는 트리기반 모델의 가장 시초 모델
# - 패턴 학습이 단순하여 패턴을 시각화할 수 있음
# - 패턴이 Tree구조를 띔
# - 비통계적 모델이므로 모델 해석이 매우 용이
# - 단일 의사결정이므로 예측력이 불안하거나 과대적합 가능성 있음

# [ 실습 : iris data의 Decision Tree 모델 적용 ]

install.packages('rpart')
library(rpart)

# 1. 모델 학습(정답이 있는 데이터)
rpart(formula = ,   # Y ~ X
      data = )      # data

iris_dt1 <- rpart(Species ~ . , data=iris)

# 2. 모델 확인(패턴, 모델 학습 결과)
iris_dt1

# 3. 시각화
# 1) 기본 plot 함수
dev.new()
plot(iris_dt1, compress = T)
text(iris_dt1, cex = 0.8)

# 2) 외부 패키지 활용
install.packages('rpart.plot')
library(rpart.plot)

dev.new()
prp(iris_dt1,
    type = 4,    # 그래프 출력 모양
    extra = 2,   # 추가 정보
    digits = 3)  # 출력 숫자의 자리수


# 4. 모델 평가
# 실제 정답을 알고 있는 평가용 데이터셋의 X들만 모델에 학습,
# 리턴되는 예측값(Y, predict value, fitted value)과 실제값을 비교하여
# 총 몇 건중 몇 건을 맞췄느냐로 평가점수를 계산

v_pre <- predict(iris_dt1,          # 예측값을 얻고자 하는 모델
                 newdata = iris,    # X값들만 input, Y는 무시됨
                 type = 'class')    # Y의 출력 형태(생략시 확률)

sum(iris$Species == v_pre) / nrow(iris) * 100

# 5. 실제 예측
new_data <- data.frame(Sepal.Length= 6.1,
                       Sepal.Width=4.0,
                       Petal.Length=2.3,
                       Petal.Width=0.5)

predict(iris_dt1, newdata = new_data)
predict(iris_dt1, newdata = new_data, type = 'class')




# [ 연습 문제 ] 
# iris data set을 train/test set으로 분리한 뒤
# train으로 학습, test로 평가 후 test set의 평가점수 확인

# 1. data set 분리
v_rn <- sample(1:nrow(iris), size = nrow(iris)*0.7)
iris_train <- iris[v_rn, ]
iris_test  <- iris[-v_rn, ]

# 2. 모델 학습
iris_dt2 <- rpart(Species ~ ., data = iris_train)

# 3. 모델 평가
v_pre2 <- predict(iris_dt2, newdata = iris_test, type = 'class')
sum(iris_test$Species == v_pre2) / nrow(iris_test)

