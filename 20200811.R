# 4. �� Ʃ��
iris_dt2$control

# 1) minsplit = minbucket
#    minbucket = round(minsplit / 3)
# - �߰� ����ġ�� �Ű�����
# - ���з� ���� > minbucket �� ��� �߰� ����ġ�� ����
# - minbucket�� ���� ���з������� �۾��������� ����ġ�� ��� �õ�
# - ������ �߰��� ����ġ���� �з� ������ ���̻� ���°�� stop
# - minbucket���� �������� �� ������ ���� ���� Ȯ�� ������
# - minbucket���� �������� ���� �������� overfit ���� �߻� Ȯ�� ����


# ����) minbucket���� ��ȭ�� ���� ���� �������� ��ȭ Ȯ��
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

# �������� : ���ο� �����Ϳ����� �������� ����, ������ �����Ϳ����� �������� ������
# ��� : overfit(��������)�� �߻����� �����鼭 test score�� ����
# �Ű������� ����

# 2) maxdepth 
# - �� Ʈ���� ����� �б� �� ���������� ���� Ƚ�� ����
# - maxdepth ���� �������� �ܼ��� Ʈ�������� ���� Ȯ�� ������

# [ ���� : minbucket�� �۾����� ���� �߰� split ���� Ȯ�� ]
iris_dt2 <- rpart(Species ~ ., data = iris_train,
                  control = rpart.control(minbucket = 2))
dev.new()
rpart.plot(iris_dt2, type = 4)

# 3) cp
# - ���������� Ʈ���� size�� �����ϴ� �Ű�����
# - �� �Ʒ� �� cptable�� cp���� tree size, error�� ���ϸ鼭
#   ������ cp������ Ʃ��
# - cp���� �ð�ȭ�ϸ� ���� ���� error�� �Բ� ������ cp�� ã�� ����

# cptable : �� split���������� ǥ�ؿ��� Ȯ��
# nsplit + 1 = tree size
iris_dt2$cptable  

# plotcp : cp�� �ð�ȭ(���� ���� Ȯ��)
dev.new()
plotcp(iris_dt2)

# rpart.plot : ���� �ð�ȭ
dev.new()
rpart.plot(iris_dt2, type = 4)


# [ ���� ���� : cancer data�� ������ ��/�Ǽ� ����(Decision Tree) ]

# 1. data loading
cancer <- read.csv('cancer.csv', stringsAsFactors = F)
cancer$id <- NULL

# 2. ���� ������ �ð�ȭ
# ������ ��/�Ǽ��� ������ ��ġ�� �ֿ� ���� Ȯ��
dev.new()
cancer$diagnosis <- as.factor(cancer$diagnosis)
plot(cancer[, 2:10], col= cancer$diagnosis)

# 3. sampling
v_rn <- sample(1:nrow(cancer), size = nrow(cancer) * 0.7)
cancer_tr <- cancer[v_rn, ]
cancer_te <- cancer[-v_rn, ]

# 4. modeling
cancer_dt1 <- rpart(diagnosis ~ ., data = cancer_tr)

# 5. �ð�ȭ
dev.new()
rpart.plot(cancer_dt1)

# 6. �� ��
pre_te <- predict(cancer_dt1, newdata = cancer_te, type = 'class')
sum(cancer_te$diagnosis == pre_te) / nrow(cancer_te) * 100

# 7. �� Ʃ��(minbucket)
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


# train score >>> test score  : ��������
# train score <<< test score  : ��������



# ���ο� �����Ϳ� ���� ����
new_data <- cancer[10, -1] + 0.01
predict(cancer_dt1, newdata = new_data, type = 'class')


# [ Ʈ�� ��� ���� ���� ���� ��� ]
# - Ʈ�� ���� �� ������ �߿䵵�� �ڽ� ����� �Ҽ����� ���� ���
# - �߿䵵�� ���� ������ ���� split �������� ���
# - ������ �߿䵵 ���̰� ū ��� Ư�� ������ ���� ���ɼ� ����
# - Ʈ�� ���� ���� �߿䵵�� �м��� �߿� ���� �ľ��� �����ϰ� ���

# �Ҽ���
# - Ư�� �������� ���� �и��� �ڽĳ���� Ŭ������ ȥ�� ����
# - �ַ� ���ϰ���� ����
# - 2���� class�� ���°�� f(p) = p(1-p)�� ���

# ����) 
# - AAAAAA : p(A�� ���� Ȯ��) = 1, f(p) = 1 * (1-1) = 0
# - AAAABB : p = 4/6, f(p) = 4/6 * 2/6 = 0.22
# - AAABBB : p = 1/2, f(p) = 1/2 * 1/2 = 0.25

# decision tree���� ���� �߿䵵 Ȯ��
iris_dt2$variable.importance

# Petal.Width Petal.Length Sepal.Length  Sepal.Width 
# 62.64908     61.22238     39.67210     26.34410 


# 2) ���Ǻ� �߷� ����
# - ���� decision tree�� ����
# - �з� ���ؿ� ���� ���ռ��� ��������� �ؼ��ϴ� ��ġ �߰�
#   (������ ���Ǽ� ����)

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
# - Decision Tree�� �������� ������ �ذ��ϱ� ���� �������� ���� �ٸ�
#   ����� tree�� ����, �����Ͽ� ���� ����� ���� ���
# - Random Forest Classifier�� Random Forest Regressor ����
# - �з����� ���� �ټ����, ȸ�͸��� ���� ������� �������

install.packages('randomForest')
library(randomForest)

# 1. ������ ���� �� sampling
v_rn <- sample(1:nrow(iris), size = nrow(iris)*0.7)

iris_tr <-iris[v_rn, ]
iris_te <-iris[-v_rn, ]

# 2. modeling
randomForest(fomular = ,
             data = )

iris_rf1 <- randomForest(Species ~ . , iris_tr)

# 3. ��
pre_te <- predict(iris_rf1, newdata = iris_te, type='class')
sum(iris_te$Species == pre_te) / nrow(iris_te) 

iris_rf1$importance

# 4. ����
new_data <- data.frame(Sepal.Length=6, 
                       Sepal.Width=0.1, 
                       Petal.Length=5, 
                       Petal.Width=2)

predict(iris_rf1, newdata = new_data, type='class')

# 5. �Ű����� Ʃ��
# 5-1) ntree : Ʈ���� ����(�⺻ 500��)
randomForest( ~, , ntree=10)
help(randomForest)

# [ �������� : ntree�� elbow point Ȯ�� ]
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
     main='tree�� ��ȭ�� ���� �������� ��ȭ')

score_te

# 5-2) mtry 
# - �� split�� �����Ǿ����� ���������� �ĺ��� ����
# - ���� �ٸ� Ʈ���� �����ϱ� ���� ���� �Ű����� ����
# - ���� �������� ���� �ٸ� Ʈ���� ����

# mtry=1 => �Ź� �����ϰ� ���õ� ���� ���� => ���� �ٸ� Ʈ�� ����
#        => Ʈ���� ���������ų� �������� ������ Ȯ�� ����
#   
# mtry=4 => �Ȱ��� �������� ���� => �Ȱ��� Ʈ��(�н��Ǵ� ������ ���� ����)

# [ �ǽ� : mtry ��ȭ�� ���� �����°� �������� ���� Ȯ�� ]
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
     main='mtry ��ȭ�� ���� �������� ��ȭ')
lines(1:4, score_te, type='o', col = 'red')


# Random Forest������ ȸ��
data('airquality')
airquality

randomForest(Ozone ~ . , airquality,
             mtry = 3, ntree=100, na.action = na.omit )

