# [ ������ �м� ]
# - �����͸��̴� : �����ͷκ��� �ǹ��ִ� ����(����)�� ã�� �м� ���
  # ex) ����³� ��������ƽ�� ���䰡 �������� �߰�

# - �ӽŷ���(����н�) 
# ����ڰ� �����ͷκ��� ���� ������ ã�°� �ƴ�
# ���(��)�� �������� �н��� ���� ��Ģ�� ã���ִ� �м� ���


# 1. �����н� : ������(Y)�� ����
#  X ~ Y
#  1) ȸ�ͱ��(Y�� ������) : ���俹��, �ε��갡�ݿ���...
#  2) �з����(Y�� ������) : ��Ż����, �������ο���, ... 

# 2. �������н� : ������(Y)�� ���� X
#    �����м�, �����м�...

# - ������
# �ΰ����� > �ӽŷ��� > ������
# �Ű�� ������ �ӽŷ��� ����� �������̶� ǥ��
# �ַ� ������ �������� �м��� ���
 


# [ �з� �м� ]
# - �����н��� �Ϻ�(Y�� ����)
# - Y�� �������� ���
# - ��ǥ�� �𵨷� Ʈ����� ��, �Ÿ���� ��, Ȯ������� �� ����

# 1. �з��м� ����
# - ������ ����(Y�� �з��� ������ ��ĥ�� ���� X���� ����)
# - ��ó��(�̻�ġ/����ġ ���� �� ����)
# - �� ����(�����Ϳ� �°�)
# - ������ �и�(train/test)
# - �� �н�(train data set)
# - �� ��(test data set)  --- ****
# - �� Ʃ��
# - ��� �ؼ� �� �ǹ� ����

# 1) Decision Tree(Ʈ����� ��)
# - �з� �м��� �����ϴ� Ʈ����� ���� ���� ���� ��
# - ���� �н��� �ܼ��Ͽ� ������ �ð�ȭ�� �� ����
# - ������ Tree������ ��
# - ������� ���̹Ƿ� �� �ؼ��� �ſ� ����
# - ���� �ǻ�����̹Ƿ� �������� �Ҿ��ϰų� �������� ���ɼ� ����

# [ �ǽ� : iris data�� Decision Tree �� ���� ]

install.packages('rpart')
library(rpart)

# 1. �� �н�(������ �ִ� ������)
rpart(formula = ,   # Y ~ X
      data = )      # data

iris_dt1 <- rpart(Species ~ . , data=iris)

# 2. �� Ȯ��(����, �� �н� ���)
iris_dt1

# 3. �ð�ȭ
# 1) �⺻ plot �Լ�
dev.new()
plot(iris_dt1, compress = T)
text(iris_dt1, cex = 0.8)

# 2) �ܺ� ��Ű�� Ȱ��
install.packages('rpart.plot')
library(rpart.plot)

dev.new()
prp(iris_dt1,
    type = 4,    # �׷��� ��� ���
    extra = 2,   # �߰� ����
    digits = 3)  # ��� ������ �ڸ���


# 4. �� ��
# ���� ������ �˰� �ִ� �򰡿� �����ͼ��� X�鸸 �𵨿� �н�,
# ���ϵǴ� ������(Y, predict value, fitted value)�� �������� ���Ͽ�
# �� �� ���� �� ���� ������ķ� �������� ���

v_pre <- predict(iris_dt1,          # �������� ����� �ϴ� ��
                 newdata = iris,    # X���鸸 input, Y�� ���õ�
                 type = 'class')    # Y�� ��� ����(������ Ȯ��)

sum(iris$Species == v_pre) / nrow(iris) * 100

# 5. ���� ����
new_data <- data.frame(Sepal.Length= 6.1,
                       Sepal.Width=4.0,
                       Petal.Length=2.3,
                       Petal.Width=0.5)

predict(iris_dt1, newdata = new_data)
predict(iris_dt1, newdata = new_data, type = 'class')




# [ ���� ���� ] 
# iris data set�� train/test set���� �и��� ��
# train���� �н�, test�� �� �� test set�� ������ Ȯ��

# 1. data set �и�
v_rn <- sample(1:nrow(iris), size = nrow(iris)*0.7)
iris_train <- iris[v_rn, ]
iris_test  <- iris[-v_rn, ]

# 2. �� �н�
iris_dt2 <- rpart(Species ~ ., data = iris_train)

# 3. �� ��
v_pre2 <- predict(iris_dt2, newdata = iris_test, type = 'class')
sum(iris_test$Species == v_pre2) / nrow(iris_test)
