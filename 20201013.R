# 모집단이 정규분포가 아닌 경우의 증명과정(이항분포)

# X ~ B(100, 0.5)를 따르는 모집단으로부터
# 샘플 사이즈가 각각 10, 50, 100인 샘플을 총 1000번 추출하여
# 표본평균이 갖는 분포를 확인(표본 평균의 평균이 모평균에 근사하는지)

# 단, E(X) = n * p, var(X) = n * p * (1-p) 라 가정
E(X) = 100 * 0.5 = 50
var(X) = 100 * 0.5 * 0.5 = 25
sd(X) = sqrt(25) = 5


# 1. 표본평균(xbar)과 모평균의 관계
mean_10 <- c() ; mean_50 <- c() ; mean_100 <- c()

for (i in 1:1000) {
  mean_10[i] <- mean(rbinom(10, size = 100, prob = 0.5))
  mean_50[i] <- mean(rbinom(50, size = 100, prob = 0.5))
  mean_100[i] <- mean(rbinom(100, size = 100, prob = 0.5))
}

mean(mean_10)   # 50.0289 (모평균 : 50)
mean(mean_50)   # 49.99752
mean(mean_100)  # 50.0389

# 2. 표본평균의 표준편차와 모표준편차의 관계
var(xbar) = sigma^2 / n
sd(xbar) == sigma/sqrt(n)
sigma(모표준편차) = sqrt(npq) = sqrt(100*0.5*0.5) = 5
sd(mean_10)   # 1.56
sd(mean_50)   # 0.69
sd(mean_100)  # 0.49

# 3. 표본의 분포
1) sample size = 10
dev.new()
hist(mean_10, prob=T)            # 실제 분포

x10 <- seq(min(mean_10), max(mean_10), 0.01)
y10 <- dnorm(x10, mean=50, sd=5 / sqrt(10))
lines(x10, y10, type='o', col='red')
xbar ~ N(mu, (sigma/sqrt(n))^2)

2) sample size = 50
x50 <- seq(min(mean_50), max(mean_50), 0.01)
y50 <-dnorm(x50, mean=50, sd=5 / sqrt(50))
lines(x50, y50, type='o', col='red')
xbar ~ N(mu, (sigma/sqrt(n))^2)

3) sample size = 100
x100 <- seq(min(mean_100), max(mean_100), 0.01)
y100 <- dnorm(x100, mean=50, sd=5 / sqrt(100))
lines(x100, y100, type='o', col='red')
xbar ~ N(mu, (sigma/sqrt(n))^2)

########## 여기까지는 복습입니다. ##########

# 통계적 추론
전수조사를 통해 모집단의 특성(모평균, 모비율 등)을 파악할 수 없는 점을 
대신하여 일정 크기 이상의 표본을 추출,
표본으로 부터 모집단의 특성을 추정하는 과정
(중심극한정리에 의해 표본평균은 정규분포에 근사하므로
 정규분포 근사성을 이용한 통계적 추정 가능)

# 중심 극한 정리
통계적 추론시 필요한 가정으로
모집단의 분포와 상관없이 일정 이상의 크기를 갖는 표본의 경우
표본평균은 정규분포를 따른다는 가정

# 모평균의 추정
1. 점 추정 : 표본평균값으로부터 모평균 추정
표본을 추출할때마다 값이 달라지는 단점

2. 구간 추정 : 표본평균이 갖는 분포로부터 모평균의 포함 구간을 추정
95% 신뢰구간(유의수준 5%) : 정규분포 내 모평균을 포함한 95% 구간을 추정
99% 신뢰구간(유의수준 1%) : 정규분포 내 모평균을 포함한 99% 구간을 추정

xbar ~ N(mu, (sigma/sqrt(n))^2)
P(-1.96 <= Z <= 1.96) = 95%
P(-1.96 <= (xbar - mu) / sigma/sqrt(n) <= 1.96) = 95%
(모 표준편차를 알고 있다는 가정하 사용 가능)


# 예제) 우리 나라 2세 영아의 머리 둘레는 작년과 분산이 동일할 것으로 
# 확인(500). 한 번 추출한 샘플의 평균이 250일때(n=10) 
# 모평균의 95% 신뢰구간을 구하여라
1. 점 추정 : 250
2. 구간 추정
P(-1.96 <= (xbar - mu) / sigma/sqrt(n) <= 1.96) = 95%

xbar <- 250
sigma <- sqrt(500)
n <- 10

c(xbar - 1.96 * sigma/ sqrt(n) , xbar + 1.96 * sigma/ sqrt(n) )
[236.1407, 263.8593]

