# [ 표본과 모집단의 관계 ]
# 1. 표본평균(xbar)과 모평균의 관계
모집단으로부터 추출된 표본의 평균의 평균을 구하면
모집단의 평균(모평균)에 근사해짐

# 2. 표본평균의 표준편차와 모표준편차의 관계
모집단으로부터 추출된 표본들의 평균의 표준편차를 구하면
모집단의 표준편차에 표본의 크기를 나눈 값과 근사해짐

# 3. 표본의 분포
표본평균의 분포는 정규분포를 따른다****

xbar ~ N(mu, sigma^2/n)
    
# [ 증명 - 모집단이 정규분포를 따르는 경우 ]
# 1. 표본평균(xbar)과 모평균의 관계
# 2. 표본평균의 표준편차와 모표준편차의 관계

mean_v10 <- c() ; mean_v50 <- c() ; mean_v100 <- c()
  
for(i in 1:1000) {
  mean_v10[i]  <- mean(rnorm(10, mean=0, sd=1))
  mean_v50[i]  <- mean(rnorm(50, mean=0, sd=1))  
  mean_v100[i] <- mean(rnorm(100, mean=0, sd=1))
}
  
# 2-1) 표본평균의 표준편차 구하기  
sd(mean_v10)   # 0.3189538
sd(mean_v50)   # 0.1401534
sd(mean_v100)  # 0.09891253

# 2-2) 모표준편차(1)와 비교  
E(xbar) = mu
sd(xbar) = sigma / sqrt(n)

sd(mean_v10) = 1 / sqrt(10)     # 0.3189538 == 0.3162278 (근사)
sd(mean_v50) = 1 / sqrt(50)     # 0.1401534 == 0.1414214 (근사)
sd(mean_v100) = 1 / sqrt(100)   # 0.09891253 == 0.1 (근사)


# [ 결론 ]
# 표본평균의 분산을 구하면 모평균인 sigma^2 에 n(sample size)을 나눈값에
# 근사해진다
# 표본평균의 표준편차를 구하면 모표준편차인 sigma에 sqrt(n)을 나눈값에
# 근사해진다

# 3. 표본의 분포
# 표본평균은 평균이 모평균과 같고 분산이 모분산(sigma^2)을 n으로 나눈값을
# 갖는 정규분포를 따른다

# 1) 표본평균의 확률분포 시각화
dev.new()
par(mfrow=c(1,3))

hist(mean_v10)   # sample size가 10인 1000개의 표본평균이 갖는 분포
hist(mean_v50)   # sample size가 10인 1000개의 표본평균이 갖는 분포
hist(mean_v100)  # sample size가 10인 1000개의 표본평균이 갖는 분포

# 2) 표본평균의 이론적 분포(xbar ~ N(mu, sigma^2/n))
mean_v10 ~  N(0, 1/10)
mean_v50 ~  N(0, 1/50)
mean_v100 ~  N(0, 1/100)

# 3) 위 분포 시각화
x1 <- seq(-1,1,0.01)
y10  <- dnorm(x1, mean=0, sd=1/sqrt(10))
y50  <- dnorm(x1, mean=0, sd=1/sqrt(50))
y100 <- dnorm(x1, mean=0, sd=1/sqrt(100))

dev.new()
par(mfrow=c(1,3))
plot(x1,y10, type='l',main='n=10인 표본평균의 분포', cex = 3)
plot(x1,y50, type='l',main='n=50인 표본평균의 분포', cex = 3)
plot(x1,y100,type='l',main='n=100인 표본평균의 분포', cex = 3)

# 4) 표본평균의 실제 분포와 이론적 분포 비교
dev.new()
par(mfrow=c(1,3))

# 4-1) sample size 10인 표본의 분포
hist(mean_v10, prob = T)  
lines(x1,y10,type='l',col='red')

# 4-2) sample size 50인 표본의 분포
hist(mean_v50, prob = T, ylim = c(0,3))  
lines(x1,y50,type='l',col='red')

# 4-1) sample size 100인 표본의 분포
hist(mean_v100, prob = T)  
lines(x1,y100,type='l',col='red')

# [ 결론 ]
# 평균이 mu, 표준편차가 sigma인 정규분포를 따르는 표본으로부터 얻은
# 표본평균의 분포는 평균이 mu, 표준편차가 sigma/sqrt(n)인 정규분포를
# 따른다


# =========================================================

# 모집단이 정규분포가 아닌 경우의 증명과정(이항분포)

# X ~ B(100, 0.5)를 따르는 모집단으로부터
# 샘플 사이즈가 각각 10, 50, 100인 샘플을 총 1000번 추출하여
# 표본평균이 갖는 분포를 확인(표본 평균의 평균이 모평균에 근사하는지)

# 단, E(X) = n * p, var(X) = n * p * (1-p) 라 가정


# 1. 표본평균(xbar)과 모평균의 관계
mean_10 <- c() ; mean_50 <- c() ; mean_100 <- c()

for (i in 1:1000) {
  mean_10[i] <- mean(rbinom(10, size = 100, prob = 0.5))
  mean_50[i] <- mean(rbinom(50, size = 100, prob = 0.5))
  mean_100[i] <- mean(rbinom(100, size = 100, prob = 0.5))
}

mean(mean_10)   # 50.0289
mean(mean_50)   # 49.99752
mean(mean_100)  # 50.0389

# 2. 표본평균의 표준편차와 모표준편차의 관계
sd(xbar) == sigma/sqrt(n)
sigma(모표준편차) = sqrt(npq) = sqrt(100*0.5*0.5) = 5

# 3. 표본의 분포
