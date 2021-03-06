# [ 연습 문제 ]
# 각 사건의 성공확률이 0.1인 게임을 수행하고자 한다.
# 10번의 기회 중 5번 이상을 성공하면 1000만원을 준다고 한다.
# 상금을 획득할 확률과 그 구간을 시각화 하여 표현하여라.
X ~ B(n,p)
X ~ B(10,0.1)

P(X>=5) = P(X=5) + P(X=6) + P(X=7) + ... + P(X=10)
= sum(dbinom(5:10,10,0.1))

= 1 - P(X<=4)
= 1 - pbinom(4,10,0.1)
= 0.001634937


dev.new()
y1 <- dbinom(0:10,10,0.1)
plot(0:10, y1, type = 'l')

abline(h=0)
abline(v=5)

f_binom(x,n,p)
v_x <- c(5,5:10,10)
v_y <- c(0,dbinom(5:10,10,0.1),0)

polygon(v_x, v_y, col='red')

# [ 연습 문제 ]
# 각 사건의 성공확률이 0.3인 게임에서 총 20번 반복한다고 할때
# 이길 확률이 75% 이상이 되려면 최소 몇회 이상 성공해야 하는지 횟수와
# (몇 회 이상일 경우의 확률이 75%를 만족하는가)
# 그 구간을 시각화하여 표현하여라.

X ~ B(20,0.3)

P(X>=x) = 0.75 이상
P(X<x)  = 0.25 이하

dev.new()
y2 <- dbinom(0:20, 20, 0.3)
plot(0:20, y2, type = 'l')

P(X<=x)  = 0.25 이하
qbinom(0.25, 20, 0.3)

P(X<=5) = pbinom(5,20,0.3) = 0.41 => P(X>=6) = 1-0.41 = 0.59 
P(X<=4) = pbinom(4,20,0.3) = 0.23 => P(X>=5) = 1-0.23 = 0.77



# 정규 분포
# - 분포의 모양이 종모양을 갖춘, 평균 기준 좌우 대칭의 형태
# - 확률변수의 대부분의 가정이 되는 분포
# - 모수 : 평균과 표준편차
# - X ~ N(mu, sigma^2)

# 정규 분포 곡선 시각화
dev.new()
x1 <- seq(-3,3,0.01)
y1 <- dnorm(x1, mean=0, sd=1)
y2 <- dnorm(x1, mean=0, sd=2)
y3 <- dnorm(x1, mean=0, sd=0.1)
y4 <- dnorm(x1, mean=1, sd=1)

plot(x1, y1, type = 'l', col = 2, xlim = c(-3,6), ylim = c(0,5))
lines(x1, y2, type = 'l', col = 3)
lines(x1, y3, type = 'l', col = 4)
lines(x1, y4, type = 'l', col = 5)


# 표준 정규 분포
# - 정규분포를 따르는 확률변수 X를 표준화 시킨 변수 Z이 따르는 분포
# - 평균이 0, 표준편차가 1인 정규분포
# - X ~ N(mu, sigma^2)  => Z ~ N(0,1)
# - Z* = (X - mu)/sigma


# 표준 정규 분포에서의 95% 구간 찾기
Z ~ N(0,1)
P(-1.96 <= Z <= 1.96) = 0.95
P(Z <= -1.96) = 0.025

pnorm(-1.96,0,1) = 0.0249979
qnorm(0.025, mean=0, sd=1)  = -1.959964


# 표준 정규 분포 시각화
dev.new()
y_z <- dnorm(x1, mean=0, sd=1)

plot(x1, y_z, type = 'l')
abline(v=-1.96)
abline(v=1.96)

v_x <- c(-1.96, seq(-1.96,1.96,0.01), 1.96)
v_y <- c(0, dnorm(seq(-1.96,1.96,0.01), mean=0, sd=1), 0)

polygon(v_x, v_y, density = 30)
text(0,0.2,'P(-1.96<=Z<=1.96)=95%', col='red', cex=3)


# [ 표본과 모집단의 관계 ]
# 1. 표본평균(xbar)과 모평균의 관계
# 모집단으로부터 추출된 표본의 평균의 평균을 구하면
# 모집단의 평균(모평균)에 근사해짐

# 2. 표본평균의 표준편차와 모표준편차의 관계
# 모집단으로부터 추출된 표본들의 평균의 표준편차를 구하면
# 모집단의 표준편차에 표본의 크기를 나눈 값과 근사해짐

# 3. 표본의 분포
# 표본평균의 분포는 정규분포를 따른다******


# [ 증명 ]
# 1. 표본평균(xbar)과 모평균의 관계
# E(xbar) = mean(xbar) = mu  
  
# 모집단을 구할 수 없으므로
# 알려져있는 모집단(표준정규분포)으로부터 랜덤 샘플링 된 데이터로 증명

# 1) 표준정규분포를 따르는 모집단으로부터 샘플 사이즈가 각각
# 10, 50, 100인 샘플을 각각 1000번 추출
  
v10  <- rnorm(10, mean=0, sd=1)  
v50  <- rnorm(50, mean=0, sd=1)  
v100 <- rnorm(100, mean=0, sd=1)  

# 2) 한 번 샘플링 된 샘플의 평균(하나의 표본 평균)
mean(v10)   # 0.2086971
mean(v50)   # -0.03317162
mean(v100)  # 0.1554989

# => 한 번 샘플링 된 결과로 얻은 표본평균이
#    모평균에 근사하다라는 결론은 다소 무리

# 3) 각각 1000번 반복 샘플링 후 표본평균의 평균 확인

mean_v10 <- c() ; mean_v50 <- c() ; mean_v100 <- c()

for(i in 1:1000) {
  mean_v10[i]  <- mean(rnorm(10, mean=0, sd=1))
  mean_v50[i]  <- mean(rnorm(50, mean=0, sd=1))  
  mean_v100[i] <- mean(rnorm(100, mean=0, sd=1))
}

mean(mean_v10)   # -0.00262729
mean(mean_v50)   # 0.01173143
mean(mean_v100)  # 0.002241618

# => 반복 추출된 표본평균으로부터 모집단의 평균과 근사함을
#    확인하는 과정은 의미가 있어보임

# => 표본 평균의 평균은 모평균에 근사해진다


# [ 연습 문제 ]
# X ~ B(100, 0.5)를 따르는 모집단으로부터
# 샘플 사이즈가 각각 10, 50, 100인 샘플을 총 1000번 추출하여
# 표본평균이 갖는 분포를 확인(표본 평균의 평균이 모평균에 근사하는지)

# 단, E(X) = n * p, var(X) = n * p * (1-p) 라 가정

rbinom(10, size = 100, prob = 0.5)
rbinom(50, size = 100, prob = 0.5)
rbinom(100, size = 100, prob = 0.5)

