# [ 과제풀이 - 신약 개발 가설 검정 ]
v1 <- c(9,9,9,9,9,9,9,9,9,9,10,8,10,8,10,8,11,7,11,7,11,7,11,7,9)
length(v1)
mean(v1)
library(BSDA)

z.test(v1, alternative = 'less', mu = 10, sigma.x = 3)

H0 : mu >= 10
H1 : mu < 10

data:  v1
z = -1.6667, p-value = 0.04779
alternative hypothesis: true mean is less than 10
95 percent confidence interval:
  NA 9.986912
sample estimates:
  mean of x 
9 

# # 가설 검정과 신뢰구간
# 1. 양측 검정
# 1) 표준화된 확률변수 Z(검정통계량)의 채택역
# [-1.96, 1.96]   # 유의수준 5%
 
# 2) 모평균의 신뢰구간
P(-1.96<= Z <= 1.96) = 0.95
-1.96 <= (xbar - mu)/(sigma/sqrt(n)) <= 1.96

-1.96*sigma/sqrt(n) <= xbar - mu <= 1.96 * sigma/sqrt(n)
-xbar -1.96*sigma/sqrt(n) <= -mu <= -xbar + 1.96 * sigma/sqrt(n)

xbar - 1.96 * sigma/sqrt(n) <= mu <= xbar +1.96*sigma/sqrt(n)

[xbar - 1.96 * sigma/sqrt(n), xbar +1.96*sigma/sqrt(n)] 

# 3) 유의 확률 : P(|Z| > Z*)

# 2. 왼쪽 검정
# 1) 표준화된 확률변수 Z(검정통계량)의 채택역
# [-1.64, INF ]
qnorm(0.05,0,1)    # -1.644854

# 2) 모평균의 신뢰구간
P(Z >= -1.64) = 0.95
Z >= -1.64
(xbar - mu)/(sigma/sqrt(n)) >= -1.64
(xbar - mu) >= -1.64 * sigma/sqrt(n)
-mu >= -xbar -1.64 * sigma/sqrt(n)
mu <= xbar + 1.64 * sigma/sqrt(n)

# 3) 유의 확률 : P(Z < Z*)
 
# 3. 오른쪽 검정
# 1) 표준화된 확률변수 Z(검정통계량)의 채택역
# [INF, 1.64 ]

# 2) 모평균의 신뢰구간
# 3) 유의 확률 : P(Z > Z*)



# [ 연습 문제 ] A사 K모델 자동차의 연비는 평균 12.5(km/l),
# 표준편차 0.5(km/l)로 알려져 있는데, 새로 개발된 엔진을 장착한
# 40대의 자동차 연비를 측정한 결과 표본평균이 12.64(km/l)로 나왔다.
# 기존 연비보다 개선되었는지 여부를 신뢰구간을 통한 가설 검정
# H0 : mu <= 12.5  # 영가설, 귀무가설
# H1 : mu > 12.5   # 대립가설, 대안가설

# 오른쪽 검정
P(Z<=1.64) = 0.95
(xbar - mu) / (sigma/sqrt(n)) <= 1.64
xbar - mu <= 1.64 * sigma/sqrt(n)
-mu <= -xbar + 1.64 * sigma/sqrt(n)
mu >= xbar - 1.64 * sigma/sqrt(n)

[ xbar - 1.64 * sigma/sqrt(n) , INF ]
[ 12.51035, INF ] , mu = 12.5가 신뢰구간에 포함되어 있지 않으므로
H0 기각!!

xbar <- 12.64
mu <- 12.5
n <- 40
sigma <- 0.5




[ Z 분포와 T 분포의 관계 ]
중심극한 정리에 의하여 표본의 크기가 어느 정도 큰 경우라면,
모집단의 분포와 상관없이
모집단으로부터 랜덤 샘플링된 표본의 표본평균은 다음과 같은 분포를 따름

xbar ~ N(mu, (sigma/sqrt(n))^2)

따라서 이 이론에 의해
모집단의 평균을 추정할 수 있고, 
모평균에 대한 가설 검정을 수행할 수 있음 => Z test(Z 검정)

하지만, Z검정은 모집단의 분산(혹은 표준편차)을 알 수 있을때 가능,
모집단의 분산을 모르는 경우 표본의 분산으로 대체,
이 경우의 확률변수를 T라 부르며 자유도가 n-1인 T분포를 따른다

xbar ~ N(mu, (sigma/sqrt(n))^2)
Z ~ N(0,1)
(xbar - mu) / (sigma/sqrt(n)) ~ N(0,1)    # Z검정
(xbar - mu) / (s/sqrt(n)) ~ T(n-1)        # T검정


# (xbar - mu) / (s/sqrt(n)) ~ T(n-1) 

# 평균이 10, 분산이 4(표준편차가 2)인 정규분포를 따르는 모집단으로부터
# 1000개의 표본을 추출, 표본평균이 갖는 분포를 시각화

# 1. 난수 추출
vmean <- c() ; vsd <- c()

for (i in 1:1000) {
  v_norm <- rnorm(100, mean = 10, sd = 2)
  vmean  <- c(vmean, mean(v_norm))
  vsd    <- c(vsd, sd(v_norm))
}

# 2. 분포 시각화
1) 실제 분포
dev.new()
hist(vmean, probability = T)

# 2) sigma를 알고 있을 경우 이론적 분포(정규분포에 근사)
# 2-1) xbar ~ N(10, 2/sqrt(100))
vx1 <- seq(min(vmean), max(vmean), 0.01)
vy1 <- dnorm(vx1, mean = 10, sd = 2/sqrt(100))

lines(vx1, vy1, type = 'l', col = 'red')

2-2) (xbar - mu) / (sigma/sqrt(n)) ~ N(0, 1)


# 2) sigma를 모를 경우 이론적 분포(T분포에 근사)

(xbar - mu) / (s/sqrt(n)) ~ T(n-1)

vt <- (vmean - 10) / (vsd / sqrt(100))
dev.new()
hist(vt, probability = T)   # 표준화된 확률변수 T의 실제 분포

vx2 <- seq(min(vt), max(vt), 0.01)
vy2 <- dt(x=vx2, df = 100-1)

lines(vx2, vy2, type='l', col='red')



# [ 참고 : 자유도 변화에 따른 T분포의 시각화 ]
n1 <- 10
n2 <- 50
n3 <- 100
n4 <- 1000

x1   <- seq(-3, 3, 0.01)
y10  <- dt(x1, n1-1)
y50  <- dt(x1, n2-1)
y100 <- dt(x1, n3-1)
y1000 <- dt(x1, n4-1)

dev.new()
plot(x1,y10, type = 'l', col = 2)
lines(x1,y50, type = 'l', col = 3)
lines(x1,y100, type = 'l', col = 4)
lines(x1,y1000, type = 'l', col = 5)

# => 자유도가 작을수록 꼬리가 두껍고, 커질수록 꼬리가 얇으면서
#    중앙의 밀집도가 높아짐


