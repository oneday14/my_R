- 유의수준 : 기각역 부분, H1을 채택할 확률

유의확률(p-value) < 유의수준 => H0기각
유의확률(p-value) > 유의수준 => H0선택

# 검정의 종류
# 1. 양측검정
# 1) 가설 형태
H0 : mu = 0
H1 : mu != 0

# 2) 유의확률
# P(|Z|>Z*)

# 3) 표준화된 신뢰구간
# [-1.96, 1.96]

# 4) 유의확률을 통한 가설 검정
P(|Z|>Z*) < 0.025(유의수준 5%일때) => H0 기각
P(|Z|>Z*) < 0.005(유의수준 1%일때) => H0 기각


# 2. 왼쪽 검정
# 1) 가설 형태
H0 : mu >= 0
H1 : mu < 0

# 2) 유의확률
# P(Z<Z*)

# 3) 표준화된 신뢰구간
# [-1.64, ...]

# 3. 오른쪽 검정
# 1) 가설 형태
H0 : mu >= 0
H1 : mu > 0

# 2) 유의확률
# P(Z>Z*)

# 3) 표준화된 신뢰구간
# [..., 1.64]

# [ 연습 문제 ]
# 여아 신생아 몸무게에 대한 가설 검정(유의수준 5%)
df1 <- read.table('여아신생아.txt')
v1 <- unlist(df1)
names(v1) <- NULL

# 1) 신뢰구간을 통한 가설 검정
# 2) 검정통계량을 통한 가설 검정
# 3) 유의 확률(p-value)을 통한 가설 검정
p-value = P(|Z|>Z*)  = 1 - p(Z<2.82) 
= 1 - pnorm(2.82, mean = 0, sd = 1)
= 0.002 <<< 0.025(5/2%)
=> H0 기각

# 4) Z검정을 통한 가설 검정
install.packages('BSDA')
library(BSDA)

z.test(x,
       y = NULL,
       alternative = 'two.sided', # 대립가설 채택 방향 greater, less
       mu,
       sigma.x = ,
       sigma.y = NULL,
       conf.level = 0.95)

z.test(v1, mu = 2800, sigma.x = 500)

One-sample z-Test

data:  v1
z = 2.8209, p-value = 0.004789    <<< 0.05
alternative hypothesis: true mean is not equal to 2800
95 percent confidence interval:
  2901.460 3363.428
sample estimates:
  mean of x 
3132.444 

# 양측검정일때 H0 기각 기준
p-value < 0.025
p-value * 2 < 0.05 (in z.test, t.test)


샘플링 한 표본평균 => 정규분포(mu, sigma^/n)
샘플링 한 표준화된 표본평균(Z) => 정규분포(0, 1)



# 연습) A사 K모델 자동차의 연비는 평균 12.5(km/l),
# 표준편차 0.5(km/l)로 알려져 있는데, 새로 개발된 엔진을 장착한
# 40대의 자동차 연비를 측정한 결과 표본평균이 12.64(km/l)로 나왔다.
# 기존 연비보다 개선되었는지 여부를 유의확률로 검정하여라
# H0 : mu <= 12.5  # 영가설, 귀무가설
# H1 : mu > 12.5   # 대립가설, 대안가설(오른쪽검정)

xbar <- 12.64
mu <- 12.5
sigma <- 0.5
n <- 40

z* = (xbar - mu) / (sigma/sqrt(n)) = 1.7708

표준화된 확률변수 Z의 95%신뢰구간(H0 채택역) : [..., 1.64]
위 구간안에 Z*(검정통계량)의 값이 포함되어 있지 않으므로
H0 기각!

p-value = P(Z > Z*)
        = P(Z > 1.7708)
        = 1 - P(Z < 1.7708)
        = 1 - pnorm(1.7708, mean = 0, sd = 1)
        = 0.03829698 < 0.05
        => H0 기각

