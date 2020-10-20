# Z 검정과 T검정
xbar ~ N(mu, (sigma/sqrt(n))^)
Z = (xbar - mu)/(sigma/sqrt(n)) ~ N(0, 1)
T = (xbar - mu)/(s/sqrt(n))     ~ T(n-1)

# [ 연습 문제 : T-test]
# 랜덤하게 샘플링한 초콜릿 50개 무게의 측정 데이터가 다음과 같을 때,
# 모평균이 200과 다르다고 할 수 있는지 유의수준 5%에서 검정하시오.

# 1. data loading 및 필요 변수 설정
v1 <- read.table('초콜릿.txt')
v1 <- unlist(v1)
names(v1) <- NULL

v_xbar <- mean(v1)   # 표본 평균
v_s    <- sd(v1)     # 표본 표준편차 
v_n    <- length(v1) # 표본 크기
mu     <- 200        # H0가 참이라는 가정하의 모평균

# 2. 가설 설정
# H0 : mu  = 200
# H1 : mu != 200

# 3. T분포에서의 기각역과 채택역(유의수준5%)
qt(p=0.025,  # 누적확률이 5/2%가 되는 하한 임계값
   df=v_n-1)

qt(p=1-0.025,  # 누적확률이 1 - 5/2%가 되는 상한 임계값
   df=v_n-1)


# 자유도가 49인 T분포의 채택역 : [-2.009575, 2.009575]

# 4. 검정통계량 계산
T = (xbar - mu)/(s/sqrt(n))
T* = (v_xbar - mu)/(v_s/sqrt(v_n))   # -0.7740378

=> 채택역인 [-2.009575, 2.009575] 구간안에 검정통계량(-0.7740378)이
   포함되어 있으므로 H0 채택!!

# 5. 유의 확률 계산
P(|Z|>Z*)   # Z분포에서의 양측검정의 유의 확률
P(|T|>T*)   # T분포에서의 양측검정의 유의 확률

P(T < -0.7740378)
pt(-0.7740378,    # 누적확률을 계산할 임계값
   df=v_n-1)      # 0.22 >>> 0.025

=> 유의확률이 0.22로 양측검정에서의 유의수준 0.025보다 크므로
   H0가 유의하다 볼 수 있다. 즉, H0 채택!!

# 6. 신뢰구간
c(v_xbar -2.009575 * v_s/sqrt(v_n),  v_xbar + 2.009575 * v_s/sqrt(v_n))    

[198.058, 200.862]

=> mu=200이라는 H0는 5%의 오차확률로 추정된 모평균의 신뢰구간인 
   [198.058, 200.862] 안에 포함되어 있으므로 H0를 채택!!
  
  
# 7. T.test
help(t.test  )
t.test(x, 
       y = NULL,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, 
       conf.level = 0.95)
  
t.test(v1, alternative = "two.sided", mu = 200)
# -----------------------------------------------------
One Sample t-test

data:  v1
t = -0.77404, df = 49, p-value = 0.4426 >>> 0.05

alternative hypothesis: true mean is not equal to 200

95 percent confidence interval:
  198.058 200.862

sample estimates:
  mean of x 
199.46 
# ------------------------------------------------------

# [ 연습문제 ]
# 여아신생아.txt 파일을 읽고 신생아 몸무게가 2800보다
# 클것이다라는 가설에 대한 가설 검정 수행

# 1. data loading 및 필요 변수 설정
v1 <- read.table('여아신생아.txt')
v1 <- unlist(v1)
names(v1) <- NULL  

xbar <- mean(v1)  
s    <- sd(v1)
n    <- length(v1)
mu   <- 2800   

# 2. 가설 설정
# H0 : mu <= 2800
# H1 : mu >  2800  (오른쪽 검정)

# 3. T분포에서의 기각역과 채택역(유의수준5%)  
qt(1-0.05, df=n-1)     # 1.739607

# 자유도가 17인 T분포에서의 채택역 [ INF, 1.739607 ]

# 4. 검정통계량 계산
T* = (xbar - mu)/(s/sqrt(n))    # 2.233188
 
# 위에서 구한 채택역 [ INF, 1.739607 ]에 검정통계량이 포함되어 있지
# 않으므로 H0 기각!!
  
# 5. 유의 확률 계산
P(T>T*) = P(T>2.233188) = 1 - P(T < 2.233188)
                        = 1 - pt(2.233188, df=n-1)
                        = 0.01963421

# 6. 신뢰구간
P(T < 1.739607) = 0.95
P((xbar - mu)/(s/sqrt(n)) < 1.739607) = 0.95
(xbar - mu)/(s/sqrt(n)) < 1.739607
(xbar - mu) < 1.739607 * (s/sqrt(n))
-mu < -xbar + 1.739607 * (s/sqrt(n))
mu > xbar - 1.739607 * (s/sqrt(n))        # [2873.477,INF]

모평균의 95% 신뢰구간인 [2873.477,INF]에
H0의 mu인 2800이 포함되어 있지 않으므로 H0 기각!!

# 7. t.test      

t.test(v1, alternative = 'greater', mu = 2800)  
# -----------------------------------------------------
One Sample t-test

data:  v1
t = 2.2332, df = 17, p-value = 0.01963

alternative hypothesis: true mean is greater than 2800

95 percent confidence interval:
  2873.477      Inf

sample estimates:
  mean of x 
3132.444

# -----------------------------------------------------


