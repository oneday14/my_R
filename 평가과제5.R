# 1. 기존 치료법의 치료기간이 평균 10일, 표준편차가 3일인 정규분포를 따른다고 알려져 있다. 
# 새로운 치료법을 25명의 환자에게 적용해서 평균 9일, 표준편차가 3일인 성적을 얻었다. 
# 이 경우, 새로운 치료법의 효과를 인정할 수 있는가
xbar <- 9
mu <- 10
sigma <- 3
n <- 25

H0 : mu >= 10
H1 : mu < 10      # 왼쪽 검정


# 검정통계량을 통한 가설검정
Z|H0 = (xbar - mu)/(sigma/sqrt(n))     # -1.67

유의수준 5%라 할때 [-1.64, ...] 구간안에 
Z*(검정통계량)의 값이 포함되어 있지 않으므로 H0를 기각. (즉, H1을 채택)
따라서 새로운 치료법이 효과가 있다.

# 시각화
v_x <- seq(-3,3,0.01)
v_y <- dnorm(v_x,0,1)

dev.new()

plot(v_x,v_y,type = 'l')
abline(v=-1.96)
abline(v=1.96)

f <- function(x) {
  dnorm(x,0,1)
}

z <- (xbar - mu) / (sigma/sqrt(n))

arrows(z,0,z,f(z),length = 0, col = 'blue')
text(z, 0.12, "검정통계량", col = 'blue')

ld <- qnorm(0.05,0,1)
polygon(c(ld,seq(ld,-3,-0.01),-3),
        c(0,f(seq(ld,-3,-0.01)),0),col='red')

text(-1.64, 0, "-1.64")


# 2. 랜덤하게 샘플링한 초콜릿 무게의 값이 초콜릿.txt 파일에 있다. 모분산이 25.0라고 알려져 있을 때, 
# 해당 라인에서 생산된 초콜릿의 무게는 200이다 라는 가설에 대한 검정 수행
cho <- read.table('초콜릿.txt', header = F, stringsAsFactors = F)
cho <- unlist(cho)
names(cho) <- NULL

xbar <- mean(cho)
mu <- 200
sigma <- 25
n <- length(cho)

H0 : mu = 200
H1 : mu != 200

# 검정통계량을 통한 가설검정
Z|H0 = (xbar - mu)/(sigma/sqrt(n))     # -0.15

유의수준 5%라 할때 [-1.96, 1.96] 구간안에 
Z*(검정통계량)의 값이 포함되어 있으므로 H0를 기각하지 못함. (즉, H0을 채택)
따라서 초콜릿의 무게는 200이다

# 검토   
library(BSDA)
z.test(cho, mu = 200, sigma.x = 25)    # z = -0.15274으로 위와 동일, 
                                       # p-value = 0.8786이라 0.05보다 크므로 H0을 기각하지 못함.

# 시각화
v_x <- seq(-3,3,0.01)
v_y <- dnorm(v_x,0,1)

dev.new()

plot(v_x,v_y,type = 'l')
abline(v=-1.96)
abline(v=1.96)

f <- function(x) {
  dnorm(x,0,1)
}

z <- (xbar - mu) / (sigma/sqrt(n))

arrows(z,0,z,f(z),length = 0, col = 'blue')
text(z, 0.41, "검정통계량", col = 'blue')

ld <- qnorm(0.05/2,0,1)
lu <- qnorm(1-0.05/2,0,1)

polygon(c(ld,seq(ld,-3,-0.01),-3),
        c(0,f(seq(ld,-3,-0.01)),0),col='red')   # 왼쪽기각역
polygon(c(lu,seq(lu,3,0.01),3),
        c(0,f(seq(lu,3,0.01)),0),col='red')   # 왼쪽기각역
text(-1.96, 0, "-1.96")
text(1.96, 0, "1.96")

