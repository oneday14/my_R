# 1. subway2.csv 파일을 읽고
library(stringr)
df1 <- read.csv('subway2.csv ', stringsAsFactors = F, skip = 1)
head(df1)
str(df1)

# step1) 컬럼에 시간정보만 표현(X05.06 => 5)
colnames(df1)[-c(1,2)] <- as.numeric(str_sub(colnames(df1)[-c(1,2)], 
                                             2,3))

# step2) 승차/하차 데이터 분리
df1_1 <- df1[df1$구분=='승차', -2]
rownames(df1_1) <- df1_1$전체
df1_1 <- df1_1[,-1]

df1_2 <- df1[df1$구분=='하차', -2]
rownames(df1_2) <- rownames(df1_1)
df1_2 <- df1_2[,-1]

# 1) 역별 승차의 총 합
apply(df1_1, 1, sum)

# 2) 역별 하차의 총 합
apply(df1_2, 1, sum)

# 3) 시간대별 총 합
apply(df1[, -c(1,2)], 2, sum)

# 2.employment.csv 파일을 읽고
df2 <- read.csv('employment.csv', stringsAsFactors = F)

# 1) 년도별 총근로일수의 평균
# step1) 총근로일수 컬럼 선택
head(df2)
df2_1 <- df2[ , df2[1,] == '총근로일수 (일)']
df2_1 <- df2_1[-1,]
rownames(df2_1) <- df2$고용형태[-1]

# step2) '-'을 0으로 치환
str_replace_all(df2_1,'-','0')                   # 2차원 적용 불가
df2_1[,] <- apply(df2_1, c(1,2), str_replace_all, '-', '0')
df2_1[,] <- apply(df2_1, c(1,2), as.numeric)

# step3) 년도별 평균
apply(df2_1, 2, mean)


# 2) 고용형태별 월급여액 평균
# (전체근로자와 전체근로자(특수형태포함)가 같은 그룹이 되도록)
# step1) 월급여액 컬럼 선택
df2_2 <- df2[ , df2[1,] == '월급여액 (천원)']
df2_2 <- df2_2[-1,]
rownames(df2_2) <- df2$고용형태[-1]

# step2) 컬럼 정리(X2007.4 => 2007)
colnames(df2_2) <- str_sub(colnames(df2_2), 2,5)

# step3) 천단위 구분기호 삭제 및 숫자변경
f1 <- function(x) {
  as.numeric(str_remove_all(x, ','))
}

df2_2[,] <- apply(df2_2, c(1,2), f1)

# step4) 고용형태별 평균(행별 평균)
df2_2$total <- apply(df2_2, 1, mean)

# step5) 전체근로자와 전체근로자(특수형태포함)의 그룹평균 구하기
str_split(rownames(df2_2), '\\(')   # 각 층마다 원소 추출 불가

f2 <- function(x) {
  str_split(x, '\\(')[[1]][1]
}

vgroup <- sapply(rownames(df2_2), f2)

tapply(df2_2$total, vgroup, mean)


# ----------- 다른 풀이 : 컬럼값 + 첫번째 행 결합값으로 컬럼 수정
# step1) 컬럼 및 첫번째 행 값 정리
v1 <- str_sub(colnames(df2)[-1], 2, 5)

f3 <- function(x) {
  str_split(x, ' ')[[1]][1]
}

v2 <- sapply(df2[1,][-1], f3)

# step2) 가공된 값으로 컬럼 수정
colnames(df2)[-1] <- str_c(v1,v2,sep='_')
df2 <- df2[-1,]

# step3) '-'을 NA로 수정
f_na <- function(x) {
  if (x=='-') {
    NA
  } else {
    x
  }
}

df2[,] <- apply(df2, c(1,2), f_na)

# step4) 천단위 구분기호 제거 후 숫자 변경
df2[,-1] <- apply(df2[,-1], c(1,2), f1)

# step5) 월급여액 컬럼 선택
df2[, str_detect(colnames(df2), '월급여액')]



vbool <- str_detect(colnames(df3), '2007') & 
  str_detect(df3[1,], '월급여액')

df3[,vbool, drop=F]


# 참고 : f_shift 함수 생성
# f_shift(vector, n) : 빈문자열일 경우 n번째 이전값 가져오기
df1

f_shift <- function(vector, n=1) {
  v_vector <- vector
  for (i in 1:length(vector)) {
    if (v_vector[i] =='' | is.na(v_vector[i])) {
      v_vector[i] <- v_vector[i-n]
    }
  }
  return(v_vector)
}

f_shift(df1$전체)
v1 <- c('a','','','b','','')
f_shift(v1)


# zoo::na.locf : NA값을 이전 혹은 이후 값으로 치환

install.packages('zoo')
library(zoo)

v2 <- c(1,NA,2,NA,3)
na.locf(v2)                # 이전값(ffill)으로 치환
na.locf(v2, fromLast = T)  # 이후값(bfill)으로 치환

########## 여기까지는 복습입니다. ##########

# [ apply 실습 정리 ]
# 문제) 각 지점의 1분기 매출의 총 합
df1 <- read.csv('apply_test2.csv', stringsAsFactors = F)

# step1) '-', '.', '?' 0으로 치환
# sol1) str_replace_all + apply
str_replace_all(df1, '[-.?]', '0')  # 데이터프레임 적용 불가
apply(df1, c(1,2), str_replace_all, '[-.?]', '0')

# sol2) 사용자 정의 함수 + apply
f_rep <- function(x)  {
  if ((x=='-') | (x=='.') | (x=='?')) {     # x %in% c('-', '.', '?')
    '0'
  } else {
    x
  }
}

f_rep(0)
f_rep('-')
f_rep('?')

f_rep(df1)                  # 2차원 적용 불가
apply(df1, c(1,2), f_rep)   # 2차원 적용 가능, 치환 불가(앞뒤 공백)

df1[,] <- apply(df1, c(1,2), str_trim, 'both')
df1[,] <- apply(df1, c(1,2), f_rep)

# step2) 숫자컬럼 변경
f2 <- function(x) {
  as.numeric(str_remove_all(x, ','))
}

df1[,-1] <- apply(df1[,-1], c(1,2), f2)
apply(df1[,-1], 2, f2)
sapply(df1[,-1], f2)

# step3) 1분기 컬럼 추출
str_detect(colnames(df1), '1$')
df1_1 <- df1[ , str_detect(colnames(df1), '1$')]
rownames(df1_1) <- df1$name

# step4) 지점별 1분기 매출 총 합
apply(df1_1, 1, sum)

# 정렬
# 1. order(...,            # 정렬대상(벡터, 데이터프레임 불가)
#          na.last = T,    # NA 배치 순서
#          decreasing = T) # 정렬 순서

v1 <- c(1,10,2,9,4)
order(v1)                  # 위치값 리턴(1 3 5 4 2)
v1[order(v1)]              # 정렬결과(순서대로 행 재배치)

# 예제) emp 데이터에서 sal이 큰 순서대로 정렬
emp <- read.csv('emp.csv', stringsAsFactors = F)
order(emp, decreasing = T)
vord <- order(emp$SAL, decreasing = T)
emp[vord, ]

# 예제) emp 데이터에서 deptno가 작은순 정렬, 
# 같은 deptno내에서는 sal이 큰 순서대로 정렬
vord <- order(emp$DEPTNO, emp$SAL, decreasing = c(F,T))
emp[vord, ]

# 2. sort
# sort(x,               # 정렬대상
#      decreasing = ,   # 정렬순서
#      ...)             # 기타옵션

sort(v1)                # 정렬결과 직접 출력
sort(emp$SAL)           # sort의 결과로는 데이터프레임 정렬 불가


# 3. doBy::orderBy
install.packages('doBy')
library(doBy)

doBy::orderBy(formula = ,   # Y ~ X1 + X2 + ...
              data = )      # 정렬할 데이터프레임

# 예제) emp 데이터에서 sal이 낮은 순서대로 정렬
orderBy( ~ SAL, emp)

# 예제) emp 데이터에서 deptno, sal이 낮은 순서대로 정렬
orderBy( ~ DEPTNO + SAL, emp)

# 예제) emp 데이터에서 deptno는 낮은순, sal이 높은 순서대로 정렬
orderBy( ~ DEPTNO - SAL, emp)

# [ 연습 문제 ] 
# student.csv 파일을 읽고
# 남,여 순서대로 데이터를 정렬하고, 같은 성별내에서는 키가 높은순
std <- read.csv('student.csv', stringsAsFactors = F)

# step1) 성별컬럼 가공
std$g1 <- as.numeric(str_sub(std$JUMIN, 7,7))
std$g2 <- ifelse(std$g1==1,'M','F')
std$g3 <- ifelse(std$g1==1,'남자','여자')

# step2) order를 통한 정렬
vord1 <- order(std$g1, std$HEIGHT, decreasing = c(F, T))
std[vord1, ]

vord2 <- order(std$g2, std$HEIGHT, decreasing = c(F, T),
               method = 'radix')  # 문자, 숫자컬럼 각각 정렬순서 전달
std[vord2, ]

vord3 <- order(std$g3, std$HEIGHT, decreasing = c(F, T),
               method = 'radix')  # 한글 정렬 컬럼 사용 불가
std[vord3, ]

# step3) orderBy를 통한 정렬
orderBy( ~ g3 -HEIGHT, data = std)



# sampling
# 1. sample(x,          # 추출할 원본 데이터(벡터)
#           size = ,    # 추출 개수
#           replace = , # 복원 추출 여부
#           prob = )    # 추출 비율

sample(c(1,3,5,13,5,7), size = 1)
sample(c(1,3,5,13,5,7), size = 10)  # 모집단보다 더 큰 표본 추출 불가
sample(c(1,3,5,13,5,7), size = 10, 
       replace = T)                 # 모집단보다 더 큰 표본 추출 가능

sample(1:2, size=150, replace = T, prob = c(0.7, 0.3))

# [ 연습 문제 ]
# iris 데이터를 랜덤하게 샘플링 하여 각각 70%와 30% 데이터로 분리,
# df_train, df_test에 저장(랜덤하게 선택된 로우넘버로 풀이)

v_rn <- sample(1:nrow(iris), size = nrow(iris) * 0.7)
iris[v_rn, ]

df_train <- iris[v_rn, ]
df_test <- iris[-v_rn, ]

nrow(df_train)   # 105
nrow(df_test)    # 45

