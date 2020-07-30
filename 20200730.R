# 1. data2 데이터를 읽고 
df1 <- read.csv('data2.csv', stringsAsFactors = F)
library(stringr)

# 1) 다음과 같이 노선별 승하차의 총 합을 표현
#     line_1  line_2  line_3  line_4
#      XXXXX   XXXXX   XXXXX   XXXXX

df1$승차 <- as.numeric(str_remove_all(df1$승차, ','))
df1$하차 <- as.numeric(str_remove_all(df1$하차, ','))

f1 <- function(x) {
  as.numeric(str_remove_all(x, ','))
}

df1[,c(3,4)] <- apply(df1[,c(3,4)], c(1,2), f1)

tapply(df1$승차 + df1$하차, df1$노선번호, sum, na.rm=T)

# [ 참고 - 평균 계산 컬럼의 치환값 필요시 ]
mean(c(1,2,3,'-'))           # '-'값의 0 또는 NA로 치환 필요
mean(c(1,2,3,0))             # 4개의 평균
mean(c(1,2,3,NA), na.rm=T)   # 3개의 평균

str_replace_all(c(1,2,3,'-'), '-', '0')
str_replace_all(c(1,2,3,'-'), '-', NA)
str_replace_na(NA, 0)
ifelse(c(1,2,3,'-')=='-', NA, c(1,2,3,'-'))


# 2) 오전 오후의 승하차의 총 합을 표현
# 오전 오후
# XXXX XXXXX

vtime <- as.numeric(str_sub(str_pad(df1$시간, 4, 'left', 0), 1, 2))
vtime2 <- ifelse(vtime < 12, '오전','오후')

tapply(df1$승차 + df1$하차, vtime2, sum)

# 2. emp2.csv 데이터를 읽고 
df2 <- read.csv('emp2.csv', stringsAsFactors = F)

# 1) 현재 직급이 없는 직원은 사원으로 치환
# sol1) '-'를 NA로 불러온 후 사원으로 치환
df2 <- read.csv('emp2.csv', stringsAsFactors = F, na.strings = '-')
str_replace_na(df2$POSITION, '사원')   # sql nvl함수와 비슷

# sol2) '-'를 사원으로 직접 치환(선택 베이스)
df2 <- read.csv('emp2.csv', stringsAsFactors = F)
df2$POSITION[df2$POSITION=='-'] <- '사원'

# sol3) '-'를 사원으로 직접 치환(ifelse)
df2 <- read.csv('emp2.csv', stringsAsFactors = F)
ifelse(df2$POSITION=='-', '사원', df2$POSITION)

# sol4) '-'를 사원으로 직접 치환(치환함수)
df2$POSITION <- str_replace(df2$POSITION,'-','사원')

# 2) 직급별 평균연봉을 출력
df2$PAY[df2$PAY=='-'] <- NA
df2$PAY <- as.numeric(df2$PAY)
tapply(df2$PAY, df2$POSITION , mean, na.rm=T)

# 3. emp.csv 파일을 읽고 다음과 같이 표현
emp <- read.csv('emp.csv', stringsAsFactors = F)

# deptno  ename 
# 10      CLARK KING MILLER     
# 20      SMITH JONES....    
# 30      ALLEN WARD...
str_c(emp[emp$DEPTNO==10, 'ENAME'], collapse = ' ')

vec1 <- tapply(emp$ENAME, emp$DEPTNO, str_c, collapse = ' ')
df3 <- as.data.frame(vec1)
df3$deptno <- rownames(df3)
colnames(df3)[1] <- 'ename'
df3[,c(2,1)]

# [ 참고 - sql 결합 함수 ]
# select deptno, 
#        listagg(ename,                  -- 결합컬럼
#                ' ')                    -- 구분자
#        within group(order by sal desc) -- 결합순서
#   from emp
#  group by deptno;

########## 여기까지는 복습입니다. ##########

# 정렬
# 1. order
# - 벡터만 정렬 가능, 여러 벡터 전달 가능(1차, 2차,...정렬 필요시)
# - 각 벡터마다 정렬 순서 전달 가능
# - 문자와 숫자 벡터의 동시 정렬 시 정렬 순서 전달 오류 발생
#   (method 지정 필요)
# - 정렬 결과는 정렬 순서대로 위치값 리턴
# - 데이터프레임 직접 정렬 불가, 위치값 기반 색인으로 해결

# 2. sort
# - 하나의 벡터만 정렬 가능
# - 정렬 순서대로 정렬된 결과 직접 리턴
# - 데이터프레임 정렬 불가
 
# 3. doBy::orderBy
# - 데이터 프레임 정렬 가능
# - formular에 정렬하고자 하는 여러 컬럼 나열 가능
# - formular의 +, - 기호로 각 정렬 순서 전달 가능


# sampling
# - 데이터 분석 전 train/test data로 분리하기 위해 주로 사용
# - raw data로 부터 랜덤하게 추출된 sample을 얻기 위해 사용
 
# 1. sample
# - row number를 사용한 표본 추출
# - group name/group number를 사용한 표본 추출
# - class별 균등 추출 불가(대체적으로 raw data 비율과 비슷하게 추출됨)
 
# [ 예제 - iris data를 70%, 30%의 두 집단으로 분리 ]
# 1) row number를 사용한 표본 추출
v_rn <- sample(1:nrow(iris), size = nrow(iris) * 0.7)

iris_train <- iris[v_rn, ]
iris_test <- iris[-v_rn, ]

nrow(iris_train)             # 150 * 0.7 = 105건 추출 확인
nrow(iris_test)              # 150 * 0.3 =  45건 추출 확인

table(iris$Species)          # 1 : 1 : 1의 비율
table(iris_train$Species)    # 1 : 1 : 1의 비율은 아니지만 비슷

# 2) group name/group number
v_gname <- sample(c('a','b'),          # a,b로 구성된 표본 추출
                  size = nrow(iris),   # 추출된 표본 개수
                  replace = T,         # 복원추출 허용
                  prob = c(0.7,0.3))   # a가 선택될 확률 70%

iris_train2 <- iris[v_gname=='a', ]
iris_test2 <- iris[v_gname=='b', ]

nrow(iris_train2)          # 112
nrow(iris_test2)           #  38

table(iris_train2$Species) # 37 : 37 : 38

# 2. doBy::sampleBy
# - data frame에서 직접 표본 추출 가능
# - 추출된 표본(train) 이외의 집단(test) 추출이 어려움
# - class별 균등 추출 가능
library(doBy)
sampleBy(formula = ,     # ~ Y
         frac = ,        # 추출 비율
         replace = ,     # 복원 추출 여부
         data = )        # raw data

# [ 예제 - iris data를 70%, 30%의 두 집단으로 분리 ]
# step1) train set 추출
iris_train3 <- sampleBy( ~ Species, frac = 0.7, data = iris)

# step2) 추출된 train set의 row number 획득
# sol1) 위치기반 추출
v_lo <- str_locate(rownames(iris_train3), '\\.')[,1]
v_rn <- as.numeric(str_sub(rownames(iris_train3), v_lo+1))

# sol2) 분리기반 추출
str_split(rownames(iris_train3), '\\.')   # 각 층마다 추출 어려움

f_split <- function(x) {
  as.numeric(str_split(x, '\\.')[[1]][2])
}

v_rn2 <- sapply(rownames(iris_train3), f_split)
sum(v_rn != v_rn2)

# step3) row number 기반 반대 집단 추출
iris_test3 <- iris[-v_rn, ]
iris_test4 <- iris[-v_rn2, ]

nrow(iris_train3)           # 105 * 0.7 = 105건 추출
table(iris_train3$Species)  # 1:1:1로 정확하게 균등 추출

nrow(iris_test3)            # 105 * 0.3 = 45건 추출
table(iris_test3$Species)   # 1:1:1로 정확하게 균등 추출


# 조인
# - 두 테이블(데이터프레임)의 조인만 가능
# - equi join만 가능(non equi join 불가)
# - 여러개 컬럼으로 조인 가능
# - 기본 연산은 inner join, outer join 가능

merge(x,                       # 조인할 첫 번째 데이터프레임
      y,                       # 조인할 두 번째 데이터프레임
      by = ,                   # 조인 컬럼(동일한 이름)
      by.x = ,                 # 첫 번째 데이터의 조인 컬럼
      by.y = ,                 # 두 번째 데이터의 조인 컬럼
      all = ,                  # full outer join 여부(T/F)
      all.x = ,                # left outer join 여부(T/F)
      all.y =                  # right outer join 여부(T/F)
      suffixes = c(".x",".y")  # 동일 컬럼의 접미어
      )

# 예제) emp.csv와 dept.csv 파일을 각각 불러온 후 조인하여
# 각 직원의 이름, 부서번호, 부서이름 출력
emp <- read.csv('emp.csv', stringsAsFactors = F)
dept <- read.csv('dept.csv', stringsAsFactors = F)

merge(emp, dept, by='DEPTNO')[ , c('ENAME','DEPTNO','DNAME')]


# 예제) student.csv와 professor.csv 파일을 각각 불러온 후 조인하여
# 각 학생의 이름, 학년, 지도교수 이름 출력
std <- read.csv('student.csv', stringsAsFactors = F)
pro <- read.csv('professor.csv', stringsAsFactors = F)

merge(std, pro, by='PROFNO')                # inner join
merge(std, pro, by='PROFNO', all.x = T)     # left outer join

merge(std, pro, by='PROFNO', all.x = T)[,c('NAME.x','GRADE','NAME.y')]

# [ 연습 문제 ]
# emp.csv 파일을 읽고 각 직원의 상위관리자 이름 출력
# 단, 상위관리자가 없는 경우는 본인 이름으로 치환
emp2 <- merge(emp, emp, by.x = 'MGR', by.y = 'EMPNO', all.x = T)
emp2 <- emp2[, c('ENAME.x', 'ENAME.y')]

str_replace_na(emp2$ENAME.y, '홍길동')
str_replace_na(emp2$ENAME.y, emp2$ENAME.x)              # 치환 불가
ifelse(is.na(emp2$ENAME.y), emp2$ENAME.x, emp2$ENAME.y) # 치환 가능

# [ 연습문제 ]
# gogak.csv, gift.csv 파일을 읽고 
# 각 직원의 수령상품을 출력(조인은 R 문법으로)
# 1) data loading
library('RJDBC')
jdbcDriver <- JDBC(driverClass = "oracle.jdbc.OracleDriver", 
                   classPath = "C:/app/KITCOOP/product/11.2.0/client_1/ojdbc6.jar")

con1 <- dbConnect(jdbcDriver, 
                 "jdbc:oracle:thin:@192.168.0.115:1521:orcl",
                 "scott",   # userid
                 "oracle")  # passwd

gogak <- dbGetQuery(con1, 'select * from gogak')
gift  <- dbGetQuery(con1, 'select * from gift')

# 2) for문을 사용한 처리

gift[(gift$G_START <= 980000) & (980000 <= gift$G_END), 'GNAME']
gift[(gift$G_START <= 73000) & (73000 <= gift$G_END), 'GNAME']
gift[(gift$G_START <= 320000) & (320000 <= gift$G_END), 'GNAME']

vresult <- c()

for (i in gogak$POINT) {
  vgift <- gift[(gift$G_START <= i) & (i <= gift$G_END), 'GNAME']
  vresult <- c(vresult, vgift)
}

gogak$GIFT <- vresult


# 3) 사용자 정의함수 + 적용함수
f_gift <- function(x) {
  gift[(gift$G_START <= x) & (x <= gift$G_END), 'GNAME']
}

sapply(gogak$POINT, f_gift)


# 그룹연산
# - 그룹별 특정 함수의 적용
# - 분리 - 적용 - 결합

# 1. tapply
# - 벡터로 리턴
# - 조건별 그룹연산 가능
# - 데이터프레임 입력 불가
# - 동시에 여러 컬럼 그룹연산 불가

tapply(emp$SAL, emp$DEPTNO, sum)
tapply(emp$SAL, emp$COMM, emp$DEPTNO, sum)

# 2. aggregate
# - 두가지 문법
# - 데이터프레임 리턴
# - 여러 컬럼 연산 가능
# - 여러 컬럼 그룹핑 가능
# - 여러 연산 컬럼에 서로 다른 함수 적용 불가

aggregate(x,       # 연산대상(벡터, 데이터프레임 가능)
          by,      # 그룹대상(group by 컬럼)
          FUN,     # 적용함수
          ...)     # 적용함수 필요 인자

aggregate(formula, # 연산대상 ~ 그룹대상
          data,    # 데이터프레임
          FUN,     # 적용함수
          ...)     # 적용함수 필요 인자

# 1) 연산 대상 1개, group  by  컬럼 1개
# 예제) student 데이터에서 각 학년별 키의 평균
aggregate(std$HEIGHT, list(std$GRADE), mean, na.rm=T)
aggregate(HEIGHT ~ GRADE, std, mean, na.rm=T)

# 2) 연산 대상 2개, group  by  컬럼 1개
# 예제) student 데이터에서 각 학년별 키, 몸무게의 평균

aggregate(std[,c('HEIGHT','WEIGHT')], 
          list(std$GRADE), mean, na.rm=T)

aggregate(HEIGHT + WEIGHT ~ GRADE, std, mean, na.rm=T)       # 불가
aggregate(cbind(HEIGHT,WEIGHT) ~ GRADE, std, mean, na.rm=T)  # 가능

# 2) 연산 대상 1개, group  by 컬럼 2개
# 예제) student 데이터에서 각 학년별, 학과별 키 평균
aggregate(std$HEIGHT, 
          list(std$GRADE, std$DEPTNO1), mean, na.rm=T)

aggregate(HEIGHT ~ GRADE + DEPTNO1, std, mean, na.rm=T)           

aggregate(cbind(HEIGHT,WEIGHT) ~ GRADE + DEPTNO1, 
          data = std, 
          FUN = c('min', 'max'), na.rm=T)         # 불가

# [ 연습 문제 ]
# student.csv 파일과 exam_01.csv 파일을 읽고
std <-read.csv('student.csv', stringsAsFactors = F)
exam <-read.csv('exam_01.csv', stringsAsFactors = F)

# 1) 각 학년별 시험성적의 평균을 구하세요.
std2 <- merge(std, exam, 
              by = 'STUDNO')[, c('STUDNO','NAME','GRADE','TOTAL')]
aggregate(TOTAL ~ GRADE, data = std2, FUN=mean)

# 2) 각 학년별 최고성적을 갖는 학생 이름, 성적, 학년 출력
std_max <- aggregate(TOTAL ~ GRADE, data = std2, FUN=max)
merge(std2, std_max, by = c('GRADE', 'TOTAL'))



# 3. plyr::ddply()


# 문자의 포맷 변경

# sprintf
# - 문자로 리턴

# to_char(1234, '09999')=> '01234')
# to_char(1234, '99999')=> ' 1234')
# to_char(1234, '9999.99')=> '1234.00')

sprintf(fmt = ,  # 변경할 포맷(s:문자열, d:정수, f:실수)
        ...)     # 변경 대상

sprintf('%5d', 1234)     # " 1234"
sprintf('%05d', 1234)    # "01234"
sprintf('%8.2f', 1234)   # " 1234.00"
sprintf('%7.2f', 1234)   # "1234.00"
sprintf('%.2f', 1234)    # "1234.00"
sprintf('%5s', 'abc')    # "  abc"
















