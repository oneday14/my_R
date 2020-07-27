# 가변형 길이 인자를 갖는 사용자 정의 함수
f1 <- function(...) {
  var1 <- list(...)
  for (i in var1) {
    반복문장
  }
}

# 예제) 입력된 수의 총 합을 출력하는 사용자 정의함수 생성
# fsum(2,6,8,43)
fsum <- function(...) {
  vsum <- c(...)
  vhap <- 0
  for (i in vsum) {
    vhap <- vhap + i
  }
  return(vhap)
}

fsum(2,6,8,43,1)


# 전역변수와 지역변수
# 전역변수 : 변수의 정의가 세션 전체 적용
# 지역변수 : 변수의 정의가 일부 함수에만 유효

# 1)
v1 <- 1               # 함수 밖에서 선언, 전역변수

f1 <- function(x) {
  print(v1)
}

f1()

# 2)
v1 <- 1               # 전역변수

f2 <- function(x) {
  v1 <- 10            # 함수 내 선언, 지역변수
  print(v1)
}

f2()

# 3) 
f3 <- function(x) {
  v2 <- 10            # 지역변수, f3 함수에서만 사용 가능
  print(v2)
}

f4 <- function(x) {
  print(v2)
}

f3()
f4()                  # object not found error
v2                    # object not found error


# 4) 지역변수의 전역변수 설정 
f3 <- function(x) {
  v2 <<- 10            # 지역변수, f3 함수에서만 사용 가능
  print(v2)
}

f4 <- function(x) {
  print(v2)
}

f3()                  # 10
f4()                  # 10
v2                    # 10

# 현 세션에 정의된 변수 생성 및 제거
ls()
rm('v2')
ls()

# iris data set : 꽃의 품종(3개)을 예측하는 분류모델 테스트용 data
iris
str(iris)

# [ 데이터분석 ]
# 1. 지도학습 : Y(정답, 예측값)가 알려진 경우의 분석 형태
# 1) 회귀 기반 분석 : Y(정답, 예측값)가 연속형인 경우
# 2) 분류 기반 분석 : Y(정답, 예측값)가 factor인 경우

# 2. 비지도학습 : Y(정답, 예측값)가 알려지지 않은 경우의 분석 형태


# 파일 입출력
# 1. read.csv : csv(컴마로 분리구분된) 파일을 불러오는 함수
# - header = T : 첫 줄을 컬럼화 시킬지 여부
read.csv('read_test.csv', header = F)  # header=T
read.table('read_test.csv')            # header=F

# - sep = "," : 각 파일의 분리 구분기호
read.csv('read_test.csv', sep=',')     # sep=','
read.table('read_test.csv', sep=',')   # sep=''

read.csv('test1.txt', sep=':', header = F)        
read.table('test1.txt', sep=':') 

# - row.names : 불러올때 각 행의 이름 부여
# - col.names : 불러올때 각 컬럼 이름 부여
read.csv('test1.txt', sep=':', header = F,
         col.names = c('name','deptno','sal'))

# - na.strings = "NA" : NA로 처리할 문자열
df1 <- read.csv('read_test.csv', 
                na.strings =c('.','-','?','!','null','nan'))
str(df1)

# - nrows = -1, : 불러올 행의 수
read.csv('emp.csv', nrows = 5)

# - skip = 0 : 스킵할 행의 개수
read.csv('emp.csv', skip = 1, header = F)

# - stringsAsFactors = T : 문자 컬럼의 팩터화 여부
# - encoding = "unknown" : 인코딩 옵션
read.csv('emp.csv', encoding='cp949')

 
# 2. write.csv(x,        # 저장할 객체 이름
#              file = ,  # 저장할 외부 파일 명
#              sep = )   # 저장시 분리 구분기호

df2 <- data.frame(col1=c('a','b'),
                  col2=c(1,5))
write.csv(df2, 'df2_write_test.csv')

# 3. scan 
# - 외부 파일을 "벡터"로 불러오기
# - 파일명 생략시 사용자에게 값 입력 대기
# - 기본이 숫자 로딩, 문자 로딩시 what='' 필요

scan()           # 숫자 연속 입력, 엔터 입력될때까지
scan(what = '')  # 문자 연속 입력, 엔터 입력될때까지

scan('file1.txt', sep = ',')             # 숫자 로딩
scan('file1.txt', sep = ',', what = '')  # 문자 로딩

# 4. readLines 
# - 외부 파일을 벡터로 불러오기
# - 각 라인을 벡터의 원소로 불러옴
readLines('file1.txt')

# 5. readline
# - 사용자에게 입력 대기
# - 불러온 값은 문자형으로 저장
ans1 <- readline('정말 삭제할건가요? (Y|N) :')
ans1

# if (ans1 == 'Y') {
#   파일 삭제
# } else {
#   print('파일을 삭제하지 않겠습니다')
# }

# ----
print('두 수를 입력받아 곱하는 프로그램')

no1 <- as.numeric(readline('첫 번째 숫자를 입력하세요 : '))
no2 <- as.numeric(readline('두 번째 숫자를 입력하세요 : '))

no1 + no2



# [ 연습 문제 ]
# seoul_new.txt 파일을 불러와서 다음의 형태를 갖는 
# 데이터프레임 생성
# id                       text                  date    cnt
# 305 무료법률상담에 대한 부탁의 말씀 입니다. 2017-09-27  2 
library(stringr)
data1 <- readLines('seoul_new.txt')

v_str <- str_trim(data1[1], side = 'both')
v_str <- str_split(v_str, ' ')[[1]]
vid <- v_str[1]
vlen <- length(v_str)
vcnt <- v_str[vlen]
vdate <- v_str[vlen-1]
vtext <- str_c(v_str[2:(vlen-2)], collapse = ' ')

vid <- c() ; vcnt <- c() ; vdate <- c() ; vtext <- c()

for (i in data1) {
  v_str <- str_trim(i, side = 'both')
  v_str <- str_split(v_str, ' ')[[1]]
  vlen <- length(v_str)
  vid <- c(vid, v_str[1])
  vcnt <- c(vcnt, v_str[vlen])
  vdate <- c(vdate, v_str[vlen-1])
  vtext <- c(vtext, str_c(v_str[2:(vlen-2)], collapse = ' '))
}

df_data <- data.frame(id=vid, text=vtext, date=vdate, cnt=vcnt)
head(df_data)

df_data$id <- as.numeric(df_data$id)
df_data$cnt <- as.numeric(df_data$cnt)



# 6. 데이터베이스 입출력(oracle)

oracle
- server : instance(memory) + DB(disk)
- client : instance(memory)










