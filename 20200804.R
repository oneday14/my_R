# [ 연습 문제 ]
# subway2.csv 파일을 읽고 역별, 시간대별 승하차의 총 합을 출력
sub <- read.csv('subway2.csv', stringsAsFactors = F, skip=1,
                na.strings = '')
head(sub)

# 1) 역이름 빈칸 채우기(이전 역이름 가져오기)
library(zoo)
sub$전체 <- na.locf(sub$전체)

# 2) 역이름에서 호선 정보 지우기
library(stringr)
str_remove_all('동9운(4)','[(0-9)]')     #[] 안에 들어가면 일반기호
str_remove_all('동운(4)','([0-9])')      # ()가 특수기호로 해석
str_remove_all('동9운(4)','\\([0-9]\\)')  # \\를 사용해야 일반기호화

unique(str_remove_all(sub$전체, '\\([0-9]\\)'))
sub$전체 <- str_remove_all(sub$전체, '\\([0-9]\\)')

# 3) 역별 승하차별 시간대별 그룹핑(ddply)
library(plyr)
ddply(sub, .(전체,구분), summarise, v5=sum(X05.06),
                                    v6=sum(X06.07), 
                                    ........) # 모든 컬럼 나열 불편

# 4) 시간대 컬럼을 stack 처리
library(reshape2)
sub2 <- melt(sub, id.vars = c('전체','구분'), 
             variable.name = '시간대', value.name = '승하차수')

sub2$시간대 <- as.numeric(str_sub(sub2$시간대, 2,3))

# 5) 역별, 승하차별, 시간대별 총 합
ddply(sub2, .(전체,구분,시간대), summarise, cnt=sum(승하차수))

# [ 참고 - 역별, 시간대별 총 합 ]
ddply(sub2, .(전체, 시간대), summarise, cnt=sum(승하차수)) # long
dcast(sub2, 전체 ~ 시간대, sum)                            # wide


# doBy     : ~by (order, sample) 상위
# plyr     : apply 계열 함수(적용함수) 상위 
# reshape2 : stack/unstack 상위
# dplyr    : 구조화된 R 문법 제공(sql처럼)

install.packages('dplyr')
library(dplyr)

# dplyr의 구조화된 문법
# 1. select : 컬럼의 선택
# 2. mudate : 컬럼 가공
# 3. filter : 행 선택
# 4. group_by : 그룹연산
# 5. arrange : 정렬
# 6. summarise_each : 그룹연산의 실제 연산 조건

# 예제1) emp 테이블에서 이름, 사번, 연봉 선택
emp <- read.csv('emp.csv', stringsAsFactors = F)

emp %>%                     # sql from절 처럼 먼저 데이터 선택 후 진행
  select(ENAME, EMPNO, SAL)

# 예제2) emp 테이블에서 이름, 사번, 연봉, 10% 인상된 연봉 출력
emp %>%                     # sql from절 처럼 먼저 데이터 선택 후 진행
  select(ENAME, EMPNO, SAL) %>%
  mutate(new_sal=SAL*1.1)

# 주의 : 문법적 순서에 따른 파싱 가능 여부(컬럼 정의 순서 달라짐)
emp %>%                     
  select(ENAME, EMPNO) %>%   # ENAME, EMPNO만 다음 라인으로 전달
  mutate(new_sal=SAL*1.1)    # mutate에서는 SAL을 알 수 없음

emp %>%
  mutate(new_sal=SAL*1.1) %>%
  select(ENAME, EMPNO, new_sal) 
   

# 예제3) emp 테이블에서 10번 부서원에 대한 이름, 부서번호, 연봉 출력
emp %>%
  select(ENAME, DEPTNO, SAL) %>%
  filter(DEPTNO==10)

# 예제4) emp 테이블에서 10번 부서원에 대한 이름, 부서번호, 연봉 출력
# 단, 연봉순으로 정렬
emp %>%
  select(ENAME, DEPTNO, SAL) %>%
  filter(DEPTNO==10) %>%
  arrange(desc(SAL))

# 예제5) emp 테이블에서 각 부서별 평균 연봉 출력
emp %>%
  select(DEPTNO, SAL) %>%
  group_by(DEPTNO) %>%
  summarise_each(mean, SAL)


# 예제6) emp 테이블에서 HIREDATE 컬럼 제외 전체 선택
emp[ , c('EMPNO','ENAME','JOB')]
emp[ , -5]
emp[ , colnames(emp) != 'HIREDATE']

emp %>%
  select(-HIREDATE) 

# [ 연습 문제 ]
# student.csv 파일을 읽고
std <- read.csv('student.csv', stringsAsFactors = F)

# 1. 각 학생의 이름, 학년, 교수번호 출력
std %>%
  select(NAME, GRADE, PROFNO)

# 2. 위 정보에 교수번호가 없는 학생은 생략
std %>%
  select(NAME, GRADE, PROFNO) %>%
  filter(!is.na(PROFNO))

# 3. 위 정보에 각 학생의 성별 컬럼 추가하여 출력
std %>%
  select(NAME, GRADE, PROFNO, JUMIN) %>%
  filter(!is.na(PROFNO)) %>%
  mutate(v1=ifelse(str_sub(JUMIN,7,7)=='1','남','여'))
  
std %>%
  filter(!is.na(PROFNO)) %>%
  mutate(v1=ifelse(str_sub(JUMIN,7,7)=='1','남','여')) %>%
  select(NAME, GRADE, PROFNO, v1) 

# 4. 위 데이터에서 학년별 정렬 
std %>%
  filter(!is.na(PROFNO)) %>%
  mutate(v1=ifelse(str_sub(JUMIN,7,7)=='1','남','여')) %>%
  select(NAME, GRADE, PROFNO, v1) %>%
  arrange(GRADE, v1)
  
# 5. 학년별 키 평균
std %>%
  select(GRADE, HEIGHT) %>%
  group_by(GRADE) %>%
  summarise_each(mean, HEIGHT)

std %>%
  select(GRADE, HEIGHT, WEIGHT) %>%
  group_by(GRADE) %>%
  summarise_each(mean, c(HEIGHT, WEIGHT))

std %>%
  select(GRADE, HEIGHT, WEIGHT) %>%
  group_by(GRADE) %>%
  summarise_each(c(mean,max), c(HEIGHT, WEIGHT))

# summarise_each의 across 대체
std %>%
  select(GRADE, HEIGHT, WEIGHT) %>%
  group_by(GRADE) %>%
  summarise(across(c(HEIGHT, WEIGHT), c(mean,max)))


# which.min, which.max 
# - 최대값과 최소값을 갖는 index 리턴(위치)

library(googleVis)
Fruits

df1 <- dcast(Fruits, Fruit ~ Year, value.var = 'Sales')
vord <- which.max(df1$`2008`)

df1[vord, 'Fruit']

# kimchi_test.csv 파일을 읽고
df2 <- read.csv('kimchi_test.csv', stringsAsFactors = F)

# 1) 1월 총각김치의 대형마트 판매량과 판매금액 출력(dplyr)
df2 %>%
  filter((판매월==1) & (제품=='총각김치') & (판매처=='대형마트')) %>%
  select(수량, 판매금액)

# 2) 년도별 월별 전체 판매량의 총 합 출력(dplyr)
df2 %>%
  select(판매년도,판매월,수량) %>%
  group_by(판매년도,판매월) %>%
  summarise_each(sum, 수량)

# 3) 년도별 판매량이 가장 많은 김치 출력
# sol1) ddply - long data
df3 <- ddply(df2, .(판매년도, 제품), summarise, total=sum(수량))
ddply(df3, .(판매년도), subset, total==max(total))

# sol2) which.max - wide data
df4 <- dcast(df2, 제품 ~ 판매년도, sum, value.var = '수량')


f1 <- function(x) {
  vord <- which.max(x)
  df4[vord,1]
}

f1(df4$`2013`)
f1(df4$`2014`)
f1(df4[,-1])             # 2차원 적용 불가 
apply(df4[,-1], 2, f1)

# str_extract_all(문자열, 패턴)
# - 원하는 패턴의 문자열 추출
# - 패턴에 정규식 표현 가능
# - 리스트 출력

# 예제) 다음의 역이름에서 역 이름만 추출
str_extract_all('을지로2가(2)', '[가-힣]')
str_c(str_extract_all('을지로2가(2)', '[:alpha:]')[[1]], collapse='')



