# 1. 부동산_매매지수1.csv 파일을 읽고
# 날짜 컬럼을 년월일 모두 표현되도록 변경 후 년도별 구별 매매지수의 평균을 출력한 뒤
# 년도별 구별 매매지수를 비교할 수 있는 막대그래프 시각화 하세요.

# 파일불러오기
df2 <- read.csv('부동산_매매지수1.csv', stringsAsFactors = F, skip = 1)
df2 <- df2[-1,]
df2 <- df2[,-c(31:ncol(df2))]

# xx월 oo일 양식 맞추기
f2 <- function(x){
  if(length(str_split(x,'/')[[1]]) == 3) {
    str_c(str_split(x,'/')[[1]][1], sprintf('%02d',as.numeric(str_split(x,'/')[[1]][2])),
          sprintf('%02d',as.numeric(str_split(x,'/')[[1]][3])), sep = '/')
  } else {
    str_c(sprintf('%02d',as.numeric(str_split(x,'/')[[1]][1])),
          sprintf('%02d',as.numeric(str_split(x,'/')[[1]][2])), sep = '/')
  }
}

df2$구분 <- sapply(df2$구분, f2)


# 년도를 가지고 있는 행 출력
df2[str_length(df2$구분) == 6,]   # 2, 41, 92, 143, 193, 244,295,344,394,444,494 이지만
# 첫번째 행을 skip했으므로 1씩 빼야함

v1 <- c(2, 41, 92, 143, 193, 244,295,344,394,444,494) # 시작점
v2 <- c(2, 41, 92, 143, 193, 244,295,344,394,444,494) -2 # 끝점

# 날짜 포멧으로 변경
df2$구분[v1[1]:v2[2]] <- str_c('08', df2$구분[v1[1]:v2[2]], sep = '/')
df2$구분[v1[2]:v2[3]] <- str_c('09', df2$구분[v1[2]:v2[3]], sep = '/')
df2$구분[v1[3]:v2[4]] <- str_c('10', df2$구분[v1[3]:v2[4]], sep = '/')
df2$구분[v1[4]:v2[5]] <- str_c('11', df2$구분[v1[4]:v2[5]], sep = '/')
df2$구분[v1[5]:v2[6]] <- str_c('12', df2$구분[v1[5]:v2[6]], sep = '/')
df2$구분[v1[6]:v2[7]] <- str_c('13', df2$구분[v1[6]:v2[7]], sep = '/')
df2$구분[v1[7]:v2[8]] <- str_c('14', df2$구분[v1[7]:v2[8]], sep = '/')
df2$구분[v1[8]:v2[9]] <- str_c('15', df2$구분[v1[8]:v2[9]], sep = '/')
df2$구분[v1[9]:v2[10]] <- str_c('16', df2$구분[v1[9]:v2[10]], sep = '/')
df2$구분[v1[10]:v2[11]] <- str_c('17', df2$구분[v1[10]:v2[11]], sep = '/')
df2$구분[v1[11]:length(df2$구분)] <- str_c('18', df2$구분[v1[11]:length(df2$구분)], sep = '/')

df2$구분 <- as.Date(df2$구분, '%y/%m/%d')

# 년도 컬럼 생성
df2$년도 <- str_sub(df2$구분,1,4)

# long data로 만들기 위한 준비과정
df2[,c(1,2)] <- NULL
df2[,] <- apply(df2, c(1,2), as.numeric)

# long data 만들기
df3 <- melt(df2, id.vars = c('년도'),
            variable.name = '지역',
            value.name = '값')

# 년도별 지역별 매매지수의 평균
df4 <- ddply(df3, .(년도, 지역), summarise, v1 = mean(값))

# 년도별 지역별 wide data 만들기
df5 <- dcast(df4, 지역 ~ 년도, sum)
rownames(df5) <- df5$지역
df5$지역 <- NULL

# 막대그래프 시각화
dev.new()
barplot(as.matrix(df5), beside = T, col=2:nrow(df5), legend = rownames(df5))          ## 결과 나옴



# 2. disease.txt 파일을 읽고,
# 1) 대장균이 가장 많이 발병한 달을 출력

# 파일불러오기
df1 <- read.table('disease.txt', stringsAsFactors = F)

# 행, 열 이름 정리
colnames(df1) <- df1[1,]
rownames(df1) <- df1[,1]
df1 <- df1[-1,]
df1 <- df1[,-1]

# NA를 0으로 치환
library(stringr)
df1[,] <- apply(df1, c(1,2), as.character)
df1[,] <- apply(df1, c(1,2), str_replace_na, '0')
df1[,] <- apply(df1, c(1,2), as.numeric)

# 대장균이 가장 많이 발병한 달 추출
df1[df1$대장균==max(df1$대장균),]          ## 결과 나옴

# 2) 데이터를 A형간염이 많은 순으로 정렬

# A형간염이 많은 순으로 정렬
library(doBy)
orderBy( ~ -A형간염, df1)          ## 결과 나옴

# 3) 콜레라 기준으로 순위 출력

# 콜레라 기준으로 순위 출력
df2 <- df1[order(df1$콜레라, decreasing = T),]
df2$A형간염순위 <- str_c(c(1:12), '위')
df2[,c('콜레라', 'A형간염순위')]          ## 결과 나옴

# 4) 각 질병 별 발병횟수의 총 합을 출력

# 각 질병 별 발병횟수의 총합
apply(df1, 2, sum)          ## 결과 나옴

# 5) na 값을 이전 행 값으로 치환

# NA가 포함된 원본데이터 다시 불러오기
df1 <- read.table('disease.txt', stringsAsFactors = F)

# 행, 열 이름 정리
colnames(df1) <- df1[1,]
rownames(df1) <- df1[,1]
df1 <- df1[-1,]
df1 <- df1[,-1]

# NA 이전행 값으로 치환하는 함수 만들기
f1 <- function(x) {
  v1 <- c()
  for (i in 1:length(x)) {
    if (is.na(x[i]) == 1){
      v1 <- c(v1, x[i-1])
    } else {
      v1 <- c(v1, x[i])
    }
  }
  return(v1)
}

# 치환하기
df1$콜레라 <- f1(df1$콜레라)
df1$장티푸스 <- f1(df1$장티푸스)
df1$이질 <- f1(df1$이질)
df1$대장균 <- f1(df1$대장균)
df1$A형간염 <- f1(df1$A형간염)
df1          ## 결과 나옴

