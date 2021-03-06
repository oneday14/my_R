# 1. project_songpa_data.csv 파일을 읽고
# 동별 LAT와 LON의 최소값을 구하세요.
# 단, 동 이름은 다음과 같이 결합하여 표현
# 
# 풍납1동 124-5  => 풍납동
# 장지동택지개발지구 => 장지동
# 거여동 136-106호 맞은편 => 거여동
# 잠실나루역 => 잠실나루역

# 파일 불러오기
df2 <- read.csv('project_songpa_data.csv', stringsAsFactors = F)

# 컬럼정리
df2 <- df2[,-1]

# 동에서 한글만 가져오기
f1 <- function(x) {
  str_c(str_extract_all(x, '[가-힣]')[[1]], collapse = '')
}

df2$name <- sapply(df2$name, f1)

# 동에서 불필요한 글 지우기
f2 <- function(x) {
  str_replace_na(str_sub(x, 1, str_locate(x,'동')[1]), x)
}

df2$name <- sapply(df2$name, f2)

# 최소값 구하기
aggregate(cbind(LAT, LON) ~ name, df2, min)          ## 결과 나옴
