# 1. 부동산_매매지수현황.csv 파일을 읽고
# 1) 년도별 각 지역의 활발함 지수의 평균을 아래와 같은 형식으로 출력
#
#       서울 부산 대구 인천 광주 대전 울산
# 2008
# 2009
# 2010

df1 <- read.csv('부동산_매매지수현황.csv', stringsAsFactors = F, skip = 1)

# 색인을 위한 백업본
df1_1 <- df1

# row와 col 이름정리
df1 <- df1[-c(1,2),]
rownames(df1) <- df1[,1]
df1 <- df1[,-1]


# 활발함과 한산함 각각의 df만들기
df1_act <- df1[,df1_1[1,2:ncol(df1_1)] == '활발함']
df1_ina <- df1[,df1_1[1,2:ncol(df1_1)] == '한산함']

# col 이름정리
library(stringr)
colnames(df1_act) <- str_remove_all(colnames(df1_act),'[.a-zA-Z]')
colnames(df1_ina) <- str_remove_all(colnames(df1_act),'[.a-zA-Z]')

# 각 성분 변환
df1_act[,] <- apply(df1_act, c(1,2), as.numeric)
df1_ina[,] <- apply(df1_ina, c(1,2), as.numeric)

# 평균출력
library(plyr)
df1_act$year <- str_sub(rownames(df1_act),1,4)
df1_act_1 <- aggregate(cbind(서울, 부산, 대구, 인천, 광주, 대전, 울산) ~ year, df1_act, mean)
rownames(df1_act_1) <- df1_act_1[,1]
df1_act_mean <- df1_act_1[,-1]
df1_act_mean

# wide -> long
library(reshape2)
df1_act_2 <- melt(df1_act_1, id.vars = 'year', variable.name='지역', value.name='매매지수')

# 정렬
orderBy( ~ -매매지수, df1_act_2)          ## 결과 나옴      

