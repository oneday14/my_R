# 1. 오늘은 2019년 6월 19일입니다. 아래 조건에 맞는 회원번호를 추출하는 R코드를 작성하세요	
# 
# (1) [고객정보]의 고객 중 최근 구매가 10~20일 없었던 고객을 고른 후, 2018년 7월 1일 이전 첫 구매 고객은 [5%]할인쿠폰, 
#     그 이후 첫 구매 고객은 [7%]할인쿠폰을 지급하려고 합니다. 각각을 추출하는 코드를 쓰시오.
#   1) [5%]할인쿠폰대상자
CUST <- read.table('clipboard', header=TRUE, stringsAsFactors = F)

CUST$최종구매일 <- as.Date(CUST$최종구매일)
today <- as.Date('2019/06/19', '%Y/%m/%d')
v_days <- today - CUST$최종구매일

CUST_select <- CUST[v_days <=20 & v_days >= 10,]   #최근구매가 10~20일 없었던 고객
CUST_select$첫구매일 <- as.Date(CUST_select$첫구매일)

CUST_select[CUST_select$첫구매일 < as.Date('2018/07/01', '%Y/%m/%d'),]   # 5%할인쿠폰          ## 결과 나옴

#   2) [7%]할인쿠폰대상자
CUST_select[CUST_select$첫구매일 > as.Date('2018/07/01', '%Y/%m/%d'),]   # 7%할인쿠폰          ## 결과 나옴

# (2) [고객정보]의 고객 중 평균주문금액이 40000원 미만인 고객 중 멤버쉽 가입과 신규 멤버쉽 가입이 모두 N인 고객을 고른 후,
#     전일 문자 발송자는 제외하려고 합니다. 해당 회원번호를 추출하는 코드를 쓰시오.
CUST <- read.table('clipboard', header=TRUE, stringsAsFactors = F)
PUSH <- read.table('clipboard', header=TRUE, stringsAsFactors = F)

CUST_select2 <- CUST[(CUST$평균주문금액 < 40000) & (CUST$멤버쉽가입 == 'N') & (CUST$신규멤버쉽가입 == 'N'),] #평균주문금액이 40000원 미만, 멤버쉽가입과 신규멤버쉽가입이 N인 고객

for (i in PUSH$회원번호) {
  if (CUST_select2$회원번호 != i) {
    CUST_select2 <- CUST_select2[CUST_select2$회원번호 != i,]
  }
}

CUST_select2 #평균주문금액이 40000원 미만, 멤버쉽가입과 신규멤버쉽가입이 N인 고객, 전일문자 발송자 제외          ## 결과 나옴

# 2. [주문정보]의 데이터로 분석해볼 때, 개인의 5월 주문금액이 4월 주문금액 대비 50% 미만으로 감소한 고객의 회원번호를 추출하는 코드를 쓰시오.
ORD <- read.table('clipboard', header=TRUE, stringsAsFactors = F)

ORD$주문일 <- as.Date(ORD$주문일,'%Y-%m-%d')
ORD$주문금액 <- as.numeric(str_remove_all(ORD$주문금액,','))
ORD_4 <- ORD[as.character(ORD$주문일,'%m') == '04',]     # 4월 주문고객
ORD_5 <- ORD[as.character(ORD$주문일,'%m') == '05',]     # 5월 주문고객

cost <- c()
for (i in ORD_5$회원번호) {
  if (ORD_4$회원번호 == i) {
    cost <- c(cost, ORD_4[(ORD_5[ORD_5$회원번호 == i,'주문금액']) > (ORD_4[ORD_4$회원번호 == i,'주문금액'] * 0.5),'회원번호'])
  } 
}

cost          ## 결과 나옴(단, 경고발생)


