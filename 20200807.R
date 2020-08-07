# 1. ±³½ÀÇöÈ².csv ÆÄÀÏÀ» ÀÐ°í
df1 <- read.csv('±³½ÀÇöÈ².csv', stringsAsFactors=F, skip=1)
df1 <- df1[,-c(3,4)]
str(df1)

library(plyr) ; library(reshape2) ; library(stringr)

# 1) ±¸º° ±³½À°úÁ¤ÀÇ ÃÑ ±Ý¾×À» ¸·´ë±×·¡ÇÁ·Î ½Ã°¢È­
# step1) ±¸ ÃßÃâ
str_extract_all(df1$±³½À¼ÒÁÖ¼Ò[1], '..±¸')
str_extract_all(df1$±³½À¼ÒÁÖ¼Ò[1], '[°¡-ÆR]{1,}±¸')
str_extract_all(df1$±³½À¼ÒÁÖ¼Ò[1], '[:alpha:]{1,}±¸')[[1]]

f1 <- function(x) {
  str_extract_all(x, '..±¸')[[1]][1]
}

df1$±¸ <- sapply(df1$±³½À¼ÒÁÖ, f1)
unique(df1$±¸)

# step2) ±³½À±Ý¾× ÄÃ·³ ÃßÃâ
df2 <- df1[, str_detect(colnames(df1), '^X')]

# step3) ÇÐ¿øº° ÃÑ ±³½À±Ý¾× °è»ê(Çàº° ÇÕ)
f_num <- function(x) {
  as.numeric(str_remove_all(x, ','))
}

df2[,] <- apply(df2, c(1,2), f_num)

df1$total <- apply(df2, 1, sum)

# step4) ±¸º°, ±³½À°úÁ¤º° ±³½À±Ý¾× ÇÕ °è»ê
str_remove_all('½Ç¿ë¿Ü±¹¾î(À¯¾Æ/ÃÊ¡¤Áß¡¤°í)','\\(.{1,}\\)')
str_remove_all('ÄÄÇ»ÅÍ(¼Ò)','\\(.{1,}\\)')

unique(str_remove_all(df1$±³½À°úÁ¤,'\\(.{1,}\\)'))
df1$±³½À°úÁ¤ <- str_remove_all(df1$±³½À°úÁ¤,'\\(.{1,}\\)')

df1_total <- dcast(df1, ±³½À°úÁ¤ ~ ±¸, sum, value.var='total')

# step5) ½Ã°¢È­
dev.new()
barplot(as.matrix(df1_total[,-1])/1000000, beside = T, 
        ylim = c(0,60000), col=1:nrow(df1_total))

legend(1,60000,df1_total$±³½À°úÁ¤, fill = 1:nrow(df1_total))

# 2) ³âµµº° º¸½À ±³°ú°úÁ¤ÀÇ ÁöÃâ±Ý¾×ÀÌ °¡Àå Å« µ¿ÀÌ¸§, ÁöÃâ±Ý¾× Ãâ·Â
# step1) µ¿ÀÌ¸§ ÃßÃâ
# --
str_extract_all(df1$±³½À¼ÒÁÖ¼Ò[1], '[°¡-ÆR]{1,}µ¿')[[1]][1]

f2 <- function(x) {
  str_extract_all(x, '[°¡-ÆR]{1,}µ¿')[[1]][1]
}

unique(sapply(df1$±³½À¼ÒÁÖ¼Ò, f2))
df1$±³½À¼ÒÁÖ[sapply(df1$±³½À¼ÒÁÖ¼Ò, f2) == '°ü¾ÇÇª¸£Áö¿À»ó°¡µ¿']

# --
str_extract_all(df1$±³½À¼ÒÁÖ¼Ò[1], '\\([°¡-ÆR]{1,}µ¿')[[1]][1]

f2 <- function(x) {
  str_extract_all(x, '\\([°¡-ÆR]{1,}µ¿')[[1]][1]
}

unique(sapply(df1$±³½À¼ÒÁÖ¼Ò, f2))
df1$±³½À¼ÒÁÖ¼Ò[is.na(sapply(df1$±³½À¼ÒÁÖ¼Ò, f2))]

# --
str_extract_all(df1$±³½À¼ÒÁÖ¼Ò[1], '\\([°¡-ÆR0-9]{1,}µ¿')[[1]][1]

f2 <- function(x) {
  str_extract_all(x, '\\([°¡-ÆR0-9]{1,}µ¿')[[1]][1]
}

unique(sapply(df1$±³½À¼ÒÁÖ¼Ò, f2))
df1$µ¿ <- str_remove_all(sapply(df1$±³½À¼ÒÁÖ¼Ò, f2), '[(0-9]')

# step2) ÇÊ¿äµ¥ÀÌÅÍ ÃßÃâ
df2$µ¿ <- df1$µ¿
df2$±³½À°úÁ¤ <- df1$±³½À°úÁ¤

# step3) ³âµµ ÄÃ·³ stack
df3 <- melt(df2, id.vars = c('±³½À°úÁ¤','µ¿'), 
            variable.name='³âµµ', value.name='±Ý¾×')
df3$³âµµ <- as.numeric(str_sub(df3$³âµµ,2,5))

# step4) ³âµµº° µ¿º° ±³½À°úÁ¤º° ±Ý¾×ÀÇ ÃÑÇÕ
df4 <- ddply(df3, .(³âµµ,µ¿,±³½À°úÁ¤), summarise, vsum=sum(±Ý¾×))

# step5) ³âµµº° ±³½À°úÁ¤º° ±Ý¾×ÀÇ ÃÖ´ë¸¦ °®´Â Çà ¼±ÅÃ 
ddply(df4, .(³âµµ,±³½À°úÁ¤), subset, vsum==max(vsum))

# 3) °¢ º¸½À°úÁ¤º° ¸ÅÃâÀÌ °¡Àå ³ôÀº ±³½À¼Ò¸íÀ» Ãâ·ÂÇÑµÚ
#    °¢ ±³½À¼Ò¸í(±³½À°úÁ¤) °ú ¸ÅÃâ¾×À» ºñ±³ÇÒ ¼ö ÀÖ´Â ¸·´ë±×·¡ÇÁ Ãâ·Â
# step1) º¸½À°úÁ¤º° ±³½À¼Ò¸íº° ¸ÅÃâ¾×
df2$±³½À¼Ò¸í <- df1$±³½À¼Ò¸í
df2$total <- df1$total

df_total <- ddply(df2, .(±³½À°úÁ¤, ±³½À¼Ò¸í), summarise, vsum=sum(total))

# step2) º¸½À°úÁ¤º° ÃÖ´ë±Ý¾×À» °®´Â Çà ¼±ÅÃ
df_total2 <- ddply(df_total, .(±³½À°úÁ¤), subset, vsum==max(vsum))

# step3) ½Ã°¢È­
dev.new()
par(oma=c(5,0,0,0))  # ÇÏ,ÁÂ,»ó,¿ì
vname <- str_c(df_total2$±³½À¼Ò¸í,'\n',df_total2$±³½À°úÁ¤)
barplot(df_total2$vsum/100000, col = rainbow(nrow(df_total2)),
        ylim = c(0,30000), names.arg = vname, las=2)

# 2. total.csv ÆÄÀÏÀ» ÀÐ°í
data1 <- read.csv('total.csv', stringsAsFactors = F)

# 1) ³âµµº° °¢ Ç°¸ñ¿¡ ´ëÇÑ ¸ÅÃâÀ» ¸·´ë±×·¡ÇÁ·Î ½Ã°¢È­
# step1) ³âµµ¿Í ÁöÁ¡À» °áÇÕÇÑ ÇüÅÂ·Î ÄÃ·³ÀÌ¸§ º¯°æ(stackÀ» À§ÇÑ Ã³¸®)
colnames(data1) <- str_c(str_sub(colnames(data1),2,5), '_', data1[1,])
data1 <- data1[-1,]

# step2) stack
colnames(data1)[1] <- 'name'
data2 <- melt(data1, id.vars = 'name',
              variable.name = '³âµµ', value.name='±Ý¾×')

# step3) ³âµµ¿Í Áö°Ë ÄÃ·³ ºÐ¸®
data2$ÁöÁ¡ <- str_sub(data2$³âµµ,6,6)
data2$³âµµ <- str_sub(data2$³âµµ,1,4)

# step4) ±Ý¾×ÄÃ·³ ¼ýÀÚ º¯°æ
data2$±Ý¾× <- sapply(data2$±Ý¾×, f_num)

# step5) ³âµµº° Á¦Ç°º° ¸ÅÃâ¿¡ ´ëÇÑ ±³Â÷Å×ÀÌºí »ý¼º
data_total <- dcast(data2, name ~ ³âµµ, sum, value.var='±Ý¾×')

# step6) ½Ã°¢È­
dev.new()
barplot(as.matrix(data_total[,-1])/1000, beside = T, col = 2:4,
        ylim = c(0,100), legend = data_total$name,
        args.legend = list(cex=0.7))

# 2) ÁöÁ¡º°·Î °¡Àå ¸ÅÃâÀÌ ³ôÀº Ç°¸ñ°ú ÃÑ ¸ÅÃâ¾×À» ÇÔ²² Ãâ·Â
# step1) ÁöÁ¡º° Ç°¸ñº° ¸ÅÃâ¾× ÃÑ ÇÕ
data3 <- ddply(data2, .(ÁöÁ¡, name), summarise, vsum=sum(±Ý¾×))

# step2) ÁöÁ¡º° ÃÖ´ë°ª °®´Â Çà ¼±ÅÃ
ddply(data3, .(ÁöÁ¡), subset, vsum==max(vsum))

########## ¿©±â±îÁö´Â º¹½ÀÀÔ´Ï´Ù. ##########

# [ ¿¬½À ¹®Á¦ ]
# taxi_call.csv ÆÄÀÏÀ» ÀÐ°í
# °¢ ¿äÀÏº°·Î ½Ã°£´ëº° ÅÃ½Ã ÀÌ¿ë·üÀ» ÆÄÀÌÂ÷Æ®·Î Ãâ·Â(7°³ÆÄÀÌ)
# 1) ³¯Â¥ ÆÄ½Ì
taxi <- read.csv('taxi_call.csv', stringsAsFactors = F)
taxi$±âÁØ³â¿ùÀÏ <- as.Date(as.character(taxi$±âÁØ³â¿ùÀÏ), '%Y%m%d')

# 2) ¿äÀÏ ÃßÃâ
taxi$¿äÀÏ <- as.character(taxi$±âÁØ³â¿ùÀÏ, '%A')

# 3) ½Ã°£´ëº° ¿äÀÏº° ±³Â÷Å×ÀÌºí »ý¼º
taxi_total <- dcast(taxi, ½Ã°£´ë ~ ¿äÀÏ, sum, value.var='ÅëÈ­°Ç¼ö')

# 4) °¢ ¿äÀÏº° ½Ã°£´ëÀÇ ÅëÈ­°Ç¼ö ºñÀ²
f_rate <- function(x) {
  round(x / sum(x) * 100, 1)
}

taxi_total[,-1] <- apply(taxi_total[,-1],2,f_rate)

# 5) °¢ ÆÄÀÌÀÇ label °ª °¡°ø
# 0½Ã(8.9%)
str_c(taxi_total$½Ã°£´ë,'½Ã(',taxi_total$¿ù¿äÀÏ,'%)')

dev.new()
par(mfrow=c(2,4))
library(plotrix)
pie3D(taxi_total$¿ù¿äÀÏ, 
      labels=str_c(taxi_total$½Ã°£´ë,'½Ã(',taxi_total$¿ù¿äÀÏ,'%)'),
      labelcex=0.5, main='¿ù¿äÀÏ')

pie3D(taxi_total$È­¿äÀÏ, 
      labels=str_c(taxi_total$½Ã°£´ë,'½Ã(',taxi_total$È­¿äÀÏ,'%)'),
      labelcex=0.5, main='È­¿äÀÏ')

pie3D(taxi_total$¼ö¿äÀÏ, 
      labels=str_c(taxi_total$½Ã°£´ë,'½Ã(',taxi_total$¼ö¿äÀÏ,'%)'),
      labelcex=0.5, main='¼ö¿äÀÏ')

pie3D(taxi_total$¸ñ¿äÀÏ, 
      labels=str_c(taxi_total$½Ã°£´ë,'½Ã(',taxi_total$¸ñ¿äÀÏ,'%)'),
      labelcex=0.5, main='¸ñ¿äÀÏ')

pie3D(taxi_total$±Ý¿äÀÏ, 
      labels=str_c(taxi_total$½Ã°£´ë,'½Ã(',taxi_total$±Ý¿äÀÏ,'%)'),
      labelcex=0.5, main='±Ý¿äÀÏ')


# µ¥ÀÌÅÍ ºÐ¼®
# - µ¥ÀÌÅÍ ¸¶ÀÌ´×
# - ¹Ì·¡ ¿¹Ãø
# 1. ¸Ó½Å·¯´×(Á¤Çüµ¥ÀÌÅÍ)
#   - Æ®¸®±â¹Ý ¸ðµ¨
#   - È®·ü/Åë°è ¸ðµ¨
#     ...
#   - ½Å°æ¸Á ¸ðµ¨(µö·¯´×)
# 2. µö·¯´×(ºñÁ¤Çüµ¥ÀÌÅÍ)


