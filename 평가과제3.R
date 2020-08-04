# 1. project_songpa_data.csv ÆÄÀÏÀ» ÀÐ°í
# µ¿º° LAT¿Í LONÀÇ ÃÖ¼Ò°ªÀ» ±¸ÇÏ¼¼¿ä.
# ´Ü, µ¿ ÀÌ¸§Àº ´ÙÀ½°ú °°ÀÌ °áÇÕÇÏ¿© Ç¥Çö
# 
# Ç³³³1µ¿ 124-5  => Ç³³³µ¿
# ÀåÁöµ¿ÅÃÁö°³¹ßÁö±¸ => ÀåÁöµ¿
# °Å¿©µ¿ 136-106È£ ¸ÂÀºÆí => °Å¿©µ¿
# Àá½Ç³ª·ç¿ª => Àá½Ç³ª·ç¿ª

# ÆÄÀÏ ºÒ·¯¿À±â
df2 <- read.csv('project_songpa_data.csv', stringsAsFactors = F)

# ÄÃ·³Á¤¸®
df2 <- df2[,-1]

# µ¿¿¡¼­ ÇÑ±Û¸¸ °¡Á®¿À±â
f1 <- function(x) {
  str_c(str_extract_all(x, '[°¡-ÆR]')[[1]], collapse = '')
}

df2$name <- sapply(df2$name, f1)

# µ¿¿¡¼­ ºÒÇÊ¿äÇÑ ±Û Áö¿ì±â
f2 <- function(x) {
  str_replace_na(str_sub(x, 1, str_locate(x,'µ¿')[1]), x)
}

df2$name <- sapply(df2$name, f2)

# ÃÖ¼Ò°ª ±¸ÇÏ±â
aggregate(cbind(LAT, LON) ~ name, df2, min)          ## °á°ú ³ª¿È
