# [ ¿¬½À ¹®Á¦ ]
# subway2.csv ÆÄÀÏÀ» ÀĞ°í ¿ªº°, ½Ã°£´ëº° ½ÂÇÏÂ÷ÀÇ ÃÑ ÇÕÀ» Ãâ·Â
sub <- read.csv('subway2.csv', stringsAsFactors = F, skip=1,
                na.strings = '')
head(sub)

# 1) ¿ªÀÌ¸§ ºóÄ­ Ã¤¿ì±â(ÀÌÀü ¿ªÀÌ¸§ °¡Á®¿À±â)
library(zoo)
sub$ÀüÃ¼ <- na.locf(sub$ÀüÃ¼)

# 2) ¿ªÀÌ¸§¿¡¼­ È£¼± Á¤º¸ Áö¿ì±â
library(stringr)
str_remove_all('µ¿9¿î(4)','[(0-9)]')     #[] ¾È¿¡ µé¾î°¡¸é ÀÏ¹İ±âÈ£
str_remove_all('µ¿¿î(4)','([0-9])')      # ()°¡ Æ¯¼ö±âÈ£·Î ÇØ¼®
str_remove_all('µ¿9¿î(4)','\\([0-9]\\)')  # \\¸¦ »ç¿ëÇØ¾ß ÀÏ¹İ±âÈ£È­

unique(str_remove_all(sub$ÀüÃ¼, '\\([0-9]\\)'))
sub$ÀüÃ¼ <- str_remove_all(sub$ÀüÃ¼, '\\([0-9]\\)')

# 3) ¿ªº° ½ÂÇÏÂ÷º° ½Ã°£´ëº° ±×·ìÇÎ(ddply)
library(plyr)
ddply(sub, .(ÀüÃ¼,±¸ºĞ), summarise, v5=sum(X05.06),
                                    v6=sum(X06.07), 
                                    ........) # ¸ğµç ÄÃ·³ ³ª¿­ ºÒÆí

# 4) ½Ã°£´ë ÄÃ·³À» stack Ã³¸®
library(reshape2)
sub2 <- melt(sub, id.vars = c('ÀüÃ¼','±¸ºĞ'), 
             variable.name = '½Ã°£´ë', value.name = '½ÂÇÏÂ÷¼ö')

sub2$½Ã°£´ë <- as.numeric(str_sub(sub2$½Ã°£´ë, 2,3))

# 5) ¿ªº°, ½ÂÇÏÂ÷º°, ½Ã°£´ëº° ÃÑ ÇÕ
ddply(sub2, .(ÀüÃ¼,±¸ºĞ,½Ã°£´ë), summarise, cnt=sum(½ÂÇÏÂ÷¼ö))

# [ Âü°í - ¿ªº°, ½Ã°£´ëº° ÃÑ ÇÕ ]
ddply(sub2, .(ÀüÃ¼, ½Ã°£´ë), summarise, cnt=sum(½ÂÇÏÂ÷¼ö)) # long
dcast(sub2, ÀüÃ¼ ~ ½Ã°£´ë, sum)                            # wide


# doBy     : ~by (order, sample) »óÀ§
# plyr     : apply °è¿­ ÇÔ¼ö(Àû¿ëÇÔ¼ö) »óÀ§ 
# reshape2 : stack/unstack »óÀ§
# dplyr    : ±¸Á¶È­µÈ R ¹®¹ı Á¦°ø(sqlÃ³·³)

install.packages('dplyr')
library(dplyr)

# dplyrÀÇ ±¸Á¶È­µÈ ¹®¹ı
# 1. select : ÄÃ·³ÀÇ ¼±ÅÃ
# 2. mudate : ÄÃ·³ °¡°ø
# 3. filter : Çà ¼±ÅÃ
# 4. group_by : ±×·ì¿¬»ê
# 5. arrange : Á¤·Ä
# 6. summarise_each : ±×·ì¿¬»êÀÇ ½ÇÁ¦ ¿¬»ê Á¶°Ç

# ¿¹Á¦1) emp Å×ÀÌºí¿¡¼­ ÀÌ¸§, »ç¹ø, ¿¬ºÀ ¼±ÅÃ
emp <- read.csv('emp.csv', stringsAsFactors = F)

emp %>%                     # sql fromÀı Ã³·³ ¸ÕÀú µ¥ÀÌÅÍ ¼±ÅÃ ÈÄ ÁøÇà
  select(ENAME, EMPNO, SAL)

# ¿¹Á¦2) emp Å×ÀÌºí¿¡¼­ ÀÌ¸§, »ç¹ø, ¿¬ºÀ, 10% ÀÎ»óµÈ ¿¬ºÀ Ãâ·Â
emp %>%                     # sql fromÀı Ã³·³ ¸ÕÀú µ¥ÀÌÅÍ ¼±ÅÃ ÈÄ ÁøÇà
  select(ENAME, EMPNO, SAL) %>%
  mutate(new_sal=SAL*1.1)

# ÁÖÀÇ : ¹®¹ıÀû ¼ø¼­¿¡ µû¸¥ ÆÄ½Ì °¡´É ¿©ºÎ(ÄÃ·³ Á¤ÀÇ ¼ø¼­ ´Ş¶óÁü)
emp %>%                     
  select(ENAME, EMPNO) %>%   # ENAME, EMPNO¸¸ ´ÙÀ½ ¶óÀÎÀ¸·Î Àü´Ş
  mutate(new_sal=SAL*1.1)    # mutate¿¡¼­´Â SALÀ» ¾Ë ¼ö ¾øÀ½

emp %>%
  mutate(new_sal=SAL*1.1) %>%
  select(ENAME, EMPNO, new_sal) 
   

# ¿¹Á¦3) emp Å×ÀÌºí¿¡¼­ 10¹ø ºÎ¼­¿ø¿¡ ´ëÇÑ ÀÌ¸§, ºÎ¼­¹øÈ£, ¿¬ºÀ Ãâ·Â
emp %>%
  select(ENAME, DEPTNO, SAL) %>%
  filter(DEPTNO==10)

# ¿¹Á¦4) emp Å×ÀÌºí¿¡¼­ 10¹ø ºÎ¼­¿ø¿¡ ´ëÇÑ ÀÌ¸§, ºÎ¼­¹øÈ£, ¿¬ºÀ Ãâ·Â
# ´Ü, ¿¬ºÀ¼øÀ¸·Î Á¤·Ä
emp %>%
  select(ENAME, DEPTNO, SAL) %>%
  filter(DEPTNO==10) %>%
  arrange(desc(SAL))

# ¿¹Á¦5) emp Å×ÀÌºí¿¡¼­ °¢ ºÎ¼­º° Æò±Õ ¿¬ºÀ Ãâ·Â
emp %>%
  select(DEPTNO, SAL) %>%
  group_by(DEPTNO) %>%
  summarise_each(mean, SAL)


# ¿¹Á¦6) emp Å×ÀÌºí¿¡¼­ HIREDATE ÄÃ·³ Á¦¿Ü ÀüÃ¼ ¼±ÅÃ
emp[ , c('EMPNO','ENAME','JOB')]
emp[ , -5]
emp[ , colnames(emp) != 'HIREDATE']

emp %>%
  select(-HIREDATE) 

# [ ¿¬½À ¹®Á¦ ]
# student.csv ÆÄÀÏÀ» ÀĞ°í
std <- read.csv('student.csv', stringsAsFactors = F)

# 1. °¢ ÇĞ»ıÀÇ ÀÌ¸§, ÇĞ³â, ±³¼ö¹øÈ£ Ãâ·Â
std %>%
  select(NAME, GRADE, PROFNO)

# 2. À§ Á¤º¸¿¡ ±³¼ö¹øÈ£°¡ ¾ø´Â ÇĞ»ıÀº »ı·«
std %>%
  select(NAME, GRADE, PROFNO) %>%
  filter(!is.na(PROFNO))

# 3. À§ Á¤º¸¿¡ °¢ ÇĞ»ıÀÇ ¼ºº° ÄÃ·³ Ãß°¡ÇÏ¿© Ãâ·Â
std %>%
  select(NAME, GRADE, PROFNO, JUMIN) %>%
  filter(!is.na(PROFNO)) %>%
  mutate(v1=ifelse(str_sub(JUMIN,7,7)=='1','³²','¿©'))
  
std %>%
  filter(!is.na(PROFNO)) %>%
  mutate(v1=ifelse(str_sub(JUMIN,7,7)=='1','³²','¿©')) %>%
  select(NAME, GRADE, PROFNO, v1) 

# 4. À§ µ¥ÀÌÅÍ¿¡¼­ ÇĞ³âº° Á¤·Ä 
std %>%
  filter(!is.na(PROFNO)) %>%
  mutate(v1=ifelse(str_sub(JUMIN,7,7)=='1','³²','¿©')) %>%
  select(NAME, GRADE, PROFNO, v1) %>%
  arrange(GRADE, v1)
  
# 5. ÇĞ³âº° Å° Æò±Õ
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

# summarise_eachÀÇ across ´ëÃ¼
std %>%
  select(GRADE, HEIGHT, WEIGHT) %>%
  group_by(GRADE) %>%
  summarise(across(c(HEIGHT, WEIGHT), c(mean,max)))


# which.min, which.max 
# - ÃÖ´ë°ª°ú ÃÖ¼Ò°ªÀ» °®´Â index ¸®ÅÏ(À§Ä¡)

library(googleVis)
Fruits

df1 <- dcast(Fruits, Fruit ~ Year, value.var = 'Sales')
vord <- which.max(df1$`2008`)

df1[vord, 'Fruit']

# kimchi_test.csv ÆÄÀÏÀ» ÀĞ°í
df2 <- read.csv('kimchi_test.csv', stringsAsFactors = F)

# 1) 1¿ù ÃÑ°¢±èÄ¡ÀÇ ´ëÇü¸¶Æ® ÆÇ¸Å·®°ú ÆÇ¸Å±İ¾× Ãâ·Â(dplyr)
df2 %>%
  filter((ÆÇ¸Å¿ù==1) & (Á¦Ç°=='ÃÑ°¢±èÄ¡') & (ÆÇ¸ÅÃ³=='´ëÇü¸¶Æ®')) %>%
  select(¼ö·®, ÆÇ¸Å±İ¾×)

# 2) ³âµµº° ¿ùº° ÀüÃ¼ ÆÇ¸Å·®ÀÇ ÃÑ ÇÕ Ãâ·Â(dplyr)
df2 %>%
  select(ÆÇ¸Å³âµµ,ÆÇ¸Å¿ù,¼ö·®) %>%
  group_by(ÆÇ¸Å³âµµ,ÆÇ¸Å¿ù) %>%
  summarise_each(sum, ¼ö·®)

# 3) ³âµµº° ÆÇ¸Å·®ÀÌ °¡Àå ¸¹Àº ±èÄ¡ Ãâ·Â
# sol1) ddply - long data
df3 <- ddply(df2, .(ÆÇ¸Å³âµµ, Á¦Ç°), summarise, total=sum(¼ö·®))
ddply(df3, .(ÆÇ¸Å³âµµ), subset, total==max(total))

# sol2) which.max - wide data
df4 <- dcast(df2, Á¦Ç° ~ ÆÇ¸Å³âµµ, sum, value.var = '¼ö·®')


f1 <- function(x) {
  vord <- which.max(x)
  df4[vord,1]
}

f1(df4$`2013`)
f1(df4$`2014`)
f1(df4[,-1])             # 2Â÷¿ø Àû¿ë ºÒ°¡ 
apply(df4[,-1], 2, f1)

# str_extract_all(¹®ÀÚ¿­, ÆĞÅÏ)
# - ¿øÇÏ´Â ÆĞÅÏÀÇ ¹®ÀÚ¿­ ÃßÃâ
# - ÆĞÅÏ¿¡ Á¤±Ô½Ä Ç¥Çö °¡´É
# - ¸®½ºÆ® Ãâ·Â

# ¿¹Á¦) ´ÙÀ½ÀÇ ¿ªÀÌ¸§¿¡¼­ ¿ª ÀÌ¸§¸¸ ÃßÃâ
str_extract_all('À»Áö·Î2°¡(2)', '[°¡-ÆR]')
str_c(str_extract_all('À»Áö·Î2°¡(2)', '[:alpha:]')[[1]], collapse='')



