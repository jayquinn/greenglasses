library("psych")
library(dplyr)
dat<-read.csv("C:/git/greenglasses/data120.csv",header = T)
#미싱 데이터는 엑셀 단위에서 처리하였음
#### 역문항 처리 ####
dat<-as_tibble(dat)
rnumbers<-grep("R",colnames(dat))
dat[rnumbers]<-6-dat[rnumbers]
#### 알파 분석 ####
alpha(dat[262:280]) #가격정보 .65
alpha(dat[c(63:70,191,210,218,219,224)]) #감정 .77
alpha(dat[c(359,373,374,385,347,361)])#개방 .63
alpha(dat[c(83:87,199)]) #개방가치 -.08
alpha(dat[79:81]) #개방사고 .63
alpha(dat[c(73:75,77:78)]) #개방행동 .68 제외:71,72,76
alpha(dat[c(325,326,328:345,228,229)])#경조 제외: 327
alpha(dat[13:18]) #경직기질 .5
alpha(dat[19:25]) #기분긍정 .87
alpha(dat[c(94:98,213)]) #긴장 .4
alpha(dat[c(101,109,115,212,247,248,211,103)]) #독특 .53 제외: 99, 105
alpha(dat[c(441:445)]) #동정 .55
alpha(dat[c(410:412,414)]) #모호 .51 제외:413
alpha(dat[c(108,122,102,106,114,121,255)]) #목표지향 .69 제외: 100
alpha(dat[c(395,400,282,394)]) #무기력 .033
alpha(dat[c(146:149)]) #물질지지 .75
alpha(dat[c(40:62)])#미적정서 .87 제외:39
alpha(dat[c(419:429)]) #반응 .78 ## 알파는 높은데 대체 뭘 측정하는건지 감이 안온다..
alpha(dat[c(150:158,160:163)]) #분노 .8 제외:159
alpha(dat[c()]) #
alpha(dat[c()]) #
alpha(dat[c()]) #
alpha(dat[c()]) #
alpha(dat[c()]) #
alpha(dat[c()]) #

alpha(dat[1:31]) #기질차원척도 .69
alpha(dat[1:6]) #활동수준 .68
alpha(dat[7:12]) #접근기질 .68


alpha(dat[26:31]) #집중력 .71

alpha(dat[32:87]) #개방성 척도

