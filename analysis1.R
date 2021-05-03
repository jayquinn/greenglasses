library("psych")
library(dplyr)
library(mirt)
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
gzirt<-dat[19:25]
model.pcm <- 'F1 = 1-7' 
results.pcm <- mirt(data=gzirt, model=model.pcm, itemtype="Rasch", SE=TRUE, verbose=FALSE)
coef.pcm <- coef(results.pcm, IRTpars=TRUE, simplify=TRUE)
items.pcm <- as.data.frame(coef.pcm$items)
print(items.pcm)
summary(results.pcm)
plot(results.pcm, type = 'trace', which.items = c(1:7))


alpha(dat[c(94:98,213)]) #긴장 .4
alpha(dat[c(101,109,115,212,247,248,211,103)]) #독특 .53 제외: 99, 105
alpha(dat[c(441:445)]) #동정 .55
alpha(dat[c(410:412,414)]) #모호 .51 제외:413
alpha(dat[c(108,122,102,106,114,121,255)]) #목표지향 .69 제외: 100
alpha(dat[c(395,400,282,394)]) #무기력 .033
alpha(dat[c(146:149)]) #물질지지 .75
alpha(dat[c(40:62)])#미적정서 .87 제외:39
alpha(dat[c(419:429)]) #반응 .78 ## 알파는 높은데 대체 뭘 측정하는건지 감이 안온다..
alpha(dat[c(150:158,160:163)]) #분노 .80 제외:159
alpha(dat[c(209,398,401)]) #비난경향성 .43
alpha(dat[c(214,216,217,232,246,348,349,352,355,360,366,368,376,377,380,383)]) #성실 .71 제외:365,242
alpha(dat[c(257,261,281)]) #성적 개방성 .36
alpha(dat[c(312,314,321)]) #승화 .24
alpha(dat[c(367,370,387,353)]) #신경 .2
alpha(dat[c(416,418,425)]) #신뢰 .13
alpha(dat[c(200,202,203)]) #안믿어 .21
alpha(dat[c(110,118,245,253)]) #안정.54 제외:117
alpha(dat[c(88,89,92,93)]) #야망 .40
alpha(dat[c(311,313,317,322)]) #억제 .44
alpha(dat[c(288:291)]) #연인불안 .82
alpha(dat[c(292:296)]) #연인붙임 .74
alpha(dat[c(284:286)]) #연인신뢰 .78 제외:283,287
alpha(dat[c(167,168,183,186,225,171,180)]) #완벽사회 .7
alpha(dat[c(164,169,170,173,175,177,179,182)]) #완벽자기 .69(다 넣으면 .6정도) 제외 222,185,396,188,184,174,397,187
alpha(dat[c(165,166,172,176,178,181,189)]) #완벽타인 .48
alpha(dat[c(195,196,208,243)]) #외로움 .65
alpha(dat[c(350,354,358,363,369,372,382)]) #외향 .64
alpha(dat[c(197,223,227,234,235,241,346,351,356,357,362,364,371,378,379,386)]) #우호.78 제외:375,381
alpha(dat[c()]) #
alpha(dat[c()]) #
alpha(dat[c()]) #


alpha(dat[1:31]) #기질차원척도 .69
alpha(dat[1:6]) #활동수준 .68
alpha(dat[7:12]) #접근기질 .68


alpha(dat[26:31]) #집중력 .71

alpha(dat[32:87]) #개방성 척도

