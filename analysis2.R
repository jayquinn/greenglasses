library("psych")
library(dplyr);library(lavaan); library(semPlot)
library(mirt)
dat<-read.csv("C:/git/greenglasses/data120.csv",header = T)
#미싱 데이터는 엑셀 단위에서 처리하였음
#### 역문항 처리 ####
dat<-as_tibble(dat)
rnumbers<-grep("R",colnames(dat))
dat[rnumbers]<-6-dat[rnumbers]

#일단 기준치까지 내려가고 문항 수 줄이는건 그 다음에 생각

#기분긍정 5문항 User A base R CFI 1 RMSEA 0 SRMR .02
#제외: a21,a23
head(dat[19:25])
mod<-'GBGJ=~a19+a20R+a22R+a24+a25'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)
model.pcm <- 'F1 = 1-5' 
results.pcm <- mirt(dat[c(19,20,22,24,25)],model.pcm,itemtype="Rasch", SE=TRUE, verbose=FALSE)
itemfit(results.pcm,'infit',na.rm=TRUE)

#미적정서 5문항 User A base R CFI .996 RMSEA .033 SRMR .038
#MI issue, 제외: b9R,b10R,b17,b11R,b30,b29,b24R,b25R,b12R,b13R,b15,b16,b18,b19,b28R
head(dat[c(40:56,59:62)])
mod<-'MJJS=~b14+b20+b21+b22R+b31'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#친구(정서지지) 5문항, User A Base R CFI .997 RMSEA .033 SRMR .028
#제외: g3,g6
head(dat[c(129:135)])
mod<-'JSGG=~g1+g2+g4+g5+g6'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#친구(정보지지) 4문항, User A Base R CFI 1 RMSEA 0 SRMR .007
head(dat[c(142:145)])
mod<-'JBGG=~g14+g15+g16+g17'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#친구(평가지지) 5문항, User A base R CFI .999 RMSEA .012 SRMR .035
#제외: g9
head(dat[c(136:141)])
mod<-'PGGG=~g8+g10+g11+g12+g13'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)


#친구(물질지지) 4문항, User A Base R CFI 1 RMSEA .004 SRMR .022
head(dat[c(146:149)])
mod<-'MGGG=~g18+g19+g20+g21'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#연인(연인불안) 4문항, User A base R CFI 1 RMSEA 0 SRMR .011
head(dat[c(288:291)])
mod<-'YYBA=~m6+m7+m8+m9'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#연인(연인신뢰) 3문항, User R base R CFI 1 RMSEA 0 SRMR 0
head(dat[c(284:286)])
mod<-'YYSR=~m2+m3+m4'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#연인(연인붙임) 5문항, User R base R CFI .943 RMSEA .108 SRMR .051
head(dat[c(292:296)])
mod<-'YYBY=~m10+m11R+m12R+m13+m14R'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#자기(완벽사회) 4문항, User A base R CFI .996 RMESA .035 SRMR .026
head(dat[c(167,168,183,186,225,171,180)])
mod<-'YBSH=~i4+i5+i23+j36'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#자기(완벽자기) 4문항 User A base R CFI .998 RMSEA .028 SRMR .029
head(dat[c(164,169,170,173,175,177,179,182)])
mod<-'YBJG=~i6+i12+i16+i19'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#자기 합체맨 쉣
head(dat[c(167,168,183,186,225,171,180,164,169,170,173,175,177,179,182)])
mod<-'YBSH=~i4+i5+i23+j36
  YBJG=~i6+i12+i16+i19'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#환상 6문항 User A Base R CFI 1 RMSEA 0 SRMR .031
head(dat[c(32:38,236:240)])
mod<-'HS=~b6+j47+j48+j49+j50+j51'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)


#분노 1 4문항 가벼운 화 ? 일종의 예민... (화가 많은데 못참음)
head(dat[c(150:158,160:163)])
mod<-'HS=~h1+h3R+h5+h12R'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)
#분노 2 4문항 뭔가 내재된 분노..(화가 많은데 억누름)
head(dat[c(150:158,160:163)])
mod<-'HS=~h2+h8+h9+h11'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)
#분노 2요인 모형 User A base R CFI .974 RMSEA .062 SRMR .062
head(dat[c(150:158,160:163)])
mod<-'BN1=~h1+h3R+h5+h12R
  BN2=~h2+h8+h9+h11'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#우호 1 협동 4문항
head(dat[c(197,223,227,234,235,241,346,351,356,357,362,364,371,378,379,386)])
mod<-'UH1=~q6R+q41+q11+q19'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#우호 2 인식 4문항
head(dat[c(197,223,227,234,235,241,346,351,356,357,362,364,371,378,379,386)])
mod<-'UH2=~q1+q26+q33R+j38'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#우호 2요인 8문항 User A base R CFI .988 RMSEA .023 SRMR .058
head(dat[c(197,223,227,234,235,241,346,351,356,357,362,364,371,378,379,386)])
mod<-'UH1=~q6R+q41+q11+q19
  UH2=~q1+q26+q33R+j38
  q11 ~~ q33R'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)
########################################################




fa(dat[c(197,223,227,234,235,241,346,351,356,362,364,371,378,379,386)],nfactors=2,rotate="varimax")

#활동수준 User R base R CFI .913 RMSEA .088 MI NA
head(dat[c(1:6)])
mod<-'HDSJ=~a1+a2+a3R+a4+a5+a6'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE)
modindices(fit,sort=TRUE)

#자기주장 User A base R CFI 1 RMSEA 0 SRMR .037 MI NA
mod<-'jujang=~e6+j12R+j16+j37+j62R+j67R'
fit<-cfa(mod,dat, std.lv=TRUE,missing="fiml")
summary(fit,fit.measures=TRUE)
modindices(fit,sort=TRUE)

