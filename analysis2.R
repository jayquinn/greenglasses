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

#기분긍정 5문항 User A base R CFI 1 RMSEA 0 SRMR .02 MI NA
#제외: a21,a23
head(dat[19:25])
mod<-'GBGJ=~a19+a20R+a22R+a24+a25'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)
model.pcm <- 'F1 = 1-5' 
results.pcm <- mirt(dat[c(19,20,22,24,25)],model.pcm,itemtype="Rasch", SE=TRUE, verbose=FALSE)
itemfit(results.pcm,'infit',na.rm=TRUE)

#미적정서 5문항 User A base R CFI .996 RMSEA .033 SRMR .038 MI NA
#MI issue, 제외: b9R,b10R,b17,b11R,b30,b29,b24R,b25R,b12R,b13R,b15,b16,b18,b19,b28R
head(dat[c(40:56,59:62)])
mod<-'MJJS=~b14+b20+b21+b22R+b31'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#친구(정서지지) 5문항, User A Base R CFI .997 RMSEA .033
#제외: g3,g6
head(dat[c(129:135)])
mod<-'JSGG=~g1+g2+g4+g5+g6'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#친구(정보지지) 4문항, User A Base R CFI 1 RMSEA 0
head(dat[c(142:145)])
mod<-'JBGG=~g14+g15+g16+g17'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#친구(평가지지) 5문항, User A base R CFI .999 RMSEA .012
#제외: g9
head(dat[c(136:141)])
mod<-'PGGG=~g8+g10+g11+g12+g13'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)


#친구(물질지지) 4문항, User A Base R CFI 1 RMSEA .004
head(dat[c(146:149)])
mod<-'MGGG=~g18+g19+g20+g21'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)


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

