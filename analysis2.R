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


#분노 1 4문항 User A base R CFI 1 RMSEA 0 SRMR .022 가벼운 화 ? 일종의 예민... (화가 많은데 못참음)
head(dat[c(150:158,160:163)])
mod<-'HS=~h1+h3R+h5+h12R'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)
semPaths(fit)
#분노 2 4문항 뭔가 내재된 분노..(화가 많은데 억누름)
# USER A base R CFI .997 RMSEA .036 SRMR .027
head(dat[c(150:158,160:163)])
mod<-'HS=~h2+h8+h9+h11'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)
semPaths(fit)
#분노 2요인 모형 User A base R CFI .974 RMSEA .062 SRMR .062
head(dat[c(150:158,160:163)])
mod<-'BN1=~h1+h3R+h5+h12R
  BN2=~h2+h8+h9+h11'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#우호 1 협동 4문항
#USER A BASE R CFI 1 RMSEA 0 SRMR .017
head(dat[c(197,223,227,234,235,241,346,351,356,357,362,364,371,378,379,386)])
mod<-'UH1=~q6R+q41+q11+q19'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#우호 2 인식 4문항
# USER A BASE R CFI 1 RMSEA 0 SRMR .024
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
semPaths(fit)
#경조 아이디어 4문항
#USER A BASE R CFI 1 RMSEA 0 SRMR .009
head(dat[c(325,326,328:345,228,229)])
mod<-'KJidea=~p2+p8+p9+p12'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#경조 폭발력 4문항
#USER A BASE R CFI 1 RMSEA 0 SRMR .034
head(dat[c(325,326,328:345,228,229)])
mod<-'KJPB=~p10+p13+p18+p21+j39'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#경조 2요인 User A base R CFI .992 RMSEA .018 SRMR .056
head(dat[c(325,326,328:345,228,229)])
mod<-'KJidea=~p2+p8+p9+p12
  KJPB=~p10+p13+p18+j39'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

semPaths(fit)
#집중력 5문항 User A base R CFI 1 RMSEA 0 SRMR .024
head(dat[c(26:31)])
mod<-'KJidea=~a26+a27+a29+a30+a31R'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#인지 5문항 User A base R CFI .959 RMSEA .077 SRMR .053
head(dat[c(402:409)])
mod<-'KJidea=~t1+t2+t3+t4R+t5R'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#감정(기복?예민?민감?) 4문항 User A base R CFI .988 RMSEA .078 SRMR .028 
head(dat[c(63:66,68:70,191,210,218,219,224)])
mod<-'KJidea=~b32R+b39+j2+j21'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#목표지향 5문항 User A base R CFI 1 RMSEA 0 SRMR .03
head(dat[c(108,122,102,106,114,121,255)])
mod<-'KJidea=~e10+e8R+e16R+e23R+j66R'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#자기의식 4문항 User A base R CFI .991 RMSEA .059 SRMR .029
head(dat[c(123,124,126,127,128,194,308,310)])
mod<-'KJidea=~f4+f6+o2+o4'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#개방행동 4문항 User A base R CFI .988 RMSEA .065 SRMR .03
head(dat[c(73:75,77:78)])
mod<-'KJidea=~b42+b43+b46+b47'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)

#접근기질 4문항 User A base R CFI .976 RMSEA .109 SRMR .034
head(dat[c(7:12)])
mod<-'KJidea=~a8+a9+a10+a11'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE,standardized=TRUE)
modindices(fit,sort=TRUE)




#활동수준 4문항 User R base R CFI .956 RMSEA .076 SRMR .053
head(dat[c(1:6)])
mod<-'HDSJ=~a1+a3R+a4+a5+a6'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE)
modindices(fit,sort=TRUE)

#외로움 4문항 User A base R CFI 1 RMSEA 0 SRMR .024
head(dat[c(195,196,208,243)])
mod<-'HDSJ=~j6+j7+j19+j54'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE)
modindices(fit,sort=TRUE)

#유머 3문항 안잡힘
head(dat[c(315,316,319,323)])
mod<-'HDSJ=~o9+o10+o13'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE)
modindices(fit,sort=TRUE)

#외향(자극추구) 5문항 User A base R CFI 1 RMSEA 0 SRMR .023
head(dat[c(350,354,358,363,369,372,382)])
mod<-'HDSJ=~q5+q13+q18+q27+q37'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE)
modindices(fit,sort=TRUE)

#자기애 5문항 User A base R CFI 1 RMSEA 0 SRMR .035
head(dat[c(190,198,297:306)])
mod<-'HDSJ=~j1+j9+n1+n6+n8'
fit<-cfa(mod,dat, std.lv=TRUE)
summary(fit,fit.measures=TRUE)
modindices(fit,sort=TRUE)


#자기주장 6문항 User A base R CFI 1 RMSEA 0 SRMR .037
mod<-'jujang=~e6+j12R+j16+j37+j62R+j67R'
fit<-cfa(mod,dat, std.lv=TRUE,missing="fiml")
summary(fit,fit.measures=TRUE)
modindices(fit,sort=TRUE)

#성실(계획) 5문항
head(dat[c(214,216,217,232,246,348,349,352,355,360,366,368,376,377,380,383)])
mod<-'SS=~j25+j43R+q3R+q4+q35R'
fit<-cfa(mod,dat, std.lv=TRUE,missing="fiml")
summary(fit,fit.measures=TRUE)
modindices(fit,sort=TRUE)

#성실 q7 q15 시간엄수
mod<-'SS=~j25+q3R+q4+q31'
fit<-cfa(mod,dat, std.lv=TRUE,missing="fiml")
summary(fit,fit.measures=TRUE)
modindices(fit,sort=TRUE)
fa(dat[c(214,216,217,232,246,348,349,352,355,360,366,368,376,377,380,383)],nfactors=1,rotate="varimax")
fa(dat[c(216,217,246,352,355,360,366,368,376,377,383)],nfactors=1,rotate="varimax")
#q7 q15 q21 q23R 
#j27 j28 q10
#j25 j43R j57R q3R q4 q35R