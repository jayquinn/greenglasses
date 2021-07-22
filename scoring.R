library("psych")
library(dplyr);library(lavaan); library(semPlot)
library(mirt)
dat<-read.csv("C:/git/greenglasses/data120.csv",header = T)
#미싱 데이터는 엑셀 단위에서 처리하였음
#### 역문항 처리 ####
dat<-as_tibble(dat)
rnumbers<-grep("R",colnames(dat))
dat[rnumbers]<-6-dat[rnumbers]
#### 해당 문항 추출
score<-dat[,c("b32R","b39",	"j2",	"j21",	"b42",	"b43",	"b46",	"b47",	"p2",	"p8",	"p9",	"p12",	"p10",	"p13",	"p18",	"p21",	"j39",	"a19",	"a20R",	"a22R",	"a24",	"a25",	"e10",	"e8R",	"e16R",	"e23R",	"j66R",	"b14",	"b20",	"b21",	"b22R",	"b31",	"h2",	"h8",	"h9",	"h11",	"h1",	"h3R",	"h5",	"h12R",	"j25",	"j43R",	"q3R",	"q4",	"q35R",	"m6",	"m7",	"m8",	"m9",	"m10",	"m11R",	"m12R",	"m13",	"m14R",	"m2",	"m3",	"m4",	"j6",	"j7",	"j19",	"j54",	"q5",	"q13",	"q18",	"q27",	"q37",	"q1",	"q26",	"q33R",	"j38",	"q6R",	"q41",	"q11",	"q19",	"o9",	"o10",	"o13",	"t1",	"t2",	"t3",	"t4R",	"t5R",	"i4",	"i5",	"i23",	"j36",	"i6",	"i12",	"i16",	"i19",	"j1",	"j9",	"n1",	"n6",	"n8",	"f4",	"f6",	"o2",	"o4",	"e6",	"j12R",	"j16",	"j37",	"j62R",	"j67R",	"a8",	"a9",	"a10",	"a11",	"a26",	"a27",	"a29",	"a30",	"a31R",	"g18",	"g19",	"g20",	"g21",	"g14",	"g15",	"g16",	"g17",	"g1",	"g2",	"g4",	"g5",	"g6",	"g8",	"g10",	"g11",	"g12",	"g13",	"b6",	"j47",	"j48",	"j49",	"j50",	"j51",	"a1",	"a3R",	"a4",	"a5",	"a6")]
score<-as.matrix(score)
a1<-1:4
a2<-5:8
a3<-9:12
a4<-13:17
a5<-18:22
a6<-23:27
a7<-28:32
a8<-33:36
a9<-37:40
a10<-41:45
a11<-46:49
a12<-50:54
a13<-55:57
a14<-58:61
a15<-62:66
a16<-67:70
a17<-71:74
a18<-75:77
a19<-78:82
a20<-83:86
a21<-87:90
a22<-91:95
a23<-96:99
a24<-100:105
a25<-106:109
a26<-110:114
a27<-115:118
a28<-119:122
a29<-123:127
a30<-128:132
a31<-133:138
a32<-139:143
eye<-list(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32)


z<-score[,47:50]
mean(apply(z,1,sum,na.rm=T))
sd(apply(z,1,sum,na.rm=T))
apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)]

z<-score[,51:55]
mean(apply(z,1,sum,na.rm=T))
sd(apply(z,1,sum,na.rm=T))
apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)]

z<-score[,56:58]
mean(apply(z,1,sum,na.rm=T))
sd(apply(z,1,sum,na.rm=T))
apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)]


z<-score[,59:62]
mean(apply(z,1,sum,na.rm=T))
sd(apply(z,1,sum,na.rm=T))
apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)]


z<-score[,63:67]
mean(apply(z,1,sum,na.rm=T))
sd(apply(z,1,sum,na.rm=T))
apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)]

z<-score[,68:71]
mean(apply(z,1,sum,na.rm=T))
sd(apply(z,1,sum,na.rm=T))
apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)]

z<-score[,72:75]
mean(apply(z,1,sum,na.rm=T))
sd(apply(z,1,sum,na.rm=T))
apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)]

z<-score[,76:78]
mean(apply(z,1,sum,na.rm=T))
sd(apply(z,1,sum,na.rm=T))
apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)]

z<-score[,79:83]
mean(apply(z,1,sum,na.rm=T))
sd(apply(z,1,sum,na.rm=T))
apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)]

z<-score[,84:87]
mean(apply(z,1,sum,na.rm=T))
sd(apply(z,1,sum,na.rm=T))
apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)]

z<-score[,88:91]
mean(apply(z,1,sum,na.rm=T))
sd(apply(z,1,sum,na.rm=T))
apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)]

z<-score[,92:96]
mean(apply(z,1,sum,na.rm=T))
sd(apply(z,1,sum,na.rm=T))
apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)]

z<-score[,97:100]
mean(apply(z,1,sum,na.rm=T))
sd(apply(z,1,sum,na.rm=T))
apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)]

z<-score[,101:106]
mean(apply(z,1,sum,na.rm=T))
sd(apply(z,1,sum,na.rm=T))
apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)]

z<-score[,107:110]
mean(apply(z,1,sum,na.rm=T))
sd(apply(z,1,sum,na.rm=T))
apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)]

z<-score[,111:115]
mean(apply(z,1,sum,na.rm=T))
sd(apply(z,1,sum,na.rm=T))
apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)]

z<-score[,116:119]
mean(apply(z,1,sum,na.rm=T))
sd(apply(z,1,sum,na.rm=T))
apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)]

z<-score[,120:123]
mean(apply(z,1,sum,na.rm=T))
sd(apply(z,1,sum,na.rm=T))
apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)]

z<-score[,124:128]
mean(apply(z,1,sum,na.rm=T))
sd(apply(z,1,sum,na.rm=T))
apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)]

z<-score[,129:133]
mean(apply(z,1,sum,na.rm=T))
sd(apply(z,1,sum,na.rm=T))
apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)]

z<-score[,134:139]
mean(apply(z,1,sum,na.rm=T))
sd(apply(z,1,sum,na.rm=T))
apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)]

z<-score[,140:143]
mean(apply(z,1,sum,na.rm=T))
sd(apply(z,1,sum,na.rm=T))
apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)]


for(i in eye) {
  z<-score[,i]
  print(apply(z,1,sum,na.rm=T)[c(3:5,10,11,15:20,32,35,37,39,41,61,81,89,98,101,103,110,114:117,119,120)])
}
