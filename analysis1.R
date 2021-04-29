library("psych")
library(dplyr)
dat<-read.csv("C:/git/greenglasses/data120.csv",header = T)
#미싱 데이터는 엑셀 단위에서 처리하였음
#### 역문항 처리 ####
dat<-as_tibble(dat)
rnumbers<-grep("R",colnames(dat))
dat[rnumbers]<-6-dat[rnumbers]
edit(dat)
#### 알파 분석 ####
alpha(dat[1:6]) #활동수준
alpha(dat[c(325,326,328:345,228,229)])#경조 p3R제거
#어떻게 통합되는지 확인
