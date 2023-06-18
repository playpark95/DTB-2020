setwd("C:/Users/playp/Desktop")
regression<-read.csv("regression2.csv",header=T)
head(regression)
regression_rm<-regression[,-c(1:3)]
regression_rm2<-regression_rm[,-7]
pairs(regression_rm2)
regression_rm3<-regression_rm2[,-3]
pairs(regression_rm3)
regression_rm4<-regression_rm3[,-3]
pairs(regression_rm4)
regression_rm4[,1]<-regression_rm4[,1]/1000    ##Y값이 지나치게 커서 가독성을 위해 1000으로 나눔
regression_rm4<-regression_rm4[-67,]
library(MASS)

full.model<-lm(profit_per_stores~.,data=regression_rm4)   ##가공된 데이터 바탕으로 탐색적데이터분석(EDA) 시작
par(mfrow=c(2,2))
plot(full.model)
car::vif(full.model)                                     #공선성 확인
shapiro.test(full.model$residuals)                       #잔차 정규성 가정 만족 여부 확인
temp1<-as.vector(scale(full.model$residuals))
head(sort(temp1,decreasing=T))
which(temp1>2)                                           #아웃라이어 식별
 
regression_rm5<-regression_rm4[-c(31,43,53),]            #아웃라이어 1차 제거 
full.model<-lm(profit_per_stores~.,data=regression_rm5)  #모형 2차 적합
plot(full.model)
shapiro.test(full.model$residuals)
temp1<-as.vector(scale(full.model$residuals))            #잔차 정규성 가정 만족 여부 확인
head(sort(temp1,decreasing=T))
which(temp1>2)
temp2<-fitted.values(full.model)                         #아웃라이어 추가 식별용
which(temp2>3300)

regression_rm6<-regression_rm5[-c(22,61),]               #아웃라이어 2차 제거
full.model<-lm(profit_per_stores~.,data=regression_rm6)  #모형 3차 적합
plot(full.model)
shapiro.test(full.model$residuals)                       #잔차 정규성 검정; 통과 (W=0.96661, p=0.07632; 5% 유의수준에서 만족)
car::vif(full.model)                                     #공선성 최종 확인

stepAIC(full.model,direction="both")                     #1차 변수 선택
rm.model1<-lm(formula = profit_per_stores ~ naver_ratings + naver_visitors + naver_blogs, data = regression_rm6)
plot(rm.model1)                                 
shapiro.test(rm.model1$residuals)

stepAIC(rm.model1,direction="both")
summary(rm.model1)

rm.model2<-lm(formula = profit_per_stores ~ naver_visitors + naver_blogs, data = regression_rm6)
   #rm.model1의 경우, naver_ratings 변수의 계수값이 통계적으로 유의하지 않게 나와 제거함
plot(rm.model2)
shapiro.test(rm.model2$residuals)       #다시 정규성 위반; 아웃라이어 제거 작업
summary(rm.model2)

temp1<-as.vector(scale(rm.model2$residuals))   #잔차 상 아웃라이어 잡아내기
head(sort(temp1,decreasing=T))
head(sort(temp1,decreasing=F))
which(temp1>1.8)

regression_rm7<-regression_rm6[-c(13,37),]    #아웃라이어 3차 제거
rm.model3<-lm(formula = profit_per_stores ~ naver_visitors + naver_blogs, data = regression_rm7)
plot(rm.model3)
shapiro.test(rm.model3$residuals)

temp1<-as.vector(scale(rm.model3$residuals))
head(sort(temp1,decreasing=T))
head(sort(temp1,decreasing=F))
which(temp1>1.8);which(temp1< -1.5)

regression_rm8<-regression_rm7[-c(23,28,42,48,51),]      #아웃라이어 최종 제거
rm.model4<-lm(formula = profit_per_stores ~ naver_visitors + naver_blogs, data = regression_rm8)
plot(rm.model4)
shapiro.test(rm.model4$residuals)

summary(rm.model4)       ##최종 모형

############################

info<-read.csv("info_final.csv",header=T)
info_rm<-info[,c(11,1,2,6,7)]
library(tidyverse)
temp1<-which(str_detect(info$name, "스타벅스")==TRUE)
starbucks<-info_rm[temp1,]
profit_starbucks<-1.5248*as.numeric(starbucks$naver_visitors)-2.3078*as.numeric(starbucks$naver_blogs)+2091.7537
cost<-read.csv("임대료 정보.csv",header=T)
colnames(cost)<-c("상권명","임대료")
land<-as.vector(scale(cost$임대료,center=1))
cost$land<-land
cost_starbucks<-c(rep(land[1],4),rep(land[2],3),land[3],rep(land[4],2),rep(land[5],3),rep(land[6],3),rep(land[8],2),land[7])
w.profit_starbucks<-profit_starbucks/cost_starbucks
temp1<-w.profit_starbucks[-c(2,15)]
w.profit_starbucks<-(w.profit_starbucks-mean(temp1))/sd(temp1)
starbucks$performance<-w.profit_starbucks
starbucks
subset(starbucks,starbucks$performance>=0)
subset(starbucks,starbucks$performance<0)

temp1<-which(str_detect(info$name, "투썸플레이스")==TRUE)
twosome<-info_rm[temp1,]
cost_twosome<-c(rep(land[1],2),land[2],land[3],land[9],rep(land[4],2),rep(land[5],2),land[8],land[7])
profit_twosome<-1.5248*as.numeric(twosome$naver_visitors)-2.3078*as.numeric(twosome$naver_blogs)+2091.7537
w.profit_twosome<-profit_twosome/cost_twosome
w.profit_twosome<-as.vector(scale(w.profit_twosome))
twosome$performance<-w.profit_twosome
twosome
subset(twosome,twosome$performance>=0)
subset(twosome,twosome$performance<0)

temp1<-which(str_detect(info$name, "커피빈")==TRUE)
coffeebean<-info_rm[temp1,]
coffeebean;cost
cost_coffeebean<-c(rep(land[1],2),land[3],rep(land[4],2),rep(land[5],2),land[6],land[8])
profit_coffeebean<-1.5248*as.numeric(coffeebean$naver_visitors)-2.3078*as.numeric(coffeebean$naver_blogs)+2091.7537
w.profit_coffeebean<-profit_coffeebean/cost_coffeebean
w.profit_coffeebean<-as.vector(scale(w.profit_coffeebean))
coffeebean$performance<-w.profit_coffeebean
coffeebean
subset(coffeebean,coffeebean$performance>=0)
subset(coffeebean,coffeebean$performance<0)

temp1<-which(str_detect(info$name, "이디야")==TRUE)
ediya<-info_rm[temp1,]
ediya;cost
cost_ediya<-c(rep(land[1],2),rep(land[2],3),land[4],rep(land[5],2),rep(land[6],2),land[7])
profit_ediya<-1.5248*as.numeric(ediya$naver_visitors)-2.3078*as.numeric(ediya$naver_blogs)+2091.7537
w.profit_ediya<-profit_ediya/cost_ediya
w.profit_ediya<-as.vector(scale(w.profit_ediya))
ediya$performance<-w.profit_ediya
ediya
subset(ediya,ediya$performance>=0)
subset(ediya,ediya$performance<0)

temp1<-which(str_detect(info$name, "메가커피")==TRUE)
mega<-info_rm[temp1,]
mega;cost
cost_mega<-c(rep(land[1],3),land[5])
profit_mega<-1.5248*as.numeric(mega$naver_visitors)-2.3078*as.numeric(mega$naver_blogs)+2091.7537
w.profit_mega<-profit_mega/cost_mega
w.profit_mega<-as.vector(scale(w.profit_mega))
mega$performance<-w.profit_mega
mega
subset(mega,mega$performance>=0)
subset(mega,mega$performance<0)

temp1<-which(str_detect(info$name, "매머드")==TRUE)
mammoth<-info_rm[temp1,]
mammoth;cost
cost_mammoth<-c(land[5],land[6],land[8],land[7])
profit_mammoth<-1.5248*as.numeric(mammoth$naver_visitors)-2.3078*as.numeric(mammoth$naver_blogs)+2091.7537
w.profit_mammoth<-profit_mammoth/cost_mammoth
w.profit_mammoth<-as.vector(scale(w.profit_mammoth))
mammoth$performance<-w.profit_mammoth
mammoth
subset(mammoth,mammoth$performance>=0)
subset(mammoth,mammoth$performance<0)

temp1<-which(str_detect(info$name, "빽다방")==TRUE)
paik<-info_rm[temp1,]
paik;cost
cost_paik<-c(land[2],land[3],land[9],land[4],land[5],land[5],land[6],land[7])
profit_paik<-1.5248*as.numeric(paik$naver_visitors)-2.3078*as.numeric(paik$naver_blogs)+2091.7537
w.profit_paik<-profit_paik/cost_paik
w.profit_paik<-as.vector(scale(w.profit_paik))
paik$performance<-w.profit_paik
paik
subset(paik,paik$performance>=0)
subset(paik,paik$performance<0)

result<-rbind(starbucks,twosome,coffeebean,ediya,mega,mammoth,paik)
grade<-ifelse(result$performance>=qnorm(0.7),"상",ifelse(result$performance<=qnorm(0.3),"하","중"))
result$grade<-grade
write.csv(result,"result.csv")

result_high<-rbind(twosome,coffeebean,ediya,paik)
result_high<-result_high[,-6]
profit.high<-c(profit_twosome,profit_coffeebean,profit_ediya,profit_paik)
cost.high<-c(cost_twosome,cost_coffeebean,cost_ediya,cost_paik)
w.profit.high<-profit.high/cost.high
temp1<-w.profit.high[-c(2,32)]
w.profit.high<-(w.profit.high-mean(temp1))/sd(temp1)   ##아웃라이어 제거, 추후 scale된값이 표준정규분포 따르게 만들기 위함.
result_high$performance<-w.profit.high

result_low<-rbind(mega,mammoth)
result_low<-result_low[,-6]
profit.low<-c(profit_mega,profit_mammoth)
cost.low<-c(cost_mega,cost_mammoth)
w.profit.low<-profit.low/cost.low
temp1<-w.profit.low[-3]
w.profit.low<-(w.profit.low-mean(temp1))/sd(temp1)
result_low$performance<-w.profit.low

result_final<-rbind(starbucks,result_high,result_low)
grade<-ifelse(result_final$performance>2,"최상",
              ifelse(result_final$performance>=qnorm(0.7)&result_final$performance<=2,"상",
                     ifelse(result_final$performance<= -2,"최하",
                            ifelse(result_final$performance>-2&result_final$performance<=qnorm(0.3),"하","중"))))
result_final$grade<-grade
write.csv(result_final,"result_final2.csv")

########
result2<-result[,-c(6,7)]
performance.total<-1.5248*as.numeric(result2$naver_visitors)-2.3078*as.numeric(result2$naver_blogs)+2091.7537
cost.total<-c(cost_starbucks,cost_twosome,cost_coffeebean,cost_ediya,cost_mega,cost_mammoth,cost_paik)
w.profit_total<-performance.total/cost.total
w.profit_total<-as.vector(scale(w.profit_total))
result2$performance<-w.profit_total
grade<-ifelse(result2$performance>=qnorm(0.7),"상",ifelse(result2$performance<=qnorm(0.3),"하","중"))
result2$grade<-grade
write.csv(result2,"result2.csv")