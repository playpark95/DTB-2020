setwd("D:/## DATA ##/DATA FROM SONY/Korea Univ/2018-1~/2020/2학기/강의 외/공모전/3차 본선 진행과정")
area<-read.csv("서울시 우리마을가게 상권분석서비스(상권영역).csv",header=T)
population<-read.csv("서울시 우리마을가게 상권분석서비스(상권-추정유동인구).csv",header=T)

population_rm<-population[,c(1:9)]
population_rm2<-subset(population_rm,population_rm$기준.년코드=="2018")
developed_area<-subset(population_rm2,population_rm2$X.상권_구분_코드=="D")
head(developed_area);dim(developed_area)
colnames(developed_area)<-c("기준년도","기준분기","상권구분코드","상권구분코드명","상권코드","상권코드명","총유동인구","유동인구(남성)","유동인구(여성)")
developed_area_rm<-developed_area[,c(1,2,4,6,7:9)]
developed_area_rm2<-subset(developed_area_rm,developed_area_rm$기준분기=="4")
head(developed_area_rm2);dim(developed_area_rm2)
unique(developed_area_rm2$상권코드명)
write.csv(developed_area_rm2,"서울시내 발달상권 유동인구(2018_4).csv")