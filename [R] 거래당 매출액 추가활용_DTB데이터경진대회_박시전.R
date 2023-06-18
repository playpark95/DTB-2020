setwd("C:/Users/playp/Desktop")
profit_data<-read.csv("profit_data.csv",header=T)
profit_data_rm<-profit_data[,c(2,6)]
temp1<-which(profit_data_rm$신우편번호 %in% result_final$code)
profit_data_rm<-profit_data_rm[temp1,]
profit_data_rm[,2]<-gsub(",","",profit_data_rm[,2],fixed=T)
head(profit_data_rm)
profit_data_rm[,1]<-as.numeric(profit_data_rm[,1])
profit_data_rm[,2]<-as.numeric(profit_data_rm[,2])
colnames(profit_data_rm)<-c("code","profit_per_cases")
profit_data_rm<-profit_data_rm[order(profit_data_rm$code),]
result_final<-result_final[order(result_final$code),]
result_final2<-merge(result_final,profit_data_rm,by="code")
head(result_final2)
write.csv(result_final2, "result_final2.csv")
head(result_final)
