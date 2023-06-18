setwd("D:/DATA FROM SONY/Korea Univ/2018-1~/2020/2학기/강의 외/공모전/3차 본선 진행과정/임시")
developed<-read.csv("developed.csv",header=T)
dim(developed);length(unique(developed$상권명))
developed_rank1<-developed[order(developed$유동인구합,decreasing=T),]
developed_rank2<-developed[order(developed$구획당유동인구,decreasing=T),]
head1<-head(developed_rank1,10);head2<-head(developed_rank2,10)
