install.packages("reshape2")
install.packages("pracma")
library(ggplot2)
#library(reshape2)
library(pracma)
c_googl$EMAClose<-movavg(c_googl$Close,n=2,type='e')
ggplot(c_googl, aes(Date,group=1)) +geom_line(aes(y=Close),colour ="red")+geom_line(aes(y=EMAClose),colour ="green")+ggtitle("Actual(red colour) v/s Predicted(green colour) Closing values for Google stocks")

C_amzn$EMAClose<-movavg(C_amzn$Close,n=2,type='e')
ggplot(C_amzn, aes(Date,group=2)) +geom_line(aes(y=Close),colour ="red")+geom_line(aes(y=EMAClose),colour ="green")+ggtitle("Actual(red colour) v/s Predicted(green colour) Closing values for Amazon stocks")

C_ibm$EMAClose<-movavg(C_ibm$Close,n=2,type='e')
ggplot(C_ibm, aes(Date,group=3)) +geom_line(aes(y=Close),colour ="red")+geom_line(aes(y=EMAClose),colour ="green")+ggtitle("Actual(red colour) v/s Predicted(green colour) Closing values for IBM stocks")

C_msft$EMAClose<-movavg(C_msft$Close,n=2,type='e')
ggplot(C_msft, aes(Date,group=4)) +geom_line(aes(y=Close),colour ="red")+geom_line(aes(y=EMAClose),colour ="green")+ggtitle("Actual(red colour) v/s Predicted(green colour) Closing values for Microsoft stocks")

c_nike$EMAClose<-movavg(c_nike$Close,n=2,type='e')
ggplot(c_nike, aes(Date,group=5)) +geom_line(aes(y=Close),colour ="red")+geom_line(aes(y=EMAClose),colour ="green")+ggtitle("Actual(red colour) v/s Predicted(green colour) Closing values for Nike stocks")

cor(c_googl$Close,c_googl$EMAClose)
cor(C_amzn$Close,C_amzn$EMAClose)
cor(C_ibm$Close,C_ibm$EMAClose)
cor(C_msft$Close,C_msft$EMAClose)
cor(c_nike$Close,c_nike$EMAClose)