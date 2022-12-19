#reading the entire dataset
df=read.csv("D:\\VIT\\SEM 5\\Data Analytics\\J component\\dataset\\Extracted\\c_ibm.csv")
df

library(timeDate)
library(timeSeries)
library(quantmod)
library(forecast)
library(TTR)
library(xts)
library(zoo)

dfI = xts(df$Close, order.by=as.Date(df$Date))
dfI
class(dfI)
plot(dfI)

#For arima model you need P,D,Q values
#graph the ACF and PACF values -> ACF=Q and PACF=P
par(mfrow=c(1,2))
Acf(dfI,main="ACE for Differenced Series")    #gives q value
#here we observe that the first value is 1 and trends downwards 

Pacf(dfI, main = "PACF for Differeced Series")   #->gives p value
#here again we observe a lag at 1 which gives us the p-value of 1

#log residuals to eliminate the areas that are non-stationary
#making the data more stable
logs=diff(log(dfI),lag=1)
logs=logs[!is.na(logs)]            #residual -> observed value - predicted value

#plot the logs
par(mfrow=c(1,1))
plot(logs,type='l',main='log return plot')
auto.arima(logs,seasonal = FALSE)  #->(0,0,0)
str(logs)

#Divide the dataset into training and testing
sample_size=floor(0.80*nrow(logs))
set.seed(150)
train_indices <- sample(seq_len(nrow(logs)),size = sample_size)

train <- logs[train_indices,]
test <- logs[-train_indices]
test

par(mfrow=c(1,2))
Acf(train,main="ACE for Differenced Series") 
Pacf(train, main = "PACF for Differeced Series") 
auto.arima(train,seasonal = FALSE) #2,0,2
#pdq -> 0,0,7

#plot the models, get accuracy and draw conclusions
fit1<-auto.arima(train,seasonal = FALSE)
tsdisplay(residuals(fit1),lag.max = 40,main = '(0,0,0) Model Residuals')

fit2=arima(train,order = c(0,0,9))
tsdisplay(residuals(fit2),lag.max = 40,main="(0,0,7) Model Residuals")
#if it busts out the blue line earlier its a weak model, if it doesn't its a strong model

#ORIGINAL DATA auto.arima
fitA=auto.arima(dfI, seasonal = FALSE)
tsdisplay(residuals(fitA),lag.max = 40,main = 'Training Dataset (0,1,0) Model Residuals')

#ORIGINAL DATASET arima
fitB=arima(dfI,order = c(10,1,5))
tsdisplay(residuals(fitB),lag.max = 30,main="Training dataset (0,1,0) Model Residuals")


#plots of ARIMA model
par(mfrow=c(2,2))   
#auto arima (0,1,0)
period<-100   #100 days
fcast1 <- forecast(fitA,h=period) 
plot(fcast1)    #straight
#custom arima 
fcast2 <- forecast(fitB,h=period)
plot(fcast2)
fcast3 <- forecast(fitA,h=period)
plot(fcast3)  
fcast4 <- forecast(fitB,h=period)
plot(fcast4)

par(mfrow=c(1,2))  
fcast3 <- forecast(fitA,h=period)
plot(fcast3)  
fcast4 <- forecast(fitB,h=period)
plot(fcast4)


par(mfrow=c(1,2)) 
accuracy(fcast3) #100-0.936 = 99.064% 
accuracy(fcast4) #100-0.935 = 99.065% 

par(mfrow=c(1,1))
fit<-auto.arima(dfI)
fit
plot(as.ts(dfI),col='green')
lines(fitted(fit),col='red')
fit.forecast <- forecast.Arima(fit)
fit.forecast

#Conclusion, the trend might remain the same for a couple of days 
