#reading the entire dataset
df=read.csv("D:\\VIT\\SEM 5\\Data Analytics\\J component\\dataset\\Extracted\\c_googl.csv")
df

library(timeDate)
library(timeSeries)
library(quantmod)
library(forecast)
library(TTR)
library(xts)
library(zoo)

dfx = xts(df$Close, order.by=as.Date(df$Date))
dfx
class(dfx)
plot(dfx)

#For arima model you need P,D,Q values
#graph the ACF and PACF values -> ACF=Q and PACF=P
par(mfrow=c(1,2))
Acf(dfx,main="ACE for Differenced Series")    #gives q value
#here we observe that the first value is 1 and trends downwards 

Pacf(dfx, main = "PACF for Differeced Series")   #->gives p value
#here again we observe a lag at 1 which gives us the p-value of 1

#log residuals to eliminate the areas that are non-stationary
#making the data more stable
logs=diff(log(dfx),lag=1)
logs=logs[!is.na(logs)]            #residual -> observed value - predicted value

#plot the logs
par(mfrow=c(1,1))
plot(logs,type='l',main='log return plot')
auto.arima(logs,seasonal = FALSE)
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
auto.arima(train,seasonal = FALSE) #0,0,0
#pdq -> 9,0,26

#plot the models, get accuracy and draw conclusions
fit1<-auto.arima(train,seasonal = FALSE)
tsdisplay(residuals(fit1),lag.max = 40,main = '(0,0,0) Model Residuals')

fit2=arima(train,order = c(9,0,26))
tsdisplay(residuals(fit2),lag.max = 20,main="(9,0,26) Model Residuals")
#if it busts out the blue line earlier its a weak model, if it doesn't its a strong model

#Training DATA auto.arima
fitA=auto.arima(dfx, seasonal = FALSE)
tsdisplay(residuals(fitA),lag.max = 40,main = 'Training Dataset (0,1,1) Model Residuals')

#Training DATASET arima
fitB=arima(dfx,order = c(9,0,26))
tsdisplay(residuals(fitB),lag.max = 40,main="Training dataset (9,0,26) Model Residuals")


#plots of ARIMA model
par(mfrow=c(2,2))   
#auto arima (2,0,2)
period<-365   #365 days
fcast3 <- forecast(fitA,h=period)
plot(fcast3)  
fcast4 <- forecast(fitB,h=period)
plot(fcast4)

par(mfrow=c(1,1))  
fcast3 <- forecast(fitA,h=period)
plot(fcast3)  

fcast4

par(mfrow=c(1,2)) 
accuracy(fcast3)
accuracy(fcast4 )#100-1.213 = 98.787% 


par(mfrow=c(1,1))
fit<-auto.arima(dfx)
fit
plot(as.ts(dfx),col='red')
lines(fitted(fit),col='green')
fit.forecast <- forecast.Arima(fit)
fit.forecast

#Conclusion, the trend might go about a 10-15% high 
#(auto arima original dataset) maybe you could buy the stock for the next 365 days



