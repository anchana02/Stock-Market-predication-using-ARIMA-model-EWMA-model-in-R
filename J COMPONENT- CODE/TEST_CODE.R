#Load the data and install packages and libraries
#load libraries
install.packages('timeSeries')
install.packages('quantmod')
install.packages('tseries')
install.packages('forecast')
install.packages('xts')

library(timeDate)
library(timeSeries)
library(quantmod)
library(forecast)
library(TTR)
library(xts)
library(zoo)
#xts objects are matrix objects internally. xts objects are indexed by a formal time object. 
#it stands for extensible time series and is an extension of zoo

#Pull the data 
getSymbols('SPY',from='2015-01-01',to='2022-10-10')
class(SPY) 
SPY#-> it will give you xts,zoo

#the days of close price for each trading day (4th column is the close values)
#1)Open 
#2)High
#3)Low
#4)Close
SPY_CP=SPY[,4]
SPY_CP

#Plotting the closing values so far
plot(SPY_CP)
class(SPY_CP)

#For arima model you need P,D,Q values
#graph the ACF and PACF values -> ACF=Q and PACF=P
par(mfrow=c(1,2))
Acf(SPY_CP,main="ACE for Differenced Series")    #gives q value
#here we observe that the first value is 1 and trends downwards 

Pacf(SPY_CP, main = "PACF for Differeced Series")   #->gives p value
#here again we observe a lag at 1 which gives us the p-value of 1

#log residuals to eliminate the areas that are non-stationary
#making the data more stable
logs=diff(log(SPY_CP),lag=1)
logs=logs[!is.na(logs)]            #residual -> observed value - predicted value
logs

#plot the logs
par(mfrow=c(1,1))
plot(logs,type='l',main='log return plot')
auto.arima(logs,seasonal = FALSE)
str(logs)


#split the data into two parts -> 80/20 is training and testing
sample_size=floor(0.80*nrow(logs))
set.seed(150)
train_indices <- sample(seq_len(nrow(logs)),size = sample_size)

train <- logs[train_indices,]
test <- logs[-train_indices]

par(mfrow=c(1,2))
Acf(train,main="ACE for Differenced Series") 
Pacf(train, main = "PACF for Differeced Series") 
auto.arima(train,seasonal = FALSE)     #pdq -> 1,0,1
#from plots of acf and pacf graphs the pdq -> 5,0,11

#plot the models, get accuracy and draw conclusions
fit1<-auto.arima(train,seasonal = FALSE)
tsdisplay(residuals(fit1),lag.max = 40,main = '(0,0,3) Model Residuals')

fit2=arima(train,order = c(5,0,11))
tsdisplay(residuals(fit2),lag.max = 40,main="(5,0,11) Model Residuals")
#if it busts out the blue line earlier its a weak model, if it doesn't its a strong model

#ORIGINAL DATA auto.arima
fitA=auto.arima(SPY_CP, seasonal = FALSE)
tsdisplay(residuals(fitA),lag.max = 40,main = 'Training Dataset (1,0,1) Model Residuals')

#ORIGINAL DATASET arima
fitB=arima(SPY_CP,order = c(5,0,11))
tsdisplay(residuals(fitB),lag.max = 40,main="Training dataset (5,0,11) Model Residuals")



#plots of ARIMA model
par(mfrow=c(2,2))   
#auto arima (2,0,2)
period<-100   #100 days
fcast1 <- forecast(fit1,h=period) 
plot(fcast1)  
#custom arima 
fcast2 <- forecast(fit2,h=period)
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

accuracy(fcast3) #100-0.749 = 99.251% 
accuracy(fcast4) #100-0.751 = 99.249%  

#auto arima with 0,1,2
fcast3

#arima with 5,0,11
fcast4

#Conclusion
#Here we are just giving them directional perspective because there are no. of variables that a 
#stock market change both of the graphs shows that both the custom and auto arima shows
#0% percent of change in the trend and that its not a negative or positive
#over the next 100 days but all of this is inconclusive of other factors
#like terrorists attack,earthquakes,political variations,currency values


#ADF test on log return for the p value
library(tseries)
print(adf.test(logs))

#FOR THE ORIGINAL DATASET (CLOSE VALUE)
#for getting the actual p value
#ADF test for p-values  - DICKEY FULLER TEST
#if p value is < .05 we fail to reject NULL hypo -> not stationary
library(tseries)
x=SPY_CP
print(adf.test(x))      #p=0.4096       ARIMA(0,1,2) what it recommends
auto.arima(SPY_CP,seasonal = FALSE)

#fit values to test their accuracy for the forecasting
#values--- ARIMA(P,D,Q)
fitA=auto.arima(SPY_CP, seasonal = FALSE)
tsdisplay(residuals(fitA),lag.max = 40,main = '(0,1,2) Model Residuals')
auto.arima(SPY_CP,seasonal = FALSE)
#we need AIC  and BIC value as low as possible 

fitB=arima(SPY_CP,order = c(2,1,4))
tsdisplay(residuals(fitB),lag.max = 40,main="(2,1,4) Model Residuals")

fitC=arima(SPY_CP,order = c(4,2,4))
tsdisplay(residuals(fitC),lag.max = 40,main="(4,2,4) Model Residuals")

fitD=arima(SPY_CP,order = c(1,1,1))   #default
tsdisplay(residuals(fitD),lag.max = 40,main="(1,1,1) Model Residuals")

#plots of ARIMA model
par(mfrow=c(2,2))   
#auto arima (2,0,2)
term<-100   #100 days
fcast1 <- forecast(fitA,h=term) 
plot(fcast1)    #straight
#custom arima 
fcast2 <- forecast(fitB,h=term)
plot(fcast2)   #down
fcast3 <- forecast(fitC,h=term)
plot(fcast3)    #up
fcast4 <- forecast(fitD,h=term)
plot(fcast4)    #straight 

accuracy(fcast1) #100-0.749 = 99.251%    *
accuracy(fcast2) #100-0.748 = 99.252%    *
accuracy(fcast3) #100-0.752 = 99.248%
accuracy(fcast4) #100-0.749 = 99.251%    *   

par(mfrow=c(1,1))  
fit<-auto.arima(SPY_CP,ic="bic")
fit
plot(as.ts(SPY_CP))
lines(fitted(fit),col='red')
fit.forecast <- forecast.Arima(fit)
fit.forecast
plot(fit.forecast)
# The trends will remain the same or else might slightly go up in the next 100 days



