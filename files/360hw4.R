library(fpp)
library(forecast)
library(readxl)
library(lubridate)
library(dplyr)
library(scales)
library(data.table)
library(ggplot2)
data <- read_excel("C:/Users/Safiye/Desktop/360HW4.xlsx")
#View(data)

ggAcf(data$consumption, lag.max = 365*2,color="red")+
  ggtitle("a")+
  ylab("b")+
  theme(legend.position = "topleft")+
  theme_minimal()
ggAcf(data$consumption, lag.max = 7*4, color="red")+
  ggtitle("a")+
  ylab("b")+
  theme_minimal()
tsdata<-ts(data$consumption,start=c(2017,0),end=c(2021,8),frequency= 365)
autoplot(tsdata,color="salmon1")+
  ggtitle("Plot of Daily Mean Consumption Time Series Data")+
  ylab("Mean Consumption")+
  theme(legend.position = "topleft")+
  theme_gray()
ggseasonplot(tsdata)



data_multip<-decompose(tsdata,type="multiplicative")
#data_add<-decompose(lag(tsdata),type="additive")
#data_add<-decompose(tsdata,type="additive")
plot(data_multip,col="salmon1")
#plot(data_add)

deseasonalized<-tsdata/data_multip$seasonal
autoplot(deseasonalized,color="salmon1")+
  ggtitle("Plot of Daily Mean Consumption Deseasonalized Time Series Data")+
  ylab("Mean Consumption")+
  theme(legend.position = "topleft")+
  theme_gray()
ggAcf(deseasonalized,lag.max=7*4,color="salmon1")+
  ggtitle("Autocorrelation plot of deseasonalized data")+
  ylab("ACF")+
  theme(legend.position = "topleft")+
  theme_gray()
detrend<-deseasonalized/data_multip$trend
random<- data_multip$random
autoplot(random,color="salmon1")+
  ggtitle("Plot of Random of Decomposed Time Series Data")+
  ylab("Random")+
  theme(legend.position = "topleft")+
  theme_gray()

#View(random)


ggAcf(random,na.action = na.pass,lag.max=7*4,color="salmon1")+
  ggtitle("Autocorrelation plot of random")+
  ylab("ACF")+
  theme(legend.position = "topleft")+
  theme_gray()
ggPacf(random,na.action = na.pass,lag.max=7*4,color="salmon1")+
  ggtitle("Partial autocorrelation plot of random")+
  ylab("PACF")+
  theme(legend.position = "topleft")+
  theme_gray()


arima(random, order=c(2,0,0))
arima(random, order = c(3, 0, 1))
arima(random, order=c(3,1,2))

model <- arima(random, order=c(5,1,2))
model_fitted <- random - residuals(model)
#View(model_fitted)
model_fitted_transformed <- model_fitted*data_multip$trend*data_multip$seasonal
#View(model_fitted_transformed)

plot(random, xlab = "Year", ylab = "Random",main="Plot of Random", col="salmon1")
points(model_fitted, type = "l", col = "yellow", lty = 3)

plot(tsdata, xlab = "Year", ylab = "Mean Consumption",main="Plot of Daily Mean Consumption Time Series Data",col="salmon1")
points(model_fitted_transformed, type = "l", col = "yellow", lty = 3)

model_forecast <- predict(model, n.ahead = 14)$pred
model_forecast=ts(model_forecast,frequency = 365,start=c(2021,9))

last_trend_value <-tail(data_multip$trend[!is.na(data_multip$trend)],1)
seasonality=data_multip$seasonal[9:22]
#back to the original series
model_forecast=model_forecast*last_trend_value*seasonality

plot(tsdata, xlab = "Year", ylab = "Mean Consumption",main="Plot of Daily Mean Consumption Time Series Data",col="salmon1")
points(model_fitted_transformed, type = "l", col = "yellow", lty = 3)
points(model_forecast, type = "l", col = 3)
model_forecast
model_forecast*24

statistic<- function(actual, forecasted){
  n=length(actual)
  error = actual-forecasted
  mean=mean(actual)
  sd=sd(actual)
  bias = sum(error)/sum(actual)
  mape = sum(abs(error/actual))/n
  mad = sum(abs(error))/n
  wmape = mad/mean
  l = data.frame(n,mean,sd,bias,mape,mad,wmape)
  return(l)
}

actualdata <- read_excel("C:/Users/Safiye/Desktop/actualdata.xlsx")

statistic (actualdata$consumption, model_forecast)

