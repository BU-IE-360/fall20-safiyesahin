library(readxl)
library(TSstudio)
library(feasts)
library(tsibble)
library(readxl)
library(lubridate)
library(ggthemes)
library(xts)
library(forecast)
library(ggcorrplot)
library(scales)
library(fpp)
library(ggplot2)
library(data.table)
library(readxl)
library(zoo)
library(GGally)
data <- read_excel("C:/Users/Safiye/Desktop/datasetforhw3.xlsx")
head(data)
data$Date <- as.Date(parse_date_time(data$Date,"Ym"), format = "%Y-%m-%d")
all.data= data[c(-1)]
ts_dataset <- ts( data$Total_Sales, frequency = 12, start=c(2013,1))
head(ts_dataset)

plot(ts_dataset,xlab="Year", ylab="Total Sales", main= "Total housing sales of Turkey", type= "l")

dataset_log <- log(ts_dataset)

plot(dataset_log,xlab="Year", ylab="Total Sales", main= "Total housing sales of Turkey", type= "l")

ts_all.data <- ts( all.data, frequency = 12, start=c(2013,1))
dataset_log2 <- log(ts_all.data)
plot(zoo(ts_all.data), main= "Graph of the all data")


cor.test(x = all.data$Total_Sales, y = all.data$USD, method = "pearson", alternative = "greater")
cor.test(x = all.data$Total_Sales, y = all.data$EURO, method = "pearson", alternative = "greater")
cor.test(x = all.data$Total_Sales, y = all.data$interest, method = "pearson", alternative = "greater")
cor.test(x = all.data$Total_Sales, y = all.data$economic_level, method = "pearson", alternative = "greater")
cor.test(x = all.data$Total_Sales, y = all.data$unemp, method = "pearson", alternative = "greater")
cor.test(x = all.data$Total_Sales, y = all.data$car, method = "pearson", alternative = "greater")



ggpairs(all.data)
ggcorrplot(corr = cor(all.data),
           hc.order = TRUE,
           outline.col = "white",
           type = "upper",lab = TRUE,
           title = "Correlation Matrix",
           colors = c("steelblue4","darkseagreen2","ivory3"),
           legend.title = "Correlation")

sales=data.table(log_sales=as.numeric(dataset_log))
#trend:
sales[,trend:=1:.N]
sales
acf(as.numeric(ts_dataset)) #lag 12 autocorrelation

#add month information
month=seq(1,12,by=1)
sales=cbind(sales,month)



fit1 <- lm(log_sales~-1+as.factor(month), data = sales)
summary(fit1)
plot(fit1$residual)


fit2 <- lm(log_sales~-1+as.factor(month)+trend, data = sales)
summary(fit2)
plot(fit2$residual)

boxplot(dataset_log)
sales[,outliers:=0]
sales[log_sales> 12.03 ,outliers:= 1]
sales[log_sales< 11.03 ,outliers:= 1]

fit3 <- lm(log_sales~ -1 +as.factor(month)+ trend+ outliers, data = sales)
summary(fit3)
plot(fit3$residual)
checkresiduals(fit2,lag=12)
checkresiduals(fit3,lag=12)



sales[,lag1:=0]
sales[ month ==1 ,lag1:= 1]
sales[,lag3:=0]
sales[ month ==3 ,lag3:= 1]
View(sales)
fit4 <- lm(log_sales~ -1 +as.factor(month)+ trend + lag1+ lag3, data = sales)
summary(fit4)
plot(fit4$residual)
checkresiduals(fit4,lag=12) #kontrol et ve kullanma improve etmiyor

sales[,money:= all.data$USD]

fit4 <- lm(log_sales~-1 + as.factor(month)+ trend +outliers+money, data = sales)
summary(fit4)
plot(fit4$residual)
plot(sales[,list(money,log_sales)], sub= "table 1")
plot(sales[,list(money,residual=fit4$residual)], sub= "table 2")
sales[, moneyp1:=0]
sales[, moneyp2:=0]
sales[, moneyp3:=0]
sales[money <=5, moneyp1:= money]
sales[money>= 7 ,moneyp3 :=  1/money ]
sales[money< 7 & money>5,moneyp2:= money^2]
fit5 <- lm(log_sales~-1 + as.factor(month)+ trend+outliers +moneyp1+moneyp2+moneyp3, data = sales)
summary(fit5)
plot(sales[,list(money,residual=fit5$residual)],sub= "table 3")
plot(fit5$residual)
checkresiduals(fit5,lag=12)

sales[, unemp:= all.data$unemp]
sales[, car:= all.data$car]
sales[, int:= all.data$interest]

fit6 <- lm(log_sales~-1 + as.factor(month)+ trend +unemp , data = sales)
summary(fit6)
plot(fit6$residual)


fit7 <- lm(log_sales~-1 + as.factor(month)+ trend +unemp+car , data = sales)
summary(fit7)
plot(fit7$residual)



fit8 <- lm(log_sales~-1 + as.factor(month)+ trend+ outliers+ unemp+int+ car, data = sales)
summary(fit8)
plot(fit8$residual)
checkresiduals(fit8)


#adding lag1 
sales_new <- sales[-1,]
lagnew <- lag( residuals(fit8))
View(lagnew)
View(sales_new)
sales_new <- cbind(sales_new, lagnew)

View(sales_new)
sales_new <- sales_new[c(1:94),]
View(sales_new)
fit9=lm( log_sales~-1 + as.factor(month)+ trend +  unemp+int+car+ lagnew, sales_new)
summary(fit9)
plot(fit9$residual)
checkresiduals(fit9)



plot(sales_new[,list(unemp,residual=fit9$residual)])
sales_new[, residuals_with_unmep:=fit9$residuals]
sales_new[, q5:= quantile(residuals_with_unmep,0.05)]
sales_new[, q95:= quantile(residuals_with_unmep,0.90)]
sales_new[, small_outlier:= as.numeric(residuals_with_unmep< q5) ]
sales_new[, large_outlier:= as.numeric(residuals_with_unmep>q95) ]
fit9=lm( log_sales~-1 + as.factor(month)+ trend +  unemp+int+car+ lagnew+ small_outlier+ large_outlier, sales_new)
summary(fit9)
plot(fit9$residual)
plot(sales_new[,list(unemp,residual=fit9$residual)])
checkresiduals(fit9)

#get fitted values


sales_new [,fitted:=fitted(fit9)]
sales_new [,residual:=residuals(fit9)]
View(sales_new)
sales_new %>%
  ggplot(aes(x=fitted, y=residual)) + geom_point() +ggtitle("Graph of fitted vs residuals")

sales_new %>%
  ggplot(aes(x=fitted, y=log_sales)) + 
  geom_point() +
  geom_abline(slope=1, intercept=0) +ggtitle("Graph of fitted vs actuals")
View(sales_new)

#get forecasts for next month 
month_info <- c(12)
sales_new=rbind(sales_new,data.table(month=as.factor(month_info)),fill=T)

sales_new[is.na(car)==T,car:= 11.90]
sales_new[is.na(small_outlier)==T,small_outlier:= 0]
sales_new[is.na(large_outlier)==T,large_outlier:= 0]
sales_new[is.na(trend)==T,trend:= 95]
sales_new[is.na(int)==T,int:= 18.19]
sales_new[is.na(unemp)==T,unemp:= 63.64]
sales_new[is.na(lagnew)==T,lagnew:= -0.190691523]


p1<-predict(fit9,sales_new[is.na(fitted)==T])
sales_new[is.na(fitted)==T,fitted:=predict(fit9,sales_new[is.na(fitted)==T])]
exp(p1) #predicted value for DEC 2020

cols <- c("predicted" = "orange", "actual" = "purple")
ggplot() + 
  geom_line(data = sales_new, aes(x = trend, y = fitted,color = "predicted")) +
  geom_line(data = sales_new, aes(x = trend, y = log_sales,color = "actual")) +
  xlab('time') +
  ylab('log_sales') +
  scale_color_manual(values = cols)

sales_new[,predicted_sales:=exp(fitted)]
sales_new[,actual_sales:=exp(log_sales)]
#tail(sales_new)
ggplot() + 
  geom_line(data = sales_new, aes(x = trend, y = predicted_sales,color = "predicted")) +
  geom_line(data = sales_new, aes(x = trend, y = actual_sales,color = "actual")) +
  xlab('time') +
  ylab('sales') +
  ggtitle("Graph of sales over time")+
  scale_color_manual(values = cols)