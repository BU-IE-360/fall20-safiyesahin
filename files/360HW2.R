install.packages("lubridate")
install.packages("xts")
install.packages("ggthemes")
install.packages("ggcorrplot")
install.packages("forecast")
install.packages("feasts")
install.packages("tsibble")
install.packages("TSstudio")
library(TSstudio)
library(feasts)
library(tsibble)
library(readxl)
library(ggplot2)
library(data.table)
library(lubridate)
library(ggthemes)
library(xts)
library(forecast)
library(ggcorrplot)
# read the data 
dataset <- read_excel("360HW2DATA.xlsx", col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
head(dataset)
#df without date , year and exchange_rate
new_dataset <- dataset[c(-1,-2,-6)]
new_dataset
new_dataset2 <- dataset[c(-1,-2,-7)]
new_dataset2

# time series object  is constructed
tsdataset<-ts(new_dataset,freq=12,start=c(2014,1))
head(tsdataset)
ts_info(tsdataset)

#PLOT
cols<-c("export"="orange" ,"import"="pink","net_export"="purple", "exchange_rate"="yellow")
as.Date(parse_date_time(dataset$date,"Ym"),format="%Y-%m-%d")
g<- ggplot() +
  geom_line(data=dataset,aes(x= date,y=export,color="export")) +
  geom_line(data=dataset,aes(x= date,y=import,color="import")) +
  geom_line(data=dataset,aes(x= date,y=net_export,color="net_export"))+
  geom_line(data=dataset,aes(x= date,y=exchange_rate,color="exchange_rate"))+
  xlab("Date(2014 Jan- 2019 Dec)")+
  ggtitle("Total export-import, net export & USD exchange Rate (TRY) relationship \n                                 between(2014-2020)") +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y %b",
               date_minor_breaks = "1 month") +
 scale_y_continuous(
    name = "Values",
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~./10000000, name="Real value of exchange rate")) +
  theme(legend.position = "topleft")

g

#HÝSTOGRAMS
dataset$year = as.factor(dataset$year)
g1<-ggplot(data = dataset ,aes(x = export)) +
  geom_histogram(bins = 14,alpha = 0.8,aes(color = year, fill =year))+
  facet_wrap(facets= year ~.,scales="free_x" ,ncol=3)+
  scale_color_brewer( palette = 4, direction = 1,aesthetics = c("color", "fill"))+
  ggtitle("Histogram charts of export data by years") +
  theme(legend.position =  "none")

g1

g2<-ggplot(data = dataset ,aes(x = import)) +
  geom_histogram(bins = 14, alpha = 0.8,aes(color = year, fill =year))+
  facet_wrap(facets= year ~. ,scales="free_x",ncol=3)+
  scale_color_brewer( palette = 4, direction = 1,aesthetics = c("color", "fill"))+
  ggtitle("Histogram charts of import data by years")+
  theme(legend.position = "none")
  
g2

g3<-ggplot(data = dataset ,aes(x = net_export)) +
  geom_histogram(bins = 14, alpha = 0.8,aes(color = year, fill =year))+
  facet_wrap(facets= year ~. ,scales="free_x",ncol=3)+
  scale_color_brewer( palette = 4, direction = 1,aesthetics = c("color", "fill"))+
  ggtitle("Histogram charts of net export data by years")+
  theme(legend.position = "none")
 
g3
g4<-ggplot(data = dataset ,aes(x = rexchange_rate)) +
  geom_histogram(bins = 14, alpha = 0.8,aes(color = year, fill =year))+
  facet_wrap(facets= year ~. ,scales="free_x",ncol=3)+
  scale_color_brewer( palette = 4, direction = 1,aesthetics = c("color", "fill"))+
  ggtitle("Histogram charts of USD exchange rate(TRY) data by years")+
  theme(legend.position = "none") 
  

g4


# BOXPLOT
data_forboxplot <- cbind("Date" = rep(dataset$year,4), stack(new_dataset2))
data_forboxplot$Date = as.factor(data_forboxplot$Date)
g1<-ggplot(data = data_forboxplot ,mapping=aes(x = Date, y=values, fill=ind)) +
  geom_boxplot( )+
  scale_color_brewer( palette = 4, direction = 1,aesthetics = c("color", "fill"))+
  ggtitle("Histogram charts of USD exchange rate(TRY) data by years")+
  theme(legend.position = "right") 
  
g1




# SCATTER PLOTS

c1 <- ggplot(data=dataset, aes(x=rexchange_rate, y=export))+
  geom_point() +
  geom_smooth(method =lm, color="pink", fill="yellow" ,se=TRUE)+
  ggtitle("Plot of export vs USD exchange rate (TRY)")+
  xlab("USD exchange rate (TRY)")+
  ylab("Total export")+
  theme_gray()


c2<-ggplot(data=dataset, aes(x=rexchange_rate, y=import))+
  geom_point()+
  geom_smooth(method =lm, color="pink", fill="yellow" ,se=TRUE)+
  ggtitle("Plot of import vs USD exchange rate (TRY)")+
  xlab("USD exchange rate (TRY)")+
  ylab("Total import")+
  theme_gray()

c3<-ggplot(data=dataset, aes(x=rexchange_rate, y=net_export))+
  geom_point()+
  geom_smooth(method =lm, color="pink", fill="yellow" ,se=TRUE)+
  ggtitle("Plot of net export vs USD exchange rate (TRY)")+
  xlab("USD exchange rate (TRY)")+
  ylab("Net export")+
  theme_gray()
par(mfrow=c(1,3))
c1
c2
c3

#CORRELATÝON
cor.test(x = new_dataset$export, y = dataset$rexchange_rate, method = "pearson", alternative = "greater")
cor.test(x = new_dataset$import, y = dataset$rexchange_rate, method = "pearson", alternative = "greater")
cor.test(x = new_dataset$net_export, y = dataset$rexchange_rate, method = "pearson", alternative = "greater")
ggcorrplot(corr = cor(new_dataset),
           hc.order = TRUE,
           outline.col = "white",
           type = "upper",lab = TRUE,
           title = "Correlation Matrix",
           colors = c("steelblue4","darkseagreen2","ivory3"),
           legend.title = "Correlation"
          )
  





