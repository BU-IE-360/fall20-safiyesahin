install.packages("ggplot2")
library(ggplot2)
library(readxl)

#While doing my homework, I had problems pulling my data from the csv file at first, and since this took a lot of time,
#I transferred my data directly to vectors with copy and paste. Later, after finding the method to solve the problem, 
#I corrected a few places to see it in the application, but I did not update all the data again as there was nothing to affect my result.

# Code for first data
df <- read_excel("C:/Users/Safiye/Desktop/Kitap1.xlsx")

par(mfrow=c(2,2))
hist(df$y2016,
     col = "purple", 
     xlab = "Total clothing and accessories expenditures made\n via credit card and debit card(thousand liras)",
     ylab = "Frequency",
     main = "Histogram of Total Clothing and\n Accessories Expenditures for 2016",
     breaks = seq(min(df$y2016), max(df$y2016),length.out = 11))
hist(df$y2017,
     col = "orange", 
     xlab = "Total clothing and accessories expenditures made\n via credit card and debit card(thousand liras)",
     ylab = "Frequency",
     main = "Histogram of Total Clothing and\n Accessories Expenditures for 2017",
     breaks = seq(min(df$y2017), max(df$y2017),length.out = 11))
hist(df$y2018,col = "pink", 
     xlab = "Total clothing and accessories expenditures made\n via credit card and debit card(thousand liras)",
     ylab = "Frequency",
     main = "Histogram of Total Clothing and\n Accessories Expenditures for 2018",
     breaks = seq(min(df$y2018), max(df$y2018),length.out = 11))
hist(df$y2019,col = "yellow", 
     xlab = "Total clothing and accessories expenditures made\n via credit card and debit card(thousand liras)",
     ylab = "Frequency",
     main = "Histogram of Total Clothing and\n Accessories Expenditures for 2019",
     breaks= seq(min(df$y2019), max(df$y2019),length.out = 11))

col <- c("purple","orange","pink","yellow")
boxplot(df, col = col )
legend(0.5, 9000000, legend=c("2016","2017","2018","2019"), col=col, pch=1, cex=0.8)

data_for2016<- c(63,58, 68,82,89,67,70, 60, 62,89,68,66)
data_for2017<- c(76,76,93, 134,92, 97, 117, 95, 93,143,125,130)
data_for2018<- c(103,113, 136,190,127,147,164, 111,154, 134,212,163)
data_for2019<- c(119, 152,204, 151, 183,202,145,140,197,159,187,197)


data2 <- data.frame(data_for2016, data_for2017, data_for2018, data_for2019)
col <- c("purple","orange","pink","yellow")
boxplot(data2, col = col)
legend(0.5, 200, legend=c("2016","2017","2018","2019"), col=col, pch=1, cex=0.8)



#Code for second data
df2 <- read_excel("C:/Users/Safiye/Desktop/Kitap2.xlsx")

par(mfrow=c(2,2))
hist(df2$y2016,
     col = "purple", 
     xlab = "Tourism Expenditures(000$)",
     ylab = "Frequency",
     main = "Histogram of Total Tourism Expenditures\n for 2016",
     breaks = seq(min(df2$y2016), max(df2$y2016),length.out = 11))
hist(df2$y2017,
     col = "orange", 
     xlab = "Tourism Expenditures(000$)",
     ylab = "Frequency",
     main = "Histogram of Total Tourism Expenditures\n for 2017",
     breaks = seq(min(df2$y2017), max(df2$y2017),length.out = 11))
hist(df2$y2018,
     col = "pink", 
     xlab = "Tourism Expenditures(000$)",
     ylab = "Frequency",
     main = "Histogram of Total Tourism Expenditures\n for 2018",
     breaks = seq(min(df2$y2018), max(df2$y2018),length.out = 11))
hist(df2$y2019,
     col = "yellow", 
     xlab = "Tourism Expenditures(000$)",
     ylab = "Frequency",
     main = "Histogram of Total Tourism Expenditures\n for 2019",
     breaks = seq(min(df2$y2019), max(df2$y2019),length.out = 11))

col <- c("purple","orange","pink","yellow")
boxplot(df2, col = col)
legend(0.5, 200, legend=c("2016","2017","2018","2019"), col=col, pch=1, cex=0.8)

data_for2016<- c(139, 115,111, 132,205,262,270,251,162, 124,96, 106)
data_for2017<- c(146,125, 129,198,190,205,451,306, 172,149, 116,166)
data_for2018<- c(148, 137,144,221,176,271, 470, 305,211,132, 123, 176)
data_for2019<- c(151,143, 184,178,194,389,364,324,224,141,142, 174)

data2 <- data.frame(data_for2016,data_for2017,data_for2018,data_for2019)

col <- c("purple","orange","pink","yellow")
boxplot(data2, col = col)
legend(0.5, 450, legend=c("2016","2017","2018","2019"), col=col, pch=1, cex=0.8)




#Code for third data
values_for2016<-c(1826950, 1039630, 864837,677854, 619258,543890,389586,281480,62810,60815,59988, 61189)
values_for2017<-c(60640,60171,59546,59093,58601,55946,54629,53518,52735,52139,51603,53591)
values_for2018<-c(53247,52876,52444,52124,51832, 51655, 50955,50089, 49539,48507,47263,46711)
values_for2019<-c(50264,49944,49490,49183, 48813,48802,48367, 47678,47143, 46626,46006,45371)

data1 <- data.frame(values_for2016,values_for2017,values_for2018,values_for2019)
par(mfrow=c(2,2))
hist(data1$values_for2016,col = "purple", 
     xlab = "Total housing credit(thousand liras)",
     ylab = "Frequency",
     main = "Histogram of Total housing credit for 2016",
     breaks = seq(min(data1$values_for2016), max(data1$values_for2016),length.out = 11))
hist(data1$values_for2017,col = "orange", 
     xlab = "Total housing credit(thousand liras)",
     ylab = "Frequency",
     main = "Histogram of Total housing credit for 2017",
     breaks = seq(min(data1$values_for2017), max(data1$values_for2017),length.out = 11))
hist(data1$values_for2018,col = "pink", 
     xlab = "Total housing credit(thousand liras)",
     ylab = "Frequency",
     main = "Histogram of Total housing credit for 2018",
     breaks = seq(min(data1$values_for2018), max(data1$values_for2018),length.out = 11))
hist(data1$values_for2019,col = "yellow", 
     xlab = "Total housing credit(thousand liras)",
     ylab = "Frequency",
     main = "Histogram of Total housing credit for 2019",
     breaks = seq(min(data1$values_for2019), max(data1$values_for2019),length.out = 11))

col <- c("purple","orange","pink","yellow")

par(mfrow=c(1,4))
boxplot(data1$values_for2016, col = col[1])
legend(0.5, 1800000, legend=c("2016","2017","2018","2019"), col=col, pch=1, cex=0.8)
boxplot(data1$values_for2017, col = col[2])
boxplot(data1$values_for2018, col = col[3])
boxplot(data1$values_for2019, col = col[4])

data_for2016<- c(121,111,117,111,134,101,145,132,125,147,114,105)
data_for2017<- c(150,145,125,148,114,120,178,129,126,146,112,138)
data_for2018<- c(118,120, 120,143,117,114,163,127,146,96,86,111)
data_for2019<- c(96,105, 136, 101,87, 149,126,157, 178,130, 128,159)


data2 <- data.frame(data_for2016, data_for2017, data_for2018, data_for2019)
col <- c("purple","orange","pink","yellow")
boxplot(data2, col = col)
legend(0.5, 180, legend=c("2016","2017","2018","2019"), col=col, pch=1, cex=0.8)