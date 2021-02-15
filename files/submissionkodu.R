# install the required packages first
require(jsonlite)
require(httr)
require(data.table)

get_token <- function(username, password, url_site){
  
  post_body = list(username=username,password=password)
  post_url_string = paste0(url_site,'/token/')
  result = POST(post_url_string, body = post_body)
  
  # error handling (wrong credentials)
  if(result$status_code==400){
    print('Check your credentials')
    return(0)
  }
  else if (result$status_code==201){
    output = content(result)
    token = output$key
  }
  
  return(token)
}

get_data <- function(start_date='2020-03-20', token, url_site){
  
  post_body = list(start_date=start_date,username=username,password=password)
  post_url_string = paste0(url_site,'/dataset/')
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  result = GET(post_url_string, header, body = post_body)
  output = content(result)
  data = data.table::rbindlist(output)
  data[,event_date:=as.Date(event_date)]
  data = data[order(event_date)]
  return(data)
}


send_submission <- function(predictions, token, url_site, submit_now=F){
  
  format_check=check_format(predictions)
  if(!format_check){
    return(FALSE)
  }
  
  post_string="list("
  for(i in 1:nrow(predictions)){
    if(i<nrow(predictions)){
      post_string=sprintf("%s%s,",post_string,predictions$forecast[i])
    } else {
      post_string=sprintf("%s%s)",post_string,predictions$forecast[i])
    }
  }
  
  submission = eval(parse(text=post_string))
  json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
  submission=list(submission=json_body)
  
  print(submission)
  # {"31515569":2.4,"32939029":2.4,"4066298":2.4,"6676673":2.4,"7061886":2.4,"85004":2.4} 
  
  if(!submit_now){
    print("You did not submit.")
    return(FALSE)      
  }
  
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  post_url_string = paste0(url_site,'/submission/')
  result = POST(post_url_string, header, body=submission)
  
  if (result$status_code==201){
    print("Successfully submitted. Below you can see the details of your submission")
  } else {
    print("Could not submit. Please check the error message below, contact the assistant if needed.")
  }
  
  print(content(result))
  
}

check_format <- function(predictions){
  
  if(is.data.frame(predictions) | is.data.frame(predictions)){
    if('forecast' %in% names(predictions)){
      if(nrow(predictions)==24){
        if(all(is.numeric(predictions$forecast))){
          print("Format OK")
          return(TRUE)
        } else {
          print("forecast information is not numeric")
          return(FALSE)                
        }
      } else {
        print("Forecasts for 24 hours should be provided, current number of rows:")
        print(nrow(predictions))
        return(FALSE)     
      }
    } 
  } else {
    print("Wrong format. Please provide data.frame or data.table object")
    return(FALSE)
  }
  
}

# this part is main code

subm_url = 'http://46.101.124.77'

u_name = "Group4"
p_word = "ktZCloisL3uKU6Aq"
submit_now = FALSE

username = u_name
password = p_word

token = get_token(username=u_name, password=p_word, url=subm_url)
data = get_data(token=token,url=subm_url)
data <- data.table(data)
as.Date(data$event_date)

require(data.table)
consumption=fread('C:/Users/ilos3/Desktop/BOUN/Fall2020/IE 360/bulk_consumption_with_temp.csv')
head(consumption,25)
consumption[,Date:=as.Date(Date)]
consumption <- rbind(consumption, data, use.names=FALSE)
as.data.table(consumption)

#05.02.2021 (for 06.02.2021)
#consumption$Consumption[35882:35885]=c(34500, 35500, 36500, 37500)

#06.02.2021 (for 07.02.2021)
#eksikveri <- (32747.26/43564) *c(44340,43633,43084,42706,42467,41660,40046,39212,38521,37520)
#consumption$Consumption[35908:35917]=eksikveri







daily_consumption=consumption[,list(mean_consumption=mean(Consumption,na.rm=T), 
                                    mean_maxt=mean(max(T_1), max(T_2), max(T_3), max(T_4), max(T_5), max(T_6), max(T_7)),
                                    mean_mint=mean(min(T_1), min(T_2), min(T_3), min(T_4), min(T_5), min(T_6), min(T_7))),by=list(Date)]
daily_consumption <- data.table(daily_consumption)
daily_consumption1 <- daily_consumption[1:(.N-2),]



daily_consumption1[,w_day:=weekdays(Date)]
daily_consumption1[,y_day:=yday(Date)]
daily_consumption1[,mon:=months(Date)]
daily_consumption1[,time_index:=1:.N]
daily_consumption1[,pandemic:=0]
daily_consumption1[,lockdown:=0]
daily_consumption1$pandemic[1171:(length(daily_consumption1$pandemic))] = 1
daily_consumption1$lockdown[1420:(length(daily_consumption1$lockdown))] = 1


daily_consumption1[,holiday:=0]

daily_consumption1$holiday[c(177, 178, 244, 247, 366, 531, 598, 599, 600, 601, 731, 886, 887, 888, 
                             954, 955, 956, 1096, 1241, 1242, 1308, 1311, 1462 )] = 1



#daily_consumption1[,differ:= lag_1]
tail(daily_consumption1, 10)
#daily_consumption <- daily_consumption[c(2:.N),]
head(daily_consumption1,10)

#fitlm_1=lm(mean_consumption~time_index+mean_maxt, daily_consumption)
#summary(fitlm_1)

#fitlm_2=lm(mean_consumption~time_index+mean_maxt+w_day, daily_consumption)
#summary(fitlm_2)

#fitlm_3=lm(mean_consumption~time_index+mean_maxt+w_day+y_day, daily_consumption)
#summary(fitlm_3)

#fitlm_4=lm(mean_consumption~time_index+mean_maxt+w_day+y_day+mon, daily_consumption)
#summary(fitlm_4)

#fitlm_5=lm(mean_consumption~time_index+mean_maxt+w_day+mon+pandemic, daily_consumption)
#summary(fitlm_5)

#fitlm_6=lm(mean_consumption~time_index+mean_maxt+w_day+mon+pandemic+holiday, daily_consumption)
#summary(fitlm_6)
library(forecast)
#checkresiduals(fitlm_6)

fitlm_7=lm(mean_consumption~time_index+mean_maxt+w_day+mon+pandemic+holiday+mean_mint+lockdown, daily_consumption1)
summary(fitlm_7)
checkresiduals(fitlm_7)

daily_consumption1[, residuals_offit:=fitlm_7$residuals]
daily_consumption1[, q10:= quantile(residuals_offit,0.10)]
daily_consumption1[, q95:= quantile(residuals_offit,0.95)]
daily_consumption1[, small_outlier:= as.numeric(residuals_offit < q10) ]
daily_consumption1[, large_outlier:= as.numeric(residuals_offit > q95) ]



todays_index <- as.numeric(difftime(Sys.Date(), daily_consumption$Date[1],units="days")) + 1
#todays_index <- 1490

#adding lag1 with shifting
trydata <- daily_consumption1[-1,]
lag1 <- lag( residuals(fitlm_7))
length(residuals(fitlm_7))


lag1_manip <- lag1[1:(todays_index - 2)]


trydata <- cbind(trydata, lag1_manip)
fittry=lm(mean_consumption~time_index+mean_maxt+w_day+mon+pandemic+holiday+mean_mint+lag1_manip+small_outlier+large_outlier+lockdown, trydata)
summary(fittry)
checkresiduals(fittry)


residuals <- fittry$residuals
#library(urca)
#summary(ur.kpss(residuals))


#auto.arima(residuals)
#arima_fitted <- auto.arima(residuals)
#residuals_from_arima <- arima_fitted$fitted
#acf(residuals_from_arima)
#acf(fittry$residuals)
#summary(ur.kpss(residuals_from_arima))

#plot(residuals, type="l")
#lines(residuals_from_arima, col="2")



#get forecasts for next day 
trydata[,fitted:=fitted(fittry)]
trydata[,residual:=residuals(fittry)]

#added a new row for the prediction
trydata=rbind(trydata,data.table(Date=Sys.Date() ),fill=T)
tail(trydata)

trydata[is.na(lag1_manip)==T,lag1_manip:= lag1[length(lag1)] ]
trydata[is.na(time_index)==T,time_index:= todays_index]
trydata[is.na(mean_maxt)==T,mean_maxt:= daily_consumption$mean_maxt[todays_index] ]
trydata[is.na(mean_mint)==T,mean_mint:= daily_consumption$mean_mint[todays_index] ]
trydata[is.na(pandemic)==T,pandemic:= 1 ]
trydata[is.na(mon)==T,mon:= months(Sys.Date()) ]
trydata[is.na(holiday)==T,holiday:= 0 ]
trydata[is.na(w_day)==T,w_day:= weekdays(as.Date(daily_consumption$Date[todays_index])) ]
trydata[is.na(small_outlier)==T,small_outlier:= 0 ]
trydata[is.na(large_outlier)==T,large_outlier:= 0 ]
trydata[is.na(lockdown)==T,lockdown:= 1 ]



tail(trydata)

prediction <- predict(fittry, trydata[is.na(fitted)==T])
trydata[is.na(fitted)==T,fitted:=prediction]
tail(trydata)

trydata[is.na(mean_consumption)==T,mean_consumption:= prediction ]


fitlm_7=lm(mean_consumption~time_index+mean_maxt+w_day+mon+pandemic+holiday+mean_mint+lockdown, trydata)


##############################
#prediction of the next 2 days
lag2 <- lag( residuals(fitlm_7))
trydata2 <- trydata[-1,]
length(residuals(fitlm_7))
head(residuals(fitlm_7))

lag2_manip <- lag2[1:(todays_index -2 )]



trydata2 <- cbind(trydata2, lag2_manip)
fittry2=lm(mean_consumption~time_index+mean_maxt+w_day+mon+pandemic+holiday+mean_mint+lag2_manip+small_outlier+large_outlier+lockdown, trydata2)
summary(fittry2)
checkresiduals(fittry2)



#get forecast of forecast 
#trydata2 <- trydata2[, c(1:11)]
trydata2[,fitted_2:=fitted(fittry2)]
trydata2[,residual_2:=residuals(fittry2)]

#added a new row for the prediction
trydata2=rbind(trydata2,data.table(Date=(Sys.Date()+1)),fill=T)
tail(trydata2)

trydata2[is.na(lag2_manip)==T,lag2_manip:= lag2[length(lag2)] ]
trydata2[is.na(time_index)==T,time_index:= (todays_index+1)]
trydata2[is.na(mean_maxt)==T,mean_maxt:= daily_consumption$mean_maxt[(todays_index+1)] ]
trydata2[is.na(mean_mint)==T,mean_mint:= daily_consumption$mean_mint[(todays_index+1)] ]
trydata2[is.na(pandemic)==T,pandemic:= 1 ]
trydata2[is.na(mon)==T,mon:= months(Sys.Date()+1) ]
trydata2[is.na(holiday)==T,holiday:= 0 ]
trydata2[is.na(w_day)==T,w_day:= weekdays(as.Date(daily_consumption$Date[(todays_index+1)])) ]
trydata2[is.na(small_outlier)==T,small_outlier:= 0 ]
trydata2[is.na(large_outlier)==T,large_outlier:= 0 ]
trydata2[is.na(lockdown)==T,lockdown:= 1 ]


tail(trydata2)

prediction2 <- predict(fittry2, trydata2[is.na(fitted_2)==T])
trydata2[is.na(fitted_2)==T,fitted_2:=prediction2]
tail(trydata2)

trydata2[is.na(mean_consumption)==T,mean_consumption:= prediction2 ]


#transforming to the hourly
#hourly <- consumption[c(35377:35712),]       2-week period
#hourly <- consumption[c(35041:35712),]       4-week period

hour_index<-todays_index*24
#hourly <- consumption[ c((hour_index-(168*4)+1):(hour_index-(168*4)+24),
#                         (hour_index-(168*3)+1):(hour_index-(168*3)+24) ,(hour_index-(168*2)+1):(hour_index-(168*2)+24),
#                         (hour_index-168+1):(hour_index-168+24))]

hourly <- consumption[ c((hour_index-(168*4)+1):(hour_index-(168*4)+24),
                         (hour_index-(168*3)+1):(hour_index-(168*3)+24) ,(hour_index-(168*2)+1):(hour_index-(168*2)+24),
                         (hour_index-(168*5)+1):(hour_index-(168*5)+24))]



hourly_ordered <- hourly[order(hourly$Hour)]
str(hourly_ordered)

j=1
hourly_mean <- c()
#for(i in 1:24){
#  hourly_mean[i] <- mean(hourly_ordered$Consumption[j:(j+13)])
#  j <- j+14}

for(i in 1:24){
  hourly_mean[i] <- mean(hourly_ordered$Consumption[j:(j+3)])
  j <- j+4}



percentage_hours <- hourly_mean / sum(hourly_mean)

final_prediciton <- prediction2 * percentage_hours *24
plot(final_prediciton)




# this part is where you need to provide your forecasting function / or set of R codes
predictions=data.table(Date=rep(as.Date(Sys.time())+1,24),Hour=0:23)
# be sure if ordered
predictions=predictions[order(Date,Hour)]
# dummy forecast
predictions[,forecast:=final_prediciton]

send_submission(predictions, token, url=subm_url, submit_now=F)






