library(vars) # ur.df,
library(forecast)
library(urca)
library(xts)
library(zoo)
------------------#importing and viewing the data#------------#
View(oct_march)
oct_march = read.csv("oct.csv",stringsAsFactors = F)
head(oct_march)
str(oct_march)
summary(oct_march)
colSums(is.na(oct_march))
oct_march$date=as.Date(oct_march$date,format="%d-%m-%y")

--------------------#subsetting each variable with date--------#
  
oct_view=oct_march[,c("views","date")]
oct_subs=oct_march[,c("subscriber","date")]
oct_vid=oct_march[,c("videoscount","date")]

------------# plotting each subset as timeseries #------------------- #

ts1=xts(oct_view[,1],order.by = as.Date(oct_view[,2],"%d-%m-%y"))
plot(ts1,ylab="pviews", xlab="date",main="Views",col="blue")
ts2=xts(oct_subs[,1],order.by = as.Date(oct_subs[,2],"%d-%m-%y"))
plot(ts2,ylab="Views", xlab="date",main="Views",col="blue")
ts3=xts(oct_vid[,1],order.by = as.Date(oct_vid[,2],"%d-%m-%y"))
plot(ts3,ylab="Views", xlab="date",main="Views",col="green")

-----------#checking stationarity of each variable-----------#

#adf.test(oct_march, null hypothesis=non-stationary, alternative = "stationary")
w = ur.df(oct_march$videoscount,type = "drift", selectlags = "AIC")
summary(w) #null hupothesis rejected; w is stationary
x = ur.df(oct_march$views,type = "drift", selectlags = "AIC")
summary(x) #null hupothesis rejected; x is stationary
y = ur.df(oct_march$subscriber,type = "drift", selectlags = "AIC")
summary(y) #null hupothesis rejected; y is stationary


-------------#predicting of variable "Views"----------#
View(oct_march)
oct_view=oct_march[,c("views","date")]
head(oct_view)
ts1=xts(oct_view[,1],order.by = as.Date(oct_view[,2],"%d-%m-%y"))
plot(diff(ts1,differences = 1)) #trend removed
tsview=diff(ts1,differences=1)
colSums(is.na(tsview))
tsview[,1][which(is.na(tsview[,1]))]=median(tsview[,1],na.rm = T)
acf(tsview,main="ACF plot")
pacf(tsview,main="PACF plot")
ARIMA=auto.arima(ts1)
ARIMA#(0,1,0)
ARIMAview = arima(ts1,c(0,1,0))
summary(ARIMAview)
predv=predict(ARIMAview,n.ahead=90)
predv
predv2=predv$pred+2*predv$se
predv2
A1 = round(predv2)
-------------#predicting of variable "subscriber"----------#
View(oct_march)

oct_subs=oct_march[,c("subscriber","date")]
head(oct_view)
ts2=xts(oct_subs[,1],order.by = as.Date(oct_subs[,2],"%d-%m-%y"))
plot(diff(ts2,differences = 1)) #trend removed
tssub=diff(ts2,differences=1)
colSums(is.na(tssub))
tssub[,1][which(is.na(tssub[,1]))]=median(tssub[,1],na.rm = T)
acf(tssub,main="ACF plot")
pacf(tssub,main="PACF plot")
ARIMA=auto.arima(ts2)
ARIMA #(0,1,1)
ARIMAsub=arima(ts2,c(0,1,1))
summary(ARIMAsub)
preds=predict(ARIMAsub,n.ahead=90)
preds
preds2=preds$pred+2*preds$se
preds2
A2 = round(preds2)

-------------#predicting of variable "videocount"----------#
  View(oct_march)

oct_vid=oct_march[,c("videoscount","date")]
head(oct_vid)
ts3=xts(oct_vid[,1],order.by = as.Date(oct_vid[,2],"%d-%m-%y"))
plot(diff(ts3,differences = 1)) #trend removed
tsvid=diff(ts3,differences=1)
colSums(is.na(tsvid))
tsvid[,1][which(is.na(tsvid[,1]))]=median(tsvid[,1],na.rm = T)
acf(tsvid,main="ACF plot")#ACF PLOT -- Moving Average or q
pacf(tsvid,main="PACF plot")#PACF PLOT -- Auto Regressive or p
ARIMA = auto.arima(ts3)
ARIMA#(0,1,1)
ARIMAvid=arima(ts3,c(0,1,1))
summary(ARIMAvid)
predvc=predict(ARIMAvid,n.ahead=91)
predvc
predvc2=predvc$pred+2*predvc$se
predvc2
A3 = round(predvc2)

---------------------#save the output in csv#-----------

Answer = cbind(A1,A2,A3)
colnames(Answer) =c("views","subscription","videocount")
Answer
write.csv(Answer,"Solution_channel_stats",row.names = FALSE)
read.csv("Solution_channel_stats")









