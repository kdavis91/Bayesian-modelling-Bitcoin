#Libraries

library("anytime")
library("bsts")
library("car")
library("caret")
library("forecast")
library("keras")
library("MCMCpack")
library("smooth")
library("tensorflow")
library("tseries")
library("TTR")
library("ggplot2")
library("readr")

#Data

# Python script scrapes data and exports .csv 

train <- read.csv(file.choose()) #2010 -30-04-2019
test <- read.csv(file.choose()) #01/05/2019
head(train)
testdata <- test[,2]

#Wrangle

train$Date <- as.Date(anytime(train$Date))
test$Date <- as.Date(anytime(test$Date))
train$Volume <- gsub(",", "", train$Volume)
train$Market.Cap <- gsub(",", "", train$Market.Cap)
train$Market.Cap <- as.numeric(train$Market.Cap)
train$Volume <- as.numeric(train$Volume)

#Difference between high and low on each day
a <- matrix(c(0), nrow = 0, ncol = 1)
for(i in 1:nrow(train)){
  a <- rbind(a, train[i,3] - train[i,4])
  i <- i + 1
}
train <- cbind(train,a)

#Volume has missing values#
#Data Manipulation#
fifty_avg <- round(mean(train$Volume[train$a < 50], na.rm = TRUE), digits = 2)
hun_avg <- round(mean(train$Volume[train$a > 50 & train$a < 100], na.rm = TRUE), digits = 2)
hf_avg <- round(mean(train$Volume[train$a > 100 & train$a < 150], na.rm = TRUE), digits = 2)
th_avg <- round(mean(train$Volume[train$a > 150 & train$a < 350], na.rm = TRUE), digits = 2)
for(i in 1:nrow(train)){
  if(is.na(train[i,6])){
    if(train$a[i] < 50){
      train$Volume[i] <- fifty_avg
    } else if(train$a[i] < 100){
      train$Volume[i] <- hun_avg
    } else if(train$a[i] < 150){
      train$Volume[i] <- hf_avg
    } else if(train$a[i] < 350){
      train$Volume[i] <- th_avg
    }else
      print("Uncaught Title")
  }
}
train <- train[, - 8] #Removing column 8
ggplot(train, aes(Date, Close..)) + geom_line() + scale_x_date("year") + ylim(0,20000) + ylab("Closing Price")

#Convert data set to time series
Train <- xts(train[, -1], order.by = as.POSIXct(train$Date)) 
tsr <- ts(Train[,4], frequency = 365.25,start = c(2013,4,27))
plot(Train$Close..,lwd = 1.5,col='red', ylim = c(0,20000), main = "Bitcoin Closing Price") 
#checking for trends and seasonality
dects <- decompose(tsr) #Obtaining the trends and seasonality
plot(dects)


#holtt
holtt <-  holt(Train[1289:1655,'Close..'], type = "additive", damped = F) #holt forecast values
holtf <- forecast(holtt, h = 10)
holtdf <- as.data.frame(holtf)
plot(holtf, ylim = c(0,10000)) 

head(holtdf)


ETS <- ets((Train[,'Close..'])) # ETS forecast values
ETSf <- forecast(ETS, h = 10)
etsdf <- as.data.frame(ETSf)
plot(forecast(ETS, h = 10), ylim = c(0,20000)) 
etsp <- predict(ETS, n.ahead = 10, prediction.interval = T, level = 0.95)
accuracy(etsdf[,1], testdata)

