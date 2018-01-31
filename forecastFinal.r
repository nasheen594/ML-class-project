library(fpp)
library(forecast)
library(R.utils)
library(rugarch)
library(stats)
library(mFilter)
library(xts)

errorFunction<-function(trueValue,predictedValue){
  den<-trueValue - predictedValue
  #nu<-trueValue + predictedValue
  #ijkaiLoss<-abs(den/nu)
  mae<-abs(den/trueValue)
  
  #error<- ijkaiLoss * mae 
  error<- mae
}
Gaussian.Filter<-function(x,sigma)
{
  return ( exp(-(x^2/(2*sigma^2)))/sqrt(2*pi*sigma^2) )
}

plus_one<-function(x)
{
  log(log(length(x)+1)+1)
}

path<-"E:/Spring 2017/ML/project/"
#outputFile="C:/Users/Nasheen Nur/Downloads/attachments/outputFinal.out"

file.names <- dir(path, pattern ="*.txt")
#
#
as.numeric(gsub('^userPay([0123456789]*)\\.txt$','\\1',file.names))->fileNum;
files.sortednames<-file.names[order(fileNum)]
for(i in 1:length(files.sortednames)){
  print(files.sortednames[i])

data<- read.table(files.sortednames[i],sep=" ",
                  header=FALSE,colClasses=c("character","character"))
#View(data)

dataSorted<-data[order(as.Date(data$V1, format="%Y-%m-%d")),]

dataGrouped<-aggregate(dataSorted$V2, by=list(V1=dataSorted$V1), FUN=length)
lengofData<-nrow(dataGrouped)
data<-xts(dataGrouped[,-1], order.by=as.Date(dataGrouped[,1], "%Y-%m-%d"),frequency = lengofData)

# salestimeseriescomponents <- decompose(data)
# plot(salestimeseriescomponents)

hpFilteredValue<-hpfilter(data,freq=13322500,type=c("lambda"),drift=FALSE)


n<-nrow(hpFilteredValue$trend)
k<-n-14



trainingData<-head(hpFilteredValue$trend,n-14)
vaidationData<-tail(hpFilteredValue$trend,14)

mae1<-mae2 <- mae3 <- matrix(NA,n-k,14)
#
#
fit1 <- auto.arima(trainingData)
fcast1 <- forecast(fit1, h=14)

fit3<-arfima(trainingData)
fcast3<-forecast(fit3,h=14)

aicArima=AIC(fit1)
aicArfima=AIC(fit3)

if(aicArima<aicArfima)
{
  bestfit <- list(aicc=Inf)
  for(i in 1:25)
  {
    fit <- auto.arima(trainingData,seasonal=FALSE)
    if(fit$aicc < bestfit$aicc)
      bestfit <- fit
    else break;
  }
  fc <- forecast(bestfit, h=14)
  #plot(fc)
}else{
  bestfit <- list(aicc=Inf)
  for(i in 1:25)
  {
    fit <- arfima(trainingData, seasonal=FALSE)
    if(fit$aicc < bestfit$aicc)
      bestfit <- fit
    else break;
  }
  fc <- forecast(bestfit, h=14)
  
 # plot(fc)
}
FF <- as.matrix(round(t(fc$mean)))
#print(FF)
write.table(FF, file = "ARIMAUsingBoostingAndFiltering.csv",sep=",",col.names = FALSE,append = TRUE)
#write.table(FF, file = "ARIMAUsingGaussian.csv",sep=",",col.names = FALSE,append = TRUE)

}