library(fpp)
library(forecast)
library(R.utils)
library(rugarch)
library(stats)
library(mFilter)

errorFunction<-function(trueValue,predictedValue){
  den<-trueValue - predictedValue
  nu<-trueValue + predictedValue
  ijkaiLoss<-abs(den/nu)
  # mae<-abs(den/(trueValue + 1))
  # 
  # error<- ijkaiLoss * mae 
}
find.freq <- function(x)
{
  n <- length(x)
  x <- as.ts(x)
  # Remove trend from data
  x <- residuals(tslm(x ~ trend))
  # Compute spectrum by fitting ar model to largest section of x
  n.freq <- 500
  spec <- spec.ar(c(na.contiguous(x)), plot=FALSE, n.freq=n.freq)
  if(max(spec$spec)>10) # Arbitrary threshold chosen by trial and error.
  {
    period <- floor(1/spec$freq[which.max(spec$spec)] + 0.5)
    if(period==Inf) # Find next local maximum
    {
      j <- which(diff(spec$spec)>0)
      if(length(j)>0)
      {
        nextmax <- j[1] + which.max(spec$spec[(j[1]+1):n.freq])
        if(nextmax < length(spec$freq))
          period <- floor(1/spec$freq[nextmax] + 0.5)
        else
          period <- 1L
      }
      else
        period <- 1L
    }
  }
  else
    period <- 1L
  
  return(as.integer(period))
}
start_end_points <- function(x){
  x_is_na <- is.na(x)
  prev_is_na_or_first <- c(TRUE, x_is_na[1:length(x)-1])
  next_is_na_or_last <- c(x_is_na[2:length(x)], TRUE)
  x_is_start_point <- !x_is_na & prev_is_na_or_first
  x_is_end_point <- !x_is_na & next_is_na_or_last
  data.frame(start_point=attributes(x)$index[x_is_start_point],
             end_point=attributes(x)$index[x_is_end_point])
}

plus_one<-function(x)
{
  log(log(length(x)+1)+1)
}

expoenntial.back<-function(x,mp,ml){
  x<-x * mp/ml
  finalX<-round(exp(exp(x)-1)-1)
}


path<-"C:/Users/ceidsness/Documents/School Work/Businesses/"
outputFile="E:/Spring 2017/ML/project//outputMean.out"

file.names <- dir(path, pattern ="*.txt")


as.numeric(gsub('^userPay([0123456789]*)\\.txt$','\\1',file.names))->fileNum;
files.sortednames<-file.names[order(fileNum)]
# 
FileUserPay<-"E:/Spring 2017/ML/project/userPay2.txt"

data<- read.table(FileUserPay,sep=" ",
                  header=FALSE,colClasses=c("character","character"))

    #data <- read.table(paste(path,files.sortednames[j],sep=""),sep=" ",
                   #header=FALSE,colClasses=c("character","character"))

    dataSorted<-data[order(as.Date(data$V1, format="%Y-%m-%d")),]

    dataGrouped<-aggregate(dataSorted$V2, by=list(V1=dataSorted$V1), FUN=plus_one)
    dataGrouped<-dataGrouped[!(dataGrouped$x==0),]
    

    
    n<-nrow(dataGrouped)
    k<-n-14
    amount = floor(k / 5)
    start = 1
    end = amount

    for(id in 1:5)
    {
    trainingData<-ts(dataGrouped[start:end,2],start=c(dataGrouped[1,2]))
    vaidationData<-ts(dataGrouped[(n-13):n,],start=c(dataGrouped[1,2]))

    mae<- matrix(NA,n-k,14)

    
    returnIndex<-row(dataGrouped)[which.max(dataGrouped$x)]
    data<-trainingData    #ts(dataGrouped[,2],start=c(dataGrouped[1,2]),frequency=length(dataGrouped$V1))
    if(n-returnIndex>14){
      data<-tail(data,n-returnIndex)
    }else{
      data<-tail(data,90)
    }
    


    hpFilteredValue<-hpfilter(data,freq=13322500,type=c("lambda"),drift=FALSE)
    hpfit<-auto.arima(hpFilteredValue$trend)
    plot(hpFilteredValue$trend,ylim=range(1:10))
    lines(data,col=2)
    hpfitForecast<-forecast(hpfit,h=14)



    dataOfLast14<-tail(data,14)
    meanOfPredicted14<-mean(hpfitForecast$mean)
    meanOfLast14<-mean(dataOfLast14)
    finalPrediction<-lapply(dataOfLast14, function(x) expoenntial.back(x,meanOfPredicted14,meanOfLast14))
    finalPrediction<-as.matrix(t(finalPrediction))
    
    write.table(finalPrediction, file = "ArimaWithHPFiltering.csv",sep=",",col.names = FALSE,append = TRUE)
    #print(finalPrediction)

    for(i in 1:nrow(vaidationData))
    {
      mae[i,1:nrow(vaidationData)] <- errorFunction(vaidationData[i,2],hpfitForecast[['mean']])

    }

    if (id == 1){
      minVal = mean(mae)
    }
    else if(id == 5){
      start = end + 1
      amount = n - 14
    }else{
      start = end + 1
      end = start + amount
    }
    
    if (mean(mae) < minVal)
    {
      minVal = mean(mae)
    }
    print(paste('least',mean(mae)))
  }

  cat(paste(FileUserPay,minVal,collapse = " "), file=outputFile, append=TRUE, sep = "\n")

#}




















# 
#   data <- read.table(files.sortednames[i],sep=" ",
#                      header=FALSE,colClasses=c("character","character"))
#   #print(files.sortednames[i])
#   dataSorted<-data[order(as.Date(data$V1, format="%Y-%m-%d")),]
#   
#   dataGrouped<-aggregate(dataSorted$V2, by=list(V1=dataSorted$V1), FUN=plus_one)
#   dataGrouped<-dataGrouped[!(dataGrouped$x==0),]
#   
#   n<-nrow(dataGrouped)
#   k<-n-14
#   trainingData<-ts(dataGrouped[1:(n-14),2],start=c(dataGrouped[1,2]))
#   vaidationData<-ts(dataGrouped[(n-13):n,],start=c(dataGrouped[1,2]))
#   
#   mae1 <- mae2 <- matrix(NA,n-k,14)
#   
#   
#   data<-ts(dataGrouped[,2],start=c(dataGrouped[1,2]))
#   
#   stupidData<-append(tail(data,7),tail(data,7))
#   
#   #1stMethod
#   fitStupid<-auto.arima(stupidData)
#   stupidForcast<-forecast(fitStupid,h=14)
#   #finalStupidPrediction<-lapply(stupidForcast$mean, function(x) exp(exp(x)-1)-1)
# 
#   #2ndMethod
#   hpFilteredValue<-hpfilter(stupidData,freq=13322500,type=c("lambda"),drift=FALSE)
#   hpfit<-auto.arima(hpFilteredValue$trend)
#   plot(hpFilteredValue$trend,ylim=range(1:10))
#   lines(stupidData,col=2)
#   hpfitForecast<-forecast(hpfit,h=14)
#   # 
#   # 
#   # dataOfLast14<-tail(data,14)
#    meanOfPredicted14<-mean(hpfitForecast$mean)
#    meanOfLast14<-mean(stupidData)
#    for(i in 1:nrow(vaidationData))
#    {
#      mae1[i,1:nrow(vaidationData)] <- errorFunction(vaidationData[i,2],stupidForcast[['mean']])
#      mae2[i,1:nrow(vaidationData)] <- errorFunction(vaidationData[i,2],hpfitForecast[['mean']])
#    }
#    
#    #finalStupidPredictionWithHp<-lapply(stupidData, function(x) expoenntial.back(x,meanOfPredicted14,meanOfLast14))
#   #finalPrediction<-as.matrix(t(finalPrediction))
#   
#    meanOfMethod1<-mean(mae1)
#    meanOfMethod2<-mean(mae2)
#    
#    
#    # if(meanOfMethod1<meanOfMethod2){
#    #   finalPrediction<-lapply(stupidForcast$mean, function(x) exp(exp(x)-1)-1)
#    #   finalPrediction<-as.matrix(round(t(finalPrediction[[1]])))
#    #}else{
#      finalPrediction<-lapply(stupidData, function(x) expoenntial.back(x,meanOfPredicted14,meanOfLast14))
#      finalPrediction<-as.matrix(t(finalPrediction))
#    #}
#    # 
# 
#   write.table(finalPrediction, file = "ArimaWithHPFiltering.csv",sep=",",col.names = FALSE,append = TRUE)
#   
#   #write.table(finalPrediction, file = "ArimaWithHPFiltering.csv",sep=",",col.names = FALSE,append = TRUE)
#   #cat(paste(fcast2[[1]],collapse = " "), file=outputFile, append=TRUE, sep = "\n")
#   
# #}










