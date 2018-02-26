library(zoo)
library(Quandl)
library(quantmod)
library(TTR)
library(blotter)
library(xts)
library(FinancialInstrument)
library(PerformanceAnalytics)
library(graphics)
getSymbols("XU100.IS",src="yahoo")
P<-XU100.IS[complete.cases(XU100.IS),]
P<-P[-2789,]
#There is a mistake in Feb,2018 data. Because between 9 Feb and 23 Feb 
#is missing, i deleted 23 Feb. 
P$return<-Cl(P)/lag(Cl(P),1)-1

Ind<-P$return-lag(P$return,1)
Sig<-NA*Ind
for (i in 3:length(Ind)){
  if(as.numeric(Ind[i])>0) { 
    Sig[i]<-1 
    }
  else if (as.numeric(Ind[i])<=0) { 
    Sig[i]<- -1
    }
}

Pos<-lag(na.locf(Sig),1)
Port.Rets<-na.omit(Pos*P$return)
Port<-cumprod(1+Port.Rets)
BuyHold.Rets<-window(P$return,time(Port.Rets))
BuyHold<-cumprod(1+BuyHold.Rets)
plot(Port,col="black", main="", xlab="x",ylab="y")
lines(BuyHold,col="red")

  
