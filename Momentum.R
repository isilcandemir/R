library(zoo)
library(Quandl)
library(quantmod)
library(TTR)
library(blotter)
library(xts)
library(FinancialInstrument)
getSymbols("XU100.IS",src="yahoo")
P<-XU100.IS[complete.cases(XU100.IS),]
P<-P[-2789,]
MA.Slow<-SMA(x=Cl(P),n=200)
MA.Fast<-SMA(x=Cl(P),n=50)
plot(as.zoo(Cl(P)),col="grey")   
lines(as.zoo(MA.Slow),col="red",lwd=2)
lines(as.zoo(MA.Fast),col="blue",lwd=2)
legend("topleft",c("Price","Slow MA","Fast MA"),text.col=c(1,2,4),bty="n")

Ind<-MA.Fast-MA.Slow
Sig<-NA*Ind
for (i in 201:length(Ind)){
  if((as.numeric(Ind[i-1])<0) & 
     (as.numeric(Ind[i])>=0)){
      Sig[i]<-1
    }
    if ((as.numeric(Ind[i-1])>0)&
  (as.numeric(Ind[i])<=0)){
    Sig[i]<--1
  }
}
Sig[complete.cases(Sig),]
Sig[278,]<-NA
Pos<-lag(na.locf(Sig),1)
Rets<-Cl(P)/lag(Cl(P),1)-1
Port.Rets<-na.omit(Pos*Rets)
Port<-cumprod(1+Port.Rets)
BuyHold.Rets<-window(Rets,time(Port.Rets))
BuyHold<-cumprod(1+BuyHold.Rets)
plot(Port,col="red")
lines(BuyHold,col="green")
  