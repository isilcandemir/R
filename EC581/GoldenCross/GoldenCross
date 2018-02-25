library(zoo)
library(Quandl)
library(quantmod)
library(TTR)
library(blotter)
library(xts)
library(FinancialInstrument)
getSymbols("SPY",src="yahoo")
getSymbols("XU100.IS",src="yahoo")
P<-SPY
tail(P)

currency("USD")
stock("SPY",currency="USD",multiplier=1)
ls(envir=FinancialInstrument:::.instrument)
Sys.setenv(TZ="UTC")
SPY<-xts(coredata(SPY),as.POSIXct(time(SPY)))
SPY$MA50<-SMA(Cl(SPY),50)
SPY$MA200<-SMA(Cl(SPY),200)
my.strategy<-"GoldenCross"
initPortf(my.strategy, "SPY",initDate = "2006-12-31")
initAcct(my.strategy,portfolios=my.strategy, initDate = "2006-12-31",initEq = 1000000)

ls()
ls(.blotter)
ls(envir=FinancialInstrument:::.instrument)
chart_Series(x=SPY,name="SPY",TA="add_SMA(n=50,col=4);add_SMA(n=200,col=2)")

for (i in 1:nrow(SPY)){
  CurrentDate<-time(SPY)[i]
  equity=getEndEq(my.strategy,CurrentDate)
  ClosePrice<-as.numeric(Cl(SPY[i,]))
  Posn<-getPosQty(my.strategy,Symbol='SPY',Date=CurrentDate)
  UnitSize=as.numeric(trunc(equity/ClosePrice))
  FMA<-as.numeric(SPY[i,"MA50"])
  SMA<-as.numeric(SPY[i,'MA200'])
  if(!(is.na(FMA) | is.na(SMA))){
    if(Posn==0){
      if(FMA>=SMA){
        addTxn(my.strategy,symbol='SPY',TxnDate = CurrentDate,TxnPrice = ClosePrice,TxnQty = UnitSize, TxnFees = 0)
      }
    }else{
      if(FMA<SMA){
        addTxn(my.strategy,Symbol = 'SPY',TxnDate = CurrentDate, TxnPrice = ClosePrice,TxnQty=-Posn,TxnFees = 0)
      } else {
        if(i==nrow(SPY))
addTxn(my.strategy,Symbol = 'SPY',TxnDate = CurrentDate,TxnPrice = ClosePrice,TxnQty = -Posn, TxnFees = 0)
      }
    }
  }
  updatePortf(my.strategy,Dates=CurrentDate)
  updateAcct(my.strategy,Dates=CurrentDate)
  updateEndEq(my.strategy,CurrentDate)
}
