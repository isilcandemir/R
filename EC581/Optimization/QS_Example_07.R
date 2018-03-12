################################
###   POSITION SIZING
################################

args(ruleSignal) #Default rule function

args(osNoOp) #Default order sizing function


# Volatility based order sizing function
# This order sizing function adjusts the share quantity such that the
# transaction value is approximately equal to a pre-defined multiple of
# target volatility/current volatility
# orderqty = tradeSize * target volatility / current volatility

osVol_Long <- function(timestamp,orderqty, portfolio, symbol, ruletype, ...)
{
  ClosePrice <- as.numeric(Cl(mktdata[timestamp,"Close"]))
  Last.ATR <- as.numeric(mktdata[timestamp,"atr.ATR"])/ClosePrice
  orderqty <- floor(tradeSize*0.025/Last.ATR)
  return(orderqty)
}
osVol_Short <- function(timestamp,orderqty, portfolio, symbol, ruletype, ...)
{
  ClosePrice <- as.numeric(Cl(mktdata[timestamp,"Close"]))
  Last.ATR <- as.numeric(mktdata[timestamp,"atr.ATR"])/ClosePrice
  orderqty <- floor(tradeSize*0.025/Last.ATR)
  return(-orderqty)
}


# Define strategy component names
strategy.st = 'GoldenCross_Vol'
portfolio.st = 'TrendFollowing_Vol'
account.st = 'AkInvestment_Vol'

#Initialize
initDate<-as.character(as.Date(.from)-1) # One day before data starts
initEq<-1000000

initPortf(portfolio.st, 
          symbols='BIST', 
          initDate=initDate, 
          currency='USD')
initAcct(account.st, 
         portfolios=portfolio.st, 
         initDate=initDate, 
         currency='USD',
         initEq=initEq)
initOrders(portfolio.st, 
           initDate=initDate)
strategy(strategy.st, 
         store=TRUE)


# Add indicators
.fast = 10
.slow = 20
add.indicator(strategy.st, name = "SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .fast
              ),
              label="nFast"
)

add.indicator(strategy.st, name="SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .slow
              ),
              label="nSlow"
)

add.indicator(strategy.st, name="ATR",
              arguments = list(
                HLC = quote(mktdata[,c("High","Low","Close")]),
                n = 14,
                maType="EMA"),
              label="ATR"
)

# Add signals

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="gte"
           ),
           label='long'
)

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="lt"
           ),
           label='short'
)

# Add rules (i.e. when to send orders)
.orderqty = 1
.threshold = 0.005
.txnfees = 0		# round-trip fee
tradeSize=100000

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , 
                        sigval=TRUE,
                        orderside='long' ,
                        ordertype='stoplimit', 
                        prefer='High', 
                        threshold=.threshold,
                        tmult=TRUE,
                        #   orderqty=+.orderqty,
                        osFUN=osVol_Long,
                        replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', 
                        sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees, #Only on exits
                        replace=TRUE #Replace any pending open orders
         ),
         type='exit',
         label='Exit2SHORT'
)


add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', 
                        sigval=TRUE,
                        orderside='short',
                        ordertype='stoplimit', 
                        prefer='Low', 
                        threshold=-.threshold,
                        tmult=TRUE,
                        #  orderqty=-.orderqty,
                        osFUN=osVol_Short,
                        replace=FALSE
         ),
         type='enter',
         label='EnterSHORT'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , 
                        sigval=TRUE,
                        orderside='short',
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,#Only on exits
                        replace=TRUE #Replace any pending open orders
         ),
         type='exit',
         label='Exit2LONG'
)

# Apply strategy
applyStrategy(strategy.st, portfolio.st)

# Update portfolio & account
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)

###############################################################################
# Analyze performance
chart.Posn(portfolio.st, "BIST")

PerTrdStat<-perTradeStats(portfolio.st)
View(PerTrdStat)

tmp<-ATR(BIST[,c("High","Low","Close")],n=14,maType = "EMA")

plot(tradeSize*0.025/as.numeric(tmp[PerTrdStat[,1],"atr"]/BIST[PerTrdStat[,1],"Close"]),
     as.numeric(abs(PerTrdStat[,"Max.Notional.Cost"])),
     las=1,xlab="Target Position Size",ylab="Actual Position Size")




