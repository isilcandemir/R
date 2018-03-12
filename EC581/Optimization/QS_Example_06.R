################################
###   POSITION SIZING
################################

args(ruleSignal) #Default rule function

args(osNoOp) #Default order sizing function


# Fixed-dollar order sizing function
# This order sizing function adjusts the share quantity such that the
# transaction value is approximately equal to a pre-defined tradesize
# orderqty = tradeSize / ClosePrice

osFixedDollar_Long <- function(timestamp,orderqty, portfolio, symbol, ruletype, ...)
{
  ClosePrice <- as.numeric(Cl(mktdata[timestamp,"Close"]))
  orderqty <- floor(tradeSize/ClosePrice)
  return(orderqty)
}
osFixedDollar_Short <- function(timestamp,orderqty, portfolio, symbol, ruletype, ...)
{
  ClosePrice <- as.numeric(Cl(mktdata[timestamp,"Close"]))
  orderqty <- floor(tradeSize/ClosePrice)
  return(-orderqty)
}


# Define strategy component names
strategy.st = 'GoldenCross_Fixed'
portfolio.st = 'TrendFollowing_Fixed'
account.st = 'AkInvestment_Fixed'

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
tradeSize=1000000

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , 
                        sigval=TRUE,
                        orderside='long' ,
                        ordertype='stoplimit', 
                        prefer='High', 
                        threshold=.threshold,
                        tmult=TRUE,
                     #   orderqty=+.orderqty,
                        osFUN=osFixedDollar_Long,
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
                        osFUN=osFixedDollar_Short,
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

barplot(abs(PerTrdStat[,"Max.Notional.Cost"]),las=1)
abline(h=tradeSize,col=2)





