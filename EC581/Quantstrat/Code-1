######################################
# LUXOR STRATEGY MAIN SETUP
######################################


require(quantstrat)
Sys.setenv(TZ="UTC")

# If you previously run the same strategy: 
# You should first remove old strategy/order book/account/portfolio objects 
rm.strat(strategy.st)
rm.strat(portfolio.st)
rm.strat(account.st)
if (!exists('.blotter')) .blotter <- new.env()
if (!exists('.strategy')) .strategy <- new.env() 



### FinancialInstrument
currency(c('GBP', 'USD'))
exchange_rate('GBPUSD', tick_size=0.0001)

### quantmod
initDate = '2002-10-21'
.from=initDate
.to='2002-10-30'
getSymbols.FI(Symbols='GBPUSD',
              dir=system.file('extdata',package='quantstrat'),
              # dir='~/R/OHLC',
              from=.from, to=.to)

### blotter
strategy.st = 'luxor'
portfolio.st = 'forex'
account.st = 'IB'

initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD')

### quantstrat
initOrders(portfolio.st, initDate=initDate)

### define strategy
strategy(strategy.st, store=TRUE)
summary(get.strategy(strategy.st))

### indicators
.fast = 10
.slow = 30
add.indicator(strategy.st, 
              name = "SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .fast
              ),
              label="nFast"
)
add.indicator(strategy.st, 
              name="SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .slow
              ),
              label="nSlow"
)

summary(get.strategy(strategy.st))

### signals
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

summary(get.strategy(strategy.st))

### rules
.orderqty = 100000
.threshold = 0.0005
.txnfees = -6 # round-trip fee
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , 
                        sigval=TRUE,
                        orderside='long' ,
                        ordertype='stoplimit', prefer='High', threshold=.threshold,
                        orderqty=+.orderqty,
                        replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        replace=TRUE
         ),
         type='exit',
         label='Exit2SHORT'
)
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', 
                        sigval=TRUE,
                        orderside='short',
                        ordertype='stoplimit', prefer='Low', threshold=-.threshold,
                        orderqty=-.orderqty,
                        replace=FALSE
         ),
         type='enter',
         label='EnterSHORT'
)
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='short',
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        replace=TRUE
         ),
         type='exit',
         label='Exit2LONG'
)


summary(get.strategy(strategy.st))


###############################################################################
# Apply strategy
applyStrategy(strategy.st, portfolio.st)

# Updates
updatePortf(portfolio.st, Symbols='GBPUSD')
updateAcct(account.st)
updateEndEq(account.st)

###############################################################################
# Analyze indicators, signals, orders, txns
View(mktdata)
View(getOrderBook(portfolio.st)[[portfolio.st]]$GBPUSD)
View(t(tradeStats(portfolio.st, 'GBPUSD')))
View(perTradeStats(portfolio.st))

# MFE and MAE charts
chart.ME(portfolio.st, 'GBPUSD', scale='percent', type='MAE')
chart.ME(portfolio.st, 'GBPUSD', scale='percent', type='MFE')

# Analyze portfolio object
myPort <- getPortfolio(portfolio.st)
names(myPort)

names(myPort$symbols)
names(myPort$symbols$GBPUSD)
head(myPort$symbols$GBPUSD$txn)
head(myPort$symbols$GBPUSD$posPL.USD)
head(myPort$symbols$GBPUSD$posPL)

names(myPort$summary)

library(lattice)
plot(xyplot(myPort$summary,xlab="",type="h",col=4))

###############################################################################
# Perf chart and equity
chart.Posn(portfolio.st, "GBPUSD")

Eq<-getAccount(account.st)$summary[,"End.Eq"]
plot(as.zoo(Eq))
###############################################################################
# save the strategy in an .RData object for later retrieval
save.strategy(strategy.st)




