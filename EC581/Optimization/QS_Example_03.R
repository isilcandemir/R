################################
###   ADD MONEY MANAGEMENT
################################

# Define component names for new strategy
strategy.st = 'GoldenCross.MM'
portfolio.st = 'TrendFollowing.MM'
account.st = 'AkInvestment.MM'

#Initialize
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
.fast = 1
.slow = 20

add.indicator(strategy.st, name = "SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = 1
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

summary(get("GoldenCross.MM",envir = .strategy))

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

summary(get("GoldenCross.MM",envir = .strategy))

# Add rules (i.e. when to send orders)
.orderqty = 1
.threshold = 0.005
.txnfees = 0		# round-trip fee

.stoploss <- 0.03
.stoptrailing <- 0.04
.takeprofit <- 0.05

# LONG SIDE: Enter
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , 
                        sigval=TRUE,
                        orderside='long' ,
                        ordertype='stoplimit', 
                        prefer='High', 
                        threshold=.threshold,
                        tmult=TRUE,
                        orderqty=+.orderqty,
                        replace=FALSE,
                        orderset="ocolong"
         ),
         type='enter',
         label='EnterLONG'
)

# LONG SIDE: Signal Based Exit
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', 
                        sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees, #Only on exits
                        replace=TRUE, #Replace any pending open orders
                        orderset="ocolong"
         ),
         type='exit',
         label='Exit2SHORT'
)

# SHORT SIDE: Enter
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', 
                        sigval=TRUE,
                        orderside='short',
                        ordertype='stoplimit', 
                        prefer='Low', 
                        threshold=-.threshold,
                        tmult=TRUE,
                        orderqty=-.orderqty,
                        replace=FALSE,
                        orderset="ocoshort"
         ),
         type='enter',
         label='EnterSHORT'
)

# SHORT SIDE: Signal Based Exit
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , 
                        sigval=TRUE,
                        orderside='short',
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,#Only on exits
                        replace=TRUE, #Replace any pending open orders
                        orderset="ocoshort"
         ),
         type='exit',
         label='Exit2LONG'
)


####
# LONG SIDE: Stop Loss
add.rule(strategy.st, 
         name = 'ruleSignal',
         arguments=list(sigcol='long' , 
                        sigval=TRUE,
                        replace=FALSE,
                        orderside='long',
                        ordertype='stoplimit', 
                        tmult=TRUE, 
                        threshold=.stoploss,
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocolong'
         ),
         type='chain', 
         parent='EnterLONG',
         label='StopLossLONG',
         enabled=TRUE
)
# SHORT SIDE: Stop Loss
add.rule(strategy.st, 
         name = 'ruleSignal',
         arguments=list(sigcol='short' , 
                        sigval=TRUE,
                        replace=FALSE,
                        orderside='short',
                        ordertype='stoplimit', 
                        tmult=TRUE, 
                        threshold=.stoploss,
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocoshort'
         ),
         type='chain', 
         parent='EnterSHORT',
         label='StopLossSHORT',
         enabled=TRUE
)




# LONG SIDE: Trailing Stop Loss
add.rule(strategy.st, 
         name = 'ruleSignal',
         arguments=list(sigcol='long' , 
                        sigval=TRUE,
                        replace=FALSE,
                        orderside='long',
                        ordertype='stoptrailing', 
                        tmult=TRUE, 
                        threshold=.stoptrailing,
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocolong'
         ),
         type='chain', parent='EnterLONG',
         label='StopTrailingLONG',
         enabled=TRUE
)
# SHORT SIDE: Trailing Stop Loss
add.rule(strategy.st, 
         name = 'ruleSignal',
         arguments=list(sigcol='short' , 
                        sigval=TRUE,
                        replace=FALSE,
                        orderside='short',
                        ordertype='stoptrailing', 
                        tmult=TRUE, 
                        threshold=.stoptrailing,
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocoshort'
         ),
         type='chain', 
         parent='EnterSHORT',
         label='StopTrailingSHORT',
         enabled=TRUE
)



# LONG SIDE: Take-profit
add.rule(strategy.st, 
         name = 'ruleSignal',
         arguments=list(sigcol='long' , 
                        sigval=TRUE,
                        replace=FALSE,
                        orderside='long',
                        ordertype='limit', 
                        tmult=TRUE, 
                        threshold=.takeprofit,
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocolong'
         ),
         type='chain', parent='EnterLONG',
         label='TakeProfitLONG',
         enabled=TRUE
)
# SHORT SIDE: Take-profit
add.rule(strategy.st, 
         name = 'ruleSignal',
         arguments=list(sigcol='short' , 
                        sigval=TRUE,
                        replace=FALSE,
                        orderside='short',
                        ordertype='limit', 
                        tmult=TRUE, 
                        threshold=.takeprofit,
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocoshort'
         ),
         type='chain', parent='EnterSHORT',
         label='TakeProfitSHORT',
         enabled=TRUE
)



summary(get("GoldenCross.MM",envir = .strategy))


# Apply strategy
applyStrategy(strategy.st, portfolio.st)

# Update portfolio & account
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)

# Analyze performance
chart.Posn(portfolio.st, "BIST")



# End equity
Acc<-get("account.AkInvestment.MM",envir = .blotter)
Eq<-Acc$summary$End.Eq
saveRDS(Eq,"GC_1_20_MM.rds")


#Compare portfolios
GC_10_20<-readRDS("GC_10_20.rds")
GC_1_20<-readRDS("GC_1_20.rds")
GC_1_20_MM<-readRDS("GC_1_20_MM.rds")
BH<-readRDS("BH.rds")

AllRets<-Return.calculate(merge(BH,GC_1_20,GC_1_20_MM),method="log")
names(AllRets)<-c("BH","1_20","1_20_MM")

charts.PerformanceSummary(AllRets, 
                          colorset = rich6equal,
                          main="Strategy Performance")



