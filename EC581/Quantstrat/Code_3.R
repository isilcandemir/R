# If you previously run the same strategy: 
# You should first remove old strategy/order book/account/portfolio objects 
rm.strat(strategy.st)
rm.strat(portfolio.st)
rm.strat(account.st)
if (!exists('.blotter')) .blotter <- new.env()
if (!exists('.strategy')) .strategy <- new.env() 




### parameters
.fast = 6
.slow = 44

.stoploss <- 0.05
.stoptrailing <- 0.07
.takeprofit <- 0.2

.StopLoss<-(1:10)/100
.StopTrailing<-(1:10)/100+0.005
.TakeProfit<-seq(5,40,length.out = 10)/100
###
strategy(strategy.st, store=TRUE)
### indicators
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
### rules ############
# normal entry rules
add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        replace=FALSE,
                        orderside='long' ,
                        ordertype='stoplimit',
                        prefer='High',
                        threshold=.threshold,
                        TxnFees=0,
                        orderqty=+.orderqty,
                        osFUN=osMaxPos,
                        orderset='ocolong'
         ),
         type='enter',
         label='EnterLONG'
)
add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        replace=FALSE,
                        orderside='short',
                        ordertype='stoplimit',
                        prefer='Low',
                        threshold=.threshold,
                        TxnFees=0,
                        orderqty=-.orderqty,
                        osFUN=osMaxPos,
                        orderset='ocoshort'
         ),
         type='enter',
         label='EnterSHORT'
)
# normal exit rules
add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        replace=TRUE,
                        orderside='short',
                        ordertype='market',
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocoshort'
         ),
         type='exit',
         label='Exit2LONG'
)
add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        replace=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocolong'
         ),
         type='exit',
         label='Exit2SHORT'
)
### parameter sets
# SMA
add.distribution(strategy.st,
                 paramset.label = 'SMA',
                 component.type = 'indicator',
                 component.label = 'nFast',
                 variable = list(n = .FastSMA),
                 label = 'nFAST'
)
add.distribution(strategy.st,
                 paramset.label = 'SMA',
                 component.type = 'indicator',
                 component.label = 'nSlow',
                 variable = list(n = .SlowSMA),
                 label = 'nSLOW'
)
add.distribution.constraint(strategy.st,
                            paramset.label = 'SMA',
                            distribution.label.1 = 'nFAST',
                            distribution.label.2 = 'nSLOW',
                            operator = '<',
                            label = 'SMA'
)
# stop-loss
add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        replace=FALSE,
                        orderside='long',
                        ordertype='stoplimit', 
                        tmult=TRUE, 
                        threshold=quote(.stoploss),
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocolong'
         ),
         type='chain', 
         parent='EnterLONG',
         label='StopLossLONG',
         enabled=FALSE
)
add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='short' , sigval=TRUE,
                        replace=FALSE,
                        orderside='short',
                        ordertype='stoplimit', 
                        tmult=TRUE, 
                        threshold=quote(.stoploss),
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocoshort'
         ),
         type='chain', 
         parent='EnterSHORT',
         label='StopLossSHORT',
         enabled=FALSE
)
add.distribution(strategy.st,
                 paramset.label = 'StopLoss',
                 component.type = 'chain',
                 component.label = 'StopLossLONG',
                 variable = list(threshold = .StopLoss),
                 label = 'StopLossLONG'
)
add.distribution(strategy.st,
                 paramset.label = 'StopLoss',
                 component.type = 'chain',
                 component.label = 'StopLossSHORT',
                 variable = list(threshold = .StopLoss),
                 label = 'StopLossSHORT'
)
add.distribution.constraint(strategy.st,
                            paramset.label = 'StopLoss',
                            distribution.label.1 = 'StopLossLONG',
                            distribution.label.2 = 'StopLossSHORT',
                            operator = '==',
                            label = 'StopLoss'
)
# stop-trailing
add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        replace=FALSE,
                        orderside='long',
                        ordertype='stoptrailing', 
                        tmult=TRUE, 
                        threshold=quote(.stoptrailing),
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocolong'
         ),
         type='chain', 
         parent='EnterLONG',
         label='StopTrailingLONG',
         enabled=FALSE
)
add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='short' , sigval=TRUE,
                        replace=FALSE,
                        orderside='short',
                        ordertype='stoptrailing', 
                        tmult=TRUE, 
                        threshold=quote(.stoptrailing),
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocoshort'
         ),
         type='chain', 
         parent='EnterSHORT',
         label='StopTrailingSHORT',
         enabled=FALSE
)
add.distribution(strategy.st,
                 paramset.label = 'StopTrailing',
                 component.type = 'chain',
                 component.label = 'StopTrailingLONG',
                 variable = list(threshold = .StopTrailing),
                 label = 'StopTrailingLONG'
)
add.distribution(strategy.st,
                 paramset.label = 'StopTrailing',
                 component.type = 'chain',
                 component.label = 'StopTrailingSHORT',
                 variable = list(threshold = .StopTrailing),
                 label = 'StopTrailingSHORT'
)
add.distribution.constraint(strategy.st,
                            paramset.label = 'StopTrailing',
                            distribution.label.1 = 'StopTrailingLONG',
                            distribution.label.2 = 'StopTrailingSHORT',
                            operator = '==',
                            label = 'StopTrailing'
)
# take-profit
add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        replace=FALSE,
                        orderside='long',
                        ordertype='limit', 
                        tmult=TRUE, 
                        threshold=quote(.takeprofit),
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocolong'
         ),
         type='chain', parent='EnterLONG',
         label='TakeProfitLONG',
         enabled=FALSE
)
add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='short' , sigval=TRUE,
                        replace=FALSE,
                        orderside='short',
                        ordertype='limit', tmult=TRUE, threshold=quote(.takeprofit),
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocoshort'
         ),
         type='chain', parent='EnterSHORT',
         label='TakeProfitSHORT',
         enabled=FALSE
)
add.distribution(strategy.st,
                 paramset.label = 'TakeProfit',
                 component.type = 'chain',
                 component.label = 'TakeProfitLONG',
                 variable = list(threshold = .TakeProfit),
                 label = 'TakeProfitLONG'
)
add.distribution(strategy.st,
                 paramset.label = 'TakeProfit',
                 component.type = 'chain',
                 component.label = 'TakeProfitSHORT',
                 variable = list(threshold = .TakeProfit),
                 label = 'TakeProfitSHORT'
)
add.distribution.constraint(strategy.st,
                            paramset.label = 'TakeProfit',
                            distribution.label.1 = 'TakeProfitLONG',
                            distribution.label.2 = 'TakeProfitSHORT',
                            operator = '==',
                            label = 'TakeProfit'
)
summary(get.strategy(strategy.st))
save.strategy(strategy.st)
