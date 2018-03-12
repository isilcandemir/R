################################
###   OPTIMIZE MONEY MANAGEMENT
################################

# STOP LOSS PARAMSET
.StopLoss = seq(0.01, 0.07, length.out=20)

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


# TRAILING STOPLOSS PARAMSET
.StopTrailing = seq(0.01, 0.07, length.out=20)

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


# TAKE PROFIT PARAMSET
.TakeProfit = seq(0.01, 0.07, length.out=20)


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


summary(get("GoldenCross.MM",envir = .strategy))


# Use nsamples if you want random samples from the parameter space
registerDoParallel(cores=8) # Parallel computing
results <- apply.paramset(strategy.st, 
                          paramset.label='StopLoss', 
                          portfolio.st=portfolio.st, 
                          account.st=account.st, 
                          verbose=TRUE)
results <- apply.paramset(strategy.st, 
                          paramset.label='StopTrailing', 
                          portfolio.st=portfolio.st, 
                          account.st=account.st, 
                          verbose=TRUE)
results <- apply.paramset(strategy.st, 
                          paramset.label='TakeProfit', 
                          portfolio.st=portfolio.st, 
                          account.st=account.st, 
                          verbose=TRUE)

stopImplicitCluster()

# Analyze results
class(results) # A long list object containing results
names(results) # "tradeStats" contains summaries

stats <- results$tradeStats
View(stats)
names(stats)

# Plot results
par(mfrow=c(3,2),mar=c(2,4,2,2)) # 3x2 plots on same page
plot(stats[,1],stats[,"Net.Trading.PL"],
     type="b",las=1,xlab="",ylab="",main="Net.Trading.PL")
plot(stats[,1],stats[,"Percent.Positive"],
     type="b",las=1,xlab="",ylab="",main="Percent.Positive")
plot(stats[,1],stats[,"Profit.Factor"],
     type="b",las=1,xlab="",ylab="",main="Profit.Factor")
plot(stats[,1],stats[,"Ann.Sharpe"],
     type="b",las=1,xlab="",ylab="",main="Ann.Sharpe")
plot(stats[,1],stats[,"Max.Drawdown"],
     type="b",las=1,xlab="",ylab="",main="Max.Drawdown")
plot(stats[,1],stats[,"Profit.To.Max.Draw"],
     type="b",las=1,xlab="",ylab="",main="Profit.To.Max.Draw")
par(mfrow=c(1,1),mar=c(5.1, 4.1, 4.1, 2.1))


# Find the best strategy using Profit/MaxDrawdown: 1 & 20
stats[order(stats[,"Profit.To.Max.Draw"],decreasing = T),
      c("StopLossLONG","Profit.To.Max.Draw")]



