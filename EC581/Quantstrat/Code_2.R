##################################
# OPTIMIZATION
##################################


### Distributions for paramset analysis
.nsamples=10
.FastSMA = (1:20)
.SlowSMA = (30:80)


### SMA paramset
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
###
summary(get.strategy(strategy.st))
save.strategy(strategy.st)


############################

require(doParallel)
registerDoParallel(cores=8)


results <- apply.paramset(strategy.st,
                          paramset.label='SMA',
                          portfolio.st=portfolio.st,
                          account.st=account.st,
                          nsamples=.nsamples,
                          verbose=TRUE)
stats <- results$tradeStats
View(t(stats))
plot(stats$Profit.Factor,
     stats$Net.Trading.PL,
     xlab='Profit Factor',
     ylab='Net.Trading.PL',
     main='Luxor')
barplot(stats$Profit.To.Max.Draw,
        names.arg=paste(stats$nFAST,stats$nSLOW,sep="/"),
        las=2,cex.names=0.75)






