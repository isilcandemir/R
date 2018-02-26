strategy.st <- 'luxor'
load.strategy(strategy.st)
###
initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')

addPosLimit(
  portfolio=portfolio.st,
  symbol='GBPUSD',
  timestamp=initDate,
  maxpos=.orderqty)

initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD')
initOrders(portfolio.st, initDate=initDate)


### uncomment lines to activate StopLoss and/or StopTrailing and/or TakeProfit rules
enable.rule('luxor', 'chain', 'StopLoss')
#enable.rule('luxor', 'chain', 'StopTrailing')
#enable.rule('luxor', 'chain', 'TakeProfit')


############################
require(doParallel)
registerDoParallel(cores=8)
results <- apply.paramset(strategy.st,
                          paramset.label='StopLoss',
                          portfolio.st=portfolio.st,
                          account.st=account.st,
                          nsamples=10,
                          verbose=TRUE)
stats <- results$tradeStats
View(t(stats))