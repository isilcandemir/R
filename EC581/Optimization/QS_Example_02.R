################################
###   OPTIMIZE PARAMETERS
################################


# Define parameter space 
.FastSMA = c(1,3,5,10,15,20,50)
.SlowSMA = c(10,20,50,100,150,200)

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

summary(get("GoldenCross",envir = .strategy))

# Apply parameter optimization
library(doParallel)
detectCores()
registerDoParallel(cores=8) # Parallel computing

# Use nsamples if you want random samples from the parameter space
results <- apply.paramset(strategy.st, 
                          paramset.label='SMA', 
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

# Function for plotting
require(akima)
require(plot3D)

Heat.Map<-function(x,y,z,title){
  s=interp(x,y,z)
  image2D(s,main=title)
}


# Plot results
par(mfrow=c(3,2),mar=c(2,2,2,2)) # 3x2 plots on same page
Heat.Map(stats[,"nFAST"],stats[,"nSLOW"],stats[,"Net.Trading.PL"],"Net.Trading.PL")
Heat.Map(stats[,"nFAST"],stats[,"nSLOW"],stats[,"Percent.Positive"],"Percent.Positive")
Heat.Map(stats[,"nFAST"],stats[,"nSLOW"],stats[,"Profit.Factor"],"Profit.Factor")
Heat.Map(stats[,"nFAST"],stats[,"nSLOW"],stats[,"Ann.Sharpe"],"Ann.Sharpe")
Heat.Map(stats[,"nFAST"],stats[,"nSLOW"],stats[,"Max.Drawdown"],"Max.Drawdown")
Heat.Map(stats[,"nFAST"],stats[,"nSLOW"],stats[,"Profit.To.Max.Draw"],"Profit.To.Max.Draw")
par(mfrow=c(1,1),mar=c(5.1, 4.1, 4.1, 2.1))


# Find the best strategy using Profit/MaxDrawdown: 1 & 20
stats[order(stats[,"Profit.To.Max.Draw"],decreasing = T),
      c("nFAST","nSLOW","Profit.To.Max.Draw")]


# Re-run the strategy with optimized parameters
saveRDS(Eq,"GC_1_20.rds")


#Compare portfolios
GC_10_20<-readRDS("GC_10_20.rds")
GC_1_20<-readRDS("GC_1_20.rds")
BH<-readRDS("BH.rds")

AllRets<-Return.calculate(merge(BH,GC_1_20,GC_10_20),method="log")
names(AllRets)<-c("BH","1_20","10_20")

charts.PerformanceSummary(AllRets, 
                          colorset = bluefocus,
                          main="Strategy Performance")

