################################
###   BASE STRATEGY
################################

#Load libraries
library(quantstrat)


if (!exists('.blotter')) .blotter <- new.env()
if (!exists('.strategy')) .strategy <- new.env() 
ls(all=T) #.blotter and .strategy environments added
class(.blotter)

# Define instruments
currency("USD")
stock("BIST",currency="USD",multiplier=1)

# Get data
setwd("D:/GoogleDrive/ACADEMIC/LECTURES/BOUN_2017_2/Lecture Notes/Part_05")#Change path
Data<-read.csv(file = "XU100.csv",sep = ";")
Data<-zoo(Data[,-1],as.Date(as.character(Data[,1]),format="%Y%m%d"))
names(Data)<-c("Open","High","Low","Close","Volume")
plot(Data)

.from='2005-08-01'
.to='2016-05-25'

BIST<-xts(coredata(Data),
          as.POSIXct(time(Data)))#Must be POSIXct
BIST<-BIST[paste0(.from,"/",.to)]

# Define strategy component names
strategy.st = 'GoldenCross'
portfolio.st = 'TrendFollowing'
account.st = 'AkInvestment'

# If you removed all objects from the global environment,
# then you may need to recreate .blotter and .strategy environments
#.blotter<-new.env()
#.strategy<-new.env()

# If you previously run the same strategy: 
# You should first remove old strategy/order book/account/portfolio objects 
rm.strat(strategy.st)
rm.strat(portfolio.st)
rm.strat(account.st)
if (!exists('.blotter')) .blotter <- new.env()
if (!exists('.strategy')) .strategy <- new.env() 
 

# Initialize portfolio&account in .blotter, 
# and orderbook&strategy in .strategy environments
initDate<-as.character(as.Date(.from)-1) # One day before data starts
initEq<-30000

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



# See what's inside the environments
ls(envir=FinancialInstrument:::.instrument)
temp<-get("BIST",envir = FinancialInstrument:::.instrument)
temp<-get("USD",envir = FinancialInstrument:::.instrument)

ls(all=T) #.blotter and .strategy environments are inside Global Env

ls(all=T,envir=.blotter)
temp<-get("account.AkInvestment",envir = .blotter)
temp<-get("portfolio.TrendFollowing",envir = .blotter)

ls(all=T,envir=.strategy)
temp<-get("order_book.TrendFollowing",envir = .strategy)
temp<-get("GoldenCross",envir = .strategy)

class(temp) #Analyze the object class
str(temp) # And its structure 
summary(temp) # Use this especially for strategy object




# Add indicators
.fast = 10
.slow = 20

add.indicator(strategy.st, 
              name = "SMA",
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

summary(get("GoldenCross",envir = .strategy))

# Add signals

add.signal(strategy.st, 
           name='sigCrossover',
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

summary(get("GoldenCross",envir = .strategy))

# Add rules (i.e. when to send orders)
.orderqty = 1
.threshold = 0.005
.txnfees = 0		# round-trip fee

add.rule(strategy.st, 
         name='ruleSignal',
         arguments=list(sigcol='long' , 
                        sigval=TRUE,
                        orderside='long' ,
                        ordertype='stoplimit', 
                        prefer='High', 
                        threshold=.threshold,
                        tmult=TRUE,
                        orderqty=+.orderqty,
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
                        orderqty=-.orderqty,
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


summary(get("GoldenCross",envir = .strategy))


# Apply strategy
applyStrategy(strategy.st, portfolio.st)

# Update portfolio & account
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)

###############################################################################
# Analyze performance
chart.Posn(portfolio.st, "BIST")


###############################################################################
# Get details

# Analyze mktdata object
View(mktdata)

# All orders
OB<-getOrderBook(portfolio.st)[[portfolio.st]]$BIST
View(OB)

# All transactions
Txns<-getTxns(Portfolio=portfolio.st, Symbol="BIST")
View(Txns)

# Stats for each trade
PerTrdStat<-perTradeStats(portfolio.st)
View(PerTrdStat)

# Aggregate trading stats
TrdStat<-t(tradeStats(portfolio.st, 'BIST'))
View(TrdStat)

# Max Adverse/Favorable Excursion Charts
chart.ME(Portfolio=portfolio.st, 
         Symbol='BIST', 
         type='MAE', 
         scale='percent')
chart.ME(Portfolio=portfolio.st, 
         Symbol='BIST', 
         type='MFE', 
         scale='percent')

# Account details
library(lattice)
Acc<-get("account.AkInvestment",envir = .blotter)
xyplot(Acc$summary,type="h",col=4)

# End equity
Eq<-Acc$summary$End.Eq
plot(Eq)
ret <- Return.calculate(Eq,method="log")
charts.PerformanceSummary(ret, 
                          colorset = bluefocus,
                          main="Strategy Performance")

BuyHold<-initEq*BIST[,"Close"][paste(initDate,"/",sep="")]/
  as.numeric(head(BIST[,"Close"][paste(initDate,"/",sep="")],1))                        
ret <- merge(ret,Return.calculate(BuyHold,method="log"))
charts.PerformanceSummary(ret, 
                          colorset = bluefocus,
                          main="Strategy Performance")

# Save end equity for future use
saveRDS(BuyHold,"BH.rds")
saveRDS(Eq,"GC_10_20.rds")


###############################################################################

# save the strategy in an .RData object for later retrieval

save.strategy(strategy.st)




