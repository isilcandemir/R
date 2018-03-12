################################
###   MULTI ASSET PORTFOLIOS
################################

### Sector ETFs
#XLY: Consumer Discretionary
#XLP: Consumer Staples
#XLE: Energy
#XLF: Financial
#XLV: Health Care
#XLI: Industrial
#XLB: Materials
#XLK: Technology
#XLU: Utilities

getSymbols(c("XLY","XLP","XLE","XLF","XLV","XLI","XLB","XLK","XLU"))

# Define symbols
symbols = c("XLY","XLP","XLE","XLF","XLV","XLI","XLB","XLK","XLU")
for (symbol in symbols){
  stock(symbol, currency="USD",multiplier=1)
}

ls(envir=FinancialInstrument:::.instrument)


# Initialize
multi.asset <- "multiAsset"
rm.strat(multi.asset) # remove strategy etc. if this is a re-run

initPortf(multi.asset,symbols=symbols, initDate=initDate)
initAcct(multi.asset,portfolios=multi.asset, initDate=initDate,
         initEq=initEq)
initOrders(portfolio=multi.asset,initDate=initDate)

applyStrategy(strategy="GoldenCross" , portfolios=multi.asset)

updatePortf(multi.asset)
updateAcct(multi.asset)
updateEndEq(multi.asset)


# Get details

# All orders
OB<-getOrderBook(multi.asset)[[multi.asset]]
names(OB)
View(OB$XLB)

# All transactions
Txns<-getTxns(Portfolio=multi.asset, Symbol="XLB")
View(Txns)

# Stats for each trade
PerTrdStat<-perTradeStats(multi.asset, Symbol="XLB")
View(PerTrdStat)

# Aggregate trading stats
TrdStat<-t(tradeStats(multi.asset))
View(TrdStat)

# Plot results
par(mfrow=c(3,3))
for(symbol in symbols){
  chart.Posn(Portfolio=multi.asset,Symbol=symbol,
             TA="add_SMA(n=20,col='blue')")
}
par(mfrow=c(1,1))

# Individual returns
rets.multi <- PortfReturns(multi.asset)
colnames(rets.multi) <- symbols
rets.multi <- na.omit(cbind(rets.multi,Return.calculate(getAccount(multi.asset)$summary$End.Eq)))
names(rets.multi)[length(names(rets.multi))] <- "TOTAL"
rets.multi <- rets.multi[,c("TOTAL",symbols)]
round(tail(rets.multi,5),6)

chart.CumReturns(rets.multi, colorset= rich10equal, 
                 legend.loc = "topleft",
                 main="Cumulative Returns")

# Performance table
ar.tab<-table.AnnualizedReturns(rets.multi)
ar.tab

max.risk <- max(ar.tab["Annualized Std Dev",])
max.return <- max(ar.tab["Annualized Return",])
min.return <- min(ar.tab["Annualized Return",])

chart.RiskReturnScatter(rets.multi,
        main = "Performance", 
        colorset = rich10equal,
        xlim=c(0,max.risk*1.1),ylim=c(min.return,max.return))

equity <- getAccount(multi.asset)$summary$End.Eq
plot(equity,main="Consolidated Equity Curve")


