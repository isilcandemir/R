library(zoo)
library(Quandl)
library(quantmod)
library(TTR)
library(blotter)
library(xts)
library(FinancialInstrument)
library(PerformanceAnalytics)
library(graphics)
library(xts)
library(moments)

#Loading data
getSymbols("XU100.IS",src="yahoo")
getSymbols("^DJI",src="yahoo")
getSymbols("^GSPC",src="yahoo")
getSymbols("GOOG",src="yahoo")
getSymbols("AKBNK.IS",src="yahoo")
getSymbols("EURTRY=X",src="yahoo")
getSymbols("TRY=X",src="yahoo")
WTI <- file.path("~", "Desktop", "wti.csv")
WTI<-read.csv(WTI, stringsAsFactors = FALSE)
WTI$Date<-as.Date(WTI$Date, "%B %d, %Y")
WTI<-WTI[order(WTI$Date), ]
WTI<-WTI[-c(2569,2570), ]
WTI <- xts(x=WTI[,-1],order.by= as.Date(WTI$Date))
WTI<-WTI$Price
BRENT <- file.path("~", "Desktop", "brent.csv")
BRENT<-read.csv(BRENT, stringsAsFactors = FALSE)
BRENT$Date<-as.Date(BRENT$Date, "%B %d, %Y")
BRENT<-BRENT[order(BRENT$Date), ]
BRENT<-BRENT[-c(2583,2584), ]
BRENT <- xts(x=BRENT[,-1],order.by= as.Date(BRENT$Date))
BRENT<-BRENT$Price

#To make date range same for all data, I deleted 2007 and 2018. 
BIST<-XU100.IS["2008/2017"]
BIST<-na.omit(BIST$XU100.IS.Close)
remove("XU100.IS")
DJ<-DJI["2008/2017"]
DJ<-na.omit(DJ$DJI.Close)
remove(DJI)
EUR<-`EURTRY=X`["2008/2017"]
EUR<-na.omit(EUR$`EURTRY=X.Close`)
remove(`EURTRY=X`)
USD<-`TRY=X`["2008/2017"]
USD<-na.omit(USD$`TRY=X.Close`)
remove(`TRY=X`)
GOOGLE<-GOOG["2008/2017"]
GOOGLE<-na.omit(GOOGLE$GOOG.Close)
remove(GOOG)
SP<-GSPC["2008/2017"]
SP<-na.omit(SP$GSPC.Close)
remove(GSPC)
AKBANK<-AKBNK.IS["2008/2017"]
AKBANK<-na.omit(AKBANK$AKBNK.IS.Close)
remove(AKBNK.IS)

#calculating returns
AKBANK$return<-ROC(AKBANK$AKBNK.IS.Close)
BIST$return<-ROC(BIST$XU100.IS.Close)
DJ$return<-ROC(DJ$DJI.Close)
EUR$return<-ROC(EUR$`EURTRY=X.Close`)
GOOGLE$return<-ROC(GOOGLE$GOOG.Close)
SP$return<-ROC(SP$GSPC.Close)
USD$return<-ROC(USD$`TRY=X.Close`)
BRENT$Price<-as.numeric(BRENT$Price)
BRENT$return<-ROC(BRENT$Price)
WTI$Price<-as.numeric(WTI$Price)
WTI$return<-ROC(WTI$Price)

#for different years
AKBANK.2008<-AKBANK["2008"]
AKBANK.2009<-AKBANK["2009"]
AKBANK.2010<-AKBANK["2010"]
AKBANK.2011<-AKBANK["2011"]
AKBANK.2012<-AKBANK["2012"]
AKBANK.2013<-AKBANK["2013"]
AKBANK.2014<-AKBANK["2014"]
AKBANK.2015<-AKBANK["2015"]
AKBANK.2016<-AKBANK["2016"]
AKBANK.2017<-AKBANK["2017"]
BIST.2008<-BIST["2008"]
BIST.2009<-BIST["2009"]
BIST.2010<-BIST["2010"]
BIST.2011<-BIST["2011"]
BIST.2012<-BIST["2012"]
BIST.2013<-BIST["2013"]
BIST.2014<-BIST["2014"]
BIST.2015<-BIST["2015"]
BIST.2016<-BIST["2016"]
BIST.2017<-BIST["2017"]
BRENT.2008<-BRENT["2008"]
BRENT.2009<-BRENT["2009"]
BRENT.2010<-BRENT["2010"]
BRENT.2011<-BRENT["2011"]
BRENT.2012<-BRENT["2012"]
BRENT.2013<-BRENT["2013"]
BRENT.2014<-BRENT["2014"]
BRENT.2015<-BRENT["2015"]
BRENT.2016<-BRENT["2016"]
BRENT.2017<-BRENT["2017"]
DJ.2008<-DJ["2008"]
DJ.2009<-DJ["2009"]
DJ.2010<-DJ["2010"]
DJ.2011<-DJ["2011"]
DJ.2012<-DJ["2012"]
DJ.2013<-DJ["2013"]
DJ.2014<-DJ["2014"]
DJ.2015<-DJ["2015"]
DJ.2016<-DJ["2016"]
DJ.2017<-DJ["2017"]
EUR.2008<-EUR["2008"]
EUR.2009<-EUR["2009"]
EUR.2010<-EUR["2010"]
EUR.2011<-EUR["2011"]
EUR.2012<-EUR["2012"]
EUR.2013<-EUR["2013"]
EUR.2014<-EUR["2014"]
EUR.2015<-EUR["2015"]
EUR.2016<-EUR["2016"]
EUR.2017<-EUR["2017"]
GOOGLE.2008<-GOOGLE["2008"]
GOOGLE.2009<-GOOGLE["2009"]
GOOGLE.2010<-GOOGLE["2010"]
GOOGLE.2011<-GOOGLE["2011"]
GOOGLE.2012<-GOOGLE["2012"]
GOOGLE.2013<-GOOGLE["2013"]
GOOGLE.2014<-GOOGLE["2014"]
GOOGLE.2015<-GOOGLE["2015"]
GOOGLE.2016<-GOOGLE["2016"]
GOOGLE.2017<-GOOGLE["2017"]
SP.2008<-SP["2008"]
SP.2009<-SP["2009"]
SP.2010<-SP["2010"]
SP.2011<-SP["2011"]
SP.2012<-SP["2012"]
SP.2013<-SP["2013"]
SP.2014<-SP["2014"]
SP.2015<-SP["2015"]
SP.2016<-SP["2016"]
SP.2017<-SP["2017"]
USD.2008<-USD["2008"]
USD.2009<-USD["2009"]
USD.2010<-USD["2010"]
USD.2011<-USD["2011"]
USD.2012<-USD["2012"]
USD.2013<-USD["2013"]
USD.2014<-USD["2014"]
USD.2015<-USD["2015"]
USD.2016<-USD["2016"]
USD.2017<-USD["2017"]
WTI.2008<-WTI["2008"]
WTI.2009<-WTI["2009"]
WTI.2010<-WTI["2010"]
WTI.2011<-WTI["2011"]
WTI.2012<-WTI["2012"]
WTI.2013<-WTI["2013"]
WTI.2014<-WTI["2014"]
WTI.2015<-WTI["2015"]
WTI.2016<-WTI["2016"]
WTI.2017<-WTI["2017"]

#yearly returns
yearly.AKBANK<-to.period(AKBANK, period = "years")
yearly.AKBANK$return<-ROC(yearly.AKBANK$AKBANK.Close)
yearly.BIST<-to.period(BIST, period = "years")
yearly.BIST$return<-ROC(yearly.BIST$BIST.Close)
yearly.BRENT<-to.period(BRENT, period = "years")
yearly.BRENT$return<-ROC(yearly.BRENT$BRENT.Close)
yearly.DJ<-to.period(DJ, period = "years")
yearly.DJ$return<-ROC(yearly.DJ$DJ.Close)
yearly.EUR<-to.period(EUR, period = "years")
yearly.EUR$return<-ROC(yearly.EUR$EUR.Close)
yearly.GOOGLE<-to.period(GOOGLE, period = "years")
yearly.GOOGLE$return<-ROC(yearly.GOOGLE$GOOGLE.Close)
yearly.SP<-to.period(SP, period = "years")
yearly.SP$return<-ROC(yearly.SP$SP.Close)
yearly.USD<-to.period(USD, period = "years")
yearly.USD$return<-ROC(yearly.USD$USD.Close)
yearly.WTI<-to.period(WTI, period = "years")
yearly.WTI$return<-ROC(yearly.WTI$WTI.Close)

#monthly returns
monthly.AKBANK<-to.period(AKBANK, period = "months")
monthly.AKBANK$return<-ROC(monthly.AKBANK$AKBANK.Close)
monthly.BIST<-to.period(BIST, period = "months")
monthly.BIST$return<-ROC(monthly.BIST$BIST.Close)
monthly.BRENT<-to.period(BRENT, period = "months")
monthly.BRENT$return<-ROC(monthly.BRENT$BRENT.Close)
monthly.DJ<-to.period(DJ, period = "months")
monthly.DJ$return<-ROC(monthly.DJ$DJ.Close)
monthly.EUR<-to.period(EUR, period = "months")
monthly.EUR$return<-ROC(monthly.EUR$EUR.Close)
monthly.GOOGLE<-to.period(GOOGLE, period = "months")
monthly.GOOGLE$return<-ROC(monthly.GOOGLE$GOOGLE.Close)
monthly.SP<-to.period(SP, period = "months")
monthly.SP$return<-ROC(monthly.SP$SP.Close)
monthly.USD<-to.period(USD, period = "months")
monthly.USD$return<-ROC(monthly.USD$USD.Close)
monthly.WTI<-to.period(WTI, period = "months")
monthly.WTI$return<-ROC(monthly.WTI$WTI.Close)

#weekly returns
weekly.AKBANK<-to.period(AKBANK, period = "weeks")
weekly.AKBANK$return<-ROC(weekly.AKBANK$AKBANK.Close)
weekly.BIST<-to.period(BIST, period = "weeks")
weekly.BIST$return<-ROC(weekly.BIST$BIST.Close)
weekly.BRENT<-to.period(BRENT, period = "weeks")
weekly.BRENT$return<-ROC(weekly.BRENT$BRENT.Close)
weekly.DJ<-to.period(DJ, period = "weeks")
weekly.DJ$return<-ROC(weekly.DJ$DJ.Close)
weekly.EUR<-to.period(EUR, period = "weeks")
weekly.EUR$return<-ROC(weekly.EUR$EUR.Close)
weekly.GOOGLE<-to.period(GOOGLE, period = "weeks")
weekly.GOOGLE$return<-ROC(weekly.GOOGLE$GOOGLE.Close)
weekly.SP<-to.period(SP, period = "weeks")
weekly.SP$return<-ROC(weekly.SP$SP.Close)
weekly.USD<-to.period(USD, period = "weeks")
weekly.USD$return<-ROC(weekly.USD$USD.Close)
weekly.WTI<-to.period(WTI, period = "weeks")
weekly.WTI$return<-ROC(weekly.WTI$WTI.Close)

#calculating mean
mean(AKBANK$return, na.rm=TRUE)
mean(AKBANK.2008$return, na.rm=TRUE)
mean(AKBANK.2009$return, na.rm=TRUE)
mean(AKBANK.2010$return, na.rm=TRUE)
mean(AKBANK.2011$return, na.rm=TRUE)
mean(AKBANK.2012$return, na.rm=TRUE)
mean(AKBANK.2013$return, na.rm=TRUE)
mean(AKBANK.2014$return, na.rm=TRUE)
mean(AKBANK.2015$return, na.rm=TRUE)
mean(AKBANK.2016$return, na.rm=TRUE)
mean(AKBANK.2017$return, na.rm=TRUE)
mean(yearly.AKBANK$return, na.rm=TRUE)
mean(monthly.AKBANK$return, na.rm=TRUE)
mean(weekly.AKBANK$return, na.rm=TRUE)
mean(BIST$return, na.rm=TRUE)
mean(BIST.2008$return, na.rm=TRUE)
mean(BIST.2009$return, na.rm=TRUE)
mean(BIST.2010$return, na.rm=TRUE)
mean(BIST.2011$return, na.rm=TRUE)
mean(BIST.2012$return, na.rm=TRUE)
mean(BIST.2013$return, na.rm=TRUE)
mean(BIST.2014$return, na.rm=TRUE)
mean(BIST.2015$return, na.rm=TRUE)
mean(BIST.2016$return, na.rm=TRUE)
mean(BIST.2017$return, na.rm=TRUE)
mean(yearly.BIST$return, na.rm=TRUE)
mean(monthly.BIST$return, na.rm=TRUE)
mean(weekly.BIST$return, na.rm=TRUE)
mean(BRENT$return, na.rm=TRUE)
mean(BRENT.2008$return, na.rm=TRUE)
mean(BRENT.2009$return, na.rm=TRUE)
mean(BRENT.2010$return, na.rm=TRUE)
mean(BRENT.2011$return, na.rm=TRUE)
mean(BRENT.2012$return, na.rm=TRUE)
mean(BRENT.2013$return, na.rm=TRUE)
mean(BRENT.2014$return, na.rm=TRUE)
mean(BRENT.2015$return, na.rm=TRUE)
mean(BRENT.2016$return, na.rm=TRUE)
mean(BRENT.2017$return, na.rm=TRUE)
mean(yearly.BRENT$return, na.rm=TRUE)
mean(monthly.BRENT$return, na.rm=TRUE)
mean(weekly.BRENT$return, na.rm=TRUE)
mean(DJ$return, na.rm=TRUE)
mean(DJ.2008$return, na.rm=TRUE)
mean(DJ.2009$return, na.rm=TRUE)
mean(DJ.2010$return, na.rm=TRUE)
mean(DJ.2011$return, na.rm=TRUE)
mean(DJ.2012$return, na.rm=TRUE)
mean(DJ.2013$return, na.rm=TRUE)
mean(DJ.2014$return, na.rm=TRUE)
mean(DJ.2015$return, na.rm=TRUE)
mean(DJ.2016$return, na.rm=TRUE)
mean(DJ.2017$return, na.rm=TRUE)
mean(yearly.DJ$return, na.rm=TRUE)
mean(monthly.DJ$return, na.rm=TRUE)
mean(weekly.DJ$return, na.rm=TRUE)
mean(EUR$return, na.rm=TRUE)
mean(EUR.2008$return, na.rm=TRUE)
mean(EUR.2009$return, na.rm=TRUE)
mean(EUR.2010$return, na.rm=TRUE)
mean(EUR.2011$return, na.rm=TRUE)
mean(EUR.2012$return, na.rm=TRUE)
mean(EUR.2013$return, na.rm=TRUE)
mean(EUR.2014$return, na.rm=TRUE)
mean(EUR.2015$return, na.rm=TRUE)
mean(EUR.2016$return, na.rm=TRUE)
mean(EUR.2017$return, na.rm=TRUE)
mean(yearly.EUR$return, na.rm=TRUE)
mean(monthly.EUR$return, na.rm=TRUE)
mean(weekly.EUR$return, na.rm=TRUE)
mean(GOOGLE$return, na.rm=TRUE)
mean(GOOGLE.2008$return, na.rm=TRUE)
mean(GOOGLE.2009$return, na.rm=TRUE)
mean(GOOGLE.2010$return, na.rm=TRUE)
mean(GOOGLE.2011$return, na.rm=TRUE)
mean(GOOGLE.2012$return, na.rm=TRUE)
mean(GOOGLE.2013$return, na.rm=TRUE)
mean(GOOGLE.2014$return, na.rm=TRUE)
mean(GOOGLE.2015$return, na.rm=TRUE)
mean(GOOGLE.2016$return, na.rm=TRUE)
mean(GOOGLE.2017$return, na.rm=TRUE)
mean(yearly.GOOGLE$return, na.rm=TRUE)
mean(monthly.GOOGLE$return, na.rm=TRUE)
mean(weekly.GOOGLE$return, na.rm=TRUE)
mean(SP$return, na.rm=TRUE)
mean(SP.2008$return, na.rm=TRUE)
mean(SP.2009$return, na.rm=TRUE)
mean(SP.2010$return, na.rm=TRUE)
mean(SP.2011$return, na.rm=TRUE)
mean(SP.2012$return, na.rm=TRUE)
mean(SP.2013$return, na.rm=TRUE)
mean(SP.2014$return, na.rm=TRUE)
mean(SP.2015$return, na.rm=TRUE)
mean(SP.2016$return, na.rm=TRUE)
mean(SP.2017$return, na.rm=TRUE)
mean(yearly.SP$return, na.rm=TRUE)
mean(monthly.SP$return, na.rm=TRUE)
mean(weekly.SP$return, na.rm=TRUE)
mean(USD$return, na.rm=TRUE)
mean(USD.2008$return, na.rm=TRUE)
mean(USD.2009$return, na.rm=TRUE)
mean(USD.2010$return, na.rm=TRUE)
mean(USD.2011$return, na.rm=TRUE)
mean(USD.2012$return, na.rm=TRUE)
mean(USD.2013$return, na.rm=TRUE)
mean(USD.2014$return, na.rm=TRUE)
mean(USD.2015$return, na.rm=TRUE)
mean(USD.2016$return, na.rm=TRUE)
mean(USD.2017$return, na.rm=TRUE)
mean(yearly.USD$return, na.rm=TRUE)
mean(monthly.USD$return, na.rm=TRUE)
mean(weekly.USD$return, na.rm=TRUE)
mean(WTI$return, na.rm=TRUE)
mean(WTI.2008$return, na.rm=TRUE)
mean(WTI.2009$return, na.rm=TRUE)
mean(WTI.2010$return, na.rm=TRUE)
mean(WTI.2011$return, na.rm=TRUE)
mean(WTI.2012$return, na.rm=TRUE)
mean(WTI.2013$return, na.rm=TRUE)
mean(WTI.2014$return, na.rm=TRUE)
mean(WTI.2015$return, na.rm=TRUE)
mean(WTI.2016$return, na.rm=TRUE)
mean(WTI.2017$return, na.rm=TRUE)
mean(yearly.WTI$return, na.rm=TRUE)
mean(monthly.WTI$return, na.rm=TRUE)
mean(weekly.WTI$return, na.rm=TRUE)

#calculating variance
var(AKBANK$return, na.rm=TRUE)
var(AKBANK.2008$return, na.rm=TRUE)
var(AKBANK.2009$return, na.rm=TRUE)
var(AKBANK.2010$return, na.rm=TRUE)
var(AKBANK.2011$return, na.rm=TRUE)
var(AKBANK.2012$return, na.rm=TRUE)
var(AKBANK.2013$return, na.rm=TRUE)
var(AKBANK.2014$return, na.rm=TRUE)
var(AKBANK.2015$return, na.rm=TRUE)
var(AKBANK.2016$return, na.rm=TRUE)
var(AKBANK.2017$return, na.rm=TRUE)
var(yearly.AKBANK$return, na.rm=TRUE)
var(monthly.AKBANK$return, na.rm=TRUE)
var(weekly.AKBANK$return, na.rm=TRUE)
var(BIST$return, na.rm=TRUE)
var(BIST.2008$return, na.rm=TRUE)
var(BIST.2009$return, na.rm=TRUE)
var(BIST.2010$return, na.rm=TRUE)
var(BIST.2011$return, na.rm=TRUE)
var(BIST.2012$return, na.rm=TRUE)
var(BIST.2013$return, na.rm=TRUE)
var(BIST.2014$return, na.rm=TRUE)
var(BIST.2015$return, na.rm=TRUE)
var(BIST.2016$return, na.rm=TRUE)
var(BIST.2017$return, na.rm=TRUE)
var(yearly.BIST$return, na.rm=TRUE)
var(monthly.BIST$return, na.rm=TRUE)
var(weekly.BIST$return, na.rm=TRUE)
var(BRENT$return, na.rm=TRUE)
var(BRENT.2008$return, na.rm=TRUE)
var(BRENT.2009$return, na.rm=TRUE)
var(BRENT.2010$return, na.rm=TRUE)
var(BRENT.2011$return, na.rm=TRUE)
var(BRENT.2012$return, na.rm=TRUE)
var(BRENT.2013$return, na.rm=TRUE)
var(BRENT.2014$return, na.rm=TRUE)
var(BRENT.2015$return, na.rm=TRUE)
var(BRENT.2016$return, na.rm=TRUE)
var(BRENT.2017$return, na.rm=TRUE)
var(yearly.BRENT$return, na.rm=TRUE)
var(monthly.BRENT$return, na.rm=TRUE)
var(weekly.BRENT$return, na.rm=TRUE)
var(DJ$return, na.rm=TRUE)
var(DJ.2008$return, na.rm=TRUE)
var(DJ.2009$return, na.rm=TRUE)
var(DJ.2010$return, na.rm=TRUE)
var(DJ.2011$return, na.rm=TRUE)
var(DJ.2012$return, na.rm=TRUE)
var(DJ.2013$return, na.rm=TRUE)
var(DJ.2014$return, na.rm=TRUE)
var(DJ.2015$return, na.rm=TRUE)
var(DJ.2016$return, na.rm=TRUE)
var(DJ.2017$return, na.rm=TRUE)
var(yearly.DJ$return, na.rm=TRUE)
var(monthly.DJ$return, na.rm=TRUE)
var(weekly.DJ$return, na.rm=TRUE)
var(EUR$return, na.rm=TRUE)
var(EUR.2008$return, na.rm=TRUE)
var(EUR.2009$return, na.rm=TRUE)
var(EUR.2010$return, na.rm=TRUE)
var(EUR.2011$return, na.rm=TRUE)
var(EUR.2012$return, na.rm=TRUE)
var(EUR.2013$return, na.rm=TRUE)
var(EUR.2014$return, na.rm=TRUE)
var(EUR.2015$return, na.rm=TRUE)
var(EUR.2016$return, na.rm=TRUE)
var(EUR.2017$return, na.rm=TRUE)
var(yearly.EUR$return, na.rm=TRUE)
var(monthly.EUR$return, na.rm=TRUE)
var(weekly.EUR$return, na.rm=TRUE)
var(GOOGLE$return, na.rm=TRUE)
var(GOOGLE.2008$return, na.rm=TRUE)
var(GOOGLE.2009$return, na.rm=TRUE)
var(GOOGLE.2010$return, na.rm=TRUE)
var(GOOGLE.2011$return, na.rm=TRUE)
var(GOOGLE.2012$return, na.rm=TRUE)
var(GOOGLE.2013$return, na.rm=TRUE)
var(GOOGLE.2014$return, na.rm=TRUE)
var(GOOGLE.2015$return, na.rm=TRUE)
var(GOOGLE.2016$return, na.rm=TRUE)
var(GOOGLE.2017$return, na.rm=TRUE)
var(yearly.GOOGLE$return, na.rm=TRUE)
var(monthly.GOOGLE$return, na.rm=TRUE)
var(weekly.GOOGLE$return, na.rm=TRUE)
var(SP$return, na.rm=TRUE)
var(SP.2008$return, na.rm=TRUE)
var(SP.2009$return, na.rm=TRUE)
var(SP.2010$return, na.rm=TRUE)
var(SP.2011$return, na.rm=TRUE)
var(SP.2012$return, na.rm=TRUE)
var(SP.2013$return, na.rm=TRUE)
var(SP.2014$return, na.rm=TRUE)
var(SP.2015$return, na.rm=TRUE)
var(SP.2016$return, na.rm=TRUE)
var(SP.2017$return, na.rm=TRUE)
var(yearly.SP$return, na.rm=TRUE)
var(monthly.SP$return, na.rm=TRUE)
var(weekly.SP$return, na.rm=TRUE)
var(USD$return, na.rm=TRUE)
var(USD.2008$return, na.rm=TRUE)
var(USD.2009$return, na.rm=TRUE)
var(USD.2010$return, na.rm=TRUE)
var(USD.2011$return, na.rm=TRUE)
var(USD.2012$return, na.rm=TRUE)
var(USD.2013$return, na.rm=TRUE)
var(USD.2014$return, na.rm=TRUE)
var(USD.2015$return, na.rm=TRUE)
var(USD.2016$return, na.rm=TRUE)
var(USD.2017$return, na.rm=TRUE)
var(yearly.USD$return, na.rm=TRUE)
var(monthly.USD$return, na.rm=TRUE)
var(weekly.USD$return, na.rm=TRUE)
var(WTI$return, na.rm=TRUE)
var(WTI.2008$return, na.rm=TRUE)
var(WTI.2009$return, na.rm=TRUE)
var(WTI.2010$return, na.rm=TRUE)
var(WTI.2011$return, na.rm=TRUE)
var(WTI.2012$return, na.rm=TRUE)
var(WTI.2013$return, na.rm=TRUE)
var(WTI.2014$return, na.rm=TRUE)
var(WTI.2015$return, na.rm=TRUE)
var(WTI.2016$return, na.rm=TRUE)
var(WTI.2017$return, na.rm=TRUE)
var(yearly.WTI$return, na.rm=TRUE)
var(monthly.WTI$return, na.rm=TRUE)
var(weekly.WTI$return, na.rm=TRUE)

#calculating skewness
skewness(AKBANK$return, na.rm=TRUE)
skewness(AKBANK.2008$return, na.rm=TRUE)
skewness(AKBANK.2009$return, na.rm=TRUE)
skewness(AKBANK.2010$return, na.rm=TRUE)
skewness(AKBANK.2011$return, na.rm=TRUE)
skewness(AKBANK.2012$return, na.rm=TRUE)
skewness(AKBANK.2013$return, na.rm=TRUE)
skewness(AKBANK.2014$return, na.rm=TRUE)
skewness(AKBANK.2015$return, na.rm=TRUE)
skewness(AKBANK.2016$return, na.rm=TRUE)
skewness(AKBANK.2017$return, na.rm=TRUE)
skewness(yearly.AKBANK$return, na.rm=TRUE)
skewness(monthly.AKBANK$return, na.rm=TRUE)
skewness(weekly.AKBANK$return, na.rm=TRUE)
skewness(BIST$return, na.rm=TRUE)
skewness(BIST.2008$return, na.rm=TRUE)
skewness(BIST.2009$return, na.rm=TRUE)
skewness(BIST.2010$return, na.rm=TRUE)
skewness(BIST.2011$return, na.rm=TRUE)
skewness(BIST.2012$return, na.rm=TRUE)
skewness(BIST.2013$return, na.rm=TRUE)
skewness(BIST.2014$return, na.rm=TRUE)
skewness(BIST.2015$return, na.rm=TRUE)
skewness(BIST.2016$return, na.rm=TRUE)
skewness(BIST.2017$return, na.rm=TRUE)
skewness(yearly.BIST$return, na.rm=TRUE)
skewness(monthly.BIST$return, na.rm=TRUE)
skewness(weekly.BIST$return, na.rm=TRUE)
skewness(BRENT$return, na.rm=TRUE)
skewness(BRENT.2008$return, na.rm=TRUE)
skewness(BRENT.2009$return, na.rm=TRUE)
skewness(BRENT.2010$return, na.rm=TRUE)
skewness(BRENT.2011$return, na.rm=TRUE)
skewness(BRENT.2012$return, na.rm=TRUE)
skewness(BRENT.2013$return, na.rm=TRUE)
skewness(BRENT.2014$return, na.rm=TRUE)
skewness(BRENT.2015$return, na.rm=TRUE)
skewness(BRENT.2016$return, na.rm=TRUE)
skewness(BRENT.2017$return, na.rm=TRUE)
skewness(yearly.BRENT$return, na.rm=TRUE)
skewness(monthly.BRENT$return, na.rm=TRUE)
skewness(weekly.BRENT$return, na.rm=TRUE)
skewness(DJ$return, na.rm=TRUE)
skewness(DJ.2008$return, na.rm=TRUE)
skewness(DJ.2009$return, na.rm=TRUE)
skewness(DJ.2010$return, na.rm=TRUE)
skewness(DJ.2011$return, na.rm=TRUE)
skewness(DJ.2012$return, na.rm=TRUE)
skewness(DJ.2013$return, na.rm=TRUE)
skewness(DJ.2014$return, na.rm=TRUE)
skewness(DJ.2015$return, na.rm=TRUE)
skewness(DJ.2016$return, na.rm=TRUE)
skewness(DJ.2017$return, na.rm=TRUE)
skewness(yearly.DJ$return, na.rm=TRUE)
skewness(monthly.DJ$return, na.rm=TRUE)
skewness(weekly.DJ$return, na.rm=TRUE)
skewness(EUR$return, na.rm=TRUE)
skewness(EUR.2008$return, na.rm=TRUE)
skewness(EUR.2009$return, na.rm=TRUE)
skewness(EUR.2010$return, na.rm=TRUE)
skewness(EUR.2011$return, na.rm=TRUE)
skewness(EUR.2012$return, na.rm=TRUE)
skewness(EUR.2013$return, na.rm=TRUE)
skewness(EUR.2014$return, na.rm=TRUE)
skewness(EUR.2015$return, na.rm=TRUE)
skewness(EUR.2016$return, na.rm=TRUE)
skewness(EUR.2017$return, na.rm=TRUE)
skewness(yearly.EUR$return, na.rm=TRUE)
skewness(monthly.EUR$return, na.rm=TRUE)
skewness(weekly.EUR$return, na.rm=TRUE)
skewness(GOOGLE$return, na.rm=TRUE)
skewness(GOOGLE.2008$return, na.rm=TRUE)
skewness(GOOGLE.2009$return, na.rm=TRUE)
skewness(GOOGLE.2010$return, na.rm=TRUE)
skewness(GOOGLE.2011$return, na.rm=TRUE)
skewness(GOOGLE.2012$return, na.rm=TRUE)
skewness(GOOGLE.2013$return, na.rm=TRUE)
skewness(GOOGLE.2014$return, na.rm=TRUE)
skewness(GOOGLE.2015$return, na.rm=TRUE)
skewness(GOOGLE.2016$return, na.rm=TRUE)
skewness(GOOGLE.2017$return, na.rm=TRUE)
skewness(yearly.GOOGLE$return, na.rm=TRUE)
skewness(monthly.GOOGLE$return, na.rm=TRUE)
skewness(weekly.GOOGLE$return, na.rm=TRUE)
skewness(SP$return, na.rm=TRUE)
skewness(SP.2008$return, na.rm=TRUE)
skewness(SP.2009$return, na.rm=TRUE)
skewness(SP.2010$return, na.rm=TRUE)
skewness(SP.2011$return, na.rm=TRUE)
skewness(SP.2012$return, na.rm=TRUE)
skewness(SP.2013$return, na.rm=TRUE)
skewness(SP.2014$return, na.rm=TRUE)
skewness(SP.2015$return, na.rm=TRUE)
skewness(SP.2016$return, na.rm=TRUE)
skewness(SP.2017$return, na.rm=TRUE)
skewness(yearly.SP$return, na.rm=TRUE)
skewness(monthly.SP$return, na.rm=TRUE)
skewness(weekly.SP$return, na.rm=TRUE)
skewness(USD$return, na.rm=TRUE)
skewness(USD.2008$return, na.rm=TRUE)
skewness(USD.2009$return, na.rm=TRUE)
skewness(USD.2010$return, na.rm=TRUE)
skewness(USD.2011$return, na.rm=TRUE)
skewness(USD.2012$return, na.rm=TRUE)
skewness(USD.2013$return, na.rm=TRUE)
skewness(USD.2014$return, na.rm=TRUE)
skewness(USD.2015$return, na.rm=TRUE)
skewness(USD.2016$return, na.rm=TRUE)
skewness(USD.2017$return, na.rm=TRUE)
skewness(yearly.USD$return, na.rm=TRUE)
skewness(monthly.USD$return, na.rm=TRUE)
skewness(weekly.USD$return, na.rm=TRUE)
skewness(WTI$return, na.rm=TRUE)
skewness(WTI.2008$return, na.rm=TRUE)
skewness(WTI.2009$return, na.rm=TRUE)
skewness(WTI.2010$return, na.rm=TRUE)
skewness(WTI.2011$return, na.rm=TRUE)
skewness(WTI.2012$return, na.rm=TRUE)
skewness(WTI.2013$return, na.rm=TRUE)
skewness(WTI.2014$return, na.rm=TRUE)
skewness(WTI.2015$return, na.rm=TRUE)
skewness(WTI.2016$return, na.rm=TRUE)
skewness(WTI.2017$return, na.rm=TRUE)
skewness(yearly.WTI$return, na.rm=TRUE)
skewness(monthly.WTI$return, na.rm=TRUE)
skewness(weekly.WTI$return, na.rm=TRUE)

#calculating kurtosis
kurtosis(AKBANK$return, na.rm=TRUE)-3
kurtosis(AKBANK.2008$return, na.rm=TRUE)-3
kurtosis(AKBANK.2009$return, na.rm=TRUE)-3
kurtosis(AKBANK.2010$return, na.rm=TRUE)-3
kurtosis(AKBANK.2011$return, na.rm=TRUE)-3
kurtosis(AKBANK.2012$return, na.rm=TRUE)-3
kurtosis(AKBANK.2013$return, na.rm=TRUE)-3
kurtosis(AKBANK.2014$return, na.rm=TRUE)-3
kurtosis(AKBANK.2015$return, na.rm=TRUE)-3
kurtosis(AKBANK.2016$return, na.rm=TRUE)-3
kurtosis(AKBANK.2017$return, na.rm=TRUE)-3
kurtosis(yearly.AKBANK$return, na.rm=TRUE)-3
kurtosis(monthly.AKBANK$return, na.rm=TRUE)-3
kurtosis(weekly.AKBANK$return, na.rm=TRUE)-3
kurtosis(BIST$return, na.rm=TRUE)-3
kurtosis(BIST.2008$return, na.rm=TRUE)-3
kurtosis(BIST.2009$return, na.rm=TRUE)-3
kurtosis(BIST.2010$return, na.rm=TRUE)-3
kurtosis(BIST.2011$return, na.rm=TRUE)-3
kurtosis(BIST.2012$return, na.rm=TRUE)-3
kurtosis(BIST.2013$return, na.rm=TRUE)-3
kurtosis(BIST.2014$return, na.rm=TRUE)-3
kurtosis(BIST.2015$return, na.rm=TRUE)-3
kurtosis(BIST.2016$return, na.rm=TRUE)-3
kurtosis(BIST.2017$return, na.rm=TRUE)-3
kurtosis(yearly.BIST$return, na.rm=TRUE)-3
kurtosis(monthly.BIST$return, na.rm=TRUE)-3
kurtosis(weekly.BIST$return, na.rm=TRUE)-3
kurtosis(BRENT$return, na.rm=TRUE)-3
kurtosis(BRENT.2008$return, na.rm=TRUE)-3
kurtosis(BRENT.2009$return, na.rm=TRUE)-3
kurtosis(BRENT.2010$return, na.rm=TRUE)-3
kurtosis(BRENT.2011$return, na.rm=TRUE)-3
kurtosis(BRENT.2012$return, na.rm=TRUE)-3
kurtosis(BRENT.2013$return, na.rm=TRUE)-3
kurtosis(BRENT.2014$return, na.rm=TRUE)-3
kurtosis(BRENT.2015$return, na.rm=TRUE)-3
kurtosis(BRENT.2016$return, na.rm=TRUE)-3
kurtosis(BRENT.2017$return, na.rm=TRUE)-3
kurtosis(yearly.BRENT$return, na.rm=TRUE)-3
kurtosis(monthly.BRENT$return, na.rm=TRUE)-3
kurtosis(weekly.BRENT$return, na.rm=TRUE)-3
kurtosis(DJ$return, na.rm=TRUE)-3
kurtosis(DJ.2008$return, na.rm=TRUE)-3
kurtosis(DJ.2009$return, na.rm=TRUE)-3
kurtosis(DJ.2010$return, na.rm=TRUE)-3
kurtosis(DJ.2011$return, na.rm=TRUE)-3
kurtosis(DJ.2012$return, na.rm=TRUE)-3
kurtosis(DJ.2013$return, na.rm=TRUE)-3
kurtosis(DJ.2014$return, na.rm=TRUE)-3
kurtosis(DJ.2015$return, na.rm=TRUE)-3
kurtosis(DJ.2016$return, na.rm=TRUE)-3
kurtosis(DJ.2017$return, na.rm=TRUE)-3
kurtosis(yearly.DJ$return, na.rm=TRUE)-3
kurtosis(monthly.DJ$return, na.rm=TRUE)-3
kurtosis(weekly.DJ$return, na.rm=TRUE)-3
kurtosis(EUR$return, na.rm=TRUE)-3
kurtosis(EUR.2008$return, na.rm=TRUE)-3
kurtosis(EUR.2009$return, na.rm=TRUE)-3
kurtosis(EUR.2010$return, na.rm=TRUE)-3
kurtosis(EUR.2011$return, na.rm=TRUE)-3
kurtosis(EUR.2012$return, na.rm=TRUE)-3
kurtosis(EUR.2013$return, na.rm=TRUE)-3
kurtosis(EUR.2014$return, na.rm=TRUE)-3
kurtosis(EUR.2015$return, na.rm=TRUE)-3
kurtosis(EUR.2016$return, na.rm=TRUE)-3
kurtosis(EUR.2017$return, na.rm=TRUE)-3
kurtosis(yearly.EUR$return, na.rm=TRUE)-3
kurtosis(monthly.EUR$return, na.rm=TRUE)-3
kurtosis(weekly.EUR$return, na.rm=TRUE)-3
kurtosis(GOOGLE$return, na.rm=TRUE)-3
kurtosis(GOOGLE.2008$return, na.rm=TRUE)-3
kurtosis(GOOGLE.2009$return, na.rm=TRUE)-3
kurtosis(GOOGLE.2010$return, na.rm=TRUE)-3
kurtosis(GOOGLE.2011$return, na.rm=TRUE)-3
kurtosis(GOOGLE.2012$return, na.rm=TRUE)-3
kurtosis(GOOGLE.2013$return, na.rm=TRUE)-3
kurtosis(GOOGLE.2014$return, na.rm=TRUE)-3
kurtosis(GOOGLE.2015$return, na.rm=TRUE)-3
kurtosis(GOOGLE.2016$return, na.rm=TRUE)-3
kurtosis(GOOGLE.2017$return, na.rm=TRUE)-3
kurtosis(yearly.GOOGLE$return, na.rm=TRUE)-3
kurtosis(monthly.GOOGLE$return, na.rm=TRUE)-3
kurtosis(weekly.GOOGLE$return, na.rm=TRUE)-3
kurtosis(SP$return, na.rm=TRUE)-3
kurtosis(SP.2008$return, na.rm=TRUE)-3
kurtosis(SP.2009$return, na.rm=TRUE)-3
kurtosis(SP.2010$return, na.rm=TRUE)-3
kurtosis(SP.2011$return, na.rm=TRUE)-3
kurtosis(SP.2012$return, na.rm=TRUE)-3
kurtosis(SP.2013$return, na.rm=TRUE)-3
kurtosis(SP.2014$return, na.rm=TRUE)-3
kurtosis(SP.2015$return, na.rm=TRUE)-3
kurtosis(SP.2016$return, na.rm=TRUE)-3
kurtosis(SP.2017$return, na.rm=TRUE)-3
kurtosis(yearly.SP$return, na.rm=TRUE)-3
kurtosis(monthly.SP$return, na.rm=TRUE)-3
kurtosis(weekly.SP$return, na.rm=TRUE)-3
kurtosis(USD$return, na.rm=TRUE)-3
kurtosis(USD.2008$return, na.rm=TRUE)-3
kurtosis(USD.2009$return, na.rm=TRUE)-3
kurtosis(USD.2010$return, na.rm=TRUE)-3
kurtosis(USD.2011$return, na.rm=TRUE)-3
kurtosis(USD.2012$return, na.rm=TRUE)-3
kurtosis(USD.2013$return, na.rm=TRUE)-3
kurtosis(USD.2014$return, na.rm=TRUE)-3
kurtosis(USD.2015$return, na.rm=TRUE)-3
kurtosis(USD.2016$return, na.rm=TRUE)-3
kurtosis(USD.2017$return, na.rm=TRUE)-3
kurtosis(yearly.USD$return, na.rm=TRUE)-3
kurtosis(monthly.USD$return, na.rm=TRUE)-3
kurtosis(weekly.USD$return, na.rm=TRUE)-3
kurtosis(WTI$return, na.rm=TRUE)-3
kurtosis(WTI.2008$return, na.rm=TRUE)-3
kurtosis(WTI.2009$return, na.rm=TRUE)-3
kurtosis(WTI.2010$return, na.rm=TRUE)-3
kurtosis(WTI.2011$return, na.rm=TRUE)-3
kurtosis(WTI.2012$return, na.rm=TRUE)-3
kurtosis(WTI.2013$return, na.rm=TRUE)-3
kurtosis(WTI.2014$return, na.rm=TRUE)-3
kurtosis(WTI.2015$return, na.rm=TRUE)-3
kurtosis(WTI.2016$return, na.rm=TRUE)-3
kurtosis(WTI.2017$return, na.rm=TRUE)-3
kurtosis(yearly.WTI$return, na.rm=TRUE)-3
kurtosis(monthly.WTI$return, na.rm=TRUE)-3
kurtosis(weekly.WTI$return, na.rm=TRUE)-3

#calculating E(x)/var(X)
mean(AKBANK$return, na.rm=TRUE) / var(AKBANK$return, na.rm=TRUE) 
mean(AKBANK.2008$return, na.rm=TRUE) / var(AKBANK.2008$return, na.rm=TRUE) 
mean(AKBANK.2009$return, na.rm=TRUE) / var(AKBANK.2009$return, na.rm=TRUE) 
mean(AKBANK.2010$return, na.rm=TRUE) / var(AKBANK.2010$return, na.rm=TRUE) 
mean(AKBANK.2011$return, na.rm=TRUE) / var(AKBANK.2011$return, na.rm=TRUE)
mean(AKBANK.2012$return, na.rm=TRUE) / var(AKBANK.2012$return, na.rm=TRUE)
mean(AKBANK.2013$return, na.rm=TRUE) / var(AKBANK.2013$return, na.rm=TRUE)
mean(AKBANK.2014$return, na.rm=TRUE) / var(AKBANK.2014$return, na.rm=TRUE)
mean(AKBANK.2015$return, na.rm=TRUE) / var(AKBANK.2015$return, na.rm=TRUE)
mean(AKBANK.2016$return, na.rm=TRUE) / var(AKBANK.2016$return, na.rm=TRUE)
mean(AKBANK.2017$return, na.rm=TRUE) / var(AKBANK.2017$return, na.rm=TRUE)
mean(yearly.AKBANK$return, na.rm=TRUE) / var(yearly.AKBANK$return, na.rm=TRUE)
mean(monthly.AKBANK$return, na.rm=TRUE) / var(monthly.AKBANK$return, na.rm=TRUE)
mean(weekly.AKBANK$return, na.rm=TRUE) / var(weekly.AKBANK$return, na.rm=TRUE)
mean(BIST$return, na.rm=TRUE) / var(BIST$return, na.rm=TRUE)
mean(BIST.2008$return, na.rm=TRUE)  / var(BIST.2008$return, na.rm=TRUE)  
mean(BIST.2009$return, na.rm=TRUE)  / var(BIST.2009$return, na.rm=TRUE)  
mean(BIST.2010$return, na.rm=TRUE)  / var(BIST.2010$return, na.rm=TRUE)  
mean(BIST.2011$return, na.rm=TRUE)  / var(BIST.2011$return, na.rm=TRUE)
mean(BIST.2012$return, na.rm=TRUE)  / var(BIST.2012$return, na.rm=TRUE)  
mean(BIST.2013$return, na.rm=TRUE)  / var(BIST.2013$return, na.rm=TRUE)  
mean(BIST.2014$return, na.rm=TRUE)  / var(BIST.2014$return, na.rm=TRUE)  
mean(BIST.2015$return, na.rm=TRUE)  / var(BIST.2015$return, na.rm=TRUE)  
mean(BIST.2016$return, na.rm=TRUE)  / var(BIST.2016$return, na.rm=TRUE)  
mean(BIST.2017$return, na.rm=TRUE)  / var(BIST.2017$return, na.rm=TRUE)  
mean(yearly.BIST$return, na.rm=TRUE)  / var(yearly.BIST$return, na.rm=TRUE)  
mean(monthly.BIST$return, na.rm=TRUE)  / var(monthly.BIST$return, na.rm=TRUE)  
mean(weekly.BIST$return, na.rm=TRUE)  / var(weekly.BIST$return, na.rm=TRUE)  
mean(BRENT$return, na.rm=TRUE) / var(BRENT$return, na.rm=TRUE)
mean(BRENT.2008$return, na.rm=TRUE)  / var(BRENT.2008$return, na.rm=TRUE)  
mean(BRENT.2009$return, na.rm=TRUE)  / var(BRENT.2009$return, na.rm=TRUE)  
mean(BRENT.2010$return, na.rm=TRUE) / var(BRENT.2010$return, na.rm=TRUE)
mean(BRENT.2011$return, na.rm=TRUE) / var(BRENT.2011$return, na.rm=TRUE)
mean(BRENT.2012$return, na.rm=TRUE) / var(BRENT.2012$return, na.rm=TRUE)
mean(BRENT.2013$return, na.rm=TRUE) / var(BRENT.2013$return, na.rm=TRUE)
mean(BRENT.2014$return, na.rm=TRUE) / var(BRENT.2014$return, na.rm=TRUE)
mean(BRENT.2015$return, na.rm=TRUE) / var(BRENT.2015$return, na.rm=TRUE)
mean(BRENT.2016$return, na.rm=TRUE) / var(BRENT.2016$return, na.rm=TRUE)
mean(BRENT.2017$return, na.rm=TRUE) / var(BRENT.2017$return, na.rm=TRUE)
mean(yearly.BRENT$return, na.rm=TRUE) / var(yearly.BRENT$return, na.rm=TRUE)
mean(monthly.BRENT$return, na.rm=TRUE) / var(monthly.BRENT$return, na.rm=TRUE)
mean(weekly.BRENT$return, na.rm=TRUE) / var(weekly.BRENT$return, na.rm=TRUE)
mean(DJ$return, na.rm=TRUE) / var(DJ$return, na.rm=TRUE)
mean(DJ.2008$return, na.rm=TRUE) / var(DJ.2008$return, na.rm=TRUE)
mean(DJ.2009$return, na.rm=TRUE) / var(DJ.2009$return, na.rm=TRUE)
mean(DJ.2010$return, na.rm=TRUE) / var(DJ.2010$return, na.rm=TRUE)
mean(DJ.2011$return, na.rm=TRUE) / var(DJ.2011$return, na.rm=TRUE)
mean(DJ.2012$return, na.rm=TRUE) / var(DJ.2012$return, na.rm=TRUE)
mean(DJ.2013$return, na.rm=TRUE) / var(DJ.2013$return, na.rm=TRUE)
mean(DJ.2014$return, na.rm=TRUE) / var(DJ.2014$return, na.rm=TRUE)
mean(DJ.2015$return, na.rm=TRUE) / var(DJ.2015$return, na.rm=TRUE)
mean(DJ.2016$return, na.rm=TRUE) / var(DJ.2016$return, na.rm=TRUE)
mean(DJ.2017$return, na.rm=TRUE) / var(DJ.2017$return, na.rm=TRUE)
mean(yearly.DJ$return, na.rm=TRUE) / var(yearly.DJ$return, na.rm=TRUE)
mean(monthly.DJ$return, na.rm=TRUE) / var(monthly.DJ$return, na.rm=TRUE)
mean(weekly.DJ$return, na.rm=TRUE) / var(weekly.DJ$return, na.rm=TRUE)
mean(EUR$return, na.rm=TRUE) / var(EUR$return, na.rm=TRUE)
mean(EUR.2008$return, na.rm=TRUE) / var(EUR.2008$return, na.rm=TRUE)
mean(EUR.2009$return, na.rm=TRUE) / var(EUR.2009$return, na.rm=TRUE)
mean(EUR.2010$return, na.rm=TRUE) / var(EUR.2010$return, na.rm=TRUE)
mean(EUR.2011$return, na.rm=TRUE) / var(EUR.2011$return, na.rm=TRUE)
mean(EUR.2012$return, na.rm=TRUE) / var(EUR.2012$return, na.rm=TRUE)
mean(EUR.2013$return, na.rm=TRUE) / var(EUR.2013$return, na.rm=TRUE)
mean(EUR.2014$return, na.rm=TRUE) / var(EUR.2014$return, na.rm=TRUE)
mean(EUR.2015$return, na.rm=TRUE) / var(EUR.2015$return, na.rm=TRUE)
mean(EUR.2016$return, na.rm=TRUE) / var(EUR.2016$return, na.rm=TRUE)
mean(EUR.2017$return, na.rm=TRUE) / var(EUR.2017$return, na.rm=TRUE)
mean(yearly.EUR$return, na.rm=TRUE) / var(yearly.EUR$return, na.rm=TRUE)
mean(monthly.EUR$return, na.rm=TRUE) / var(monthly.EUR$return, na.rm=TRUE)
mean(weekly.EUR$return, na.rm=TRUE) / var(weekly.EUR$return, na.rm=TRUE)
mean(GOOGLE$return, na.rm=TRUE) / var(GOOGLE$return, na.rm=TRUE)
mean(GOOGLE.2008$return, na.rm=TRUE) / var(GOOGLE.2008$return, na.rm=TRUE)
mean(GOOGLE.2009$return, na.rm=TRUE) / var(GOOGLE.2009$return, na.rm=TRUE)
mean(GOOGLE.2010$return, na.rm=TRUE) / var(GOOGLE.2010$return, na.rm=TRUE)
mean(GOOGLE.2011$return, na.rm=TRUE) / var(GOOGLE.2011$return, na.rm=TRUE)
mean(GOOGLE.2012$return, na.rm=TRUE) / var(GOOGLE.2012$return, na.rm=TRUE)
mean(GOOGLE.2013$return, na.rm=TRUE) / var(GOOGLE.2013$return, na.rm=TRUE)
mean(GOOGLE.2014$return, na.rm=TRUE) / var(GOOGLE.2014$return, na.rm=TRUE)
mean(GOOGLE.2015$return, na.rm=TRUE) / var(GOOGLE.2015$return, na.rm=TRUE)
mean(GOOGLE.2016$return, na.rm=TRUE) / var(GOOGLE.2016$return, na.rm=TRUE)
mean(GOOGLE.2017$return, na.rm=TRUE) / var(GOOGLE.2017$return, na.rm=TRUE)
mean(yearly.GOOGLE$return, na.rm=TRUE) / var(yearly.GOOGLE$return, na.rm=TRUE)
mean(monthly.GOOGLE$return, na.rm=TRUE) / var(monthly.GOOGLE$return, na.rm=TRUE)
mean(weekly.GOOGLE$return, na.rm=TRUE) / var(weekly.GOOGLE$return, na.rm=TRUE)
mean(SP$return, na.rm=TRUE) / var(SP$return, na.rm=TRUE)
mean(SP.2008$return, na.rm=TRUE) / var(SP.2008$return, na.rm=TRUE)
mean(SP.2009$return, na.rm=TRUE) / var(SP.2009$return, na.rm=TRUE)
mean(SP.2010$return, na.rm=TRUE) / var(SP.2010$return, na.rm=TRUE)
mean(SP.2011$return, na.rm=TRUE) / var(SP.2011$return, na.rm=TRUE)
mean(SP.2012$return, na.rm=TRUE) / var(SP.2012$return, na.rm=TRUE)
mean(SP.2013$return, na.rm=TRUE) / var(SP.2013$return, na.rm=TRUE)
mean(SP.2014$return, na.rm=TRUE) / var(SP.2014$return, na.rm=TRUE) 
mean(SP.2015$return, na.rm=TRUE) / var(SP.2015$return, na.rm=TRUE)
mean(SP.2016$return, na.rm=TRUE) / var(SP.2016$return, na.rm=TRUE)
mean(SP.2017$return, na.rm=TRUE) / var(SP.2017$return, na.rm=TRUE)
mean(yearly.SP$return, na.rm=TRUE) / var(yearly.SP$return, na.rm=TRUE)
mean(monthly.SP$return, na.rm=TRUE) / var(monthly.SP$return, na.rm=TRUE)
mean(weekly.SP$return, na.rm=TRUE) / var(weekly.SP$return, na.rm=TRUE)
mean(USD$return, na.rm=TRUE) / var(USD$return, na.rm=TRUE)
mean(USD.2008$return, na.rm=TRUE) / var(USD.2008$return, na.rm=TRUE)
mean(USD.2009$return, na.rm=TRUE) / var(USD.2009$return, na.rm=TRUE)
mean(USD.2010$return, na.rm=TRUE) / var(USD.2010$return, na.rm=TRUE)
mean(USD.2011$return, na.rm=TRUE) / var(USD.2011$return, na.rm=TRUE)
mean(USD.2012$return, na.rm=TRUE) / var(USD.2012$return, na.rm=TRUE)
mean(USD.2013$return, na.rm=TRUE) / var(USD.2013$return, na.rm=TRUE)
mean(USD.2014$return, na.rm=TRUE) / var(USD.2014$return, na.rm=TRUE)
mean(USD.2015$return, na.rm=TRUE) / var(USD.2015$return, na.rm=TRUE)
mean(USD.2016$return, na.rm=TRUE) / var(USD.2016$return, na.rm=TRUE)
mean(USD.2017$return, na.rm=TRUE) / var(USD.2017$return, na.rm=TRUE)
mean(yearly.USD$return, na.rm=TRUE) / var(yearly.USD$return, na.rm=TRUE)
mean(monthly.USD$return, na.rm=TRUE) / var(monthly.USD$return, na.rm=TRUE)
mean(weekly.USD$return, na.rm=TRUE) / var(weekly.USD$return, na.rm=TRUE)
mean(WTI$return, na.rm=TRUE) / var(WTI$return, na.rm=TRUE)
mean(WTI.2008$return, na.rm=TRUE) / var(WTI.2008$return, na.rm=TRUE)
mean(WTI.2009$return, na.rm=TRUE) / var(WTI.2009$return, na.rm=TRUE)
mean(WTI.2010$return, na.rm=TRUE) / var(WTI.2010$return, na.rm=TRUE)
mean(WTI.2011$return, na.rm=TRUE) / var(WTI.2011$return, na.rm=TRUE)
mean(WTI.2012$return, na.rm=TRUE) / var(WTI.2012$return, na.rm=TRUE)
mean(WTI.2013$return, na.rm=TRUE) / var(WTI.2013$return, na.rm=TRUE)
mean(WTI.2014$return, na.rm=TRUE) / var(WTI.2014$return, na.rm=TRUE)
mean(WTI.2015$return, na.rm=TRUE) / var(WTI.2015$return, na.rm=TRUE)
mean(WTI.2016$return, na.rm=TRUE) / var(WTI.2016$return, na.rm=TRUE)
mean(WTI.2017$return, na.rm=TRUE) / var(WTI.2017$return, na.rm=TRUE)
mean(yearly.WTI$return, na.rm=TRUE) / var(yearly.WTI$return, na.rm=TRUE)
mean(monthly.WTI$return, na.rm=TRUE) / var(monthly.WTI$return, na.rm=TRUE)
mean(weekly.WTI$return, na.rm=TRUE) / var(weekly.WTI$return, na.rm=TRUE)

#calculating autocorrelation
acf(AKBANK$return, na.action=na.pass)
acf(AKBANK.2008$return, na.action=na.pass)
acf(AKBANK.2009$return, na.action=na.pass)
acf(AKBANK.2010$return, na.action=na.pass)
acf(AKBANK.2011$return, na.action=na.pass)
acf(AKBANK.2012$return, na.action=na.pass)
acf(AKBANK.2013$return, na.action=na.pass)
acf(AKBANK.2014$return, na.action=na.pass)
acf(AKBANK.2015$return, na.action=na.pass)
acf(AKBANK.2016$return, na.action=na.pass)
acf(AKBANK.2017$return, na.action=na.pass)
acf(yearly.AKBANK$return, na.action=na.pass)
acf(monthly.AKBANK$return, na.action=na.pass)
acf(weekly.AKBANK$return, na.action=na.pass)
acf(BIST$return, na.action=na.pass)
acf(BIST.2008$return, na.action=na.pass)
acf(BIST.2009$return, na.action=na.pass)
acf(BIST.2010$return, na.action=na.pass)
acf(BIST.2011$return, na.action=na.pass)
acf(BIST.2012$return, na.action=na.pass)
acf(BIST.2013$return, na.action=na.pass)
acf(BIST.2014$return, na.action=na.pass)
acf(BIST.2015$return, na.action=na.pass)
acf(BIST.2016$return, na.action=na.pass)
acf(BIST.2017$return, na.action=na.pass)
acf(yearly.BIST$return, na.action=na.pass)
acf(monthly.BIST$return, na.action=na.pass)
acf(weekly.BIST$return, na.action=na.pass)
acf(BRENT$return, na.action=na.pass)
acf(BRENT.2008$return, na.action=na.pass)
acf(BRENT.2009$return, na.action=na.pass)
acf(BRENT.2010$return, na.action=na.pass)
acf(BRENT.2011$return, na.action=na.pass)
acf(BRENT.2012$return, na.action=na.pass)
acf(BRENT.2013$return, na.action=na.pass)
acf(BRENT.2014$return, na.action=na.pass)
acf(BRENT.2015$return, na.action=na.pass)
acf(BRENT.2016$return, na.action=na.pass)
acf(BRENT.2017$return, na.action=na.pass)
acf(yearly.BRENT$return, na.action=na.pass)
acf(monthly.BRENT$return, na.action=na.pass)
acf(weekly.BRENT$return, na.action=na.pass)
acf(DJ$return, na.action=na.pass)
acf(DJ.2008$return, na.action=na.pass)
acf(DJ.2009$return, na.action=na.pass)
acf(DJ.2010$return, na.action=na.pass)
acf(DJ.2011$return, na.action=na.pass)
acf(DJ.2012$return, na.action=na.pass)
acf(DJ.2013$return, na.action=na.pass)
acf(DJ.2014$return, na.action=na.pass)
acf(DJ.2015$return, na.action=na.pass)
acf(DJ.2016$return, na.action=na.pass)
acf(DJ.2017$return, na.action=na.pass)
acf(yearly.DJ$return, na.action=na.pass)
acf(monthly.DJ$return, na.action=na.pass)
acf(weekly.DJ$return, na.action=na.pass)
acf(EUR$return, na.action=na.pass)
acf(EUR.2008$return, na.action=na.pass)
acf(EUR.2009$return, na.action=na.pass)
acf(EUR.2010$return, na.action=na.pass)
acf(EUR.2011$return, na.action=na.pass)
acf(EUR.2012$return, na.action=na.pass)
acf(EUR.2013$return, na.action=na.pass)
acf(EUR.2014$return, na.action=na.pass)
acf(EUR.2015$return, na.action=na.pass)
acf(EUR.2016$return, na.action=na.pass)
acf(EUR.2017$return, na.action=na.pass)
acf(yearly.EUR$return, na.action=na.pass)
acf(monthly.EUR$return, na.action=na.pass)
acf(weekly.EUR$return, na.action=na.pass)
acf(GOOGLE$return, na.action=na.pass)
acf(GOOGLE.2008$return, na.action=na.pass)
acf(GOOGLE.2009$return, na.action=na.pass)
acf(GOOGLE.2010$return, na.action=na.pass)
acf(GOOGLE.2011$return, na.action=na.pass)
acf(GOOGLE.2012$return, na.action=na.pass)
acf(GOOGLE.2013$return, na.action=na.pass)
acf(GOOGLE.2014$return, na.action=na.pass)
acf(GOOGLE.2015$return, na.action=na.pass)
acf(GOOGLE.2016$return, na.action=na.pass)
acf(GOOGLE.2017$return, na.action=na.pass)
acf(yearly.GOOGLE$return, na.action=na.pass)
acf(monthly.GOOGLE$return, na.action=na.pass)
acf(weekly.GOOGLE$return, na.action=na.pass)
acf(SP$return, na.action=na.pass)
acf(SP.2008$return, na.action=na.pass)
acf(SP.2009$return, na.action=na.pass)
acf(SP.2010$return, na.action=na.pass)
acf(SP.2011$return, na.action=na.pass)
acf(SP.2012$return, na.action=na.pass)
acf(SP.2013$return, na.action=na.pass)
acf(SP.2014$return, na.action=na.pass)
acf(SP.2015$return, na.action=na.pass)
acf(SP.2016$return, na.action=na.pass)
acf(SP.2017$return, na.action=na.pass)
acf(yearly.SP$return, na.action=na.pass)
acf(monthly.SP$return, na.action=na.pass)
acf(weekly.SP$return, na.action=na.pass)
acf(USD$return, na.action=na.pass)
acf(USD.2008$return, na.action=na.pass)
acf(USD.2009$return, na.action=na.pass)
acf(USD.2010$return, na.action=na.pass)
acf(USD.2011$return, na.action=na.pass)
acf(USD.2012$return, na.action=na.pass)
acf(USD.2013$return, na.action=na.pass)
acf(USD.2014$return, na.action=na.pass)
acf(USD.2015$return, na.action=na.pass)
acf(USD.2016$return, na.action=na.pass)
acf(USD.2017$return, na.action=na.pass)
acf(yearly.USD$return, na.action=na.pass)
acf(monthly.USD$return, na.action=na.pass)
acf(weekly.USD$return, na.action=na.pass)
acf(WTI$return, na.action=na.pass)
acf(WTI.2008$return, na.action=na.pass)
acf(WTI.2009$return, na.action=na.pass)
acf(WTI.2010$return, na.action=na.pass)
acf(WTI.2011$return, na.action=na.pass)
acf(WTI.2012$return, na.action=na.pass)
acf(WTI.2013$return, na.action=na.pass)
acf(WTI.2014$return, na.action=na.pass)
acf(WTI.2015$return, na.action=na.pass)
acf(WTI.2016$return, na.action=na.pass)
acf(WTI.2017$return, na.action=na.pass)
acf(yearly.WTI$return, na.action=na.pass)
acf(monthly.WTI$return, na.action=na.pass)
acf(weekly.WTI$return, na.action=na.pass)




