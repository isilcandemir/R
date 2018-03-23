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
library(lmtest)
library(readxl)

path <- file.path("~", "Desktop", "DJI.csv")
DJI<-read.csv(path, stringsAsFactors = FALSE)
path <- file.path("~", "Desktop", "GSPC.csv")
GSPC<-read.csv(path, stringsAsFactors = FALSE)


DJI$return<-ROC(DJI$Close)
GSPC$return<-ROC(GSPC$Close)


#implementing granger test
grangertest(DJI$return, DJI$Volume, order=10, na.action=na.omit)
grangertest(DJI$Volume, DJI$return, order=10, na.action=na.omit)

grangertest(GSPC$return, GSPC$Volume, order=10, na.action=na.omit)
grangertest(GSPC$Volume, GSPC$return, order=10, na.action=na.omit)
