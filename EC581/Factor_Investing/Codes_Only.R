suppressMessages(library(zoo))

# Load Data
load("C:/Users/Y0577/Documents/BOUN_Lecture/Database.RData")

# ROE Factor
head(names(TrailROE),5)
Factor.Data<-TrailROE[,-c(1:4)] #Remove BIST indices

# Use monthly data
Month.Ends<-endpoints(Factor.Data, on="months")
Month.Ends.Dates<-time(Factor.Data)[Month.Ends[-1]]

Factor.Data<-window(Factor.Data,Month.Ends.Dates)#Convert to monthly
head(Factor.Data,5) 

# Plot data
hist(c(coredata(Factor.Data)),1000)

# Winsorize Outliers
dat<-coredata(Factor.Data)#convert to matrix
dat[dat< -100]<- -100
dat[dat> 100]<- 100
Factor.Data<-zoo(dat,time(Factor.Data))


# Standardize
Z.Score<-function(x) (x-mean(x,na.rm=T))/sd(x,na.rm=T)

Factor.Data.Std<-t(apply(Factor.Data,1,Z.Score)) #gives matrix
Factor.Data.Std<-zoo(Factor.Data.Std,time(Factor.Data))


# Plot standardized data
hist(c(coredata(Factor.Data.Std)),1000)


# Get prices and returns
Prices<-window(na.locf(Close[,-c(1:4)]),Month.Ends.Dates)
Rets<-Prices/lag(Prices,-1,na.pad =T)-1

Bench<-window(na.locf(Close[,"XU100"]),Month.Ends.Dates)
Bench.Rets<-Bench/lag(Bench,-1,na.pad =T)-1

# Calculate IC
IC<-vapply(2:dim(Rets)[1],
           function(i) cor(as.numeric(Factor.Data.Std[i-1,]),
                           as.numeric(Rets[i,]),
                           use="pairwise.complete.obs"),
           3.54)
IC<-zoo(IC,time(Rets)[-1])


#Plot IC
plot(IC,type="h",main="Info Coefficient",las=1,xlab="",ylab="")
lines(rollmeanr(IC,12),col=2)
abline(h=mean(IC),col=4)
title(sub=paste("Avg IC = ",round(100*mean(IC),1),"%",sep=""))



#Plot IC
plot(IC,type="h",main="Info Coefficient",las=1,xlab="",ylab="")
lines(rollmeanr(IC,12),col=2)
abline(h=mean(IC),col=4)
title(sub=paste("Avg IC = ",round(100*mean(IC),1),"%",sep=""))


# Calculate Rank IC
IC.Rank<-vapply(2:dim(Rets)[1],
                function(i) cor(
                  rank(as.numeric(Factor.Data.Std[i-1,]),na.last="keep"),
                  as.numeric(Rets[i,]),
                  use="pairwise.complete.obs"),
                3.54)
IC.Rank<-zoo(IC.Rank,time(Rets)[-1])



#Plot IC
plot(IC.Rank,type="h",main="Rank Info Coefficient",
     las=1,xlab="",ylab="")
lines(rollmeanr(IC.Rank,12),col=2)
abline(h=mean(IC.Rank),col=4)
title(sub=paste("Avg Rank IC = ",round(100*mean(IC.Rank),1),
                "%",sep=""))



#Plot IC
plot(IC.Rank,type="h",main="Rank Info Coefficient",
     las=1,xlab="",ylab="")
lines(rollmeanr(IC.Rank,12),col=2)
abline(h=mean(IC.Rank),col=4)
title(sub=paste("Avg Rank IC = ",round(100*mean(IC.Rank),1),
                "%",sep=""))



# Construct 5 portfolios
suppressMessages(library(ggplot2))

Find.Portfolios<-function(x) as.numeric(cut_number(x, n = 5))

Port.IDs<-t(apply(Factor.Data.Std,1,Find.Portfolios))
Port.IDs<-zoo(Port.IDs,time(Factor.Data.Std))

head(Port.IDs,5) # Stock i belongs to which portfolio




# Fnc for portfolio return
Construct.Port<-function(ID){
  Weights<-(Port.IDs==ID) #Weights
  Norm.Weights<-t(apply(Weights,1,function(x) x/sum(x,na.rm=T)))
  Norm.Weights<-zoo(Norm.Weights,time(Weights))#Normalized
  
  Norm.Weights<-lag(Norm.Weights,-1)#Lag weights
  
  #Calculate portfolio rets (no fees!!)
  Port.Rets<-apply(Norm.Weights*Rets[-1,],1,sum,na.rm=T)
  Port.Rets<-zoo(Port.Rets,time(Rets)[-1])        
  Port<-cumprod(1+Port.Rets)
  Port}



# Construct decile portfolios
P1<-Construct.Port(1)
P2<-Construct.Port(2)
P3<-Construct.Port(3)
P4<-Construct.Port(4)
P5<-Construct.Port(5)

All.Portfolios<-merge(P1,P2,P3,P4,P5)



plot(All.Portfolios,screens=1,col=1:5,
     xlab="",ylab="",las=1,main="Factor Portfolios")
legend("topleft",paste("Port ",1:5,sep=""),text.col=1:5,bty="n")


plot(All.Portfolios,screens=1,col=1:5,
     xlab="",ylab="",las=1,log="y",main="Factor Portfolios (Log Scale)")
legend("topleft",paste("Port ",1:5,sep=""),text.col=1:5,bty="n")



# Fnc for CAGR return
CAGR<-function(myzoo){
  (tail(myzoo,1)/as.numeric(head(myzoo,1)))^
    (365/as.numeric(diff(range(time(myzoo)))))-1
}

All.CAGR<-cbind(CAGR(All.Portfolios[,1]),
                CAGR(All.Portfolios[,2]),
                CAGR(All.Portfolios[,3]),
                CAGR(All.Portfolios[,4]),
                CAGR(All.Portfolios[,5]))


barplot(as.numeric(All.CAGR),names.arg=paste("Port ",1:5,sep=""),
        main="CAGR",las=1)

All.Vol<-apply(All.Portfolios,2,
               function(x) sqrt(12)*sd(diff(log(x))))

barplot(All.Vol,names.arg=paste("Port ",1:5,sep=""),
        main="Annualized Volatility",las=1)


# Fnc for max drawdown
MaxDD<-function(myzoo){
  min(myzoo/runMax(myzoo, n=1,cumulative =T)-1,na.rm=T)  
}

All.MDD<-apply(All.Portfolios,2,MaxDD)
barplot(All.MDD,names.arg=paste("Port ",1:5,sep=""),
        main="Max Drawdowns",las=1)


# Construct FMP
FMP.Rets<-(P5/lag(P5,-1)-1)-(P1/lag(P1,-1)-1)
FMP<-cumprod(1+FMP.Rets)

barplot(FMP.Rets, main="FMP Returns")
plot(FMP, main="FMP")

plot(FMP, main="FMP")
# t-test
t.test(FMP.Rets,alternative="greater")



# Fnc for obtaining regression slope
Get.Slope<-function(i){
  dat<-cbind(as.numeric(Rets[i,]),
             as.numeric(Factor.Data.Std[i-1,]))
  dat<-na.omit(dat)
  
  fit<-lm(dat[,1]~dat[,2])
  summary(fit)$coefficients[2,1]
  
}

# Obtain factor returns through cross sectional regressions
FMP.CS.Rets<-vapply(2:dim(Rets)[1],Get.Slope,3.54)
FMP.CS.Rets<-zoo(FMP.CS.Rets,time(Rets)[-1])

# FMP
FMP.CS<-cumprod(1+FMP.CS.Rets)


barplot(FMP.CS.Rets)

plot(FMP.CS)

# t-test
t.test(FMP.CS.Rets,alternative="greater")


# Fnc for obtaining FMP weights
Get.FMP.Weights<-function(i){
  dat<-cbind(as.numeric(Rets[i,]),
             as.numeric(Factor.Data.Std[i-1,]))
  non.nas<-which(apply(dat,1,function(x) (!is.na(x[1])) & (!is.na(x[2]))))
  dat<-na.omit(dat)
  
  XX<-cbind(rep(1,dim(dat)[1]),dat[,2])
  WW<-t(solve(t(XX)%*%XX)%*%t(XX))[,2]
  
  W<-rep(0,dim(Rets)[2])
  W[non.nas]<-WW
  W
}

# Obtain factor returns through cross sectional regressions
FMP.W<-t(vapply(2:dim(Rets)[1],Get.FMP.Weights,rep(3.54,dim(Rets)[2])))
FMP.W<-zoo(FMP.W,time(Rets)[-1])


barplot(as.numeric(tail(FMP.W,1)),
        main="FMP Weights for Last Period",las=2,
        names.arg=names(Rets),cex.names=0.4)


# Volatility and correlation of FMPs
sqrt(12)*sd(FMP.Rets)
sqrt(12)*sd(FMP.CS.Rets)

cor(FMP.Rets,FMP.CS.Rets[-1])


# Use USDTRY as macro factor
TRY<-window(FX[,"Turkey"],Month.Ends.Dates)
TRY.Rets<-TRY/lag(TRY,-1,na.pad=T)-1

# Use P1-P5 as base assets
Base.Asset.Rets<-All.Portfolios

# Fnc to compute weights
Get.TS.Weights<-function(i){
  dat<-na.omit(merge(TRY.Rets[(i-35):i],
                     All.Portfolios[(i-36):(i-1),]))
  
  fit<-lm(dat[,1]~dat[,-1])
  W<-summary(fit)$coefficients[2:6,1]
  W/sum(W)
}

FMP.W<-t(vapply(37:dim(Rets)[1],Get.TS.Weights,rep(3.54,5)))
FMP.W<-zoo(FMP.W,time(Rets)[-c(1:36)])



barplot(as.numeric(tail(FMP.W,1)),names.arg=paste("Port ",1:5,sep=""),
        las=2,main="FMP Weights")



# Principal component analysis
PCA<-princomp(coredata(diff(log(All.Portfolios))))
summary(PCA)

plot(PCA,main="Eigenvalue Scree Plot")
loadings(PCA)



PCA.FMP<-loadings(PCA)[,1]
PCA.FMP<-PCA.FMP/sum(PCA.FMP)



barplot(PCA.FMP,main="PCA FMP Weights",
        names.arg=paste("Port ",1:5,sep=""),las=2)


PCA.Factor.Rets<-zoo(PCA$scores,time(diff(log(All.Portfolios))))
head(PCA.Factor.Rets)



dat<-na.omit(merge(PCA.Factor.Rets,Bench.Rets))
cor(dat)[6,1]


TwoVarPlot(log(merge(cumprod(1-dat[,1]),cumprod(1+dat[,6]))),
           "PCA Factor Model","log(First PCA Factor)","log(Market Factor)")

