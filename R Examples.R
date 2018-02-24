###########################################################
### BASIC DATA TYPES
###########################################################

# Vectors
a <- c(1,2,5.3,6,-2,4) # numeric vector
b <- c("one","two","three") # character vector
c <- c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE) #logical vector
a[c(2,4)] # 2nd and 4th elements of vector


# Matrices
y<-matrix(1:20, nrow=5,ncol=4) # generates 5 x 4 numeric matrix 
y

# another example
cells <- c(1,26,24,68)
rnames <- c("R1", "R2")
cnames <- c("C1", "C2") 
mymatrix <- matrix(cells, nrow=2, ncol=2, byrow=TRUE,
                   dimnames=list(rnames, cnames))

mymatrix

mymatrix[,2] # 2th column of matrix
mymatrix[1,] # 1st row of matrix 
mymatrix[2,1:2] # row 2 of columns 1,2

# Data Frames
d <- c(1,2,3,4)
e <- c("red", "white", "red", NA)
f <- c(TRUE,TRUE,TRUE,FALSE)
mydata <- data.frame(d,e,f)
names(mydata) <- c("ID","Color","Passed") # variable names
mydata

mydata[2:3] # columns 2,3 of data frame
mydata[c("Color","Passed")] # columns  from data frame
mydata$ID # variable ID in the data frame

# Lists
# example of a list with 4 components - 
# a string, a numeric vector, a matrix, and a scaler 
w <- list(name="Fred", mynumbers=a, mymatrix=y, age=5.3)
str(w) #Structure
w

# example of a list containing two lists 
v <- list(w,w)
str(v)
length(v)
v

v[[1]]
v[[1]]$age
v[[1]][[4]]

###########################################################
### DATA ANALYSIS
###########################################################
library(zoo)
library(xts)
library(Quandl)


# Download data
myDF<-Quandl("CHRIS/CME_EC1")
class(myDF)
head(myDF)

# Convert to zoo
P<-zoo(myDF[,c(2,3,4,7)],as.Date(myDF[,1],format="%Y-%m-&-%d"))
class(P)
head(P)

# Convert to xts
P<-as.xts(P)
class(P)
head(P)

# Analyze
tail(P[,4],5)
range(P[,"High"]-P[,"Low"],na.rm = T)

# Arithmetic Returns
Ret<-P[,"Settle"]/lag.xts(P[,"Settle"],1)-1 #lag.zoo(x,-1) is equivalent to lag.xts(x,1) !!!!!

# Logarithmic Returns
LRet<-log(P[,"Settle"]/lag.xts(P[,"Settle"],1)) #lag.zoo(x,-1) is equivalent to lag.xts(x,1) !!!!!

# Long way to calculate arithmetic returns
Ret.LongWay<-NA*P[,"Settle"]
for (i in 2:length(Ret.LongWay)){
  Ret.LongWay[i]<-P[i,"Settle"]/as.numeric(P[i-1,"Settle"])-1
}
all.equal(Ret,Ret.LongWay) #Check whether two are equal


# Return density
hist(LRet,breaks = 100)
plot(density(LRet,na.rm=T))

# Autocorrelation function
acf(na.omit(LRet))

###########################################################
### PLOTS
###########################################################

# Plots
x<-tail(LRet,2*260) #Get some data
plot(x) # Plot as xts
plot(as.zoo(x)) #Plot as zoo
plot(as.numeric(x)) #Dates gone !
plot(as.numeric(x),type="l") #Line plot
plot(as.numeric(x),type="h") #Bars
barplot(as.numeric(x)) #Bars

# Plot adjustments
plot(as.zoo(x),xlab="Time Index",ylab="Returns",main="My plot",sub="Source:Quandl",
     cex.sub=0.5,cex.main=2,cex.lab=1.5,
     col.sub=4,col.main=2,col.lab=3,
     las=1) 
abline(h=0,col=4,lwd=2,lty=2)
abline(v=as.Date("2017-03-15"),col=4,lwd=2,lty=2)


###########################################################
### ROLLAPPLY
###########################################################

# Apply a rolling function 
X<-as.zoo(P[,"Settle"])

Calculate.Volatility<-function(Returns) sd(Returns,na.rm=T) #Write a function
Vol<-rollapply(diff(log(X)),260,Calculate.Volatility, #Rollapply it
               align="right", #Alignment is important. Right aligned: no lookahead bias
               fill = NA) #Fills initial values with NA

# Or we can directly write function inside rollapply
Vol2<-rollapply(diff(log(X)),260,function(y) sd(y,na.rm=T), #Rollapply it
               align="right", #Alignment is important. Right aligned: no lookahead bias
               fill = NA) #Fills initial values with NA

all.equal(Vol,Vol2)


# Rollapply can be applied to each column seperately
X<-as.zoo(P) 
head(X) #Multicolumn zoo

All.MovAvgs<-rollapplyr(X,200, # rollapplyr is equivalent to rollapply(..., align="right")
                        mean,na.rm=T, #We can pass na.rm=T argument to mean function
                        fill=NA)

tail(X)
tail(All.MovAvgs)# These are moving averages

# Rollapply can also be applied to multicolumns jointly
# Calculate the average of high and low prices for last 20 days
AvgHighLow<-rollapplyr(X,20,
              function(z) 0.5*mean(z[,"High"],na.rm=T)+0.5*mean(z[,"Low"],na.rm=T),
              by.column=F, #Do not apply for each column seperately
              fill=NA)
plot(X[,c("Low","High")],screens=1,col=c(2,4)) #Plot at same graph
plot(AvgHighLow)

###########################################################
### REGRESSION
###########################################################

# Regress returns for settlement  on open and high 
YY<-as.zoo(diff(log(tail(P[,"Settle"],3*260))))
XX<-as.zoo(diff(log(tail(P[,c("Low","High")],3*260))))

#Remove any missing data
which(is.na(YY),arr.ind = T)  #Find missing values (NA)
YY<-YY[-1,]# Remove first row with missing data
which(is.na(XX),arr.ind = T)  #Find missing values (NA)
XX<-XX[-1,]# Remove first row with missing data
XX<-na.locf(XX) #Replace NA's with last non-missing data 
which(is.na(XX),arr.ind = T)  #No missing data remained


fit<-lm(YY~XX)

temp<-summary(fit) #Estimation summary
str(temp) #What's inside it
temp$coefficients #Coefficients & t-stats
coefficients(fit) #Same
c(temp$r.squared,temp$adj.r.squared) # R-squared

plot(temp$residuals)#Residuals
all.equal(temp$residuals,residuals(fit)) #Alternative way

###########################################################
### ASSIGNMENT & ENVIRONMENT
###########################################################


# Assign R-squared values to a variable
# The following three are equivalent
MyRsquare<-temp$r.squared
assign("MyRsquare",temp$r.squared)
assign("MyRsquare",temp$r.squared,envir = globalenv())

# Create a new environment and assign inside it
MyEnv<-new.env()
ls(all=T,envir = MyEnv) # Now it is empty
assign("MyR2",temp$r.squared,envir = MyEnv)
ls(all=T,envir = MyEnv) # Now it is not empty
temp<-get("MyR2",envir = MyEnv) #Get a variable from environment
temp

# Each function has its own environment
i<-10 #i is in global environment
TestFnc<-function(n){
  i<-20 #The value of i is changed, but only within the fnc environment
  print(paste0("The value of i here is: ",i))
  2*n
}

TestFnc(20)
i #The value of i in global env remains same

# We can change the value of variables in global env within a function using << operator
i<-10 #i is in global environment
TestFnc<-function(n){
  i<<-20 #The value of i in **global env** is changed
  print(paste0("The value of i here is: ",i))
  2*n
}

TestFnc(20)
i #The value of i in global env changed to 20

###########################################################
### APPLY FUNCTIONS
###########################################################
# Apply
y<-matrix(1:20, nrow=5,ncol=4) # generates 5 x 4 numeric matrix 
apply(y,1,sum) # Row sums
apply(y,2,sum) # Column sums


# Three ways of squaring
VV<-rnorm(10000) #Random numbers

SQ1<- VV^2 # Using a vectorized function

SQ2<-vector(mode = "numeric",length = length(VV)) #For loop
for (i in 1:length(VV)) SQ2[i]<-VV[i]^2 

SQ3<-vapply(1:length(VV),function(j) VV[j]^2,3.54) #Apply fnc

all.equal(SQ1,SQ2,SQ3)

# Which one is fastest
system.time(for (i in 1:10000) VV^2)
system.time(for (i in 1:10000) for (i in 1:length(VV)) SQ2[i]<-VV[i]^2 )
system.time(for (i in 1:10000) vapply(1:length(VV),function(j) VV[j]^2,3.54))
# Typically time requirements: Vectorized function < apply functions < for loops


# Apply on lists: lapply
l <- list(a = 1:10, b = 11:20) # create a list with 2 elements
lapply(l, mean) # the mean of the values in each element
lapply(l, sum) # the sum of the values in each element

# sapply is similar, but returns a vector/matrix
sapply(l, mean) # the mean of the values in each element
sapply(l, sum) # the sum of the values in each element


# Apply on two lists
l1 <- list(a = c(1:10), b = c(11:20))
l2 <- list(c = c(21:30), d = c(31:40))
# sum the corresponding elements of l1 and l2
mapply(sum, l1$a, l1$b, l2$c, l2$d)

