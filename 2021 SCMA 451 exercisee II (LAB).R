#rm(list=ls())

install.packages('quantmod')
install.packages('TTR')
install.packages('ggplot2')
install.packages('matlib')
install.packages('PerformanceAnalytics')
install.packages('matrixcalc')
library(quantmod)
library(TTR) 
library(ggplot2)
library(matlib)
library(PerformanceAnalytics)
library(matrixcalc)


#####  stock info #####
stockname <- 'COM7.BK'
startdate <- '2016-01-01'
enddate <- '2019-12-31'

###extract stock info --> This requires internet connection!!!
stock.info <- getSymbols(c(stockname),
                         src = 'yahoo',
                         from = startdate,
                         to = enddate,
                         auto.assign = FALSE)

###delete N/A
stock.info <- na.omit(stock.info)
stock.info[1,]


###Extract close price 
stock.close.price <- Cl(stock.info)

###daily return
Daily.return.close <- dailyReturn(Cl(stock.info),
                                  type='log')
View(Daily.return.close)

###SD of return of close price 
SD.return.daily <- sd(Daily.return.close)
SD.return.daily


###SV of return of close price
SV <- SemiVariance(Daily.return.close)
SV

###Volatility
T.year <- 252
T.hist <- 2*T.year   #2 years (252 trading days per year)


View(Daily.return.close[1:T.hist])  ##first 2*252 trading days of return

Rolling.volatility <- volatility(Cl(stock.info),
                                 n=(T.hist),
                                 calc="close",
                                 N=T.year,
                                 mean0=FALSE)

Rolling.volatility <- na.omit(Rolling.volatility)
View(Rolling.volatility)

jpeg("Rolling.volatility.jpeg")
plot(Rolling.volatility,ylab="volatility")
dev.off()



#####################################Two asset #################################################
#####  stock info #####
stockname='COM7.BK'
startdate='2016-01-01'
enddate='2019-12-31'

###extract stock info --> This requires internet connection!!!
stock.info <- getSymbols(c(stockname),
                         src = 'yahoo',
                         from=startdate,
                         to=enddate,
                         auto.assign=FALSE) 

###delete N/A
stock.info=na.omit(stock.info)
stock.info[1,]

### Create portolio
port.price <- stock.info[,6];port.price[1,]
port.return <- dailyReturn(stock.info[,6]);View(port.return)

#####  stock info #####
stockname='SPVI.BK'
startdate='2016-01-01'
enddate='2019-12-31'

###extract stock info --> This requires internet connection!!!
stock.info <- getSymbols(c(stockname),
                         src = 'yahoo',
                         from=startdate,
                         to=enddate,
                         auto.assign=FALSE) 

###delete N/A
stock.info=na.omit(stock.info)
stock.info[1,]

###add assets in port
port.price <- cbind(port.price,stock.info[,6]);port.price[1,]
port.return <- cbind(port.return,dailyReturn(stock.info[,6]));head(port.return)

###statistics of assets
n.asset <- ncol(port.price);n.asset
mean.asset<-c(mean(annualReturn(port.price[,1])),mean(annualReturn(port.price[,2])));mean.asset

#########Q1
covariance.mat <- 252*cov(port.return);covariance.mat

###define functions
sd.port<-function(w){ 
  sd<-(t(w) %*% covariance.mat %*% w)^(0.5) 
  return(sd)
}

mean.return.port<-function(w){ 
  mean<-t(mean.asset) %*% w
  return(mean)
}

port.stat<-function(w){
  sd<-sd.port(w);
  mean<-mean.return.port(w)
  return(c(sd,mean))
}


#########Q2
###plot feasible strategies
n.point=100
port.stat.mat<-matrix(0,nrow=n.point,ncol=4)
  j=1
  for(i in seq(0, 1, length.out=n.point) )
  { 
    w=c(i,1-i)
    
    ###V02: store weights in the matrix 
    port.stat.mat[j,3]<-w[1]
    port.stat.mat[j,4]<-w[2]
    port.stat.mat[j,1]<-port.stat(w)[1]
    port.stat.mat[j,2]<-port.stat(w)[2]
    j=j+1
  }
par(mar=c(4,4,4,4))
plot(port.stat.mat[,1],port.stat.mat[,2],type="l",main="Feasible strategies",xlab="sd",ylab="mean")

#####Q3
###Find the minimum variance strategy
sd.min<-min(port.stat.mat[,1]);sd.min

#find the row that contains minimum variance 
j=which(port.stat.mat[,1]==sd.min);j

###find mean return and weights
mean<-port.stat.mat[j,2];mean
weight.min<-port.stat.mat[j,3:4];weight.min

port.stat.mat[j,]
port.min.sd<-c(sd.min,mean);port.min.sd
plot(port.stat.mat[,1],port.stat.mat[,2],type="l",main="Feasible strategies",xlab="sd",ylab="mean")
points(port.min.sd[1],port.min.sd[2],col="Red",type="p")
mtext("minimum risk strategy ", side=3,col="Red")
which(port.stat.mat[,1]==sd.min)

####Q4
####target return at 0.65
n.asset<-ncol(port.return)
A.storing<-function(){
  A<-matrix(0,nrow=n.asset+2,ncol=n.asset+2);dim(A)
  for(i in 1:n.asset){
  for(j in 1:n.asset){
    A[i,j]=2*covariance.mat[i,j]
  }
  A[i,n.asset+1]=-mean.asset[i]
  A[i,n.asset+2]=-1
}
  for(j in 1:n.asset){
  A[n.asset+1,j]=mean.asset[j]
  A[n.asset+2,j]=1
  }
  return(A)
  }
A<-A.storing();A


###set up b
target.return=0.65
b<-c(0,0,target.return,1);b
A.inv<-matrix.inverse(A);A.inv
sol<-A.inv%*%b;sol


plot(port.stat.mat[,1],
     port.stat.mat[,2],
     type="l",
     main="Feasible strategies",
     xlab="sd",
     ylab="mean")

points(port.stat(c(sol[1],sol[2]))[1],port.stat(c(sol[1],sol[2]))[2],col="Red",type="p")
mtext("optimal strategy", side=3,col="Red")


