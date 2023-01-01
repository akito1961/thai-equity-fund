########### MISC Library############

#install.packages(c('quantmod',
#                   "reshape2",
#                   "quadprog",
#                   'TTR',
#                   'autoimage',
#                   'fPortfolio',
#                   'timeSeries',
#                   'matlib',
#                   "PerformanceAnalytics",
#                   "matrixcalc",
#                   "ggplot2"))

library(quantmod)
library(TTR) 
library(ggplot2)  #Used to graph efficient frontier
library(matlib)
library(PerformanceAnalytics)
library(matrixcalc)
library(autoimage)
library(fPortfolio)
library(timeSeries)
library(reshape2) # Used to melt the data
library(quadprog) #Needed for solve.QP

########### GET DATA ###########

# 1. 1st Asset
stockname <- 'EA.BK'
startdate <- '2021-01-01'
enddate <- '2022-03-31'


stock.info <- getSymbols(c(stockname),
                         src = 'yahoo',
                         from = startdate,
                         to = enddate,
                         auto.assign=FALSE) 
head(stock.info,5)


## Clean NA
stock.info <- na.omit(stock.info)
stock.info[1,]

### Create portolio
port.price <- stock.info[,6]
port.return <- dailyReturn(stock.info[,6]);head(port.return)


# 2. 2nd Asset
stockname <- 'TISCO.BK'

stock.info <- getSymbols(c(stockname),
                         src = 'yahoo',
                         from = startdate,
                         to = enddate,
                         auto.assign=FALSE) 
head(stock.info,5)

## Clean NA
stock.info <- na.omit(stock.info)
stock.info[1,]

## Add to port
port.price <- cbind(port.price,
                    stock.info[,6])
head(port.price)  

port.return <- cbind(port.return,
                     dailyReturn(stock.info[,6]))
head(port.return)


# Some statistics
n.asset <- ncol(port.price);n.asset
mean.asset<-c(mean(annualReturn(port.price[,1])),
              mean(annualReturn(port.price[,2])))
mean.asset


# CoV of 2 Assets
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


###plot feasible strategies
n.point=100
port.stat.mat<-matrix(0,nrow=n.point,ncol=5)
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
plot(port.stat.mat[,1],
     port.stat.mat[,2],
     type="l",
     main="Feasible strategies",
     xlab="sd",
     ylab="mean")


## max sharpe ratio with 0% risk free
port.stat.mat[,5] <- port.stat.mat[,2]/port.stat.mat[,1]
sharpe.max <-max(port.stat.mat[,2]/port.stat.mat[,1])

#find the row that contains max sharpe ratio
j=which(port.stat.mat[,5]==sharpe.max);j

###find mean return and weights
mean <- port.stat.mat[j,2];mean
weight.min <- port.stat.mat[j,3:4];weight.min

port.stat.mat[j,]
port.sd <- c(port.stat.mat[j,1],mean);port.sd

plot(port.stat.mat[,1],
     port.stat.mat[,2],
     type="l",
     main="Feasible Strategies EA.BK vs TISCO.BK",
     xlab="sd",
     ylab="mean")

points(port.sd[1],
       port.sd[2],
       col="Red",
       type="p")

mtext("maximimum sharpe ratio strategy ", side=3,col="Red")
#which(port.stat.mat[,1]==sd.min)






