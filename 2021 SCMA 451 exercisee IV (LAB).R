rm(list = ls(all.names = TRUE)) #clear objects

install.packages("reshape2")
install.packages("quadprog")
install.packages('quantmod')
install.packages('TTR')
install.packages('autoimage')
install.packages('fPortfolio')
install.packages('timeSeries')
library(quantmod) 
library(TTR) 
library("ggplot2")
library(matlib)
library(autoimage)
library(fPortfolio)
library(timeSeries) 
library(ggplot2) # Used to graph efficient frontier
library(reshape2) # Used to melt the data
library(quadprog) #Needed for solve.QP

#################################Q1
#####  stock info #####
stockname='COM7.BK'
startdate='2016-01-01'
enddate='2019-12-31'

###extract stock info --> This requires internet connection!!!
stock.info<-getSymbols(c(stockname),src = 'yahoo',from=startdate,to=enddate,auto.assign=FALSE) 

###delete N/A
stock.info=na.omit(stock.info)
stock.info[1,]


###Extract adjusted close price 
stock.adj.close.price<-stock.info[,6]

### Create portolio
port.price<-stock.info[,6];port.price[1,]
port.return<-dailyReturn(stock.info[,6]);port.return

#####  stock info #####
stockname='SPVI.BK'
startdate='2016-01-01'
enddate='2019-12-31'

###extract stock info --> This requires internet connection!!!
stock.info<-getSymbols(c(stockname),src = 'yahoo',from=startdate,to=enddate,auto.assign=FALSE) 

###delete N/A
stock.info=na.omit(stock.info)
stock.info[1,]

###add assets in port
port.price<-cbind(port.price,stock.info[,6]);port.price[1,]
port.return<-cbind(port.return,dailyReturn(stock.info[,6]));port.return[1,]

#################################Q2
#####  stock info #####
stockname='TACC.BK'
startdate='2016-01-01'
enddate='2019-12-31'

###extract stock info --> This requires internet connection!!!
stock.info<-getSymbols(c(stockname),src = 'yahoo',from=startdate,to=enddate,auto.assign=FALSE) 

###delete N/A
stock.info=na.omit(stock.info)
stock.info[1,]

###add assets in port
port.price<-cbind(port.price,stock.info[,6]);port.price[1,]
port.return<-cbind(port.return,dailyReturn(stock.info[,6]));port.return[1,]

###statistics of assets
n.asset<-ncol(port.return);n.asset
mean.asset<-c(mean(annualReturn(port.price[,1])),mean(annualReturn(port.price[,2])),mean(annualReturn(port.price[,3])));mean.asset
covariance.mat<-252*cov(port.return);covariance.mat

port.annual.return<-cbind(annualReturn(port.price[,1]),annualReturn(port.price[,2]),annualReturn(port.price[,3]));port.annual.return
names(port.annual.return) <- c("COM7", "SPVI", "TACC");port.annual.return[1,]
View(port.annual.return)

###create a time series 
data1<-as.timeSeries(port.annual.return);data1[1,] 

#### Efficient Frontier function ####

eff.frontier <- function (returns, short="no", max.allocation=NULL,
                          risk.premium.up=.5, risk.increment=.005){
  # return argument should be a m x n matrix with one column per security
  # short argument is whether short-selling is allowed; default is no (short
  # selling prohibited)max.allocation is the maximum % allowed for any one
  # security (reduces concentration) risk.premium.up is the upper limit of the
  # risk premium modeled (see for loop below) and risk.increment is the
  # increment (by) value used in the for loop
  
  covariance <- cov(returns)
  print(covariance)
  n <- ncol(covariance)
  
  # Create initial Amat and bvec assuming only equality constraint
  # (short-selling is allowed, no allocation constraints)
  Amat <- matrix (1, nrow=n)
  bvec <- 1
  meq <- 1
  
  # Then modify the Amat and bvec if short-selling is prohibited
  if(short=="no"){
    Amat <- cbind(1, diag(n))
    bvec <- c(bvec, rep(0, n))
  }
  
  # And modify Amat and bvec if a max allocation (concentration) is specified
  if(!is.null(max.allocation)){
    if(max.allocation > 1 | max.allocation <0){
      stop("max.allocation must be greater than 0 and less than 1")
    }
    if(max.allocation * n < 1){
      stop("Need to set max.allocation higher; not enough assets to add to 1")
    }
    Amat <- cbind(Amat, -diag(n))
    bvec <- c(bvec, rep(-max.allocation, n))
  }
  
  # Calculate the number of loops
  loops <- risk.premium.up / risk.increment + 1
  loop <- 1
  
  # Initialize a matrix to contain allocation and statistics
  # This is not necessary, but speeds up processing and uses less memory
  eff <- matrix(nrow=loops, ncol=n+3)
  # Now I need to give the matrix column names
  colnames(eff) <- c(colnames(returns), "Std.Dev", "Exp.Return", "sharpe")
  
  # Loop through the quadratic program solver
  for (i in seq(from=0, to=risk.premium.up, by=risk.increment)){
    dvec <- colMeans(returns) * i # This moves the solution along the EF
    sol <- solve.QP(covariance, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
    eff[loop,"Std.Dev"] <- sqrt(sum(sol$solution*colSums((covariance*sol$solution))))
    eff[loop,"Exp.Return"] <- as.numeric(sol$solution %*% colMeans(returns))
    eff[loop,"sharpe"] <- eff[loop,"Exp.Return"] / eff[loop,"Std.Dev"]
    eff[loop,1:n] <- sol$solution
    loop <- loop+1
  }
  
  return(as.data.frame(eff))
}
###### end of the function

# Run the eff.frontier function based on no short and no allocation restrictions
eff <- eff.frontier(data1, short="no", max.allocation= NULL, risk.premium.up=0.5, risk.increment=.005)
View(eff)

# Find the optimal portfolio
#make sure no short selling is allowed
eff<-subset(eff,eff$COM7>=0)
eff<-subset(eff,eff$SPVI>=0)
eff<-subset(eff,eff$TACC>=0)
View(eff) #check it!

#maximize Sharpe ratio
eff.optimal.point <- eff[eff$sharpe==max(eff$sharpe),]
names(eff.optimal.point)
eff.optimal.point

#optimal port (COM7,SPVI,TACC)
c(eff.optimal.point$COM7,eff.optimal.point$SPVI,eff.optimal.point$TACC)

# graph efficient frontier
# Start with color scheme
ealred <- "#7D110C"
ealtan <- "#CDC4B6"
eallighttan <- "#F7F6F0"
ealdark <- "#423C30"

par(no.readonly = TRUE)

###Q2.1 graph of portfolio excluding risk-free asset
###Note that hjust and vjust can be used to adjust the position of text box of risk, return and Sharpe ratio.
ggplot(eff, aes(x=Std.Dev, y=Exp.Return)) + geom_point(alpha=.1, color=ealdark) +
  ggtitle("Efficient Frontier") +
  labs(x="Risk (standard deviation of portfolio)", y="Return") +
  theme(panel.background=element_rect(fill=eallighttan),
        text=element_text(color=ealdark),
        plot.title=element_text(size=24, color=ealred))

####Q2.2
### risk-free info ####
bond.info <- read.csv(file="Bond_gov.csv", head=TRUE, sep=",")
View(bond.info)
i<-which(bond.info[,1]=="T-BILL1Y")
bond.yield<-bond.info[i,2]
bond.yield<-bond.yield/100
bond.yield #bond yield

###Q2.3
###find M point
View(eff)
bond.yield.vec<-matrix(bond.yield,nrow=nrow(eff),ncol=1)
tan.theta<-(eff[,5]-bond.yield)/eff[,4];tan.theta[1]
max(tan.theta)
j<-which(tan.theta==max(tan.theta));j
eff[j,]  #the one fund

ggplot(eff, aes(x=Std.Dev, y=Exp.Return)) + geom_point(alpha=.1, color=ealdark) +
  #geom_point(data=bond.frame, aes(x=Std.Dev, y=Exp.Return),
  #          color=ealred, size=3) +
  geom_point(data=eff[j,], aes(x=Std.Dev, y=Exp.Return),
                       color=ealred, size=3) +
  annotate(geom="text", x=eff.optimal.point$Std.Dev,
           y=eff.optimal.point$Exp.Return,
           label=paste("Risk: ",
                       round(eff$Std.Dev[j]*100, digits=3),"%\nReturn: ",
                       round(eff$Exp.Return[j]*100, digits=4),"%\nSharpe: ",
                       round(eff$sharpe[j], digits=2), sep=""),
           hjust=1, vjust=1) +
  ggtitle("Efficient Frontier") +
  labs(x="Risk (standard deviation of portfolio)", y="Return") +
  theme(panel.background=element_rect(fill=eallighttan),
        text=element_text(color=ealdark),
        plot.title=element_text(size=24, color=ealred)) 


###Q2.4 
#Find joining M to the risk free asset
View(eff)
n.point=100
port.stat.mat.w<-matrix(0,nrow=n.point,ncol=4)
for(i in 1:n.point) {
  w<-i/n.point
  port.stat.mat.w[i,1]<-w
  port.stat.mat.w[i,2]<-1-w  
  port.stat.mat.w[i,3]<-w*bond.yield+(1-w)*eff$Exp.Return[j]  
  port.stat.mat.w[i,4]<-(1-w)*eff$Std.Dev[j]
}
View(port.stat.mat.w)
port.frame<-data.frame(Weight1<-port.stat.mat.w[,1],
                       Weight2<-port.stat.mat.w[,2],
                       Mean<-port.stat.mat.w[,3],
                       Sd<-port.stat.mat.w[,4])

ggplot(port.frame, aes(x=Sd, y=Mean)) + geom_point(alpha=.1, color=ealdark) +
  geom_point(data=eff[j,], aes(x=Std.Dev, y=Exp.Return),
             color=ealred, size=3) +
  annotate(geom="text", x=eff$Std.Dev[j],
           y=eff$Exp.Return[j],
           label=paste("Point M    \n", "Risk: ",
                       round(eff$Std.Dev[j]*100, digits=3),"%\nReturn: ",
                       round(eff$Exp.Return[j]*100, digits=4), sep=""),
           hjust=1.5, vjust=1) +
  ggtitle("Efficient Frontier") +
  labs(x="Risk (standard deviation of portfolio)", y="Return") +
  theme(panel.background=element_rect(fill=eallighttan),
        text=element_text(color=ealdark),
        plot.title=element_text(size=24, color=ealred)) 



    
