#Source; https://rpubs.com/JanpuHou/258620

rm(list = ls(all.names = TRUE)) #clear objects

install.packages("reshape2")
install.packages("quadprog")
install.packages('quantmod')
install.packages('TTR')
install.packages('autoimage')
install.packages('fPortfolio')
install.packages('timeSeries')
install.packages('matlib')
library(quantmod) 
library(TTR) 
library(ggplot2)
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
port.return<-dailyReturn(stock.info[,6]);head(port.return)

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
port.return<-cbind(port.return,dailyReturn(stock.info[,6]));head(port.return)

###statistics of assets
n.asset<-ncol(port.price);n.asset
mean.asset<-c(mean(annualReturn(port.price[,1])),mean(annualReturn(port.price[,2])));mean.asset
covariance.mat<-252*cov(port.return);covariance.mat

###define functions
sd.port<-function(w){ 
  sd<-(t(w) %*% covariance.mat %*% w)^(0.5) 
  return(sd)
}

mean.return.port<-function(w){ 
  mean<-t(mean.asset) %*% w
  return(mean)
}

port.stat.w<-function(w){
  sd<-sd.port(w);
  mean<-mean.return.port(w)
  return(c(w,sd,mean))
}

####store values
  n.point=100
  port.stat.mat.w<-matrix(0,nrow=n.point,ncol=4)
  j=1
  for(i in seq(0, 1, length.out=n.point) )
  { w=c(i,1-i)  
  port.stat.mat.w[j,]<-port.stat.w(w)
  j=j+1
  }

  ###risk free rate
  mu.f<-0.01
  
  ###calculate Sharpe ratios
  sharpe.mat<-matrix(0,nrow=n.point,ncol=3)
  for(j in 1:n.point) 
  { 
    sharpe.mat[j,1]<-port.stat.mat.w[j,1]
    sharpe.mat[j,2]<-port.stat.mat.w[j,2]
    sharpe.mat[j,3]<-(port.stat.mat.w[j,4]-mu.f)/port.stat.mat.w[j,3]
  }
  
  ###plot Sharpe ratios
  par(mar=c(4,4,4,4))
  plot(sharpe.mat[,1],sharpe.mat[,3],type="l",ylab="Sharpe Ratio",xlab="Weight of COM7") #plot Sharpe ratio

  #maximizing sharpe ratio
  max(sharpe.mat) #maximize Sharpe ratio
  head(sharpe.mat)
  mx<-which(sharpe.mat[,3]==max(sharpe.mat))
  mx
  port.stat.mat.w[mx,] #weight return and risk for 
                       #the port that maximizes the ratio
                       #Note that mean is above 0.6 as required
  
  ####Sharpe ratio of the portfolio with target return at 0.65
  w.a<-0.4419963 #from the previous exercise
(mean.return.port(c(w.a,1-w.a))-mu.f)/sd.port(c(w.a,1-w.a))
  
  plot(port.stat.mat.w[,3],port.stat.mat.w[,4],type="l",main="Feasible strategies",xlab="sd",ylab="mean")
  #points(port.stat.mat[mx,1],port.stat.mat[mx,2],col="Red",type="p")
  mtext("maximum sharpe ratio strategy ", side=3,col="Red")
  abline(a=0.65, b=0, col="Green") #target return at 0.65
  
  ###plot Sharpe ratios
  plot(sharpe.mat[,1],sharpe.mat[,3],type="l",ylab="Sharpe Ratio",xlab="Weight of COM7") #plot Sharpe ratio
  
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
port.annual.return<-cbind(annualReturn(port.price[,1]),annualReturn(port.price[,2]),annualReturn(port.price[,3]))
names(port.annual.return) <- c("COM7", "SPVI", "TACC");port.annual.return[1,]
View(port.annual.return)
#mean.asset<-c(mean(annualReturn(port.price[,1])),mean(annualReturn(port.price[,2])));mean.asset

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

###############Q2.1
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

###plot
###Note that hjust and vjust can be used to adjust the position of text box of risk, return and Sharpe ratio.
ggplot(eff, aes(x=Std.Dev, y=Exp.Return)) + geom_point(alpha=.1, color=ealdark) +
  geom_point(data=eff.optimal.point, aes(x=Std.Dev, y=Exp.Return, label=sharpe),
             color=ealred, size=3) +
  annotate(geom="text", x=eff.optimal.point$Std.Dev,
           y=eff.optimal.point$Exp.Return,
           label=paste("Risk: ",
                       round(eff.optimal.point$Std.Dev*100, digits=3),"%\nReturn: ",
                       round(eff.optimal.point$Exp.Return*100, digits=4),"%\nSharpe: ",
                       round(eff.optimal.point$sharpe, digits=2), sep=""),
           hjust=1, vjust=1.2) +
  ggtitle("Efficient Frontier") +
  labs(x="Risk (standard deviation of portfolio)", y="Return") +
  theme(panel.background=element_rect(fill=eallighttan),
        text=element_text(color=ealdark),
        plot.title=element_text(size=24, color=ealred))


###############Q2.2
# Run the eff.frontier function based on no short and no allocation restrictions
eff <- eff.frontier(data1, short="no", max.allocation= NULL, risk.premium.up=0.5, risk.increment=.005)

# Find the optimal portfolio
#make sure no short selling is allowed
eff<-subset(eff,eff$COM7>=0)
eff<-subset(eff,eff$SPVI>=0)
eff<-subset(eff,eff$TACC>=0)

#set target return
eff.optimal.point <- subset(eff,abs(eff$Exp.Return-0.6340)==min(abs(eff$Exp.Return-0.6340)))
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
ggplot(eff, aes(x=Std.Dev, y=Exp.Return)) + geom_point(alpha=.1, color=ealdark) +
  geom_point(data=eff.optimal.point, aes(x=Std.Dev, y=Exp.Return, label=sharpe),
             color=ealred, size=3) +
  annotate(geom="text", x=eff.optimal.point$Std.Dev,
           y=eff.optimal.point$Exp.Return,
           label=paste("Risk: ",
                       round(eff.optimal.point$Std.Dev*100, digits=3),"%\nReturn: ",
                       round(eff.optimal.point$Exp.Return*100, digits=4),"%\nSharpe: ",
                       round(eff.optimal.point$sharpe, digits=2), sep=""),
           hjust=-0.3, vjust=-0.3) +
  ggtitle("Efficient Frontier") +
  labs(x="Risk (standard deviation of portfolio)", y="Return") +
  theme(panel.background=element_rect(fill=eallighttan),
        text=element_text(color=ealdark),
        plot.title=element_text(size=24, color=ealred))

######Q2.3
# Run the eff.frontier function based on no short and no allocation restrictions
eff <- eff.frontier(data1, short="no", max.allocation= NULL, risk.premium.up=0.5, risk.increment=.005)

# Find the optimal portfolio
#make sure no short selling is allowed
eff<-subset(eff,eff$COM7>=0)
eff<-subset(eff,eff$SPVI>=0)
eff<-subset(eff,eff$TACC>=0)

#set target risk
eff <- subset(eff,eff$Std.Dev<=0.5595)

#maximize sharpe ratio
eff.optimal.point <- eff[eff$sharpe==max(eff$sharpe),]
eff.optimal.point


#optimal port (COM7,SPVI,TACC)
c(eff.optimal.point$COM7,eff.optimal.point$SPVI,eff.optimal.point$TACC)
sum(c(eff.optimal.point$COM7,eff.optimal.point$SPVI,eff.optimal.point$TACC))

# graph efficient frontier
# Start with color scheme
ealred <- "#7D110C"
ealtan <- "#CDC4B6"
eallighttan <- "#F7F6F0"
ealdark <- "#423C30"

par(no.readonly = TRUE)
ggplot(eff, aes(x=Std.Dev, y=Exp.Return)) + geom_point(alpha=.1, color=ealdark) +
  geom_point(data=eff.optimal.point, aes(x=Std.Dev, y=Exp.Return, label=sharpe),
             color=ealred, size=3) +
  annotate(geom="text", x=eff.optimal.point$Std.Dev,
           y=eff.optimal.point$Exp.Return,
           label=paste("Risk: ",
                       round(eff.optimal.point$Std.Dev*100, digits=3),"%\nReturn: ",
                       round(eff.optimal.point$Exp.Return*100, digits=4),"%\nSharpe: ",
                       round(eff.optimal.point$sharpe, digits=2), sep=""),
           hjust=1, vjust=1.2) +
  ggtitle("Efficient Frontier") +
  labs(x="Risk (standard deviation of portfolio)", y="Return") +
  theme(panel.background=element_rect(fill=eallighttan),
        text=element_text(color=ealdark),
        plot.title=element_text(size=24, color=ealred))

#######Q2.4
# Run the eff.frontier function based on allowed short-selling and no allocation restrictions
eff <- eff.frontier(data1, short="yes", max.allocation= NULL, risk.premium.up=0.5, risk.increment=.005)

# Find the optimal portfolio
#make sure TACC is invested
eff<-subset(eff,abs(eff$TACC)>=0)
names(eff)
#maximize Sharpe ratio
eff.optimal.point <- subset(eff,eff$Std.Dev==min(eff$Std.Dev))
eff.optimal.point

#optimal port (COM7,SPVI,TACC)
c(eff.optimal.point$COM7,eff.optimal.point$SPVI,eff.optimal.point$TACC)
sum(c(eff.optimal.point$COM7,eff.optimal.point$SPVI,eff.optimal.point$TACC))

# graph efficient frontier
# Start with color scheme
ealred <- "#7D110C"
ealtan <- "#CDC4B6"
eallighttan <- "#F7F6F0"
ealdark <- "#423C30"

par(no.readonly = TRUE)
ggplot(eff, aes(x=Std.Dev, y=Exp.Return)) + geom_point(alpha=.1, color=ealdark) +
  geom_point(data=eff.optimal.point, aes(x=Std.Dev, y=Exp.Return, label=sharpe),
             color=ealred, size=3) +
  annotate(geom="text", x=eff.optimal.point$Std.Dev,
           y=eff.optimal.point$Exp.Return,
           label=paste("Risk: ",
                       round(eff.optimal.point$Std.Dev*100, digits=3),"%\nReturn: ",
                       round(eff.optimal.point$Exp.Return*100, digits=4),"%\nSharpe: ",
                       round(eff.optimal.point$sharpe, digits=2), sep=""),
           hjust=-0.3, vjust=-0.3) +
  ggtitle("Efficient Frontier") +
  labs(x="Risk (standard deviation of portfolio)", y="Return") +
  theme(panel.background=element_rect(fill=eallighttan),
        text=element_text(color=ealdark),
        plot.title=element_text(size=24, color=ealred))

