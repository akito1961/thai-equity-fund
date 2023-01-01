install.packages('quantmod')
library(quantmod)

#####  info #####
stockname='CPALL.BK'
startdate='2008-01-01'
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

#################Q2
###Extract closing price 
stock.closing.price<-stock.info[,4] 
stock.closing.price[22]
stock.info[22,]

####plot
plot(stock.closing.price,main="Price",xlab='date',type='l')

###daily rate of return
stock.return.daily<-dailyReturn(stock.info[,4])
stock.return.daily[22]

###plot return
plot(stock.return.daily,main='Return',xlab='time',type='l')

####mean daily return
mean(stock.return.daily)


###annual rate of return 
stock.return.annually<-annualReturn(stock.info[,4])
stock.return.annually

###plot return
plot(stock.return.annually,main='Return',xlab='time',type='l')

####mean daily return
mean(stock.return.annually)


#################Q3
###Extract adjusted close price 
stock.adj.close.price<-stock.info[,6]
stock.adj.close.price[22]

####plot
plot(stock.adj.close.price,main="Price",xlab='date',type='l')

###daily rate of return
stock.return.adj.daily<-dailyReturn(stock.adj.close.price)
stock.return.adj.daily[22]

###plot return
plot(stock.return.adj.daily,main='Return',xlab='time',type='l')

####mean daily return
mean(stock.return.adj.daily)


###annual rate of return 
stock.return.adj.annually<-annualReturn(stock.info[,6])


###plot return
plot(stock.return.adj.annually,main='Return',xlab='time',type='l')

####mean daily return
mean(stock.return.adj.annually)

######comparison
df<-mean(stock.return.adj.annually)-mean(stock.return.annually);df



