# R script to read prices from Yahoo and construct a portfolio
# updated 08/19/2012
library(timeSeries)
library(fPortfolio)
library(quantmod)
library(caTools)

# get libraries of routines - these packages need to be installed
suppressPackageStartupMessages(require (timeSeries))
suppressPackageStartupMessages(require (fPortfolio)) # may also require installing the package require(slam)
suppressPackageStartupMessages(require(quantmod))
suppressPackageStartupMessages(require(caTools))
# create list of stock tickers - replace the tickers here with those you want to use in your portfolio
TickerList <- c("RYE", "INDA", "ISRA", "SCHA", "SLYG", "SWPPX", "GOOGL", "AMZN", "NFLX", "TTM")
# read closing prices from Yahoo keeping only the closing prices
ClosingPricesRead <- NULL
for (Ticker in TickerList)
  ClosingPricesRead <- cbind(ClosingPricesRead,
                             getSymbols.yahoo(Ticker, from="1950-01-01", verbose=FALSE, auto.assign=FALSE)[,6]) # [,6] = keep the adjusted prices
# keep only the dates that have closing prices for all tickers
ClosingPrices <- ClosingPricesRead[apply(ClosingPricesRead,1,function(x) all(!is.na(x))),]

#test

# convert prices to daily returns
returns <- as.timeSeries(tail(ClosingPrices,-1) / as.numeric(head(ClosingPrices,-1)) -1)
# calculate the efficient frontier
Frontier <- portfolioFrontier(returns)
# plot frontier
plot(Frontier, 1) # can also call the plot routine so it only plots the frontier: plot(Frontier,1)
###########################################################################################
Frontier

#Enter plot(Frotnier) in console to plot interactively- 0 to exit

plot(Frontier, 3) #add tangent line
plot(Frontier, 7) #add Monte Carlo Portfolios
plot(Frontier, 4) #Risk Reutrn of single assets
plot(Frontier, 2) #minimum risk portfolio
plot(Frontier, 5) #equal weights portfolio
plot(Frontier, 6) #Two asset Frontiers xxxxx
plot(Frontier, 8) #add Sharpe ratio

####### addtional code to get a better look at the portfolios - annualize the returns and risk
# get the means and covariance matrix of the price returns
meanReturn <- getStatistics(Frontier)$mean # data input into the efficient frontier calculator
coroleation <- cor(returns)
write.csv(meanReturn, file = "meanReturn.CSV")
write.csv(coroleation, file = "coroleation.csv")
meanReturn


# execute the next commands to plot annualized returns and risk
# convert from daily to annual returns and risk for points on the efficient frontier
# plot efficient frontier using annualized return and risk
riskReturnPoints <- frontierPoints(Frontier) # get risk and return values for points on the efficient frontier
annualizedPoints <- data.frame(targetRisk=riskReturnPoints[, "targetRisk"] * sqrt(252),
                               targetReturn=riskReturnPoints[,"targetReturn"] * 252)

plot(annualizedPoints)
annualizedPoints
write.csv(annualizedPoints, file = "annualizedPoints.csv")


# plot Sharpe ratios for each point on the efficient frontier
dev.new() #new plots page
riskFreeRate <- 0
plot((annualizedPoints[,"targetReturn"] - riskFreeRate) / annualizedPoints[,"targetRisk"], xlab="point on efficient frontier", ylab="Sharpe ratio")




# plot the allocation to each stock for each point on the efficient frontier
# weightsPlot(Frontier)
allocations <- getWeights(Frontier@portfolio) # get allocations for each instrument for each point on the efficient frontier
colnames(allocations) <- TickerList
barplot(t(allocations), axes = T, col=rainbow(ncol(allocations)+2), legend=colnames(allocations))
axis(1)



allocations
write.csv(allocations, file = "allocations.csv")

annualizedPoints

write.csv(annualizedPoints, file = "annualPoints.csv")
getwd() 

write.csv(returns, file = "returns.csv")
