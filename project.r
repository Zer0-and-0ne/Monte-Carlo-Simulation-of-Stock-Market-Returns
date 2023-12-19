
# Monte Carlo Simulation of Stock Market Returns

# library(tidyquant)
library(quantmod)


getSymbols("SPY", from = '2020-04-01')
tail(SPY)
View(SPY)
getSymbols("SPY", from=Sys.Date()-20, to=Sys.Date())
# https://www.youtube.com/watch?v=hZS31_nSlGg&t=202s
?getSymbols
getSymbols(c('SPY','QQQ','^GSPC','F','META'),from=Sys.Date()-20, to=Sys.Date())
# you can find the 'symbols' or 'tickers' here: https://finance.yahoo.com/lookup
# some popular tickers: 
# SPY is SPDR S&P 500 ETF Trust ticker symbol
# QQQ is Invesco QQQ Trust ticker symbol
# ^GSPC is S&P500
# PFE is Pfizer 
# F is Ford
# META is Meta Platforms, Inc. (Previously known as Facebook)

# Visualization:
windows() 
chartSeries(SPY, TA=NULL,theme = chartTheme("white"))
closeprice = Cl(SPY) # We assign the closing price to a new variable called closeprice.
windows()
plot(closeprice)

stock_Price <- as.matrix(as.numeric(SPY[ , 4]))
stock_Price
windows() 
plot(stock_Price)
windows() 
plot(stock_Price, type = 'l', col="blue", lwd=2)

# This function returns the consecutive relative differences
returns = function(Y){
  len = nrow(Y)
  yDif = Y[2:len, ] / Y[1:len-1, ] - 1
}

stock_Returns <- returns(stock_Price)
prices <- cumprod(c(stock_Price[1], 1+stock_Returns))
prices
as.numeric(SPY[ , 4])

# same as above, i.e. function 'returns'
# SPY$SPY.Returns <- diff(SPY$SPY.Close)/lag(SPY$SPY.Close)
# tail(SPY)

windows() 
hist(stock_Returns, col="blue", freq=TRUE)

# repeating for larger time period
getSymbols("SPY", from = '2022-04-01')
stock_Price <- as.matrix(as.numeric(SPY[ , 4]))
stock_Returns <- returns(stock_Price)
hist(stock_Returns, col="blue", freq=TRUE)

daily_mean <- mean(stock_Returns) 
daily_std_dev <- sd(stock_Returns)

no_of_days <- 20 # Set variable to 20 days
#Closing price from the first item in our frame
starting_price <- stock_Price[1] 
prices <- cumprod(c(starting_price, 1+stock_Returns))
# sanity check (yet again):
head(SPY)
prices[1:6]
windows() 
plot(prices[1:20], type='l', lwd=2, ylab="Simulated price of SPY", xlab="Days",ylim=c(350,420))

set.seed(101) #Set seed for reproducibility of the random numbers
returns <- rnorm(no_of_days, mean=daily_mean, sd=daily_std_dev) #Generate random variables
prices <- cumprod(c(starting_price, 1+returns)) #Calculate cumulative product
prices[1:6]
lines(prices, type='l')

no_of_sims <- 101
returns_per_sim <- matrix(0, nrow = no_of_sims, ncol = no_of_days) #define matrices
prices_per_sim <- matrix(0, nrow = no_of_sims, ncol = no_of_days+1) 

# loop generating 101 versions of returns and closing prices:
for(i in 1:no_of_sims) { 
  returns_per_sim[i,] <- rnorm(no_of_days, mean=daily_mean, sd=daily_std_dev) #Generate random variables
  prices_per_sim[i,] <- cumprod(c(starting_price, 1+returns_per_sim[i,]))#Calculate cumulative product
}

#Drawing a plot of 10 first simulations
for(i in 1:10) {
  lines(prices_per_sim[i, ], type = 'l', col=i)
}



#calculate total return for each 20 day simulation
total_returns <- c()
for (i in 1:no_of_sims) {
  tmp <- (prices_per_sim[i,no_of_days+1]-prices_per_sim[i, 1])/prices_per_sim[i,1]
  total_returns <- c(total_returns,tmp)
}

windows()
hist(total_returns, col="blue", freq=TRUE)

# repeating for larger number of simulations:
no_of_sims <- 10000

returns_per_sim <- matrix(0, nrow = no_of_sims, ncol = no_of_days) #define matrices
prices_per_sim <- matrix(0, nrow = no_of_sims, ncol = no_of_days+1) 
for(i in 1:no_of_sims) { 
  returns_per_sim[i,] <- rnorm(no_of_days, mean=daily_mean, sd=daily_std_dev) #Generate random variables
  prices_per_sim[i,] <- cumprod(c(starting_price, 1+returns_per_sim[i,]))#Calculate cumulative product
}

total_returns <- c()
for (i in 1:no_of_sims) {
  tmp <- (prices_per_sim[i,no_of_days+1]-prices_per_sim[i, 1])/prices_per_sim[i,1]
  total_returns <- c(total_returns,tmp)
}

quantile(total_returns)
summary(total_returns)
sd(total_returns)

#Visualizing results:
windows() 
hist(total_returns, col="blue", freq=TRUE)

boxplot(total_returns)

windows() 
hist(total_returns, col="blue", freq=FALSE)
lines(density(total_returns), lwd=2)

max <- which.max(total_returns) # Find index of maximum value
min <- which.min(total_returns) # Find index of minimum value

# Draw plot:
lb <- min(prices_per_sim[min,])
ub <- max(prices_per_sim[max,])
plot(prices_per_sim[min, ], type='l', ylab="Simulated price of SPY", xlab="Days",ylim=c(lb,ub), col="blue")
lines(prices_per_sim[max, ], type = 'l', col='red')
lines(as.numeric(SPY$SPY.Close[1:21]), col='black', lwd=2)
