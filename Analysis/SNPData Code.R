#Load the t series package and download the SNPdata
library(tseries)
SNPdata <- get.hist.quote('^gspc',quote="Close")
summary(SNPdata)

# Calculating  the log returns, 
SNPret <- log(lag(SNPdata)) - log(SNPdata)

# Calculating volatility
SNPvol <- sd(SNPret) * sqrt(250) * 100


#Creating a user defined function called getVol to calculate volatility at different periods
getVol <- function(d, logrets) {
  var = 0
  lam = 0
  varlist <- c()
  
  for (r in logrets) {
    lam = lam*(1 - 1/d) + 1
    var = (1 - 1/lam)*var + (1/lam)*r^2
    varlist <- c(varlist, var)
  }
  
  sqrt(varlist)
}


### Volatility Plot

# Get Volatility at 10,SNPret
volest <- getVol(10,SNPret)

# Get Volatility at 30,SNPret
volest2 <- getVol(30,SNPret)

# Get Volatility at 100,SNPret
volest3 <- getVol(100,SNPret)

# Plot the results
plot(volest,type="l")

# Over lay  the values of volest2 on the same plot  
lines(volest2,type="l",col="red")

#  Over lay  the values  of volest3 on the same plot  
lines(volest3, type = "l", col="blue")
