# retailers' predictions of available supply using an ARCH model

library(dynlm)
library(FinTS)
library(fGarch)
library(forecast)

# read in supply file
apples_supply <- read.csv("Desktop/Senior Thesis/Supply Data/apples_supply.csv")
citrus_supply <- read.csv("Desktop/Senior Thesis/Supply Data/citrus_supply.csv")
melons_supply <- read.csv("Desktop/Senior Thesis/Supply Data/melons_supply.csv")
strawberries_supply <- read.csv("Desktop/Senior Thesis/Supply Data/strawberries_supply.csv")

supply <- strawberries_supply

# Step 1: Estimate mean equation r = beta + error
supply.mean <- dynlm(west_lbs ~ 1, data = supply)

# Step 2: Retrieve the residuals from the former model and square them
ehatsq <- ts(resid(supply.mean)^2)

# Step 3: regress squared residuals on one-lagged squared residuals
supply.arch <- dynlm(ehatsq ~ L(ehatsq), data = ehatsq)

summary(supply.arch)

supply.archTest <- ArchTest(supply$a_ne_lbs, lags = 1, demean = TRUE)
supply.archTest

x = supply$west_lbs - mean(supply$west_lbs)
pacf(x)
acf(x)

arch.fit <- garchFit(~garch(1,0), data = x / 100000, include.mean=FALSE)
summary(arch.fit)

predict(arch.fit, n.ahead = 1)

library(rugarch)

model=ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
  mean.model = list(armaOrder = c(0, 1), include.mean = TRUE),
  distribution.model = "norm"
)

modelfit=ugarchfit(spec=model,data=x / 1000000)

modelfit@fit$coef

