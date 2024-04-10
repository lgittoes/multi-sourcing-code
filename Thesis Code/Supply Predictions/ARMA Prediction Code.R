# retailers' predictions of available supply using an ARMA model

library(lmtest)
library(forecast)
library(aTSA)

# read in supply file
apples_supply <- read.csv("Desktop/Senior Thesis/Supply Data/apples_supply.csv")
citrus_supply <- read.csv("Desktop/Senior Thesis/Supply Data/citrus_supply.csv")
melons_supply <- read.csv("Desktop/Senior Thesis/Supply Data/melons_supply.csv")
strawberries_supply <- read.csv("Desktop/Senior Thesis/Supply Data/strawberries_supply.csv")

# read in demand file
# demand <- read.csv("Desktop/Senior Thesis/Demand Data/strawberries_demand.csv")
# demand <- read.csv("Desktop/Senior Thesis/Demand Data/melons_demand.csv")
# demand <- read.csv("Desktop/Senior Thesis/Demand Data/apples_demand.csv")
# demand <- read.csv("Desktop/Senior Thesis/Demand Data/citrus_demand.csv")

# first five predictions are based on a simple linear model
start_linear_predictions <- function(supply) {
  # now make predictions based on an ARMA model
  predictions <- data.frame(matrix(NA, nrow=nrow(supply)-1, ncol=ncol(supply)))
  colnames(predictions) <- colnames(supply)
  predictions$year <- supply[2:nrow(supply),]$year
  
  # first prediction: predictions are simply the harvests in the zeroth year
  predictions[1,2:ncol(predictions)] <- supply[1,2:ncol(supply)]
  
  # predictions 2 to 5: predictions are based on a simple linear model
  for (curr_year in 2:5) {
    known_harvests <- supply[1:curr_year,]
    
    for (i in 2:ncol(predictions)) {
      curr_known_harvests <- cbind(known_harvests$year, known_harvests[,i])
      curr_known_harvests <- as.data.frame(curr_known_harvests)
      colnames(curr_known_harvests) <- c("year", "value")
      
      lm_model <- lm(value ~ year, data=curr_known_harvests)
      new_data <- data.frame(year=predictions$year[curr_year])
      predictions[curr_year,i] <- predict(lm_model, new_data)
    }
  }
  
  return(predictions)
}

apples_arma_predictions <- start_linear_predictions(apples_supply)
citrus_arma_predictions <- start_linear_predictions(citrus_supply)
melons_arma_predictions <- start_linear_predictions(melons_supply)
strawberries_arma_predictions <- start_linear_predictions(strawberries_supply)

# starting in the 6th prediction: fit ARMA models manually for each product (by checking PACF and ACF plots)
# select the current year from 6 to 17 and the region index from 2 to 7
# (change the code manually by fruit and year / region indices)
curr_supply <- strawberries_supply
curr_predictions <- strawberries_arma_predictions
curr_year <- 17
i <- 7

known_harvests <- curr_supply[1:curr_year,]
curr_known_harvests <- cbind(known_harvests$year, known_harvests[,i])
curr_known_harvests <- as.data.frame(curr_known_harvests)
colnames(curr_known_harvests) <- c("year", "value")

# adf test for stationarity
adf.test(curr_known_harvests$value)

# if not stationary, detrend data then fit ARMA model
lm_model <- lm(value ~ year, data=curr_known_harvests)
new_data <- data.frame(year=curr_known_harvests$year[1:curr_year])
lm_predict <- predict(lm_model, new_data)
curr_known_harvests$predict <- lm_predict
curr_known_harvests$detrend <- curr_known_harvests$value - curr_known_harvests$predict
adf.test(curr_known_harvests$detrend) # test again for stationarity

# fit ARMA model
ts_for <- ts(curr_known_harvests$detrend, start=c(1,1), end=c(curr_year,1), frequency=1)
pacf(ts_for) # get value of p for AR
acf(ts_for) # get value of q for MA
arma_for <- arima(ts_for / 100, order=c(0,0,1))
print(coeftest(arma_for))
predict_for <- forecast(arma_for, 1)
new_data <- data.frame(year=curr_known_harvests$year[curr_year]+1)
curr_predictions[curr_year,i] <- predict_for[2] * 100  + predict(lm_model, new_data)


# if stationary, fit ARMA model directly
ts_for <- ts(curr_known_harvests$value, start=c(1,1), end=c(curr_year,1), frequency=1)
pacf(ts_for) # get value of p for AR
acf(ts_for) # get value of q for MA
arma_for <- arima(ts_for / 100, order=c(0,0,1))
print(coeftest(arma_for))
predict_for <- forecast(arma_for, 1)
curr_predictions[curr_year,i] <- predict_for[2] * 100 

# once finished, store curr_predictions in dataframe for the selected fruit
# (change the code below by fruit)
strawberries_arma_predictions <- curr_predictions

#arma_for <- arima(ts_for / 100, order=c(0,0,0))
#print(coeftest(arma_for))
#predict_for <- forecast(arma_for, 1)
#predictions[curr_year,i] <- predict_for[2] * 100

#predictions$east_half_lbs <- predictions$southeast_lbs

#diff <- supply[2:nrow(supply),] - predictions

#supply[1:nrow(supply),]$west_half_lbs + supply[1:nrow(supply),]$east_half_lbs - (16 * demand[9:26,]$Average)

# save files
write.csv(apples_arma_predictions, "Desktop/Senior Thesis/ARMA Predicted Supply/apples_arma_predict.csv", row.names=FALSE)
write.csv(citrus_arma_predictions, "Desktop/Senior Thesis/ARMA Predicted Supply/citrus_arma_predict.csv", row.names=FALSE)
write.csv(melons_arma_predictions, "Desktop/Senior Thesis/ARMA Predicted Supply/melons_arma_predict.csv", row.names=FALSE)
write.csv(strawberries_arma_predictions, "Desktop/Senior Thesis/ARMA Predicted Supply/strawberries_arma_predict.csv", row.names=FALSE)


