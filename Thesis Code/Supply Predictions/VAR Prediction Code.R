# retailers' predictions of available supply using a VAR model

library(vars)
library(lmtest)
library(forecast)
library(aTSA)
library(dplyr)

# read in supply file
strawberries_supply <- read.csv("Desktop/Senior Thesis/Supply Data/strawberries_supply.csv")
melons_supply <- read.csv("Desktop/Senior Thesis/Supply Data/melons_supply.csv")
apples_supply <- read.csv("Desktop/Senior Thesis/Supply Data/apples_supply.csv")
citrus_supply <- read.csv("Desktop/Senior Thesis/Supply Data/citrus_supply.csv")

# read in demand file
# strawberries_demand <- read.csv("Desktop/Senior Thesis/Demand Data/strawberries_demand.csv")
# melons_demand <- read.csv("Desktop/Senior Thesis/Demand Data/melons_demand.csv")
# apples_demand <- read.csv("Desktop/Senior Thesis/Demand Data/apples_demand.csv")
# citrus_demand <- read.csv("Desktop/Senior Thesis/Demand Data/citrus_demand.csv")

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

apples_var_predictions <- start_linear_predictions(apples_supply)
citrus_var_predictions <- start_linear_predictions(citrus_supply)
melons_var_predictions <- start_linear_predictions(melons_supply)
strawberries_var_predictions <- start_linear_predictions(strawberries_supply)

# starting in the 6th prediction: fit VAR models manually for each product
# select the current year from 6 to 17
# (change the code manually by fruit and year indices)
curr_supply <- apples_supply
curr_predictions <- apples_var_predictions
curr_year <- 12

known_harvests <- curr_supply[1:curr_year,]

# adf test for stationarity
adf.test(known_harvests$west_lbs)
#adf.test(known_harvests$p_sw_lbs)
adf.test(known_harvests$mw_se_lbs)
adf.test(known_harvests$a_ne_lbs)
adf.test(known_harvests$west_half_lbs)
adf.test(known_harvests$east_half_lbs)

# detrend data then fit ARMA model

# detrend west region
west_data <- select(known_harvests, year, west_lbs)
west_lm_model <- lm(west_lbs ~ year, data=west_data)
new_data <- data.frame(year=west_data$year[1:curr_year])
lm_model <- lm(west_lbs ~ year, data=west_data)
new_data <- data.frame(year=west_data$year[1:curr_year])
lm_predict <- predict(lm_model, new_data)
west_data$predict <- lm_predict
west_data$detrend <- west_data$west_lbs - west_data$predict
adf.test(west_data$detrend) # test again for stationarity

# detrend p_sw region
p_sw_data <- select(known_harvests, year, p_sw_lbs)
p_sw_lm_model <- lm(p_sw_lbs ~ year, data=p_sw_data)
new_data <- data.frame(year=p_sw_data$year[1:curr_year])
lm_model <- lm(p_sw_lbs ~ year, data=p_sw_data)
new_data <- data.frame(year=p_sw_data$year[1:curr_year])
lm_predict <- predict(lm_model, new_data)
p_sw_data$predict <- lm_predict
p_sw_data$detrend <- p_sw_data$p_sw_lbs - p_sw_data$predict
adf.test(p_sw_data$detrend) # test again for stationarity

# detrend mw_se region
mw_se_data <- select(known_harvests, year, mw_se_lbs)
mw_se_lm_model <- lm(mw_se_lbs ~ year, data=mw_se_data)
new_data <- data.frame(year=mw_se_data$year[1:curr_year])
lm_model <- lm(mw_se_lbs ~ year, data=mw_se_data)
new_data <- data.frame(year=mw_se_data$year[1:curr_year])
lm_predict <- predict(lm_model, new_data)
mw_se_data$predict <- lm_predict
mw_se_data$detrend <- mw_se_data$mw_se_lbs - mw_se_data$predict
adf.test(mw_se_data$detrend) # test again for stationarity

# detrend a_ne region
a_ne_data <- select(known_harvests, year, a_ne_lbs)
a_ne_lm_model <- lm(a_ne_lbs ~ year, data=a_ne_data)
new_data <- data.frame(year=a_ne_data$year[1:curr_year])
lm_model <- lm(a_ne_lbs ~ year, data=a_ne_data)
new_data <- data.frame(year=a_ne_data$year[1:curr_year])
lm_predict <- predict(lm_model, new_data)
a_ne_data$predict <- lm_predict
a_ne_data$detrend <- a_ne_data$a_ne_lbs - a_ne_data$predict
adf.test(a_ne_data$detrend) # test again for stationarity

# detrend west_half region
west_half_data <- select(known_harvests, year, west_half_lbs)
west_half_lm_model <- lm(west_half_lbs ~ year, data=west_half_data)
new_data <- data.frame(year=west_half_data$year[1:curr_year])
lm_model <- lm(west_half_lbs ~ year, data=west_half_data)
new_data <- data.frame(year=west_half_data$year[1:curr_year])
lm_predict <- predict(lm_model, new_data)
west_half_data$predict <- lm_predict
west_half_data$detrend <- west_half_data$west_half_lbs - west_half_data$predict
adf.test(west_half_data$detrend) # test again for stationarity

# detrend east_half region
east_half_data <- select(known_harvests, year, east_half_lbs)
east_half_lm_model <- lm(east_half_lbs ~ year, data=east_half_data)
new_data <- data.frame(year=east_half_data$year[1:curr_year])
lm_model <- lm(east_half_lbs ~ year, data=east_half_data)
new_data <- data.frame(year=east_half_data$year[1:curr_year])
lm_predict <- predict(lm_model, new_data)
east_half_data$predict <- lm_predict
east_half_data$detrend <- east_half_data$east_half_lbs - east_half_data$predict
adf.test(east_half_data$detrend) # test again for stationarity

known_harvests_detrend <- data.frame(matrix(NA, nrow=curr_year, ncol=7))
colnames(known_harvests_detrend) <- colnames(known_harvests)
known_harvests_detrend$year <- known_harvests$year
known_harvests_detrend$west_lbs <- west_data$detrend
known_harvests_detrend$p_sw_lbs <- p_sw_data$detrend
known_harvests_detrend$mw_se_lbs <- mw_se_data$detrend
known_harvests_detrend$a_ne_lbs <- a_ne_data$detrend
known_harvests_detrend$west_half_lbs <- west_half_data$detrend
known_harvests_detrend$east_half_lbs <- east_half_data$detrend

# multi-sourcing level predictions
multi_sourcing_detrend <- known_harvests_detrend[,2:5]
# multi_sourcing_detrend <- known_harvests_detrend[,c(2,4:5)]
VARselect(multi_sourcing_detrend, lag.max=3, type="const") # select order of VAR model
var_model <- VAR(multi_sourcing_detrend, p=2, type="const")
var_pred <- predict(var_model, n.ahead=1)

new_data <- data.frame(year=curr_supply$year[curr_year]+1)
curr_predictions$west_lbs[curr_year] <- var_pred$fcst$west_lbs[1] + predict(west_lm_model, new_data)
curr_predictions$p_sw_lbs[curr_year] <- var_pred$fcst$p_sw_lbs[1] + predict(p_sw_lm_model, new_data)
curr_predictions$mw_se_lbs[curr_year] <- var_pred$fcst$mw_se_lbs[1] + predict(mw_se_lm_model, new_data)
curr_predictions$a_ne_lbs[curr_year] <- var_pred$fcst$a_ne_lbs[1] + predict(a_ne_lm_model, new_data)

# dual sourcing level predictions
dual_sourcing_detrend <- known_harvests_detrend[,6:7]
VARselect(dual_sourcing_detrend, lag.max=3, type="const")
var_model <- VAR(dual_sourcing_detrend, p=3, type="const")
var_pred <- predict(var_model, n.ahead=1)

new_data <- data.frame(year=curr_supply$year[curr_year]+1)
curr_predictions$west_half_lbs[curr_year] <- var_pred$fcst$west_half_lbs[1] + predict(west_half_lm_model, new_data)
curr_predictions$east_half_lbs[curr_year] <- var_pred$fcst$east_half_lbs[1] + predict(east_half_lm_model, new_data)

# once finished, store curr_predictions in the data frame for the chosen product
strawberries_var_predictions <- curr_predictions

# save files
write.csv(apples_var_predictions, "Desktop/Senior Thesis/VAR Predicted Supply/apples_var_predict.csv", row.names=FALSE)
write.csv(citrus_var_predictions, "Desktop/Senior Thesis/VAR Predicted Supply/citrus_var_predict.csv", row.names=FALSE)
write.csv(melons_var_predictions, "Desktop/Senior Thesis/VAR Predicted Supply/melons_var_predict.csv", row.names=FALSE)
write.csv(strawberries_var_predictions, "Desktop/Senior Thesis/VAR Predicted Supply/strawberries_var_predict.csv", row.names=FALSE)





# test code

curr_supply <- apples_supply
# curr_predictions <- strawberries_arma_predictions
curr_year <- 5
i <- 2

known_harvests <- curr_supply[1:curr_year,]
known_harvests <- known_harvests[,2:5]

VARselect(known_harvests, lag.max=2, type="const")
var_model <- VAR(known_harvests, p=1, type="const")

var_pred <- predict(var_model, n.ahead=1)
var_pred


# VAR by region

region <- "west_lbs"
curr_predict <- data.frame(matrix(NA, nrow=nrow(strawberries_supply)-5, ncol=5))
colnames(curr_predict) <- c("year", "strawberries", "melons", "apples", "citrus")
curr_predict$year <- strawberries_supply[6:nrow(strawberries_supply),]$year

# first 8 years: predictions are based off of supply in the 0th year
#curr_predict[1:5,]$strawberries <- strawberries_supply[1:5,region]
#curr_predict[1:5,]$melons <- melons_supply[1:5,region]
#curr_predict[1:5,]$apples <- apples_supply[1:5,region]
#curr_predict[1:5,]$citrus <- citrus_supply[1:5,region]

year <- 6

known_harvests <- data.frame(matrix(NA, nrow=year, ncol=4))
colnames(known_harvests) <- c("strawberries", "apples", "melons", "citrus")
known_harvests[1:year,]$strawberries <- strawberries_supply[1:year,region]
known_harvests[1:year,]$melons <- melons_supply[1:year,region]
known_harvests[1:year,]$apples <- apples_supply[1:year,region]
known_harvests[1:year,]$citrus <- citrus_supply[1:year,region]

#adf.test(known_harvests$strawberries)
#adf.test(known_harvests$melons)
#adf.test(known_harvests$apples)
#adf.test(known_harvests$citrus)

VARselect(known_harvests, lag.max=2, type="const")
var_model <- VAR(known_harvests, p=1, type="const")

var_pred <- predict(var_model, n.ahead=1)
var_pred

curr_predict$strawberries[year-5] <- var_pred$fcst$strawberries[1]
curr_predict$melons[year-6] <- var_pred$fcst$melons[1]
curr_predict$apples[year-6] <- var_pred$fcst$apples[1]
curr_predict$citrus[year-6] <- var_pred$fcst$citrus[1]

# east_half_pred <- curr_predict

# now organize into dataframes

predictions <- data.frame(matrix(NA, nrow=nrow(strawberries_supply)-7, ncol=ncol(strawberries_supply)))
colnames(predictions) <- colnames(strawberries_supply)
predictions$year <- strawberries_supply[8:nrow(strawberries_supply),]$year

predictions$west_lbs <- west_pred$citrus
predictions$plains_lbs <- plains_pred$citrus
predictions$southwest_lbs <- southwest_pred$citrus
predictions$midwest_lbs <- midwest_pred$citrus
predictions$southeast_lbs <- southeast_pred$citrus
predictions$atlantic_lbs <- atlantic_pred$citrus
predictions$northeast_lbs <- northeast_pred$citrus
predictions$west_half_lbs <- west_half_pred$citrus
predictions$east_half_lbs <- east_half_pred$citrus

write.csv(predictions, "Desktop/Senior Thesis/VAR Predicted Supply/citrus_var_predict.csv", row.names=FALSE)










