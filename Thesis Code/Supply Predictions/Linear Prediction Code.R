# retailers' predictions of available supply using a simple linear regression

# read in supply files
apples_supply <- read.csv("Desktop/Senior Thesis/Supply Data/apples_supply.csv")
citrus_supply <- read.csv("Desktop/Senior Thesis/Supply Data/citrus_supply.csv")
melons_supply <- read.csv("Desktop/Senior Thesis/Supply Data/melons_supply.csv")
strawberries_supply <- read.csv("Desktop/Senior Thesis/Supply Data/strawberries_supply.csv")

# read in demand files
# apples_demand <- read.csv("Desktop/Senior Thesis/Demand Data/apples_demand.csv")
# citrus_demand <- read.csv("Desktop/Senior Thesis/Demand Data/citrus_demand.csv")
# melons_demand <- read.csv("Desktop/Senior Thesis/Demand Data/melons_demand.csv")
# strawberries_demand <- read.csv("Desktop/Senior Thesis/Demand Data/strawberries_demand.csv")

linear_predictions <- function(supply) {
  # now make predictions based on simple linear regression
  predictions <- data.frame(matrix(NA, nrow=nrow(supply)-1, ncol=ncol(supply)))
  colnames(predictions) <- colnames(supply)
  predictions$year <- supply[2:nrow(supply),]$year
  
  # first year: predictions are simply the harvests in the zeroth year
  predictions[1,2:ncol(predictions)] <- supply[1,2:ncol(supply)]
  
  # remaining years: predictions are based on simple linear regression of previous harvests
  for (curr_year in 2:nrow(predictions)) {
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

apples_linear_predictions <- linear_predictions(apples_supply)
citrus_linear_predictions <- linear_predictions(citrus_supply)
melons_linear_predictions <- linear_predictions(melons_supply)
strawberries_linear_predictions <- linear_predictions(strawberries_supply)

# save files
write.csv(apples_linear_predictions, "Desktop/Senior Thesis/Linear Predicted Supply/apples_linear_predict.csv", row.names=FALSE)
write.csv(citrus_linear_predictions, "Desktop/Senior Thesis/Linear Predicted Supply/citrus_linear_predict.csv", row.names=FALSE)
write.csv(melons_linear_predictions, "Desktop/Senior Thesis/Linear Predicted Supply/melons_linear_predict.csv", row.names=FALSE)
write.csv(strawberries_linear_predictions, "Desktop/Senior Thesis/Linear Predicted Supply/strawberries_linear_predict.csv", row.names=FALSE)


# diff <- supply[2:nrow(supply),] - predictions

# supply[1:nrow(supply),]$west_half_lbs + supply[1:nrow(supply),]$east_half_lbs - (16 * demand[9:26,]$Average)





