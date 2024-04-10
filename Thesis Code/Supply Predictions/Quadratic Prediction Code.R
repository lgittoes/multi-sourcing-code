# retailers' predictions of available supply using a quadratic regression

# read in supply files
apples_supply <- read.csv("Desktop/Senior Thesis/Supply Data/apples_supply.csv")
citrus_supply <- read.csv("Desktop/Senior Thesis/Supply Data/citrus_supply.csv")
melons_supply <- read.csv("Desktop/Senior Thesis/Supply Data/melons_supply.csv")
strawberries_supply <- read.csv("Desktop/Senior Thesis/Supply Data/strawberries_supply.csv")

# read in demand file
# apples_demand <- read.csv("Desktop/Senior Thesis/Demand Data/apples_demand.csv")
# citrus_demand <- read.csv("Desktop/Senior Thesis/Demand Data/citrus_demand.csv")
# melons_demand <- read.csv("Desktop/Senior Thesis/Demand Data/melons_demand.csv")
# strawberries_demand <- read.csv("Desktop/Senior Thesis/Demand Data/strawberries_demand.csv")

quadratic_predictions <- function(supply) {
  # now make predictions based on a polynomial regression
  predictions <- data.frame(matrix(NA, nrow=nrow(supply)-1, ncol=ncol(supply)))
  colnames(predictions) <- colnames(supply)
  predictions$year <- supply[2:nrow(supply),]$year
  
  # first year: predictions are simply the harvests in the zeroth year
  predictions[1,2:ncol(predictions)] <- supply[1,2:ncol(supply)]
  
  # second year: predictions are based on a simple linear model
  known_harvests <- supply[1:2,]
  
  for (i in 2:ncol(predictions)) {
    curr_known_harvests <- cbind(known_harvests$year, known_harvests[,i])
    curr_known_harvests <- as.data.frame(curr_known_harvests)
    colnames(curr_known_harvests) <- c("year", "value")
    
    lm_model <- lm(value ~ year, data=curr_known_harvests)
    new_data <- data.frame(year=predictions$year[2])
    predictions[2,i] <- predict(lm_model, new_data)
  }
  
  # year 3 and after: predictions are based on polynomial regression of previous harvests
  for (curr_year in 3:nrow(predictions)) {
    known_harvests <- supply[1:curr_year,]
    
    for (i in 2:ncol(predictions)) {
      curr_known_harvests <- cbind(known_harvests$year, known_harvests[,i])
      curr_known_harvests <- as.data.frame(curr_known_harvests)
      colnames(curr_known_harvests) <- c("year", "value")
      
      lm_model <- lm(value ~ year + poly(year, degree=2), data=curr_known_harvests)
      new_data <- data.frame(year=predictions$year[curr_year])
      predictions[curr_year,i] <- predict(lm_model, new_data)
    }
  }
  
  return(predictions)
}

apples_quad_predictions <- quadratic_predictions(apples_supply)
citrus_quad_predictions <- quadratic_predictions(citrus_supply)
melons_quad_predictions <- quadratic_predictions(melons_supply)
strawberries_quad_predictions <- quadratic_predictions(strawberries_supply)

# save files
write.csv(apples_quad_predictions, "Desktop/Senior Thesis/Quadratic Predicted Supply/apples_quadratic_predict.csv", row.names=FALSE)
write.csv(citrus_quad_predictions, "Desktop/Senior Thesis/Quadratic Predicted Supply/citrus_quadratic_predict.csv", row.names=FALSE)
write.csv(melons_quad_predictions, "Desktop/Senior Thesis/Quadratic Predicted Supply/melons_quadratic_predict.csv", row.names=FALSE)
write.csv(strawberries_quad_predictions, "Desktop/Senior Thesis/Quadratic Predicted Supply/strawberries_quadratic_predict.csv", row.names=FALSE)



