# could maybe determine the optimal order using CV?

# read in supply file
supply <- read.csv("Desktop/Senior Thesis/Supply Data/strawberries_supply.csv")
supply <- read.csv("Desktop/Senior Thesis/Supply Data/melons_supply.csv")
supply <- read.csv("Desktop/Senior Thesis/Supply Data/apples_supply.csv")
supply <- read.csv("Desktop/Senior Thesis/Supply Data/citrus_supply.csv")

# read in demand file
demand <- read.csv("Desktop/Senior Thesis/Demand Data/strawberries_demand.csv")
demand <- read.csv("Desktop/Senior Thesis/Demand Data/melons_demand.csv")
demand <- read.csv("Desktop/Senior Thesis/Demand Data/apples_demand.csv")
demand <- read.csv("Desktop/Senior Thesis/Demand Data/citrus_demand.csv")

# now make predictions based on a polynomial regression
predictions <- data.frame(matrix(NA, nrow=nrow(supply)-1, ncol=ncol(supply)))
colnames(predictions) <- colnames(supply)
predictions$year <- supply[2:nrow(supply),]$year

# first year: predictions are simply the harvests in the zeroth year
predictions[1,2:ncol(predictions)] <- supply[1,2:ncol(supply)]

# years 2 and 3: predictions are based on a simple linear model
for (curr_year in 2:3) {
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

# year 4 and after: predictions are based on polynomial regression of previous harvests
for (curr_year in 4:nrow(predictions)) {
  known_harvests <- supply[1:curr_year,]
  
  for (i in 2:ncol(predictions)) {
    curr_known_harvests <- cbind(known_harvests$year, known_harvests[,i])
    curr_known_harvests <- as.data.frame(curr_known_harvests)
    colnames(curr_known_harvests) <- c("year", "value")
    
    lm_model <- lm(value ~ year + poly(year, degree=3), data=curr_known_harvests)
    new_data <- data.frame(year=predictions$year[curr_year])
    predictions[curr_year,i] <- predict(lm_model, new_data)
  }
}

diff <- supply[2:nrow(supply),] - predictions

supply[1:nrow(supply),]$west_half_lbs + supply[1:nrow(supply),]$east_half_lbs - (16 * demand[9:26,]$Average)

write.csv(predictions, "Desktop/Senior Thesis/Polynomial Predicted Supply/citrus_poly_predict.csv", row.names=FALSE)
