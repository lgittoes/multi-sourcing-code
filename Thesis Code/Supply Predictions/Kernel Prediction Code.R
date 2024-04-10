library(KernSmooth)

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

# now make predictions based on an ARMA model
predictions <- data.frame(matrix(NA, nrow=nrow(supply)-1, ncol=ncol(supply)))
colnames(predictions) <- colnames(supply)
predictions$year <- supply[2:nrow(supply),]$year

# first year: predictions are simply the harvests in the zeroth year
predictions[1,2:ncol(predictions)] <- supply[1,2:ncol(supply)]

curr_year <- 10

known_harvests <- supply[1:curr_year,]

i <- 2
curr_known_harvests <- cbind(known_harvests$year, known_harvests[,i])
curr_known_harvests <- as.data.frame(curr_known_harvests)
colnames(curr_known_harvests) <- c("year", "value")

kernel_model <- ksmooth(curr_known_harvests$year, curr_known_harvests$value, "normal", band=100*10^6)

new_data <- data.frame(year=predictions$year[curr_year])
new_data <- c(2008, 2009, 2010)
predict(kernel_model, exdat=new_data)


# Example usage
set.seed(123)
x_train <- seq(0, 10, by = 0.5)
y_train <- sin(x_train) + rnorm(length(x_train), mean = 0, sd = 0.1)

# Train the kernel regression model
kernel_reg_model <- ksmooth(x_train, y_train, kernel = "normal", bandwidth = 0.5)

# New data for prediction
new_data <- c(3, 4, 5)  # Example new data points

# Make predictions on new data
new_predictions <- predict(kernel_reg_model, new_data)$y
print(new_predictions$y)
