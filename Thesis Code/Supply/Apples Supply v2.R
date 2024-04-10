# Apples: get supply data

# updated supply data

library(tidyverse)
library(rnassqs)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(lmtest)
library(forecast)
library(aTSA)

NASS_API_KEY <- "3D4671A5-AC1D-3F88-9372-AA28F172BDE8"
nassqs_auth(key = NASS_API_KEY)

# regional divisions
west <- c("WA", "OR", "ID", "CA", "NV", "AZ")
plains <- c("MT", "WY", "UT", "CO", "ND", "SD", "NE", "KS", "IA", "MO")
southwest <- c("NM", "TX", "OK", "AR", "LA")
midwest <- c("MN", "WI", "IL", "IN", "MI", "OH")
southeast <- c("KY", "TN", "MS", "AL", "NC", "SC", "GA", "FL")
atlantic <- c("PA", "NJ", "DE", "MD", "WV", "VA")
northeast <- c("ME", "VT", "NH", "NY", "MA", "RI", "CT")

# four suppliers (for multi-sourcing model)
west <- west
p_sw <- c(plains, southwest)
mw_se <- c(midwest, southeast)
a_ne <- c(atlantic, northeast)

# two suppliers (for dual sourcing model)
westhalf <- c(west, plains, southwest)
easthalf <- c(midwest, southeast, atlantic, northeast)

# query NASS database for survey data
nass_query <- function(prod_name, reg, statisticcat) {
  reg_params <- list(commodity_desc = prod_name, 
                     unit_desc = "ACRES", 
                     domain_desc = "TOTAL", 
                     statisticcat_desc = statisticcat, 
                     state_alpha = reg)
  reg_data_raw <- nassqs(reg_params)
  
  # filter to only have survey data from 2007 to 2015
  reg_data <- filter(reg_data_raw, source_desc == "SURVEY")
  reg_data <- filter(reg_data, year >= 2007, year <= 2015)
  
  # filter to only have annual survey data
  reg_data <- filter(reg_data, freq_desc == "ANNUAL")
  reg_data <- filter(reg_data, reference_period_desc == "YEAR")
  
  # drop rows with NAs
  reg_data <- reg_data %>% drop_na(Value)
  
  return(reg_data)
}

# print the number of unique values in each column
print_unique <- function(df) {
  cols <- colnames(df)
  for (col in cols) {
    print(paste("Unique values of ", col, ":"))
    print(unique(df[,col]))
  }
}

# create dataframe to keep track of region-level harvests
apples_harvests <- data.frame(matrix(NA, nrow=9, ncol=1))
colnames(apples_harvests) <- "year"
apples_harvests$year <- 2007:2015

## Query Data for West Region

raw_df <- nass_query("Apples", west, "AREA BEARING")
print_unique(raw_df)
raw_df <- select(raw_df, state_alpha, year, Value)

#raw_df <- rbind(raw_df, list('AZ', 2015, 1000))

# aggregate data by state
full_df <- data.frame(matrix(NA, nrow=0, ncol=3))
colnames(full_df) <- colnames(raw_df)
state_list <- unique(raw_df$state_alpha)
# state_list <- state_list[state_list != "AZ"]
for (curr_state in state_list) {
  # get dataframe for current state
  curr_state_df <- filter(raw_df, state_alpha == curr_state)
  curr_state_df$year <- as.numeric(curr_state_df$year)
  curr_state_df$Value <- as.numeric(curr_state_df$Value)
  curr_state_df <- arrange(curr_state_df, year)
  
  full_df <- rbind(full_df, curr_state_df)
}
tmp <- full_df %>% group_by(year) %>% summarise(total = sum(Value))
apples_harvests$west_harvested_acres <- tmp$total

## Query data for Plains-Southwest

raw_df <- nass_query("Apples", p_sw, "AREA BEARING")
print_unique(raw_df)
raw_df <- select(raw_df, state_alpha, year, Value)

# raw_df <- rbind(raw_df, list('CO', 2015, 1200))

# aggregate data by state
full_df <- data.frame(matrix(NA, nrow=0, ncol=3))
colnames(full_df) <- colnames(raw_df)
state_list <- unique(raw_df$state_alpha)
# state_list <- state_list[state_list != "CO"]
for (curr_state in state_list) {
  # get dataframe for current state
  curr_state_df <- filter(raw_df, state_alpha == curr_state)
  curr_state_df$year <- as.numeric(curr_state_df$year)
  curr_state_df$Value <- as.numeric(curr_state_df$Value)
  curr_state_df <- arrange(curr_state_df, year)
  
  full_df <- rbind(full_df, curr_state_df)
}
tmp <- full_df %>% group_by(year) %>% summarise(total = sum(Value))
apples_harvests$p_sw_harvested_acres <- tmp$total

## Query data for Midwest-Southeast

raw_df <- nass_query("Apples", mw_se, "AREA BEARING")
print_unique(raw_df)
raw_df <- select(raw_df, state_alpha, year, Value)

# aggregate data by state
full_df <- data.frame(matrix(NA, nrow=0, ncol=3))
colnames(full_df) <- colnames(raw_df)
state_list <- unique(raw_df$state_alpha)
# only a couple data points for GA and KY and quantity of 
# harvests is negligble --> remove from dataset
state_list <- setdiff(state_list, c("GA", "KY"))

for (curr_state in state_list) {
  # get dataframe for current state
  curr_state_df <- filter(raw_df, state_alpha == curr_state)
  curr_state_df$year <- as.numeric(curr_state_df$year)
  curr_state_df$Value <- as.numeric(curr_state_df$Value)
  curr_state_df <- arrange(curr_state_df, year)
  
  full_df <- rbind(full_df, curr_state_df)
}
tmp <- full_df %>% group_by(year) %>% summarise(total = sum(Value))
apples_harvests$mw_se_harvested_acres <- tmp$total

## Query data for the Atlantic-Northeast

raw_df <- nass_query("Apples", a_ne, "AREA BEARING")
print_unique(raw_df)
raw_df <- select(raw_df, state_alpha, year, Value)

# aggregate data by state
full_df <- data.frame(matrix(NA, nrow=0, ncol=3))
colnames(full_df) <- colnames(raw_df)
state_list <- unique(raw_df$state_alpha)
for (curr_state in state_list) {
  # get dataframe for current state
  curr_state_df <- filter(raw_df, state_alpha == curr_state)
  curr_state_df$year <- as.numeric(curr_state_df$year)
  curr_state_df$Value <- as.numeric(curr_state_df$Value)
  curr_state_df <- arrange(curr_state_df, year)
  
  full_df <- rbind(full_df, curr_state_df)
}
tmp <- full_df %>% group_by(year) %>% summarise(total = sum(Value))
apples_harvests$a_ne_harvested_acres <- tmp$total

# fit ARMA models to extend data back to 1998

all_harvests <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/Apples_Harvests.csv")
all_harvests$acres <- all_harvests$acres * 1000

frac_harvests <- data.frame(matrix(NA, nrow=nrow(apples_harvests), ncol=ncol(apples_harvests)))
colnames(frac_harvests) <- colnames(apples_harvests)
frac_harvests$year <- apples_harvests$year

for (i in 2:ncol(frac_harvests)) {
  frac_harvests[,i] <- apples_harvests[,i] / rowSums(apples_harvests[,2:ncol(apples_harvests)])
}

# data frame to store extended dataset
ext_start_yr <- 1998
act_start_yr <- 2007
end_yr <- 2015
num_ext_back <- act_start_yr - ext_start_yr

full_frac_harvests <- data.frame(matrix(nrow=end_yr-ext_start_yr+1, ncol=ncol(frac_harvests)))
colnames(full_frac_harvests) <- colnames(frac_harvests)
full_frac_harvests$year <- ext_start_yr:end_yr
full_frac_harvests[10:18,2:ncol(full_frac_harvests)] <- frac_harvests[,2:ncol(frac_harvests)]

# ADF test for stationarity
adf.test(frac_harvests$west_harvested_acres)
adf.test(frac_harvests$p_sw_harvested_acres)
adf.test(frac_harvests$mw_se_harvested_acres)
adf.test(frac_harvests$a_ne_harvested_acres)

# do not have stationarity for west and atlantic-northeast
# so we detrend data for these regions before applying the ARMA model

# apply ARMA model for the west region
# detrend data
west_harvests <- select(frac_harvests, year, west_harvested_acres)
lm_model <- lm(west_harvested_acres ~ year, data=west_harvests)
new_data <- data.frame(year=west_harvests$year)
predictions <- predict(lm_model, new_data)
west_harvests$predict <- predictions
west_harvests$detrend <- west_harvests$west_harvested_acres - predictions

# fit ARMA model
adf.test(west_harvests$detrend)
pacf(west_harvests$detrend)
acf(west_harvests$detrend)
ts_back <- ts(rev(west_harvests$detrend), start=c(1,1), end=c(end_yr-act_start_yr+1,1), frequency=1)
arima_back <- arima(ts_back, c(1,0,0), include.mean = FALSE)
print(coeftest(arima_back))

# make predictions based on ARMA model
predict_back <- predict(arima_back, num_ext_back)
new_data <- data.frame(year=full_frac_harvests[1:9,]$year)
full_frac_harvests[1:9,]$west_harvested_acres <- rev(predict_back$pred) + predict(lm_model, new_data)

# apply ARMA model for the atlantic-northeast region
# detrend data
a_ne_harvests <- select(frac_harvests, year, a_ne_harvested_acres)
lm_model <- lm(a_ne_harvested_acres ~ year, data=a_ne_harvests)
new_data <- data.frame(year=a_ne_harvests$year)
predictions <- predict(lm_model, new_data)
a_ne_harvests$predict <- predictions
a_ne_harvests$detrend <- a_ne_harvests$a_ne_harvested_acres - predictions

# fit ARMA model
adf.test(a_ne_harvests$detrend)
pacf(a_ne_harvests$detrend)
acf(a_ne_harvests$detrend)
ts_back <- ts(rev(a_ne_harvests$detrend), start=c(1,1), end=c(end_yr-act_start_yr+1,1), frequency=1)
arima_back <- arima(ts_back, c(0,0,1), include.mean = FALSE)
print(coeftest(arima_back))

# make predictions based on ARMA model
predict_back <- predict(arima_back, num_ext_back)
new_data <- data.frame(year=full_frac_harvests[1:9,]$year)
full_frac_harvests[1:9,]$a_ne_harvested_acres <- rev(predict_back$pred) + predict(lm_model, new_data)

# apply ARMA model for the plains-southwest region
# fit ARMA model
p_sw_harvests <- select(frac_harvests, year, p_sw_harvested_acres)
pacf(p_sw_harvests$p_sw_harvested_acres)
acf(p_sw_harvests$p_sw_harvested_acres)
ts_back <- ts(rev(p_sw_harvests$p_sw_harvested_acres), start=c(1,1), end=c(end_yr-act_start_yr+1,1), frequency=1)
arima_back <- arima(ts_back, c(1,0,0))
print(coeftest(arima_back))
predict_back <- predict(arima_back, num_ext_back)
full_frac_harvests[1:9,]$p_sw_harvested_acres <- rev(predict_back$pred)

# apply ARMA model for the midwest-southeast region
# fit ARMA model
mw_se_harvests <- select(frac_harvests, year, mw_se_harvested_acres)
pacf(mw_se_harvests$mw_se_harvested_acres)
acf(mw_se_harvests$mw_se_harvested_acres)
ts_back <- ts(rev(mw_se_harvests$mw_se_harvested_acres), start=c(1,1), end=c(end_yr-act_start_yr+1,1), frequency=1)
arima_back <- arima(ts_back, c(0,0,1))
print(coeftest(arima_back))
predict_back <- predict(arima_back, num_ext_back)
full_frac_harvests[1:9,]$mw_se_harvested_acres <- rev(predict_back$pred)

# save extended dataset
full_apples_harvests <- data.frame(matrix(NA, nrow=nrow(full_frac_harvests), ncol=ncol(full_frac_harvests)))
colnames(full_apples_harvests) <- colnames(full_frac_harvests)
full_apples_harvests$year <- full_frac_harvests$year

for (i in 2:ncol(full_apples_harvests)) {
  full_apples_harvests[,i] <- full_frac_harvests[,i] * all_harvests$acres[9:26]
}

write.csv(full_apples_harvests, "/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Harvests/pred_apples_harvests.csv", row.names=FALSE)


# ADF test for stationarity
# pacf and acf tests for orders of ARMA models

#adf.test(frac_harvests$west_harvested_acres)
#acf(frac_harvests$west_harvested_acres)
#pacf(frac_harvests$west_harvested_acres)

#adf.test(frac_harvests$p_sw_harvested_acres)
#acf(frac_harvests$p_sw_harvested_acres)
#pacf(frac_harvests$p_sw_harvested_acres)

#adf.test(frac_harvests$mw_se_harvested_acres)
#acf(frac_harvests$mw_se_harvested_acres)
#pacf(frac_harvests$mw_se_harvested_acres)

#adf.test(frac_harvests$a_ne_harvested_acres)
#acf(frac_harvests$a_ne_harvested_acres)
#pacf(frac_harvests$a_ne_harvested_acres)

#ext_start_yr <- 1998
#act_start_yr <- 2007
#end_yr <- 2015
#num_ext_back <- act_start_yr - ext_start_yr

#full_frac_harvests <- data.frame(matrix(nrow=end_yr-ext_start_yr+1, ncol=ncol(frac_harvests)))
#colnames(full_frac_harvests) <- colnames(frac_harvests)
#full_frac_harvests$year <- ext_start_yr:end_yr
#full_frac_harvests[10:18,2:ncol(full_frac_harvests)] <- frac_harvests[,2:ncol(frac_harvests)]

#for (i in 2:ncol(full_frac_harvests)) {
#  ts_back <- ts(rev(frac_harvests[,i]), start=c(1,1), end=c(end_yr-act_start_yr+1,1), frequency=1)
#  arima_back <- auto.arima(ts_back, max.d=0, max.D=0)
#  print(coeftest(arima_back))
#  predict_back <- predict(arima_back, num_ext_back)
#  full_frac_harvests[1:9,i] <- rev(predict_back$pred)
#}

#full_apples_harvests <- data.frame(matrix(NA, nrow=nrow(full_frac_harvests), ncol=ncol(full_frac_harvests)))
#colnames(full_apples_harvests) <- colnames(full_frac_harvests)
#full_apples_harvests$year <- full_frac_harvests$year

#for (i in 2:ncol(full_apples_harvests)) {
#  full_apples_harvests[,i] <- full_frac_harvests[,i] * all_harvests$acres[9:26]
#}

#write.csv(full_apples_harvests, "/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Harvests/pred_apples_harvests.csv", row.names=FALSE)

apples_harvests <- full_apples_harvests

# national harvests in pounds
prod_pounds <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/fresh_production.csv")
apples_pounds <- data.frame(matrix(NA, nrow=nrow(prod_pounds)-1, ncol=3))
colnames(apples_pounds) <- c("year", "million_pounds", "pounds")
apples_pounds$year <- as.numeric(prod_pounds[2:nrow(prod_pounds),]$year)
apples_pounds$million_pounds <- as.numeric(prod_pounds[2:nrow(prod_pounds),]$Apples)
apples_pounds$pounds <- apples_pounds$million_pounds * 10^6 # convert from million pounds to pounds
apples_pounds <- filter(apples_pounds, year >= 1998, year <= 2015)

# estimate yield
yield <- data.frame(matrix(NA, nrow=18, ncol=1))
colnames(yield) <- "year"
yield$year <- 1998:2015
yield$acres <- rowSums(apples_harvests[,2:ncol(apples_harvests)])
yield$pounds <- apples_pounds$pounds
yield$yield_est <- yield$pounds / yield$acres

# now apply yield estimates
for (i in 2:ncol(apples_harvests)) {
  apples_harvests[,i] <- apples_harvests[,i] * yield$yield_est
}
colnames(apples_harvests) <- c("year", "west_lbs", "p_sw_lbs", "mw_se_lbs", "a_ne_lbs")

# sum up columns to get estimates for the eastern & western halves of the U.S. (for the dual sourcing scenario)
apples_harvests$west_half_lbs <- apples_harvests$west_lbs + apples_harvests$p_sw_lbs
apples_harvests$east_half_lbs <- apples_harvests$mw_se_lbs + apples_harvests$a_ne_lbs

write.csv(apples_harvests, "/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Harvests/apples_harvests.csv", row.names=FALSE)

# now adjust for imports and exports
exports <- read.csv("Desktop/Senior Thesis/all_exports.csv")
imports <- read.csv("Desktop/Senior Thesis/all_imports.csv")

apples_adj <- data.frame(matrix(NA, nrow=nrow(exports)-1, ncol=3))
colnames(apples_adj) <- c("year", "exports_lbs", "imports_lbs")
apples_adj$year <- as.numeric(exports[2:nrow(exports),]$year)
apples_adj$exports_lbs <- as.numeric(exports[2:nrow(exports),]$Apples) * 10^6
apples_adj$imports_lbs <- as.numeric(imports[2:nrow(imports),]$Apples) * 10^6
apples_adj$net_lbs <- apples_adj$imports_lbs - apples_adj$exports_lbs
apples_adj <- filter(apples_adj, year >= 1998, year <= 2015)

apples_supply <- data.frame(matrix(NA, nrow=nrow(apples_harvests), ncol=ncol(apples_harvests)))
colnames(apples_supply) <- colnames(apples_harvests)
apples_supply$year <- apples_harvests$year

# adjust by smaller region
sum_harvests <- apples_harvests$west_half_lbs + apples_harvests$east_half_lbs

apples_supply$west_lbs <- apples_harvests$west_lbs + (apples_adj$net_lbs * apples_harvests$west_lbs / sum_harvests)
apples_supply$p_sw_lbs <- apples_harvests$p_sw_lbs + (apples_adj$net_lbs * apples_harvests$p_sw_lbs / sum_harvests)
apples_supply$mw_se_lbs <- apples_harvests$mw_se_lbs + (apples_adj$net_lbs * apples_harvests$mw_se_lbs / sum_harvests)
apples_supply$a_ne_lbs <- apples_harvests$a_ne_lbs + (apples_adj$net_lbs * apples_harvests$a_ne_lbs / sum_harvests)

# adjust by half
apples_supply$west_half_lbs <- apples_harvests$west_half_lbs + (apples_adj$net_lbs * apples_harvests$west_half_lbs / sum_harvests)
apples_supply$east_half_lbs <- apples_harvests$east_half_lbs + (apples_adj$net_lbs * apples_harvests$east_half_lbs / sum_harvests)

# now apply MSA fractions
population <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/msa_population.csv")
msas <- c("Chicago", "Detroit", "Minneapolis.StPaul", "Boston", "New.York", 
          "Philadelphia", "Atlanta", "Baltimore", "Dallas.FortWorth", "Houston",
          "Miami", "DC", "Los.Angeles", "San.Francisco", "San.Diego", "Seattle")
population <- subset(population, select=c("Year", msas, "United.States"))

population['msa_sums'] <- NA
for (i in 1:nrow(population)) {
  population$msa_sums[i] <- sum(population[i,2:17])
}
population['msa_frac'] <- population$msa_sums / population$United.States

population <- filter(population, Year >= 1998, Year <= 2015)

for (i in 2:ncol(apples_harvests)) {
  apples_supply[,i] <- apples_supply[,i] * population$msa_frac
}

write.csv(apples_supply, "Desktop/Senior Thesis/Supply Data/apples_supply.csv", row.names=FALSE)




