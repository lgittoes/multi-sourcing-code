# Strawberries: get supply data

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
  reg_data <- filter(reg_data, year >= 1998, year <= 2015)
  
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
strawberries_harvests <- data.frame(matrix(NA, nrow=18, ncol=1))
colnames(strawberries_harvests) <- "year"
strawberries_harvests$year <- 1998:2015

## Query Data for West Region

raw_df <- nass_query("STRAWBERRIES", west, "AREA HARVESTED")
print_unique(raw_df)
raw_df <- select(raw_df, state_alpha, year, Value)

# interpolate missing data points using cubic splines by state
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
strawberries_harvests$west_harvested_acres <- tmp$total

## Query data for Plains-Southwest
# no significant harvests of strawberries in this region

raw_df <- nass_query("Strawberries", p_sw, "AREA HARVESTED")
print_unique(raw_df)
raw_df <- select(raw_df, state_alpha, year, Value)
strawberries_harvests$p_sw_harvested_acres <- 0

## Query data for Midwest-Southeast

raw_df <- nass_query("Strawberries", mw_se, "AREA HARVESTED")
print_unique(raw_df)
raw_df <- select(raw_df, state_alpha, year, Value)

# interpolate missing data points using cubic splines by state
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
strawberries_harvests$mw_se_harvested_acres <- tmp$total

## Query data for the Atlantic-Northeast

raw_df <- nass_query("Strawberries", a_ne, "AREA HARVESTED")
print_unique(raw_df)
raw_df <- select(raw_df, state_alpha, year, Value)

# interpolate missing data points using cubic splines by state
full_df <- data.frame(matrix(NA, nrow=0, ncol=3))
colnames(full_df) <- colnames(raw_df)
state_list <- unique(raw_df$state_alpha)
state_list <- setdiff(state_list, c("NJ", "VA"))
for (curr_state in state_list) {
  # get dataframe for current state
  curr_state_df <- filter(raw_df, state_alpha == curr_state)
  curr_state_df$year <- as.numeric(curr_state_df$year)
  curr_state_df$Value <- as.numeric(curr_state_df$Value)
  curr_state_df <- arrange(curr_state_df, year)
  
  full_df <- rbind(full_df, curr_state_df)
}
tmp <- full_df %>% group_by(year) %>% summarise(total = sum(Value))
strawberries_harvests$a_ne_harvested_acres <- tmp$total

# now apply yield rates to get pounds harvested

# national harvests in pounds
prod_pounds <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/fresh_production.csv")
strawberries_pounds <- data.frame(matrix(NA, nrow=nrow(prod_pounds)-1, ncol=3))
colnames(strawberries_pounds) <- c("year", "thousand_cwt", "pounds")
strawberries_pounds$year <- as.numeric(prod_pounds[2:nrow(prod_pounds),]$year)
strawberries_pounds$thousand_cwt <- as.numeric(prod_pounds[2:nrow(prod_pounds),]$Strawberries)
strawberries_pounds$pounds <- strawberries_pounds$thousand_cwt * 1000 * 100 # convert from thousand cwt to pounds
strawberries_pounds <- filter(strawberries_pounds, year >= 1998, year <= 2015)

# estimate yield
yield <- data.frame(matrix(NA, nrow=18, ncol=1))
colnames(yield) <- "year"
yield$year <- 1998:2015
yield$acres <- rowSums(strawberries_harvests[,2:ncol(strawberries_harvests)])
yield$pounds <- strawberries_pounds$pounds
yield$yield_est <- yield$pounds / yield$acres

acres <- strawberries_harvests

# now apply yield estimates
for (i in 2:ncol(strawberries_harvests)) {
  strawberries_harvests[,i] <- strawberries_harvests[,i] * yield$yield_est
}
colnames(strawberries_harvests) <- c("year", "west_lbs", "p_sw_lbs", "mw_se_lbs", "a_ne_lbs")

# sum up columns to get estimates for the eastern & western halves of the U.S. (for the dual sourcing scenario)
strawberries_harvests$west_half_lbs <- strawberries_harvests$west_lbs + strawberries_harvests$p_sw_lbs
strawberries_harvests$east_half_lbs <- strawberries_harvests$mw_se_lbs + strawberries_harvests$a_ne_lbs

write.csv(strawberries_harvests, "/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Harvests/strawberries_harvests.csv", row.names=FALSE)

# now adjust for imports and exports
exports <- read.csv("Desktop/Senior Thesis/all_exports.csv")
imports <- read.csv("Desktop/Senior Thesis/all_imports.csv")

strawberries_adj <- data.frame(matrix(NA, nrow=nrow(exports)-1, ncol=3))
colnames(strawberries_adj) <- c("year", "exports_lbs", "imports_lbs")
strawberries_adj$year <- as.numeric(exports[2:nrow(exports),]$year)
strawberries_adj$exports_lbs <- as.numeric(exports[2:nrow(exports),]$Strawberries) * 10^6
strawberries_adj$imports_lbs <- as.numeric(imports[2:nrow(imports),]$Strawberries) * 10^6
strawberries_adj$net_lbs <- strawberries_adj$imports_lbs - strawberries_adj$exports_lbs
strawberries_adj <- filter(strawberries_adj, year >= 1998, year <= 2015)

strawberries_supply <- data.frame(matrix(NA, nrow=nrow(strawberries_harvests), ncol=ncol(strawberries_harvests)))
colnames(strawberries_supply) <- colnames(strawberries_harvests)
strawberries_supply$year <- strawberries_harvests$year

# adjust by smaller region
sum_harvests <- strawberries_harvests$west_half_lbs + strawberries_harvests$east_half_lbs

strawberries_supply$west_lbs <- strawberries_harvests$west_lbs + (strawberries_adj$net_lbs * strawberries_harvests$west_lbs / sum_harvests)
strawberries_supply$p_sw_lbs <- strawberries_harvests$p_sw_lbs + (strawberries_adj$net_lbs * strawberries_harvests$p_sw_lbs / sum_harvests)
strawberries_supply$mw_se_lbs <- strawberries_harvests$mw_se_lbs + (strawberries_adj$net_lbs * strawberries_harvests$mw_se_lbs / sum_harvests)
strawberries_supply$a_ne_lbs <- strawberries_harvests$a_ne_lbs + (strawberries_adj$net_lbs * strawberries_harvests$a_ne_lbs / sum_harvests)

# adjust by half
strawberries_supply$west_half_lbs <- strawberries_harvests$west_half_lbs + (strawberries_adj$net_lbs * strawberries_harvests$west_half_lbs / sum_harvests)
strawberries_supply$east_half_lbs <- strawberries_harvests$east_half_lbs + (strawberries_adj$net_lbs * strawberries_harvests$east_half_lbs / sum_harvests)

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

for (i in 2:ncol(strawberries_harvests)) {
  strawberries_supply[,i] <- strawberries_supply[,i] * population$msa_frac
}

write.csv(strawberries_supply, "Desktop/Senior Thesis/Supply Data/strawberries_supply.csv", row.names=FALSE)





