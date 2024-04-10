# Melons: get supply data

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
melons_harvests <- data.frame(matrix(NA, nrow=18, ncol=1))
colnames(melons_harvests) <- "year"
melons_harvests$year <- 1998:2015

## Query Data for West Region

raw_df <- nass_query("Melons", west, "AREA HARVESTED")
print_unique(raw_df)
raw_df <- select(raw_df, state_alpha, class_desc, year, Value)

# aggregate data by state
full_df <- data.frame(matrix(NA, nrow=0, ncol=4))
colnames(full_df) <- colnames(raw_df)

state_list <- unique(raw_df$state_alpha)
for (curr_state in state_list) {
  curr_state_df <- filter(raw_df, state_alpha == curr_state)
  curr_state_df$year <- as.numeric(curr_state_df$year)
  curr_state_df$Value <- as.numeric(curr_state_df$Value)
  
  cantaloup <- filter(curr_state_df, class_desc=="CANTALOUP")
  cantaloup <- arrange(cantaloup, year)
  full_df <- rbind(full_df, cantaloup)
  
  honeydew <- filter(curr_state_df, class_desc=="HONEYDEW")
  honeydew <- arrange(honeydew, year)
  full_df <- rbind(full_df, honeydew)
  
  watermelon <- filter(curr_state_df, class_desc=="WATERMELON")
  watermelon <- arrange(watermelon, year)
  full_df <- rbind(full_df, watermelon)
}

tmp <- full_df %>% group_by(year) %>% summarise(total = sum(Value))
melons_harvests$west_harvested_acres <- tmp$total

## Query data for Plains-Southwest

raw_df <- nass_query("Melons", p_sw, "AREA HARVESTED")
print_unique(raw_df)
raw_df <- select(raw_df, state_alpha, class_desc, year, Value)

# aggregate data by state
full_df <- data.frame(matrix(NA, nrow=0, ncol=4))
colnames(full_df) <- colnames(raw_df)

state_list <- unique(raw_df$state_alpha)
for (curr_state in state_list) {
  curr_state_df <- filter(raw_df, state_alpha == curr_state)
  curr_state_df$year <- as.numeric(curr_state_df$year)
  curr_state_df$Value <- as.numeric(curr_state_df$Value)
  
  cantaloup <- filter(curr_state_df, class_desc=="CANTALOUP")
  cantaloup <- arrange(cantaloup, year)
  full_df <- rbind(full_df, cantaloup)
  
  honeydew <- filter(curr_state_df, class_desc=="HONEYDEW")
  honeydew <- arrange(honeydew, year)
  full_df <- rbind(full_df, honeydew)
  
  watermelon <- filter(curr_state_df, class_desc=="WATERMELON")
  watermelon <- arrange(watermelon, year)
  full_df <- rbind(full_df, watermelon)
}

tmp <- full_df %>% group_by(year) %>% summarise(total = sum(Value))
melons_harvests$p_sw_harvested_acres <- tmp$total

## Query data for Midwest-Southeast

raw_df <- nass_query("Melons", mw_se, "AREA HARVESTED")
print_unique(raw_df)
raw_df <- select(raw_df, state_alpha, class_desc, year, Value)

# aggregate data by state
full_df <- data.frame(matrix(NA, nrow=0, ncol=4))
colnames(full_df) <- colnames(raw_df)

state_list <- unique(raw_df$state_alpha)
# only have a couple data points for OH and MI and quantity
# of harvests is negligible --> remove from dataset 
state_list <- setdiff(state_list, c("OH", "MI"))

for (curr_state in state_list) {
  curr_state_df <- filter(raw_df, state_alpha == curr_state)
  curr_state_df$year <- as.numeric(curr_state_df$year)
  curr_state_df$Value <- as.numeric(curr_state_df$Value)
  
  # no cantaloupe harvests in SC
  if (curr_state != "SC") {
    cantaloup <- filter(curr_state_df, class_desc=="CANTALOUP")
    cantaloup <- arrange(cantaloup, year)
    full_df <- rbind(full_df, cantaloup)
  }
  
  honeydew <- filter(curr_state_df, class_desc=="HONEYDEW")
  honeydew <- arrange(honeydew, year)
  full_df <- rbind(full_df, honeydew)
  
  watermelon <- filter(curr_state_df, class_desc=="WATERMELON")
  watermelon <- arrange(watermelon, year)
  full_df <- rbind(full_df, watermelon)
}

tmp <- full_df %>% group_by(year) %>% summarise(total = sum(Value))
melons_harvests$mw_se_harvested_acres <- tmp$total

## Query data for the Atlantic-Northeast

raw_df <- nass_query("Melons", a_ne, "AREA HARVESTED")
print_unique(raw_df)
raw_df <- select(raw_df, state_alpha, class_desc, year, Value)

# aggregate data by state
full_df <- data.frame(matrix(NA, nrow=0, ncol=4))
colnames(full_df) <- colnames(raw_df)

state_list <- unique(raw_df$state_alpha)
# only have a couple data points for VA and quantity
# of harvests is negligible --> remove from dataset 
state_list <- state_list[state_list != "VA"]
for (curr_state in state_list) {
  curr_state_df <- filter(raw_df, state_alpha == curr_state)
  curr_state_df$year <- as.numeric(curr_state_df$year)
  curr_state_df$Value <- as.numeric(curr_state_df$Value)
  
  # no cantaloupe harvests in DE
  if (curr_state != "DE") {
    cantaloup <- filter(curr_state_df, class_desc=="CANTALOUP")
    cantaloup <- arrange(cantaloup, year)
    full_df <- rbind(full_df, cantaloup)
  }
  
  honeydew <- filter(curr_state_df, class_desc=="HONEYDEW")
  honeydew <- arrange(honeydew, year)
  full_df <- rbind(full_df, honeydew)
  
  watermelon <- filter(curr_state_df, class_desc=="WATERMELON")
  watermelon <- arrange(watermelon, year)
  full_df <- rbind(full_df, watermelon)
}

tmp <- full_df %>% group_by(year) %>% summarise(total = sum(Value))
melons_harvests$a_ne_harvested_acres <- tmp$total

# now apply yield rates to get pounds harvested

# national harvests in pounds
prod_pounds <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/fresh_production.csv")
melons_pounds <- data.frame(matrix(NA, nrow=nrow(prod_pounds)-1, ncol=7))
colnames(melons_pounds) <- c("year", "cantaloupe_million_lbs", "honeydew_million_lbs", 
                             "watermelon_million_lbs", "cantaloupe_pounds", "honeydew_pounds", "watermelon_pounds")
melons_pounds$year <- as.numeric(prod_pounds[2:nrow(prod_pounds),]$year)
melons_pounds$cantaloupe_million_lbs <- as.numeric(prod_pounds[2:nrow(prod_pounds),]$Cantaloupe)
melons_pounds$honeydew_million_lbs <- as.numeric(prod_pounds[2:nrow(prod_pounds),]$Honeydew)
melons_pounds$watermelon_million_lbs <- as.numeric(prod_pounds[2:nrow(prod_pounds),]$Watermelon)
melons_pounds$cantaloupe_pounds <- melons_pounds$cantaloupe_million_lbs * 10^6 # convert from million pounds to pounds
melons_pounds$honeydew_pounds <- melons_pounds$honeydew_million_lbs * 10^6 # convert from million pounds to pounds
melons_pounds$watermelon_pounds <- melons_pounds$watermelon_million_lbs * 10^6 # convert from million pounds to pounds
melons_pounds <- filter(melons_pounds, year >= 1998, year <= 2015)
melons_pounds$all_pounds <- rowSums(melons_pounds[,c("cantaloupe_pounds", "honeydew_pounds", "watermelon_pounds")])

# estimate yield
yield <- data.frame(matrix(NA, nrow=18, ncol=1))
colnames(yield) <- "year"
yield$year <- 1998:2015
yield$acres <- rowSums(melons_harvests[,2:ncol(melons_harvests)])
yield$pounds <- melons_pounds$all_pounds
yield$yield_est <- yield$pounds / yield$acres

# now apply yield estimates
for (i in 2:ncol(melons_harvests)) {
  melons_harvests[,i] <- melons_harvests[,i] * yield$yield_est
}
colnames(melons_harvests) <- c("year", "west_lbs", "p_sw_lbs", "mw_se_lbs", "a_ne_lbs")

# sum up columns to get estimates for the eastern & western halves of the U.S. (for the dual sourcing scenario)
melons_harvests$west_half_lbs <- melons_harvests$west_lbs + melons_harvests$p_sw_lbs
melons_harvests$east_half_lbs <- melons_harvests$mw_se_lbs + melons_harvests$a_ne_lbs

write.csv(melons_harvests, "/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Harvests/melons_harvests.csv", row.names=FALSE)

# now adjust for imports and exports
exports <- read.csv("Desktop/Senior Thesis/all_exports.csv")
imports <- read.csv("Desktop/Senior Thesis/all_imports.csv")

melons_adj <- data.frame(matrix(NA, nrow=nrow(exports)-1, ncol=3))
colnames(melons_adj) <- c("year", "exports_lbs", "imports_lbs")
melons_adj$year <- as.numeric(exports[2:nrow(exports),]$year)
melons_adj$exports_lbs <- as.numeric(exports[2:nrow(exports),]$Cantaloupe) * 10^6 + as.numeric(exports[2:nrow(exports),]$Honeydew) * 10^6 + as.numeric(exports[2:nrow(exports),]$Watermelon) * 10^6
melons_adj$imports_lbs <- as.numeric(imports[2:nrow(imports),]$Cantaloupe) * 10^6 + as.numeric(imports[2:nrow(imports),]$Honeydew) * 10^6 + as.numeric(imports[2:nrow(imports),]$Watermelon) * 10^6
melons_adj$net_lbs <- melons_adj$imports_lbs - melons_adj$exports_lbs
melons_adj <- filter(melons_adj, year >= 1998, year <= 2015)

melons_supply <- data.frame(matrix(NA, nrow=nrow(melons_harvests), ncol=ncol(melons_harvests)))
colnames(melons_supply) <- colnames(melons_harvests)
melons_supply$year <- melons_harvests$year

# adjust by smaller region
sum_harvests <- melons_harvests$west_half_lbs + melons_harvests$east_half_lbs

melons_supply$west_lbs <- melons_harvests$west_lbs + (melons_adj$net_lbs * melons_harvests$west_lbs / sum_harvests)
melons_supply$p_sw_lbs <- melons_harvests$p_sw_lbs + (melons_adj$net_lbs * melons_harvests$p_sw_lbs / sum_harvests)
melons_supply$mw_se_lbs <- melons_harvests$mw_se_lbs + (melons_adj$net_lbs * melons_harvests$mw_se_lbs / sum_harvests)
melons_supply$a_ne_lbs <- melons_harvests$a_ne_lbs + (melons_adj$net_lbs * melons_harvests$a_ne_lbs / sum_harvests)

# adjust by half
melons_supply$west_half_lbs <- melons_harvests$west_half_lbs + (melons_adj$net_lbs * melons_harvests$west_half_lbs / sum_harvests)
melons_supply$east_half_lbs <- melons_harvests$east_half_lbs + (melons_adj$net_lbs * melons_harvests$east_half_lbs / sum_harvests)

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

for (i in 2:ncol(melons_harvests)) {
  melons_supply[,i] <- melons_supply[,i] * population$msa_frac
}

write.csv(melons_supply, "Desktop/Senior Thesis/Supply Data/melons_supply.csv", row.names=FALSE)












