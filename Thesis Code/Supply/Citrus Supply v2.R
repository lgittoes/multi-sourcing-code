# Citrus fruits: get supply data

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
citrus_harvests <- data.frame(matrix(NA, nrow=18, ncol=1))
colnames(citrus_harvests) <- "year"
citrus_harvests$year <- 1998:2015

## Query Data for West Region

oranges_raw_df <- nass_query("ORANGES", west, "AREA BEARING")
print_unique(oranges_raw_df)
oranges_raw_df <- select(oranges_raw_df, state_alpha, class_desc, year, Value)
oranges_raw_df <- filter(oranges_raw_df, class_desc=="ALL CLASSES")
oranges_raw_df$class_desc <- "ORANGES"
oranges_raw_df$year <- as.numeric(oranges_raw_df$year)
oranges_raw_df$Value <- as.numeric(oranges_raw_df$Value)

lemons_raw_df <- nass_query("LEMONS", west, "AREA BEARING")
print_unique(lemons_raw_df)
lemons_raw_df <- select(lemons_raw_df, state_alpha, class_desc, year, Value)
lemons_raw_df$class_desc <- "LEMONS"
lemons_raw_df$year <- as.numeric(lemons_raw_df$year)
lemons_raw_df$Value <- as.numeric(lemons_raw_df$Value)

grapefruit_raw_df <- nass_query("GRAPEFRUIT", west, "AREA BEARING")
print_unique(grapefruit_raw_df)
grapefruit_raw_df <- select(grapefruit_raw_df, state_alpha, class_desc, year, Value)
grapefruit_raw_df$class_desc <- "GRAPEFRUIT"
grapefruit_raw_df$year <- as.numeric(grapefruit_raw_df$year)
grapefruit_raw_df$Value <- as.numeric(grapefruit_raw_df$Value)

# aggregate data by state
full_df <- data.frame(matrix(NA, nrow=0, ncol=4))
colnames(full_df) <- colnames(oranges_raw_df)

state_list <- unique(oranges_raw_df$state_alpha)
for (curr_state in state_list) {
  curr_state_oranges <- filter(oranges_raw_df, state_alpha==curr_state)
  curr_state_lemons <- filter(lemons_raw_df, state_alpha==curr_state)
  curr_state_grapefruit <- filter(grapefruit_raw_df, state_alpha==curr_state)
  
  if (curr_state != "AZ") {
    oranges <- filter(curr_state_oranges, class_desc == "ORANGES")
    oranges <- arrange(oranges, year)
    full_df <- rbind(full_df, oranges)
  }
  
  lemons <- filter(curr_state_lemons, class_desc == "LEMONS")
  lemons <- arrange(lemons, year)
  full_df <- rbind(full_df, lemons)
  
  if (curr_state != "AZ") {
    grapefruit <- filter(curr_state_grapefruit, class_desc == "GRAPEFRUIT")
    grapefruit <- arrange(grapefruit, year)
    full_df <- rbind(full_df, grapefruit)
  }
}
tmp <- full_df %>% group_by(year) %>% summarise(total = sum(Value))
citrus_harvests$west_harvested_acres <- tmp$total

## Query data for Plains-Southwest
# note: no significant harvests of lemons in this region

oranges_raw_df <- nass_query("ORANGES", p_sw, "AREA BEARING")
print_unique(oranges_raw_df)
oranges_raw_df <- select(oranges_raw_df, state_alpha, class_desc, year, Value)
oranges_raw_df <- filter(oranges_raw_df, class_desc=="ALL CLASSES")
oranges_raw_df$class_desc <- "ORANGES"
oranges_raw_df$year <- as.numeric(oranges_raw_df$year)
oranges_raw_df$Value <- as.numeric(oranges_raw_df$Value)

grapefruit_raw_df <- nass_query("GRAPEFRUIT", southwest, "AREA BEARING")
print_unique(grapefruit_raw_df)
grapefruit_raw_df <- select(grapefruit_raw_df, state_alpha, class_desc, year, Value)
grapefruit_raw_df$class_desc <- "GRAPEFRUIT"
grapefruit_raw_df$year <- as.numeric(grapefruit_raw_df$year)
grapefruit_raw_df$Value <- as.numeric(grapefruit_raw_df$Value)

# aggregate data by state
full_df <- data.frame(matrix(NA, nrow=0, ncol=4))
colnames(full_df) <- colnames(oranges_raw_df)

state_list <- unique(oranges_raw_df$state_alpha)
for (curr_state in state_list) {
  curr_state_oranges <- filter(oranges_raw_df, state_alpha==curr_state)
  curr_state_grapefruit <- filter(grapefruit_raw_df, state_alpha==curr_state)
  
  oranges <- filter(curr_state_oranges, class_desc == "ORANGES")
  oranges <- arrange(oranges, year)
  full_df <- rbind(full_df, oranges)
  
  grapefruit <- filter(curr_state_grapefruit, class_desc == "GRAPEFRUIT")
  grapefruit <- arrange(grapefruit, year)
  full_df <- rbind(full_df, grapefruit)
}
tmp <- full_df %>% group_by(year) %>% summarise(total = sum(Value))
citrus_harvests$p_sw_harvested_acres <- tmp$total

## Query data for Midwest-Southeast
# note: no significant harvests of lemons in this region

oranges_raw_df <- nass_query("ORANGES", mw_se, "AREA BEARING")
print_unique(oranges_raw_df)
oranges_raw_df <- select(oranges_raw_df, state_alpha, class_desc, year, Value)
oranges_raw_df <- filter(oranges_raw_df, class_desc=="ALL CLASSES")
oranges_raw_df$class_desc <- "ORANGES"
oranges_raw_df$year <- as.numeric(oranges_raw_df$year)
oranges_raw_df$Value <- as.numeric(oranges_raw_df$Value)

grapefruit_raw_df <- nass_query("GRAPEFRUIT", southeast, "AREA BEARING")
print_unique(grapefruit_raw_df)
grapefruit_raw_df <- select(grapefruit_raw_df, state_alpha, class_desc, year, Value)
grapefruit_raw_df <- filter(grapefruit_raw_df, class_desc=="ALL CLASSES")
grapefruit_raw_df$class_desc <- "GRAPEFRUIT"
grapefruit_raw_df$year <- as.numeric(grapefruit_raw_df$year)
grapefruit_raw_df$Value <- as.numeric(grapefruit_raw_df$Value)

# aggregate data by state
full_df <- data.frame(matrix(NA, nrow=0, ncol=4))
colnames(full_df) <- colnames(oranges_raw_df)

state_list <- unique(oranges_raw_df$state_alpha)
for (curr_state in state_list) {
  curr_state_oranges <- filter(oranges_raw_df, state_alpha==curr_state)
  curr_state_lemons <- filter(lemons_raw_df, state_alpha==curr_state)
  curr_state_grapefruit <- filter(grapefruit_raw_df, state_alpha==curr_state)
  
  oranges <- filter(curr_state_oranges, class_desc == "ORANGES")
  oranges <- arrange(oranges, year)
  full_df <- rbind(full_df, oranges)
  
  grapefruit <- filter(curr_state_grapefruit, class_desc == "GRAPEFRUIT")
  grapefruit <- arrange(grapefruit, year)
  full_df <- rbind(full_df, grapefruit)
}
tmp <- full_df %>% group_by(year) %>% summarise(total = sum(Value))
citrus_harvests$mw_se_harvested_acres <- tmp$total

## Query data for the Atlantic-Northeast
# no significant harvests of citrus fruits in this region
citrus_harvests$a_ne_harvested_acres <- 0

# now apply yield rates to get pounds harvested

# national harvests in pounds
prod_pounds <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/fresh_production.csv")
citrus_pounds <- data.frame(matrix(NA, nrow=nrow(prod_pounds)-1, ncol=7))
colnames(citrus_pounds) <- c("year", "oranges_thousand_tons", "lemons_thousand_tons", 
                             "grapefruit_thousand_tons", "oranges_pounds", "lemons_pounds", "grapefruit_pounds")
citrus_pounds$year <- as.numeric(prod_pounds[2:nrow(prod_pounds),]$year)
citrus_pounds$oranges_thousand_tons <- as.numeric(prod_pounds[2:nrow(prod_pounds),]$Oranges)
citrus_pounds$lemons_thousand_tons <- as.numeric(prod_pounds[2:nrow(prod_pounds),]$Lemons)
citrus_pounds$grapefruit_thousand_tons <- as.numeric(prod_pounds[2:nrow(prod_pounds),]$Grapefruit)
citrus_pounds$oranges_pounds <- citrus_pounds$oranges_thousand_tons * 1000 * 2000 # convert from million tons to pounds
citrus_pounds$lemons_pounds <- citrus_pounds$lemons_thousand_tons * 1000 * 2000 # convert from million tons to pounds
citrus_pounds$grapefruit_pounds <- citrus_pounds$grapefruit_thousand_tons * 1000 * 2000 # convert from million tons to pounds
citrus_pounds <- filter(citrus_pounds, year >= 1998, year <= 2015)
citrus_pounds$all_pounds <- rowSums(citrus_pounds[,c("oranges_pounds", "lemons_pounds", "grapefruit_pounds")])

# estimate yield
yield <- data.frame(matrix(NA, nrow=18, ncol=1))
colnames(yield) <- "year"
yield$year <- 1998:2015
yield$acres <- rowSums(citrus_harvests[,2:ncol(citrus_harvests)])
yield$pounds <- citrus_pounds$all_pounds
yield$yield_est <- yield$pounds / yield$acres

# now apply yield estimates
for (i in 2:ncol(citrus_harvests)) {
  citrus_harvests[,i] <- citrus_harvests[,i] * yield$yield_est
}
colnames(citrus_harvests) <- c("year", "west_lbs", "p_sw_lbs", "mw_se_lbs", "a_ne_lbs")

# sum up columns to get estimates for the eastern & western halves of the U.S. (for the dual sourcing scenario)
citrus_harvests$west_half_lbs <- citrus_harvests$west_lbs + citrus_harvests$p_sw_lbs
citrus_harvests$east_half_lbs <- citrus_harvests$mw_se_lbs + citrus_harvests$a_ne_lbs

write.csv(citrus_harvests, "/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Harvests/citrus_harvests.csv", row.names=FALSE)

# now adjust for imports and exports
exports <- read.csv("Desktop/Senior Thesis/all_exports.csv")
imports <- read.csv("Desktop/Senior Thesis/all_imports.csv")

citrus_adj <- data.frame(matrix(NA, nrow=nrow(exports)-1, ncol=3))
colnames(citrus_adj) <- c("year", "exports_lbs", "imports_lbs")
citrus_adj$year <- as.numeric(exports[2:nrow(exports),]$year)
citrus_adj$exports_lbs <- as.numeric(exports[2:nrow(exports),]$Oranges) * 10^6 + as.numeric(exports[2:nrow(exports),]$Lemons) * 10^6 + as.numeric(exports[2:nrow(exports),]$Grapefruit) * 10^6
citrus_adj$imports_lbs <- as.numeric(imports[2:nrow(imports),]$Oranges) * 10^6 + as.numeric(imports[2:nrow(imports),]$Lemons) * 10^6 + as.numeric(imports[2:nrow(imports),]$Grapefruit) * 10^6
citrus_adj$net_lbs <- citrus_adj$imports_lbs - citrus_adj$exports_lbs
citrus_adj <- filter(citrus_adj, year >= 1998, year <= 2015)

citrus_supply <- data.frame(matrix(NA, nrow=nrow(citrus_harvests), ncol=ncol(citrus_harvests)))
colnames(citrus_supply) <- colnames(citrus_harvests)
citrus_supply$year <- citrus_harvests$year

# adjust by smaller region
sum_harvests <- citrus_harvests$west_half_lbs + citrus_harvests$east_half_lbs

citrus_supply$west_lbs <- citrus_harvests$west_lbs + (citrus_adj$net_lbs * citrus_harvests$west_lbs / sum_harvests)
citrus_supply$p_sw_lbs <- citrus_harvests$p_sw_lbs + (citrus_adj$net_lbs * citrus_harvests$p_sw_lbs / sum_harvests)
citrus_supply$mw_se_lbs <- citrus_harvests$mw_se_lbs + (citrus_adj$net_lbs * citrus_harvests$mw_se_lbs / sum_harvests)
citrus_supply$a_ne_lbs <- citrus_harvests$a_ne_lbs + (citrus_adj$net_lbs * citrus_harvests$a_ne_lbs / sum_harvests)

# adjust by half
citrus_supply$west_half_lbs <- citrus_harvests$west_half_lbs + (citrus_adj$net_lbs * citrus_harvests$west_half_lbs / sum_harvests)
citrus_supply$east_half_lbs <- citrus_harvests$east_half_lbs + (citrus_adj$net_lbs * citrus_harvests$east_half_lbs / sum_harvests)

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

for (i in 2:ncol(citrus_harvests)) {
  citrus_supply[,i] <- citrus_supply[,i] * population$msa_frac
}

write.csv(citrus_supply, "Desktop/Senior Thesis/Supply Data/citrus_supply.csv", row.names=FALSE)




