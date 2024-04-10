library(haven)
library(plyr)
library(tidyverse)
library(forecast)
library(lmtest)

# 1 pound is about 453.6 grams
POUNDS_TO_GRAMS <- 453.6

# read in STATA files data
all_2001 <- read_dta("Desktop/Senior Thesis/STATA Files/2001.dta")
day1_2003 <- read_dta("Desktop/Senior Thesis/STATA Files/2003_day1.dta")
day2_2003 <- read_dta("Desktop/Senior Thesis/STATA Files/2003_day2.dta")
day1_2005 <- read_dta("Desktop/Senior Thesis/STATA Files/2005_day1.dta")
day2_2005 <- read_dta("Desktop/Senior Thesis/STATA Files/2005_day2.dta")
day1_2007 <- read_dta("Desktop/Senior Thesis/STATA Files/2007_day1.dta")
day2_2007 <- read_dta("Desktop/Senior Thesis/STATA Files/2007_day2.dta")
day1_2009 <- read_dta("Desktop/Senior Thesis/STATA Files/2009_day1.dta")
day2_2009 <- read_dta("Desktop/Senior Thesis/STATA Files/2009_day2.dta")
day1_2011 <- read_dta("Desktop/Senior Thesis/STATA Files/2011_day1.dta")
day2_2011 <- read_dta("Desktop/Senior Thesis/STATA Files/2011_day2.dta")
day1_2013 <- read_dta("Desktop/Senior Thesis/STATA Files/2013_day1.dta")
day2_2013 <- read_dta("Desktop/Senior Thesis/STATA Files/2013_day2.dta")
day1_2015 <- read_dta("Desktop/Senior Thesis/STATA Files/2015_day1.dta")
day2_2015 <- read_dta("Desktop/Senior Thesis/STATA Files/2015_day2.dta")
day1_2017 <- read_dta("Desktop/Senior Thesis/STATA Files/2017_day1.dta")
day2_2017 <- read_dta("Desktop/Senior Thesis/STATA Files/2017_day2.dta")
day1_2019 <- read_dta("Desktop/Senior Thesis/STATA Files/2020_day1.dta")
day2_2019 <- read_dta("Desktop/Senior Thesis/STATA Files/2020_day2.dta")

# read in WWEIA data with WWEIA codes
wweia_codes <- read_csv("/Users/liliannagittoes/Desktop/Senior Thesis/WWEIA_fresh_codes.csv")
food_list <- colnames(wweia_codes)

# read in wholesale pricing data for foods tracked by WWEIA
wholesale_prices <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/wweia_prices.csv")

# function that sets sample weight to zero for respondents that did not respond both days
both_days <- function(df, id_weight, id_code, id_grms) {
  # subset the columns that we want
  df <- select(df, seqn, all_of(id_weight), all_of(id_code), all_of(id_grms))
  
  # only include data for respondents that answered both days
  df[is.na(df[,id_weight]),id_weight] <- 0
  
  return(df)
}

# only include data for respondents that answered both days
# (this is because the sample weights are only constructed for those that answered both days)
all_2001 <- both_days(all_2001, "wtdrd1", "drdifdcd", "drxigrms" )
day1_2003 <- both_days(day1_2003, "wtdr2d", "dr1ifdcd", "dr1igrms")
day2_2003 <- both_days(day2_2003, "wtdr2d", "dr2ifdcd", "dr2igrms")
day1_2005 <- both_days(day1_2005, "wtdr2d", "dr1ifdcd", "dr1igrms")
day2_2005 <- both_days(day2_2005, "wtdr2d", "dr2ifdcd", "dr2igrms")
day1_2007 <- both_days(day1_2007, "wtdr2d", "dr1ifdcd", "dr1igrms")
day2_2007 <- both_days(day2_2007, "wtdr2d", "dr2ifdcd", "dr2igrms")
day1_2009 <- both_days(day1_2009, "wtdr2d", "dr1ifdcd", "dr1igrms")
day2_2009 <- both_days(day2_2009, "wtdr2d", "dr2ifdcd", "dr2igrms")
day1_2011 <- both_days(day1_2011, "wtdr2d", "dr1ifdcd", "dr1igrms")
day2_2011 <- both_days(day2_2011, "wtdr2d", "dr2ifdcd", "dr2igrms")
day1_2013 <- both_days(day1_2013, "wtdr2d", "dr1ifdcd", "dr1igrms")
day2_2013 <- both_days(day2_2013, "wtdr2d", "dr2ifdcd", "dr2igrms")
day1_2015 <- both_days(day1_2015, "wtdr2d", "dr1ifdcd", "dr1igrms")
day2_2015 <- both_days(day2_2015, "wtdr2d", "dr2ifdcd", "dr2igrms")
day1_2017 <- both_days(day1_2017, "wtdr2d", "dr1ifdcd", "dr1igrms")
day2_2017 <- both_days(day2_2017, "wtdr2d", "dr2ifdcd", "dr2igrms")
day1_2019 <- both_days(day1_2019, "wtdr2dpp", "dr1ifdcd", "dr1igrms")
day2_2019 <- both_days(day2_2019, "wtdr2dpp", "dr2ifdcd", "dr2igrms")

# takes in a dataframe of respondent data and classifies
# the fruits and vegetables based on WWEIA codes
classify <- function(df, id_colname) {
  df$Class <- NA
  
  for (i in 1:dim(df)[1]) {
    current_class <- colnames(wweia_codes)[grepl(df[i,id_colname], wweia_codes)]
    if (length(current_class) == 0) {
      df$Class[i] <- NA
    }
    else {
      print(i)
      df$Class[i] <- current_class
    }
  }
  
  newdf <- drop_na(df)
  return(newdf)
}

# classify the foods based on WWEIA codes
all_2001 <- classify(all_2001, "drdifdcd")
day1_2003 <- classify(day1_2003, "dr1ifdcd")
day2_2003 <- classify(day2_2003, "dr2ifdcd")
day1_2005 <- classify(day1_2005, "dr1ifdcd")
day2_2005 <- classify(day2_2005, "dr2ifdcd")
day1_2007 <- classify(day1_2007, "dr1ifdcd")
day2_2007 <- classify(day2_2007, "dr2ifdcd")
day1_2009 <- classify(day1_2009, "dr1ifdcd")
day2_2009 <- classify(day2_2009, "dr2ifdcd")
day1_2011 <- classify(day1_2011, "dr1ifdcd")
day2_2011 <- classify(day2_2011, "dr2ifdcd")
day1_2013 <- classify(day1_2013, "dr1ifdcd")
day2_2013 <- classify(day2_2013, "dr2ifdcd")
day1_2015 <- classify(day1_2015, "dr1ifdcd")
day2_2015 <- classify(day2_2015, "dr2ifdcd")
day1_2017 <- classify(day1_2017, "dr1ifdcd")
day2_2017 <- classify(day2_2017, "dr2ifdcd")
day1_2019 <- classify(day1_2019, "dr1ifdcd")
day2_2019 <- classify(day2_2019, "dr2ifdcd")

# dataframe for storing the average consumption/day
consumption <- data.frame(matrix(NA, nrow=33, ncol=length(food_list)+1))
colnames(consumption) <- c("year", food_list)
consumption$year <- 1990:2022

# function that computes the (weighted) average daily consumption in pounds
# and stores value in consumption dataframe (for a given year)
compute_consumption <- function(curr_year, day1_df, day2_df) {
  for (curr_food in food_list) {
    # concatenate dataframes from day1 and day2
    colnames(day1_df) <- c("seqn", "weight", "code", "grams", "class")
    colnames(day2_df) <- c("seqn", "weight", "code", "grams", "class")
    all_df <- rbind(day1_df, day2_df)
    
    # apply sample weights (adjusts for survey bias)
    all_df$avg <- all_df$grams * all_df$weight
    
    # compute the weighted average number of grams consumed
    all_df[all_df$class!=curr_food,]$avg <- 0
    sample_weights <- select(all_df, seqn, weight)
    sample_weights <- sample_weights %>% distinct()
    consumption[consumption$year==curr_year,curr_food] <<- sum(all_df$avg) / (sum(sample_weights$weight) * 2)
  }
}

# compute the (weighted) average daily consumption in pounds
# and store in consumption dataframe
compute_consumption(2001, all_2001, all_2001)
compute_consumption(2002, all_2001, all_2001)
compute_consumption(2003, day1_2003, day2_2003)
compute_consumption(2004, day1_2003, day2_2003)
compute_consumption(2005, day1_2005, day2_2005)
compute_consumption(2006, day1_2005, day2_2005)
compute_consumption(2007, day1_2007, day2_2007)
compute_consumption(2008, day1_2007, day2_2007)
compute_consumption(2009, day1_2009, day2_2009)
compute_consumption(2010, day1_2009, day2_2009)
compute_consumption(2011, day1_2011, day2_2011)
compute_consumption(2012, day1_2011, day2_2011)
compute_consumption(2013, day1_2013, day2_2013)
compute_consumption(2014, day1_2013, day2_2013)
compute_consumption(2015, day1_2015, day2_2015)
compute_consumption(2016, day1_2015, day2_2015)
compute_consumption(2017, day1_2017, day2_2017)
compute_consumption(2018, day1_2017, day2_2017)
compute_consumption(2019, day1_2019, day2_2019)
compute_consumption(2020, day1_2019, day2_2019)

#new_consumption <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/WWEIA_new_consumption.csv")
#consumption <- new_consumption[12:31,]

# extend data by imputation
full_consumption <- data.frame(matrix(NA, nrow=nrow(consumption)+3, ncol=ncol(consumption)))
colnames(full_consumption) <- colnames(consumption)
full_consumption[4:nrow(full_consumption),] <- consumption
full_consumption[1:3,1] <- 1998:2000
full_consumption[1:3,2:ncol(full_consumption)] <- consumption[1,2:ncol(full_consumption)]
consumption <- full_consumption[1:18,]

# dataframe of wholesale prices per pound
wholesale_prices <- wholesale_prices[2:nrow(wholesale_prices),] # in cwt
wholesale_prices <- mutate_all(wholesale_prices, function(x) as.numeric(as.character(x)))
wholesale_prices <- wholesale_prices[9:26,]
rownames(wholesale_prices) <- 1:nrow(wholesale_prices)

# compute the fraction of expenditures spent on each item
# dataframe to store the wholesale price * consumption intermediate step
prod <- data.frame(matrix(NA, nrow=nrow(wholesale_prices), ncol=ncol(wholesale_prices)))
colnames(prod) <- colnames(wholesale_prices)
prod$year <- wholesale_prices$year

# dataframe to store the fraction of expenditures for each product
frac <- data.frame(matrix(NA, nrow=nrow(wholesale_prices), ncol=ncol(wholesale_prices)))
colnames(frac) <- colnames(wholesale_prices)
frac$year <- wholesale_prices$year
prod[,2:ncol(prod)] <- wholesale_prices[,2:ncol(wholesale_prices)] / 100 * consumption[,2:ncol(consumption)] / POUNDS_TO_GRAMS
prod$sum <- rowSums(prod[,2:ncol(prod)])

for (curr_food in food_list) {
  frac[,curr_food] <- prod[,curr_food] / prod$sum
}

write.csv(consumption, "/Users/liliannagittoes/Desktop/Senior Thesis/WWEIA_short_consumption.csv", row.names=FALSE)
write.csv(frac, "/Users/liliannagittoes/Desktop/Senior Thesis/WWEIA_frac_short.csv", row.names=FALSE)

selected_frac <- select(frac, "year", "Apples", "Citrus", "Melons", "Strawberries")
colnames(selected_frac) <- c("year", "Apples", "Citrus.Fruits", "Melons", "Strawberries")
frac_melt <- melt(selected_frac, id.vars=1)
colnames(frac_melt) <- c("Year", "Fruit", "value")

frac_plot <- ggplot(frac_melt, aes(x=Year, value, color=Fruit, shape=Fruit)) + 
  geom_point(size=3) + 
  xlab("Year") + 
  ylab("Fraction of Fruit and Vegetable Expenditures") + 
  theme(plot.title = element_text(size = 24, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=20),
        axis.title.x = element_text(color = "grey20", size=20),
        axis.text.y = element_text(color = "grey20", size=20),
        axis.title.y = element_text(color = "grey20", size=20),
        legend.title = element_text(color = "grey20", size=20),
        legend.text = element_text(color = "grey20", size=20))

ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/Demand Data/frac_plot.jpg", 
       frac_plot, width=12, height=8, dpi=300)

