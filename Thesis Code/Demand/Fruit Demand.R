library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(zoo)
library(reshape2)

# estimate demand
expenses <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/full_expenditures.csv")
wweia <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/WWEIA_frac_short.csv")
wholesale_prices <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/wweia_prices.csv")
partial_retail_prices <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/partial_retail_prices.csv")
unit_size <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/ces_unitsize.csv")
population <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/msa_population.csv")

# metropolitan statistical areas that we consider
msas <- c("Chicago", "Detroit", "Minneapolis.StPaul", "Boston", "New.York",
          "Philadelphia", "Atlanta", "Baltimore", "Dallas.FortWorth", "Houston",
          "Miami", "DC", "Los.Angeles", "San.Francisco", "San.Diego", "Seattle")


population <- subset(population, select=c("Year", msas))
unit_size <- subset(unit_size, select=c("Year", msas))

# interpolate values for unit size
interpolate_unit <- function(df) {
  # convert to numeric
  df$year <- as.numeric(df$year)
  df$unit_size <- as.numeric(df$unit_size)
  
  df <- df %>% mutate(unit_size = na.approx(unit_size))
  
  return(df)
}

# first extend unit size dataset
unit_extended_df <- data.frame(matrix(nrow=nrow(unit_size), ncol=ncol(unit_size)))
colnames(unit_extended_df) <- c("year", msas)
unit_extended_df$year <- unit_size$Year

for (i in 1:length(msas)) {
  current_city <- msas[i]
  
  current_df <- subset(unit_size, select=c("Year", current_city))
  colnames(current_df) <- c("year", "unit_size")
  current_new_df <- interpolate_unit(current_df)
  
  unit_extended_df[,i+1] <- current_new_df[,c("unit_size")]
}

# divide to get the amount spent
expenses[1:nrow(expenses),2:ncol(expenses)] <- expenses[1:nrow(expenses),2:ncol(expenses)] / unit_extended_df[1:nrow(expenses),2:ncol(expenses)]
expenses[1:nrow(expenses),2:ncol(expenses)] <- expenses[1:nrow(expenses),2:ncol(expenses)] * population[1:nrow(expenses),2:ncol(expenses)]

# calculate population weighted average expenditures
population$Average <- rowMeans(population[,2:ncol(population)])
population$Sum <- rowSums(population[,2:ncol(population)])
expenses$Sum <- rowSums(expenses[,2:ncol(expenses)])
expenses$Average <- population$Average * expenses$Sum / population$Sum

# only for plotting
avg_expenses <- expenses[9:26,c(1,19)]

average <- ggplot(avg_expenses, aes(x=year, Average / (10^6))) + 
  geom_point() + 
  ggtitle("Average Metropolitan Area") + 
  xlab("Year") + 
  ylab("Average Expenditures") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/Demand Data/avg_expenditures_plot.jpg", 
       average, width=12, height=8, dpi=300)

region_expenses <- expenses[9:26,1:17]

west <- subset(region_expenses, select=c("year", "Los.Angeles", "San.Diego", "San.Francisco", "Seattle"))
west_melt <- melt(west, id.var=1)
colnames(west_melt) <- c("year", "Metropolitan_Area", "value")

p_sw <- subset(region_expenses, select=c("year", "Dallas.FortWorth", "Houston"))
p_sw_melt <- melt(p_sw, id.var=1)
colnames(p_sw_melt) <- c("year", "Metropolitan_Area", "value")

mw_se <- subset(region_expenses, select=c("year", "Atlanta", "Chicago", "Detroit", "Miami", "Minneapolis.StPaul"))
mw_se_melt <- melt(mw_se, id.var=1)
colnames(mw_se_melt) <- c("year", "Metropolitan_Area", "value")

a_ne <- subset(region_expenses, select=c("year", "Baltimore", "Boston", "DC", "New.York", "Philadelphia"))
a_ne_melt <- melt(a_ne, id.var=1)
colnames(a_ne_melt) <- c("year", "Metropolitan_Area", "value")

west <- ggplot(west_melt, aes(x=year, value / (10^6), color=Metropolitan_Area, shape=Metropolitan_Area)) + 
  geom_point(size=2) + 
  ggtitle("West U.S.") + 
  xlab("Year") + 
  ylab("Expenditures") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

p_sw <- ggplot(p_sw_melt, aes(x=year, value / (10^6), color=Metropolitan_Area, shape=Metropolitan_Area)) + 
  geom_point(size=2) + 
  ggtitle("Plains-Southwest U.S.") + 
  xlab("Year") + 
  ylab("Expenditures") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

mw_se <- ggplot(mw_se_melt, aes(x=year, value / (10^6), color=Metropolitan_Area, shape=Metropolitan_Area)) + 
  geom_point(size=2) + 
  ggtitle("Midwest-Southeast U.S.") + 
  xlab("Year") + 
  ylab("Expenditures") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

a_ne <- ggplot(a_ne_melt, aes(x=year, value / (10^6), color=Metropolitan_Area, shape=Metropolitan_Area)) + 
  geom_point(size=2) + 
  ggtitle("Atlantic-Northeast U.S.") + 
  xlab("Year") + 
  ylab("Expenditures") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

expenditures_plot <- grid.arrange(west, p_sw, mw_se, a_ne, nrow=2, ncol=2)

ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/Demand Data/expenditures_plot.jpg", 
       expenditures_plot, width=12, height=8, dpi=300)

# get yearly expenditures for each product
expenses <- select(expenses, -c('Sum'))

apples = expenses
apples[msas] = apples[msas] * wweia$Apples
apples$Average = apples$Average * wweia$Apples

melons = expenses
melons[msas] = melons[msas] * wweia$Melons
melons$Average = melons$Average * wweia$Melons

citrus = expenses
citrus[msas] = citrus[msas] * wweia$Citrus
citrus$Average = citrus$Average * wweia$Citrus

strawberries = expenses
strawberries[msas] = strawberries[msas] * wweia$Strawberries
strawberries$Average = strawberries$Average * wweia$Strawberries

# get retail prices (assume 100% markup)
wholesale_prices <- wholesale_prices[2:nrow(wholesale_prices),] # price per cwt
wholesale_prices[,1:ncol(wholesale_prices)] <- sapply(wholesale_prices[,1:ncol(wholesale_prices)], as.numeric)

partial_retail_prices <- partial_retail_prices[2:nrow(partial_retail_prices),]
partial_retail_prices[,1:ncol(partial_retail_prices)] <- sapply(partial_retail_prices[,1:ncol(partial_retail_prices)], as.numeric)

# apply markup to wholesale prices
retail_prices <- wholesale_prices
retail_prices[,2:ncol(retail_prices)] <- retail_prices[,2:ncol(retail_prices)] * 2 # price per cwt
retail_prices$Apples <- partial_retail_prices$Apples * 100
retail_prices$Strawberries <- partial_retail_prices$Strawberries * 100

short_wholesale <- wholesale_prices[9:26,]
short_retail <- retail_prices[9:26,]

short_wholesale <- select(short_wholesale, year, Apples, Citrus, Melons, Strawberries)
colnames(short_wholesale) <- c("year", "Apples", "Citrus.Fruits", "Melons", "Strawberries")
wholesale_melt <- melt(short_wholesale, id.vars=1)
colnames(wholesale_melt) <- c("Year", "Fruit", "value")

short_retail <- select(short_retail, year, Apples, Citrus, Melons, Strawberries)
colnames(short_retail) <- c("year", "Apples", "Citrus.Fruits", "Melons", "Strawberries")
retail_melt <- melt(short_retail, id.vars=1)
colnames(retail_melt) <- c("Year", "Fruit", "value")

wholesale_plot <- ggplot(wholesale_melt, aes(x=Year, value, color=Fruit, shape=Fruit)) + 
  geom_point(size=2) + 
  ggtitle("Wholesale Prices") + 
  xlab("Year") + 
  ylab("Wholesale Prices") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

retail_plot <- ggplot(retail_melt, aes(x=Year, value, color=Fruit, shape=Fruit)) + 
  geom_point(size=2) + 
  ggtitle("Retail Prices") + 
  xlab("Year") + 
  ylab("Retail Prices") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

prices_plot <- grid.arrange(wholesale_plot, retail_plot, nrow=1, ncol=2)
ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/Demand Data/prices_plot.jpg", 
       prices_plot, width=12, height=4, dpi=300)

# calculate quantity demanded
for (i in 2:ncol(citrus)) {
  apples[,i] = (apples[,i] / (retail_prices$Apples)) * 100
  citrus[,i] = (citrus[,i] / (retail_prices$Citrus)) * 100
  melons[,i] = (melons[,i] / (retail_prices$Melons)) * 100
  strawberries[,i] = (strawberries[,i] / (retail_prices$Strawberries)) * 100
}

# population data is in thousands
apples[,2:ncol(apples)] = apples[,2:ncol(apples)] * 1000
melons[,2:ncol(melons)] = melons[,2:ncol(melons)] * 1000
citrus[,2:ncol(citrus)] = citrus[,2:ncol(citrus)] * 1000
strawberries[,2:ncol(strawberries)] = strawberries[,2:ncol(strawberries)] * 1000

# average plot
avg_fruit <- cbind(apples$year, apples$Average, citrus$Average, melons$Average, strawberries$Average)
avg_fruit <- as.data.frame(avg_fruit)
colnames(avg_fruit) <- c("Year", "Apples", "Citrus.Fruits", "Melons", "Strawberries")
avg_fruit <- avg_fruit[9:26,]

avg_melt <- melt(avg_fruit, id.vars=1)
colnames(avg_melt) <- c("Year", "Fruit", "value")

avg_plot <- ggplot(avg_melt, aes(x=Year, value / (10^8), color=Fruit, shape=Fruit)) + 
  geom_point(size=3) + 
  ggtitle("Average Metropolitan Area") + 
  xlab("Year") + 
  ylab("Quantity Demanded") + 
  theme(plot.title = element_text(size = 24, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=20),
        axis.title.x = element_text(color = "grey20", size=20),
        axis.text.y = element_text(color = "grey20", size=20),
        axis.title.y = element_text(color = "grey20", size=20),
        legend.title = element_text(color = "grey20", size=20),
        legend.text = element_text(color = "grey20", size=20))

strawberries_avg <- select(avg_fruit, Year, Strawberries)
ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/Demand Data/avg_demand_plot.jpg", 
       avg_plot, width=12, height=8, dpi=300)

# plot metropolitan area demand
curr_fruit <- citrus[9:26,]

west <- subset(curr_fruit, select=c("year", "Los.Angeles", "San.Diego", "San.Francisco", "Seattle"))
west_melt <- melt(west, id.var=1)
colnames(west_melt) <- c("year", "Metropolitan_Area", "value")

p_sw <- subset(curr_fruit, select=c("year", "Dallas.FortWorth", "Houston"))
p_sw_melt <- melt(p_sw, id.var=1)
colnames(p_sw_melt) <- c("year", "Metropolitan_Area", "value")

mw_se <- subset(curr_fruit, select=c("year", "Atlanta", "Chicago", "Detroit", "Miami", "Minneapolis.StPaul"))
mw_se_melt <- melt(mw_se, id.var=1)
colnames(mw_se_melt) <- c("year", "Metropolitan_Area", "value")

a_ne <- subset(curr_fruit, select=c("year", "Baltimore", "Boston", "DC", "New.York", "Philadelphia"))
a_ne_melt <- melt(a_ne, id.var=1)
colnames(a_ne_melt) <- c("year", "Metropolitan_Area", "value")

west <- ggplot(west_melt, aes(x=year, value / (10^8), color=Metropolitan_Area, shape=Metropolitan_Area)) + 
  geom_point(size=2) + 
  ggtitle("West U.S.") + 
  xlab("Year") + 
  ylab("Quantity Demanded") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

p_sw <- ggplot(p_sw_melt, aes(x=year, value / (10^8), color=Metropolitan_Area, shape=Metropolitan_Area)) + 
  geom_point(size=2) + 
  ggtitle("Plains-Southwest U.S.") + 
  xlab("Year") + 
  ylab("Quantity Demanded") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

mw_se <- ggplot(mw_se_melt, aes(x=year, value / (10^8), color=Metropolitan_Area, shape=Metropolitan_Area)) + 
  geom_point(size=2) + 
  ggtitle("Midwest-Southeast U.S.") + 
  xlab("Year") + 
  ylab("Quantity Demanded") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

a_ne <- ggplot(a_ne_melt, aes(x=year, value / (10^8), color=Metropolitan_Area, shape=Metropolitan_Area)) + 
  geom_point(size=2) + 
  ggtitle("Atlantic-Northeast U.S.") + 
  xlab("Year") + 
  ylab("Quantity Demanded") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

demand_plot <- grid.arrange(west, p_sw, mw_se, a_ne, nrow=2, ncol=2)

ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/Demand Data/citrus_demand_plot.jpg", 
       demand_plot, width=12, height=8, dpi=300)


write.csv(apples, "/Users/liliannagittoes/Desktop/Senior Thesis/Demand Data/apples_demand.csv", row.names = FALSE)
write.csv(melons, "/Users/liliannagittoes/Desktop/Senior Thesis/Demand Data/melons_demand.csv", row.names = FALSE)
write.csv(citrus, "/Users/liliannagittoes/Desktop/Senior Thesis/Demand Data/citrus_demand.csv", row.names = FALSE)
write.csv(strawberries, "/Users/liliannagittoes/Desktop/Senior Thesis/Demand Data/strawberries_demand.csv", row.names = FALSE)

