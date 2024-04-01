library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(zoo)

# estimate demand
expenses <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/full_expenditures.csv")
wweia <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/isolated_wweia.csv")
prices <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/full_prices.csv")
unit_size <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/ces_unitsize.csv")
population <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/msa_population.csv")

# metropolitan statistical areas that we consider
msas <- c("Chicago", "Detroit", "Minneapolis.StPaul", "Boston", "New.York", 
          "Philadelphia", "Atlanta", "Baltimore", "Dallas.FortWorth", "Houston",
          "Miami", "DC", "Los.Angeles", "San.Francisco", "San.Diego", "Seattle")


population <- subset(population, select=c("Year", msas))

# interpolate values for unit size
interpolate_unit <- function(df) {
  # convert to numeric
  df$year <- as.numeric(df$year)
  df$unit_size <- as.numeric(df$unit_size)
  
  df <- df %>% mutate(unit_size = na.approx(unit_size))
  
  return(df)
}

# first extend unit size dataset
unit_extended_df <- data.frame(matrix(nrow=32, ncol=17))
colnames(unit_extended_df) <- c("year", msas)
unit_extended_df$year <- 1991:2022

for (i in 1:length(msas)) {
  current_city <- msas[i]
  
  current_df <- subset(unit_size, select=c("Year", current_city))
  colnames(current_df) <- c("year", "unit_size")
  current_bootstrap_df <- interpolate_unit(current_df)
  
  unit_extended_df[,i+1] <- current_bootstrap_df[,c("unit_size")]
}

# divide to get the amount spent
expenses[1:32,2:17] = expenses[1:32,2:17] / unit_extended_df[1:32,2:17]
expenses[1:32,2:17] = expenses[1:32,2:17] * population[1:32,2:17]
colnames(expenses) <- c("year", "Chicago", "Detroit", "Minneapolis.StPaul", "Boston", "New.York", 
                        "Philadelphia", "Atlanta", "Baltimore", "Dallas.FortWorth", "Houston",
                        "Miami", "Washington.D.C", "Los.Angeles", "San.Francisco", "San.Diego", "Seattle")

expenses <- expenses[1:31,]

midwest <- subset(expenses, select=c("year", "Chicago", "Detroit", "Minneapolis.StPaul"))
midwest_melt  <- melt(midwest,id.var=1)
colnames(midwest_melt) <- c("year", "Metropolitan_Area", "value")

northeast <- subset(expenses, select=c("year", "Boston", "New.York", "Philadelphia", "Washington.D.C"))
northmelt  <- melt(northeast,id.var=1)
colnames(northmelt) <- c("year", "Metropolitan_Area", "value")

south <- subset(expenses, select=c("year", "Atlanta", "Baltimore", "Dallas.FortWorth", "Houston", "Miami"))
south_melt  <- melt(south,id.var=1)
colnames(south_melt) <- c("year", "Metropolitan_Area", "value")

west <- subset(expenses, select=c("year", "Los.Angeles", "San.Diego", "San.Francisco", "Seattle"))
west_melt  <- melt(west,id.var=1)
colnames(west_melt) <- c("year", "Metropolitan_Area", "value")

midwest <- ggplot(midwest_melt, aes(x=year, value / (10^6), color=Metropolitan_Area)) + 
  geom_point() + 
  ggtitle("Midwest U.S.") + 
  xlab("Year") + 
  ylab("Average Amount Spent") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

northeast <- ggplot(northmelt, aes(x=year, value / (10^6), color=Metropolitan_Area)) + 
  geom_point() + 
  ggtitle("Northeast U.S.") + 
  xlab("Year") + 
  ylab("Average Amount Spent") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

south <- ggplot(south_melt, aes(x=year, value / (10^6), color=Metropolitan_Area)) + 
  geom_point() + 
  ggtitle("South U.S.") + 
  xlab("Year") + 
  ylab("Average Amount Spent") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

west <- ggplot(west_melt, aes(x=year, value / (10^6), color=Metropolitan_Area)) + 
  geom_point() + 
  ggtitle("West U.S.") + 
  xlab("Year") + 
  ylab("Average Amount Spent") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

expenditures <- grid.arrange(midwest, northeast, south, west, nrow=2, ncol=2)

ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/expenditures_plot.jpg", 
       expenditures, width=12, height=8, dpi=300)

# get yearly expenditures for each product
apples = expenses
apples[msas] = apples[msas] * wweia[wweia$Product=="Apples",]$frac[1]

carrots = expenses
carrots[msas] = carrots[msas] * wweia[wweia$Product=="Carrots",]$frac[1]

melons = expenses
melons[msas] = melons[msas] * wweia[wweia$Product=="Melons",]$frac[1]

onions = expenses
onions[msas] = onions[msas] * wweia[wweia$Product=="Onions",]$frac[1]

spinach = expenses
spinach[msas] = spinach[msas] * wweia[wweia$Product=="Spinach",]$frac[1]

strawberries = expenses
strawberries[msas] = strawberries[msas] * wweia[wweia$Product=="Strawberries",]$frac[1]

for (i in 2:17) {
  apples[,i] = (apples[,i] / (1.2 * prices$Apples)) * 100
  carrots[,i] = (carrots[,i] / (1.2 * prices$Carrots)) * 100
  melons[,i] = (melons[,i] / (1.2 * prices$Melons)) * 100
  onions[,i] = (onions[,i] / (1.2 * prices$Onions)) * 100
  spinach[,i] = (spinach[,i] / (1.2 * prices$Spinach)) * 100
  strawberries[,i] = (strawberries[,i] / (1.2 * prices$Strawberries)) * 100
}

# get retail prices
prices[1:32,2:8] <- prices[1:32,2:8] * 1.2

fruits <- subset(prices, select=c("year", "Apples", "Melons", "Strawberries"))
fruits_melt  <- melt(fruits,id.var=1)
colnames(fruits_melt) <- c("year", "Fruit.Name", "value")

veg <- subset(prices, select=c("year", "Carrots", "Onions", "Spinach"))
veg_melt  <- melt(veg,id.var=1)
colnames(veg_melt) <- c("year", "Vegetable.Name", "value")

fruits <- fruits[1:31,]
veg <- veg[1:31,]

fruits_plot <- ggplot(fruits_melt, aes(x=year, value, color=Fruit.Name)) + 
  geom_point() + 
  ggtitle("Fruit Prices") + 
  xlab("Year") + 
  ylab("Price") + 
  theme(plot.title = element_text(size = 20, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=18),
        axis.title.x = element_text(color = "grey20", size=18),
        axis.text.y = element_text(color = "grey20", size=18),
        axis.title.y = element_text(color = "grey20", size=18),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

veg_plot <- ggplot(veg_melt, aes(x=year, value, color=Vegetable.Name)) + 
  geom_point() + 
  ggtitle("Vegetable Prices") + 
  xlab("Year") + 
  ylab("Price") + 
  theme(plot.title = element_text(size = 20, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=18),
        axis.title.x = element_text(color = "grey20", size=18),
        axis.text.y = element_text(color = "grey20", size=18),
        axis.title.y = element_text(color = "grey20", size=18),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

prices_plot <- grid.arrange(fruits_plot, veg_plot, nrow=1, ncol=2)

ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/retail_prices_plot.jpg", 
       prices_plot, width=12, height=4, dpi=300)

#apples[1:32,2:17] = apples[1:32,2:17] / unit_extended_df[1:32,2:17]
#carrots[1:32,2:17] = carrots[1:32,2:17] / unit_extended_df[1:32,2:17]
#melons[1:32,2:17] = melons[1:32,2:17] / unit_extended_df[1:32,2:17]
#onions[1:32,2:17] = onions[1:32,2:17] / unit_extended_df[1:32,2:17]
#spinach[1:32,2:17] = spinach[1:32,2:17] / unit_extended_df[1:32,2:17]
#strawberries[1:32,2:17] = strawberries[1:32,2:17] / unit_extended_df[1:32,2:17]

#apples[1:32,2:17] = apples[1:32,2:17] * population[1:32,2:17]
#carrots[1:32,2:17] = carrots[1:32,2:17] * population[1:32,2:17]
#melons[1:32,2:17] = melons[1:32,2:17] * population[1:32,2:17]
#onions[1:32,2:17] = onions[1:32,2:17] * population[1:32,2:17]
#spinach[1:32,2:17] = spinach[1:32,2:17] * population[1:32,2:17]
#strawberries[1:32,2:17] = strawberries[1:32,2:17] * population[1:32,2:17]

apples[1:32,2:17] = apples[1:32,2:17] * 1000
carrots[1:32,2:17] = carrots[1:32,2:17] * 1000
melons[1:32,2:17] = melons[1:32,2:17] * 1000
onions[1:32,2:17] = onions[1:32,2:17] * 1000
spinach[1:32,2:17] = spinach[1:32,2:17] * 1000
strawberries[1:32,2:17] = strawberries[1:32,2:17] * 1000

current <- strawberries
current <- current[1:31,]

colnames(current) <- c("year", "Chicago", "Detroit", "Minneapolis.StPaul", "Boston", "New.York", 
                        "Philadelphia", "Atlanta", "Baltimore", "Dallas.FortWorth", "Houston",
                        "Miami", "Washington.D.C", "Los.Angeles", "San.Francisco", "San.Diego", "Seattle")

midwest <- subset(current, select=c("year", "Chicago", "Detroit", "Minneapolis.StPaul"))
midwest_melt  <- melt(midwest,id.var=1)
colnames(midwest_melt) <- c("year", "Metropolitan_Area", "value")

northeast <- subset(current, select=c("year", "Boston", "New.York", "Philadelphia", "Washington.D.C"))
northmelt  <- melt(northeast,id.var=1)
colnames(northmelt) <- c("year", "Metropolitan_Area", "value")

south <- subset(current, select=c("year", "Atlanta", "Baltimore", "Dallas.FortWorth", "Houston", "Miami"))
south_melt  <- melt(south,id.var=1)
colnames(south_melt) <- c("year", "Metropolitan_Area", "value")

west <- subset(current, select=c("year", "Los.Angeles", "San.Diego", "San.Francisco", "Seattle"))
west_melt  <- melt(west,id.var=1)
colnames(west_melt) <- c("year", "Metropolitan_Area", "value")

midwest <- ggplot(midwest_melt, aes(x=year, value / (10^6), color=Metropolitan_Area)) + 
  geom_point() + 
  ggtitle("Midwest U.S.") + 
  xlab("Year") + 
  ylab("Total Quantity Demanded") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

northeast <- ggplot(northmelt, aes(x=year, value / (10^6), color=Metropolitan_Area)) + 
  geom_point() + 
  ggtitle("Northeast U.S.") + 
  xlab("Year") + 
  ylab("Total Quantity Demanded") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

south <- ggplot(south_melt, aes(x=year, value / (10^6), color=Metropolitan_Area)) + 
  geom_point() + 
  ggtitle("South U.S.") + 
  xlab("Year") + 
  ylab("Total Quantity Demanded") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

west <- ggplot(west_melt, aes(x=year, value / (10^6), color=Metropolitan_Area)) + 
  geom_point() + 
  ggtitle("West U.S.") + 
  xlab("Year") + 
  ylab("Total Quantity Demanded") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

current_plot <- grid.arrange(midwest, northeast, south, west, nrow=2, ncol=2)

ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/strawberries_demand_plot.jpg", 
       current_plot, width=12, height=8, dpi=300)




# save data
write.csv(apples, "/Users/liliannagittoes/Desktop/Senior Thesis/Demand/apples_demand.csv", row.names = FALSE)
write.csv(carrots, "/Users/liliannagittoes/Desktop/Senior Thesis/Demand/carrots_demand.csv", row.names = FALSE)
write.csv(melons, "/Users/liliannagittoes/Desktop/Senior Thesis/Demand/melons_demand.csv", row.names = FALSE)
write.csv(onions, "/Users/liliannagittoes/Desktop/Senior Thesis/Demand/onions_demand.csv", row.names = FALSE)
write.csv(spinach, "/Users/liliannagittoes/Desktop/Senior Thesis/Demand/spinach_demand.csv", row.names = FALSE)
write.csv(strawberries, "/Users/liliannagittoes/Desktop/Senior Thesis/Demand/strawberries_demand.csv", row.names = FALSE)

