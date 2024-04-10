# make visualizations for harvests data

library(reshape2)
library(ggplot2)
library(gridExtra)

### Apples Predictions Visualizations

apples_harvests <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Harvests/pred_apples_harvests.csv")

pred_apples_harvests <- data.frame(matrix(NA, nrow=9, ncol=8))
pred_apples_harvests <- apples_harvests[1:9,]
colnames(pred_apples_harvests) <- c("Year", "West.Predict", "P_SW.Predict", "MW_SE.Predict", "A_NE.Predict")

act_apples_harvests <- data.frame(matrix(NA, nrow=9, ncol=8))
act_apples_harvests <- apples_harvests[10:18,]
colnames(act_apples_harvests) <- c("Year", "West", "P_SW", "MW_SE", "A_NE")

pred_west <- pred_apples_harvests[,c(1,2)]; colnames(pred_west) <- c("Year", "Estimated")
act_west <- act_apples_harvests[,c(1,2)]; colnames(act_west) <- c("Year", "Reported")
west_pred_melt <- melt(pred_west, id.vars=1)
west_act_melt <- melt(act_west, id.vars=1)
west_melt <- rbind(west_act_melt, west_pred_melt)
colnames(west_melt) <- c("Year", "Data", "value")

pred_p_sw <- pred_apples_harvests[,c(1,3)]; colnames(pred_p_sw) <- c("Year", "Estimated")
act_p_sw <- act_apples_harvests[,c(1,3)]; colnames(act_p_sw) <- c("Year", "Reported")
p_sw_pred_melt <- melt(pred_p_sw, id.vars=1)
p_sw_act_melt <- melt(act_p_sw, id.vars=1)
p_sw_melt <- rbind(p_sw_act_melt, p_sw_pred_melt)
colnames(p_sw_melt) <- c("Year", "Data", "value")

pred_mw_se <- pred_apples_harvests[,c(1,4)]; colnames(pred_mw_se) <- c("Year", "Estimated")
act_mw_se <- act_apples_harvests[,c(1,4)]; colnames(act_mw_se) <- c("Year", "Reported")
mw_se_pred_melt <- melt(pred_mw_se, id.vars=1)
mw_se_act_melt <- melt(act_mw_se, id.vars=1)
mw_se_melt <- rbind(mw_se_act_melt, mw_se_pred_melt)
colnames(mw_se_melt) <- c("Year", "Data", "value")

pred_a_ne <- pred_apples_harvests[,c(1,5)]; colnames(pred_a_ne) <- c("Year", "Estimated")
act_a_ne <- act_apples_harvests[,c(1,5)]; colnames(act_a_ne) <- c("Year", "Reported")
a_ne_pred_melt <- melt(pred_a_ne, id.vars=1)
a_ne_act_melt <- melt(act_a_ne, id.vars=1)
a_ne_melt <- rbind(a_ne_act_melt, a_ne_pred_melt)
colnames(a_ne_melt) <- c("Year", "Data", "value")

west_plot <- ggplot(west_melt, aes(x=Year, value / (10^4), color=Data, shape=Data)) + 
  geom_point(size=2) + 
  ggtitle("West Region") + 
  xlab("Year") + 
  ylab("Harvests") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

p_sw_plot <- ggplot(p_sw_melt, aes(x=Year, value / (10^4), color=Data, shape=Data)) + 
  geom_point(size=2) + 
  ggtitle("Plains-Southwest Region") + 
  xlab("Year") + 
  ylab("Harvests") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

mw_se_plot <- ggplot(mw_se_melt, aes(x=Year, value / (10^4), color=Data, shape=Data)) + 
  geom_point(size=2) + 
  ggtitle("Midwest Region") + 
  xlab("Year") + 
  ylab("Harvests") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

a_ne_plot <- ggplot(a_ne_melt, aes(x=Year, value / (10^4), color=Data, shape=Data)) + 
  geom_point(size=2) + 
  ggtitle("Mid-Atlantic Region") + 
  xlab("Year") + 
  ylab("Harvests") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

apples_pred <- grid.arrange(west_plot, p_sw_plot, mw_se_plot, a_ne_plot, nrow=2, ncol=2)
ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Harvests/apples_pred_plot_test.pdf", 
       apples_pred, width=12, height=8, dpi=300)

### Harvested Pounds Visualizations

apples_harvests <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Harvests/apples_harvests.csv")
citrus_harvests <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Harvests/citrus_harvests.csv")
melons_harvests <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Harvests/melons_harvests.csv")
strawberries_harvests <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Harvests/strawberries_harvests.csv")

# small region-level harvests plots
apples_harvests <- apples_harvests[,1:5]
citrus_harvests <- citrus_harvests[,1:5]
melons_harvests <- melons_harvests[,1:5]
strawberries_harvests <- strawberries_harvests[,1:5]

colnames(apples_harvests) <- c("Year", "West", "P-SW", "MW-SE", "A-NE")
apples_melt <- melt(apples_harvests, id.vars=1)
colnames(apples_melt) <- c("Year", "Region", "value")

citrus_harvests <- citrus_harvests[,c(1:4)]
colnames(citrus_harvests) <- c("Year", "West", "P-SW", "MW-SE")
citrus_melt <- melt(citrus_harvests, id.vars=1)
colnames(citrus_melt) <- c("Year", "Region", "value")

colnames(melons_harvests) <- c("Year", "West", "P-SW", "MW-SE", "A-NE")
melons_melt <- melt(melons_harvests, id.vars=1)
colnames(melons_melt) <- c("Year", "Region", "value")

strawberries_harvests <- strawberries_harvests[,c(1:2,4:5)]
colnames(strawberries_harvests) <- c("Year", "West", "MW-SE", "A-NE")
strawberries_melt <- melt(strawberries_harvests, id.vars=1)
colnames(strawberries_melt) <- c("Year", "Region", "value")

apples <- ggplot(apples_melt, aes(x=Year, value / (10^9), color=Region, shape=Region)) + 
  geom_point() + 
  ggtitle("Apples") + 
  xlab("Year") + 
  ylab("Harvests") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

citrus <- ggplot(citrus_melt, aes(x=Year, value / (10^9), color=Region, shape=Region)) + 
  geom_point() + 
  ggtitle("Citrus Fruits") + 
  xlab("Year") + 
  ylab("Harvests") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

melons <- ggplot(melons_melt, aes(x=Year, value / (10^9), color=Region, shape=Region)) + 
  geom_point() + 
  ggtitle("Melons") + 
  xlab("Year") + 
  ylab("Harvests") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

strawberries <- ggplot(strawberries_melt, aes(x=Year, value / (10^9), color=Region, shape=Region)) + 
  geom_point() + 
  ggtitle("Strawberries") + 
  xlab("Year") + 
  ylab("Harvests") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

harvests <- grid.arrange(apples, citrus, melons, strawberries, nrow=2, ncol=2)
ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Harvests/region_harvests_plot.jpg", 
       harvests, width=12, height=8, dpi=300)

# half-U.S. harvests plots
apples_harvests <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Harvests/apples_harvests.csv")
citrus_harvests <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Harvests/citrus_harvests.csv")
melons_harvests <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Harvests/melons_harvests.csv")
strawberries_harvests <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Harvests/strawberries_harvests.csv")

apples_harvests <- apples_harvests[,c(1,6:7)]
citrus_harvests <- citrus_harvests[,c(1,6:7)]
melons_harvests <- melons_harvests[,c(1,6:7)]
strawberries_harvests <- strawberries_harvests[,c(1,6:7)]

colnames(apples_harvests) <- c("Year", "West", "East")
apples_melt <- melt(apples_harvests, id.vars=1)
colnames(apples_melt) <- c("Year", "Region", "value")

colnames(citrus_harvests) <- c("Year", "West", "East")
citrus_melt <- melt(citrus_harvests, id.vars=1)
colnames(citrus_melt) <- c("Year", "Region", "value")

colnames(melons_harvests) <- c("Year", "West", "East")
melons_melt <- melt(melons_harvests, id.vars=1)
colnames(melons_melt) <- c("Year", "Region", "value")

colnames(strawberries_harvests) <- c("Year", "West", "East")
strawberries_melt <- melt(strawberries_harvests, id.vars=1)
colnames(strawberries_melt) <- c("Year", "Region", "value")

apples <- ggplot(apples_melt, aes(x=Year, value / (10^9), color=Region, shape=Region)) + 
  geom_point(size=2) + 
  ggtitle("Apples") + 
  xlab("Year") + 
  ylab("Harvests") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

citrus <- ggplot(citrus_melt, aes(x=Year, value / (10^9), color=Region, shape=Region)) + 
  geom_point(size=2) + 
  ggtitle("Citrus Fruits") + 
  xlab("Year") + 
  ylab("Harvests") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

melons <- ggplot(melons_melt, aes(x=Year, value / (10^9), color=Region, shape=Region)) + 
  geom_point(size=2) + 
  ggtitle("Melons") + 
  xlab("Year") + 
  ylab("Harvests") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

strawberries <- ggplot(strawberries_melt, aes(x=Year, value / (10^9), color=Region, shape=Region)) + 
  geom_point(size=2) + 
  ggtitle("Strawberries") + 
  xlab("Year") + 
  ylab("Harvests") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

harvests <- grid.arrange(apples, citrus, melons, strawberries, nrow=2, ncol=2)
ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Harvests/half_harvests_plot.jpg", 
       harvests, width=12, height=8, dpi=300)

### Imports and Exports Visualizations

exports <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/all_exports.csv")
imports <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/all_imports.csv")

apples_adj <- data.frame(matrix(NA, nrow=nrow(exports)-1, ncol=3))
colnames(apples_adj) <- c("Year", "Exports", "Imports")
apples_adj$Year <- as.numeric(exports[2:nrow(exports),]$year)
apples_adj$Exports <- as.numeric(exports[2:nrow(exports),]$Apples) * 10^6
apples_adj$Imports <- as.numeric(imports[2:nrow(imports),]$Apples) * 10^6
apples_adj <- filter(apples_adj, Year >= 1998, Year <= 2015)
apples_melt <- melt(apples_adj, id.vars=1)
colnames(apples_melt) <- c("Year", "Type", "value")

citrus_adj <- data.frame(matrix(NA, nrow=nrow(exports)-1, ncol=3))
colnames(citrus_adj) <- c("Year", "Exports", "Imports")
citrus_adj$Year <- as.numeric(exports[2:nrow(exports),]$year)
citrus_adj$Exports <- as.numeric(exports[2:nrow(exports),]$Oranges) * 10^6 + as.numeric(exports[2:nrow(exports),]$Lemons) * 10^6 + as.numeric(exports[2:nrow(exports),]$Grapefruit) * 10^6
citrus_adj$Imports <- as.numeric(imports[2:nrow(imports),]$Oranges) * 10^6 + as.numeric(imports[2:nrow(imports),]$Lemons) * 10^6 + as.numeric(imports[2:nrow(imports),]$Grapefruit) * 10^6
citrus_adj <- filter(citrus_adj, Year >= 1990, Year <= 2022)
citrus_melt <- melt(citrus_adj, id.vars=1)
colnames(citrus_melt) <- c("Year", "Type", "value")

melons_adj <- data.frame(matrix(NA, nrow=nrow(exports)-1, ncol=3))
colnames(melons_adj) <- c("Year", "Exports", "Imports")
melons_adj$Year <- as.numeric(exports[2:nrow(exports),]$year)
melons_adj$Exports <- as.numeric(exports[2:nrow(exports),]$Cantaloupe) * 10^6 + as.numeric(exports[2:nrow(exports),]$Honeydew) * 10^6 + as.numeric(exports[2:nrow(exports),]$Watermelon) * 10^6
melons_adj$Imports <- as.numeric(imports[2:nrow(imports),]$Cantaloupe) * 10^6 + as.numeric(imports[2:nrow(imports),]$Honeydew) * 10^6 + as.numeric(imports[2:nrow(imports),]$Watermelon) * 10^6
melons_adj <- filter(melons_adj, Year >= 1998, Year <= 2015)
melons_melt <- melt(melons_adj, id.vars=1)
colnames(melons_melt) <- c("Year", "Type", "value")

strawberries_adj <- data.frame(matrix(NA, nrow=nrow(exports)-1, ncol=3))
colnames(strawberries_adj) <- c("Year", "Exports", "Imports")
strawberries_adj$Year <- as.numeric(exports[2:nrow(exports),]$year)
strawberries_adj$Exports <- as.numeric(exports[2:nrow(exports),]$Strawberries) * 10^6
strawberries_adj$Imports <- as.numeric(imports[2:nrow(imports),]$Strawberries) * 10^6
strawberries_adj <- filter(strawberries_adj, Year >= 1998, Year <= 2015)
strawberries_melt <- melt(strawberries_adj, id.vars=1)
colnames(strawberries_melt) <- c("Year", "Type", "value")

apples <- ggplot(apples_melt, aes(x=Year, value / (10^9), color=Type, shape=Type)) + 
  geom_point(size=2) + 
  ggtitle("Apples") + 
  xlab("Year") + 
  ylab("Trade") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

citrus <- ggplot(citrus_melt, aes(x=Year, value / (10^9), color=Type, shape=Type)) + 
  geom_point(size=2) + 
  ggtitle("Citrus Fruits") + 
  xlab("Year") + 
  ylab("Trade") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

melons <- ggplot(melons_melt, aes(x=Year, value / (10^9), color=Type, shape=Type)) + 
  geom_point(size=2) + 
  ggtitle("Melons") + 
  xlab("Year") + 
  ylab("Trade") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

strawberries <- ggplot(strawberries_melt, aes(x=Year, value / (10^9), color=Type, shape=Type)) + 
  geom_point(size=2) + 
  ggtitle("Strawberries") + 
  xlab("Year") + 
  ylab("Trade") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

imports_exports <- grid.arrange(apples, citrus, melons, strawberries, nrow=2, ncol=2)
ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Harvests/imports_exports_plot.jpg", 
       imports_exports, width=12, height=8, dpi=300)

### MSA Fractions

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

msa_plot <- ggplot(population, aes(x=Year, msa_frac))  + 
  geom_point(size=3) + 
  xlab("Year") + 
  ylab("Fraction of U.S. Population") + 
  ylim(0.27, 0.38) + 
  theme(plot.title = element_text(size = 20, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=24),
        axis.title.x = element_text(color = "grey20", size=24),
        axis.text.y = element_text(color = "grey20", size=24),
        axis.title.y = element_text(color = "grey20", size=24),
        legend.title = element_text(color = "grey20", size=24),
        legend.text = element_text(color = "grey20", size=24))

ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Harvests/msa_plot.jpg", 
       msa_plot, width=12, height=8, dpi=300)

### Net Supply Available

apples_harvests <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/apples_supply.csv")
citrus_harvests <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/citrus_supply.csv")
melons_harvests <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/melons_supply.csv")
strawberries_harvests <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/strawberries_supply.csv")

# small region-level harvests plots
apples_harvests <- apples_harvests[,1:5]
citrus_harvests <- citrus_harvests[,1:5]
melons_harvests <- melons_harvests[,1:5]
strawberries_harvests <- strawberries_harvests[,1:5]

colnames(apples_harvests) <- c("Year", "West", "P-SW", "MW-SE", "A-NE")
apples_melt <- melt(apples_harvests, id.vars=1)
colnames(apples_melt) <- c("Year", "Region", "value")

citrus_harvests <- citrus_harvests[,c(1:4)]
colnames(citrus_harvests) <- c("Year", "West", "P-SW", "MW-SE")
citrus_melt <- melt(citrus_harvests, id.vars=1)
colnames(citrus_melt) <- c("Year", "Region", "value")

colnames(melons_harvests) <- c("Year", "West", "P-SW", "MW-SE", "A-NE")
melons_melt <- melt(melons_harvests, id.vars=1)
colnames(melons_melt) <- c("Year", "Region", "value")

strawberries_harvests <- strawberries_harvests[,c(1:2,4:5)]
colnames(strawberries_harvests) <- c("Year", "West", "MW-SE", "A-NE")
strawberries_melt <- melt(strawberries_harvests, id.vars=1)
colnames(strawberries_melt) <- c("Year", "Region", "value")

apples <- ggplot(apples_melt, aes(x=Year, value / (10^8), color=Region, shape=Region)) + 
  geom_point(size=2) + 
  ggtitle("Apples") + 
  xlab("Year") + 
  ylab("Net Supply") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

citrus <- ggplot(citrus_melt, aes(x=Year, value / (10^8), color=Region, shape=Region)) + 
  geom_point(size=2) + 
  ggtitle("Citrus Fruits") + 
  xlab("Year") + 
  ylab("Net Supply") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

melons <- ggplot(melons_melt, aes(x=Year, value / (10^8), color=Region, shape=Region)) + 
  geom_point(size=2) + 
  ggtitle("Melons") + 
  xlab("Year") + 
  ylab("Net Supply") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

strawberries <- ggplot(strawberries_melt, aes(x=Year, value / (10^8), color=Region, shape=Region)) + 
  geom_point(size=2) + 
  ggtitle("Strawberries") + 
  xlab("Year") + 
  ylab("Net Supply") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

supply <- grid.arrange(apples, citrus, melons, strawberries, nrow=2, ncol=2)
ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Harvests/region_supply_plot.jpg", 
       supply, width=12, height=8, dpi=300)

# half-U.S. harvests plots
apples_harvests <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/apples_supply.csv")
citrus_harvests <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/citrus_supply.csv")
melons_harvests <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/melons_supply.csv")
strawberries_harvests <- read.csv("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/strawberries_supply.csv")

apples_harvests <- apples_harvests[,c(1,6:7)]
citrus_harvests <- citrus_harvests[,c(1,6:7)]
melons_harvests <- melons_harvests[,c(1,6:7)]
strawberries_harvests <- strawberries_harvests[,c(1,6:7)]

colnames(apples_harvests) <- c("Year", "West", "East")
apples_melt <- melt(apples_harvests, id.vars=1)
colnames(apples_melt) <- c("Year", "Region", "value")

colnames(citrus_harvests) <- c("Year", "West", "East")
citrus_melt <- melt(citrus_harvests, id.vars=1)
colnames(citrus_melt) <- c("Year", "Region", "value")

colnames(melons_harvests) <- c("Year", "West", "East")
melons_melt <- melt(melons_harvests, id.vars=1)
colnames(melons_melt) <- c("Year", "Region", "value")

colnames(strawberries_harvests) <- c("Year", "West", "East")
strawberries_melt <- melt(strawberries_harvests, id.vars=1)
colnames(strawberries_melt) <- c("Year", "Region", "value")

apples <- ggplot(apples_melt, aes(x=Year, value / (10^8), color=Region, shape=Region)) + 
  geom_point(size=2) + 
  ggtitle("Apples") + 
  xlab("Year") + 
  ylab("Net Supply") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

citrus <- ggplot(citrus_melt, aes(x=Year, value / (10^8), color=Region, shape=Region)) + 
  geom_point(size=2) + 
  ggtitle("Citrus Fruits") + 
  xlab("Year") + 
  ylab("Net Supply") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

melons <- ggplot(melons_melt, aes(x=Year, value / (10^8), color=Region, shape=Region)) + 
  geom_point(size=2) + 
  ggtitle("Melons") + 
  xlab("Year") + 
  ylab("Net Supply") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

strawberries <- ggplot(strawberries_melt, aes(x=Year, value / (10^8), color=Region, shape=Region)) + 
  geom_point(size=2) + 
  ggtitle("Strawberries") + 
  xlab("Year") + 
  ylab("Net Supply") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

supply <- grid.arrange(apples, citrus, melons, strawberries, nrow=2, ncol=2)
ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Harvests/half_supply_plot.jpg", 
       supply, width=12, height=8, dpi=300)
