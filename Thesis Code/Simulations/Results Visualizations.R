library(reshape2)
library(ggplot2)
library(gridExtra)

# make results visualizations for a selected fruit (apples, citrus, strawberries, strawberries)
# read in demand file
demand <- read.csv("Desktop/Senior Thesis/Demand Data/strawberries_demand.csv")
demand <- demand[9:26,]
rownames(demand) <- 1:18

# read in supply + supply prediction files
supply <- read.csv("Desktop/Senior Thesis/Supply Data/strawberries_supply.csv")
linear_pred <- read.csv("Desktop/Senior Thesis/Linear Predicted Supply/strawberries_linear_predict.csv")
quad_pred <- read.csv("Desktop/Senior Thesis/Quadratic Predicted Supply/strawberries_quadratic_predict.csv")
arma_pred <- read.csv("Desktop/Senior Thesis/ARMA Predicted Supply/strawberries_arma_predict.csv")

# read in orders files (dual sourcing + identical retailers)
full_east <- read.csv("Desktop/Senior Thesis/Results/Dual Sourcing/Identical Retailers/strawberries_full_orders_east.csv")
colnames(full_east) <- colnames(demand)[2:(length(colnames(demand))-1)]
linear_east <- read.csv("Desktop/Senior Thesis/Results/Dual Sourcing/Identical Retailers/strawberries_linear_orders_east.csv")
colnames(linear_east) <- colnames(demand)[2:(length(colnames(demand))-1)]
quad_east <- read.csv("Desktop/Senior Thesis/Results/Dual Sourcing/Identical Retailers/strawberries_quadratic_orders_east.csv")
colnames(quad_east) <- colnames(demand)[2:(length(colnames(demand))-1)]
arma_east <- read.csv("Desktop/Senior Thesis/Results/Dual Sourcing/Identical Retailers/strawberries_arma_orders_east.csv")
colnames(arma_east) <- colnames(demand)[2:(length(colnames(demand))-1)]

full_west <- read.csv("Desktop/Senior Thesis/Results/Dual Sourcing/Identical Retailers/strawberries_full_orders_west.csv")
colnames(full_west) <- colnames(demand)[2:(length(colnames(demand))-1)]
linear_west <- read.csv("Desktop/Senior Thesis/Results/Dual Sourcing/Identical Retailers/strawberries_linear_orders_west.csv")
colnames(linear_west) <- colnames(demand)[2:(length(colnames(demand))-1)]
quad_west <- read.csv("Desktop/Senior Thesis/Results/Dual Sourcing/Identical Retailers/strawberries_quadratic_orders_west.csv")
colnames(quad_west) <- colnames(demand)[2:(length(colnames(demand))-1)]
arma_west <- read.csv("Desktop/Senior Thesis/Results/Dual Sourcing/Identical Retailers/strawberries_arma_orders_west.csv")
colnames(arma_west) <- colnames(demand)[2:(length(colnames(demand))-1)]

# read in orders files (dual sourcing + non-identical retailers)
nonid_full_east <- read.csv("Desktop/Senior Thesis/Results/Dual Sourcing/Non Identical Retailers/strawberries_nonid_full_orders_east.csv")
colnames(nonid_full_east) <- colnames(demand)[2:(length(colnames(demand))-1)]
nonid_linear_east <- read.csv("Desktop/Senior Thesis/Results/Dual Sourcing/Non Identical Retailers/strawberries_nonid_linear_orders_east.csv")
colnames(nonid_linear_east) <- colnames(demand)[2:(length(colnames(demand))-1)]
nonid_quad_east <- read.csv("Desktop/Senior Thesis/Results/Dual Sourcing/Non Identical Retailers/strawberries_nonid_quadratic_orders_east.csv")
colnames(nonid_quad_east) <- colnames(demand)[2:(length(colnames(demand))-1)]
nonid_arma_east <- read.csv("Desktop/Senior Thesis/Results/Dual Sourcing/Non Identical Retailers/strawberries_nonid_arma_orders_east.csv")
colnames(nonid_arma_east) <- colnames(demand)[2:(length(colnames(demand))-1)]

nonid_full_west <- read.csv("Desktop/Senior Thesis/Results/Dual Sourcing/Non Identical Retailers/strawberries_nonid_full_orders_west.csv")
colnames(nonid_full_west) <- colnames(demand)[2:(length(colnames(demand))-1)]
nonid_linear_west <- read.csv("Desktop/Senior Thesis/Results/Dual Sourcing/Non Identical Retailers/strawberries_nonid_linear_orders_west.csv")
colnames(nonid_linear_west) <- colnames(demand)[2:(length(colnames(demand))-1)]
nonid_quad_west <- read.csv("Desktop/Senior Thesis/Results/Dual Sourcing/Non Identical Retailers/strawberries_nonid_quadratic_orders_west.csv")
colnames(nonid_quad_west) <- colnames(demand)[2:(length(colnames(demand))-1)]
nonid_arma_west <- read.csv("Desktop/Senior Thesis/Results/Dual Sourcing/Non Identical Retailers/strawberries_nonid_arma_orders_west.csv")
colnames(nonid_arma_west) <- colnames(demand)[2:(length(colnames(demand))-1)]

# read in orders files (multi-sourcing + identical retailers)
multi_full_a_ne <- read.csv("Desktop/Senior Thesis/Results/Multi Sourcing/Identical Retailers/strawberries_full_orders_a_ne.csv")
colnames(multi_full_a_ne) <- colnames(demand)[2:(length(colnames(demand))-1)]
multi_linear_a_ne <- read.csv("Desktop/Senior Thesis/Results/Multi Sourcing/Identical Retailers/strawberries_linear_orders_a_ne.csv")
colnames(multi_linear_a_ne) <- colnames(demand)[2:(length(colnames(demand))-1)]
multi_quad_a_ne <- read.csv("Desktop/Senior Thesis/Results/Multi Sourcing/Identical Retailers/strawberries_quadratic_orders_a_ne.csv")
colnames(multi_quad_a_ne) <- colnames(demand)[2:(length(colnames(demand))-1)]
multi_arma_a_ne <- read.csv("Desktop/Senior Thesis/Results/Multi Sourcing/Identical Retailers/strawberries_arma_orders_a_ne.csv")
colnames(multi_arma_a_ne) <- colnames(demand)[2:(length(colnames(demand))-1)]

multi_full_mw_se <- read.csv("Desktop/Senior Thesis/Results/Multi Sourcing/Identical Retailers/strawberries_full_orders_mw_se.csv")
colnames(multi_full_mw_se) <- colnames(demand)[2:(length(colnames(demand))-1)]
multi_linear_mw_se <- read.csv("Desktop/Senior Thesis/Results/Multi Sourcing/Identical Retailers/strawberries_linear_orders_mw_se.csv")
colnames(multi_linear_mw_se) <- colnames(demand)[2:(length(colnames(demand))-1)]
multi_quad_mw_se <- read.csv("Desktop/Senior Thesis/Results/Multi Sourcing/Identical Retailers/strawberries_quadratic_orders_mw_se.csv")
colnames(multi_quad_mw_se) <- colnames(demand)[2:(length(colnames(demand))-1)]
multi_arma_mw_se <- read.csv("Desktop/Senior Thesis/Results/Multi Sourcing/Identical Retailers/strawberries_arma_orders_mw_se.csv")
colnames(multi_arma_mw_se) <- colnames(demand)[2:(length(colnames(demand))-1)]

multi_full_p_sw <- read.csv("Desktop/Senior Thesis/Results/Multi Sourcing/Identical Retailers/strawberries_full_orders_p_sw.csv")
colnames(multi_full_p_sw) <- colnames(demand)[2:(length(colnames(demand))-1)]
multi_linear_p_sw <- read.csv("Desktop/Senior Thesis/Results/Multi Sourcing/Identical Retailers/strawberries_linear_orders_p_sw.csv")
colnames(multi_linear_mw_se) <- colnames(demand)[2:(length(colnames(demand))-1)]
multi_quad_p_sw <- read.csv("Desktop/Senior Thesis/Results/Multi Sourcing/Identical Retailers/strawberries_quadratic_orders_p_sw.csv")
colnames(multi_quad_mw_se) <- colnames(demand)[2:(length(colnames(demand))-1)]
multi_arma_p_sw <- read.csv("Desktop/Senior Thesis/Results/Multi Sourcing/Identical Retailers/strawberries_arma_orders_p_sw.csv")
colnames(multi_arma_mw_se) <- colnames(demand)[2:(length(colnames(demand))-1)]

multi_full_west <- read.csv("Desktop/Senior Thesis/Results/Multi Sourcing/Identical Retailers/strawberries_full_orders_west.csv")
colnames(multi_full_west) <- colnames(demand)[2:(length(colnames(demand))-1)]
multi_linear_west <- read.csv("Desktop/Senior Thesis/Results/Multi Sourcing/Identical Retailers/strawberries_linear_orders_west.csv")
colnames(multi_linear_west) <- colnames(demand)[2:(length(colnames(demand))-1)]
multi_quad_west <- read.csv("Desktop/Senior Thesis/Results/Multi Sourcing/Identical Retailers/strawberries_quadratic_orders_west.csv")
colnames(multi_quad_west) <- colnames(demand)[2:(length(colnames(demand))-1)]
multi_arma_west <- read.csv("Desktop/Senior Thesis/Results/Multi Sourcing/Identical Retailers/strawberries_arma_orders_west.csv")
colnames(multi_arma_west) <- colnames(demand)[2:(length(colnames(demand))-1)]

## plot of excess supply
excess_supply <- data.frame(matrix(NA, nrow=nrow(demand), ncol=2))
colnames(excess_supply) <- c("year", "excess")
excess_supply$year <- demand$year
excess_supply$excess <- supply$west_half_lbs + supply$east_half_lbs - (16*demand$Average)

excess_supply_plot <- ggplot(excess_supply, aes(x=year, excess / (10^8))) + 
  geom_point(size=3) + 
  ggtitle("Total Excess Supply") + 
  xlab("Year") + 
  ylab("Total Supply - Total Demand") + 
  theme(plot.title = element_text(size = 24, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=20),
        axis.title.x = element_text(color = "grey20", size=20),
        axis.text.y = element_text(color = "grey20", size=20),
        axis.title.y = element_text(color = "grey20", size=20),
        legend.title = element_text(color = "grey20", size=20),
        legend.text = element_text(color = "grey20", size=20))

ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/Results Visualizations/strawberries_excess_supply.jpg", 
       excess_supply_plot, width=12, height=8, dpi=300)

## plot predictions (dual sourcing)

west_half_pred <- data.frame(matrix(NA, nrow=nrow(linear_pred), ncol=5))
colnames(west_half_pred) <- c("year", "Full", "Linear", "Quadratic", "ARMA")
west_half_pred$year <- linear_pred$year
west_half_pred$Full <- supply[2:nrow(supply),]$west_half_lbs
west_half_pred$Linear <- linear_pred$west_half_lbs
west_half_pred$Quadratic <- quad_pred$west_half_lbs
west_half_pred$ARMA <- arma_pred$west_half_lbs

east_half_pred <- data.frame(matrix(NA, nrow=nrow(linear_pred), ncol=5))
colnames(east_half_pred) <- c("year", "Full", "Linear", "Quadratic", "ARMA")
east_half_pred$year <- linear_pred$year
east_half_pred$Full <- supply[2:nrow(supply),]$east_half_lbs
east_half_pred$Linear <- linear_pred$east_half_lbs
east_half_pred$Quadratic <- quad_pred$east_half_lbs
east_half_pred$ARMA <- arma_pred$east_half_lbs

west_half_pred_melt <- melt(west_half_pred, id.vars=1)
colnames(west_half_pred_melt) <- c("Year", "Type", "value")

east_half_pred_melt <- melt(east_half_pred, id.vars=1)
colnames(east_half_pred_melt) <- c("Year", "Type", "value")

west_half_pred_plot <- ggplot(west_half_pred_melt, aes(x=Year, value / (10^8), color=Type, shape=Type)) + 
  geom_point(size=2) + 
  ggtitle("West Half of U.S.") + 
  xlab("Year") + 
  ylab("Supply Predictions") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

east_half_pred_plot <- ggplot(east_half_pred_melt, aes(x=Year, value / (10^8), color=Type, shape=Type)) + 
  geom_point(size=2) + 
  ggtitle("East Half of U.S.") + 
  xlab("Year") + 
  ylab("Supply Predictions") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

dual_pred <- grid.arrange(west_half_pred_plot, east_half_pred_plot, nrow=1, ncol=2)
ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/Results Visualizations/strawberries_dual_pred.jpg", 
       dual_pred, width=12, height=4, dpi=300)

## plot predictions (multi sourcing)

west_pred <- data.frame(matrix(NA, nrow=nrow(linear_pred), ncol=5))
colnames(west_pred) <- c("year", "Full", "Linear", "Quadratic", "ARMA")
west_pred$year <- linear_pred$year
west_pred$Full <- supply[2:nrow(supply),]$west_lbs
west_pred$Linear <- linear_pred$west_lbs
west_pred$Quadratic <- quad_pred$west_lbs
west_pred$ARMA <- arma_pred$west_lbs

p_sw_pred <- data.frame(matrix(NA, nrow=nrow(linear_pred), ncol=5))
colnames(p_sw_pred) <- c("year", "Full", "Linear", "Quadratic", "ARMA")
p_sw_pred$year <- linear_pred$year
p_sw_pred$Full <- supply[2:nrow(supply),]$p_sw_lbs
p_sw_pred$Linear <- linear_pred$p_sw_lbs
p_sw_pred$Quadratic <- quad_pred$p_sw_lbs
p_sw_pred$ARMA <- arma_pred$p_sw_lbs

mw_se_pred <- data.frame(matrix(NA, nrow=nrow(linear_pred), ncol=5))
colnames(mw_se_pred) <- c("year", "Full", "Linear", "Quadratic", "ARMA")
mw_se_pred$year <- linear_pred$year
mw_se_pred$Full <- supply[2:nrow(supply),]$mw_se_lbs
mw_se_pred$Linear <- linear_pred$mw_se_lbs
mw_se_pred$Quadratic <- quad_pred$mw_se_lbs
mw_se_pred$ARMA <- arma_pred$mw_se_lbs

a_ne_pred <- data.frame(matrix(NA, nrow=nrow(linear_pred), ncol=5))
colnames(a_ne_pred) <- c("year", "Full", "Linear", "Quadratic", "ARMA")
a_ne_pred$year <- linear_pred$year
a_ne_pred$Full <- supply[2:nrow(supply),]$a_ne_lbs
a_ne_pred$Linear <- linear_pred$a_ne_lbs
a_ne_pred$Quadratic <- quad_pred$a_ne_lbs
a_ne_pred$ARMA <- arma_pred$a_ne_lbs

west_pred_melt <- melt(west_pred, id.vars=1)
colnames(west_pred_melt) <- c("Year", "Type", "value")

p_sw_pred_melt <- melt(p_sw_pred, id.vars=1)
colnames(p_sw_pred_melt) <- c("Year", "Type", "value")

mw_se_pred_melt <- melt(mw_se_pred, id.vars=1)
colnames(mw_se_pred_melt) <- c("Year", "Type", "value")

a_ne_pred_melt <- melt(a_ne_pred, id.vars=1)
colnames(a_ne_pred_melt) <- c("Year", "Type", "value")

west_pred_plot <- ggplot(west_pred_melt, aes(x=Year, value / (10^8), color=Type, shape=Type)) + 
  geom_point(size=2) + 
  ggtitle("West U.S.") + 
  xlab("Year") + 
  ylab("Supply Predictions") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

p_sw_pred_plot <- ggplot(p_sw_pred_melt, aes(x=Year, value / (10^8), color=Type, shape=Type)) + 
  geom_point(size=2) + 
  ggtitle("Plains-Southwest U.S.") + 
  xlab("Year") + 
  ylab("Supply Predictions") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

mw_se_pred_plot <- ggplot(mw_se_pred_melt, aes(x=Year, value / (10^8), color=Type, shape=Type)) + 
  geom_point(size=2) + 
  ggtitle("Midwest-Southeast U.S.") + 
  xlab("Year") + 
  ylab("Supply Predictions") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

a_ne_pred_plot <- ggplot(a_ne_pred_melt, aes(x=Year, value / (10^8), color=Type, shape=Type)) + 
  geom_point(size=2) + 
  ggtitle("Atlantic-Northeast U.S.") + 
  xlab("Year") + 
  ylab("Supply Predictions") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

multi_pred <- grid.arrange(west_pred_plot, p_sw_pred_plot, mw_se_pred_plot, a_ne_pred_plot, nrow=2, ncol=2)
ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/Results Visualizations/strawberries_multi_pred.jpg", 
       multi_pred, width=12, height=8, dpi=300)

## plot retailers' ordering decisions (dual sourcing and identical retailers)
all_east <- data.frame(matrix(NA, nrow=nrow(full_west), ncol=5))
colnames(all_east) <- c("year", "Full", "Linear", "Quadratic", "ARMA")
all_east$year <- demand$year
all_east$Full <- full_east[,1]
all_east$Linear <- linear_east[,1]
all_east$Quadratic <- quad_east[,1]
all_east$ARMA <- arma_east[,1]

all_west <- data.frame(matrix(NA, nrow=nrow(full_west), ncol=5))
colnames(all_west) <- c("year", "Full", "Linear", "Quadratic", "ARMA")
all_west$year <- demand$year
all_west$Full <- full_west[,1]
all_west$Linear <- linear_west[,1]
all_west$Quadratic <- quad_west[,1]
all_west$ARMA <- arma_west[,1]

total_orders <- all_east + all_west
total_orders$year <- all_east$year

minus_demand <- total_orders
minus_demand$Full <- total_orders$Full - (demand$Average)
minus_demand$Linear <- total_orders$Linear - (demand$Average)
minus_demand$Quadratic <- total_orders$Quadratic - (demand$Average)
minus_demand$ARMA <- total_orders$ARMA - (demand$Average)
minus_demand_melt <- melt(minus_demand, id.vars=1)
colnames(minus_demand_melt) <- c("Year", "Type", "value")

minus_demand_plot <- ggplot(minus_demand_melt, aes(x=Year, value / (10^6), color=Type, shape=Type)) + 
  geom_point(size=3) + 
  ggtitle("Total Orders - Demand") + 
  xlab("Year") + 
  ylab("Retailer Order Quantities - Demand") + 
  theme(plot.title = element_text(size = 24, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=20),
        axis.title.x = element_text(color = "grey20", size=20),
        axis.text.y = element_text(color = "grey20", size=20),
        axis.title.y = element_text(color = "grey20", size=20),
        legend.title = element_text(color = "grey20", size=20),
        legend.text = element_text(color = "grey20", size=20))

ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/Results Visualizations/strawberries_dual_id_excess_orders.jpg", 
       minus_demand_plot, width=12, height=8, dpi=300)

# just for comparison (plots not actually included in report)
total_pred <- east_half_pred + west_half_pred
total_pred$year <- east_half_pred$year
pred_minus_demand <- total_pred - (16 * demand[2:nrow(demand),]$Average)
pred_minus_demand$year <- total_pred$year
pred_minus_demand_melt <- melt(pred_minus_demand, id.vars=1)
colnames(pred_minus_demand_melt) <- c("Year", "Type", "value")

## plot retailers' ordering decisions (dual sourcing and non-identical retailers)
chicago_all_east <- data.frame(matrix(NA, nrow=nrow(full_west), ncol=5))
colnames(chicago_all_east) <- c("year", "Full", "Linear", "Quadratic", "ARMA")
chicago_all_east$year <- demand$year
chicago_all_east$Full <- nonid_full_east[,1]
chicago_all_east$Linear <- nonid_linear_east[,1]
chicago_all_east$Quadratic <- nonid_quad_east[,1]
chicago_all_east$ARMA <- nonid_arma_east[,1]

chicago_all_west <- data.frame(matrix(NA, nrow=nrow(full_west), ncol=5))
colnames(chicago_all_west) <- c("year", "Full", "Linear", "Quadratic", "ARMA")
chicago_all_west$year <- demand$year
chicago_all_west$Full <- nonid_full_west[,1]
chicago_all_west$Linear <- nonid_linear_west[,1]
chicago_all_west$Quadratic <- nonid_quad_west[,1]
chicago_all_west$ARMA <- nonid_arma_west[,1]

chicago_total_orders <- chicago_all_east + chicago_all_west
chicago_total_orders$year <- chicago_all_east$year

chicago_minus_demand <- chicago_total_orders
chicago_minus_demand$Full <- chicago_total_orders$Full - (demand$Chicago)
chicago_minus_demand$Linear <- chicago_total_orders$Linear - (demand$Chicago)
chicago_minus_demand$Quadratic <- chicago_total_orders$Quadratic - (demand$Chicago)
chicago_minus_demand$ARMA <- chicago_total_orders$ARMA - (demand$Chicago)
chicago_minus_demand_melt <- melt(chicago_minus_demand, id.vars=1)
colnames(chicago_minus_demand_melt) <- c("Year", "Type", "value")

chicago_minus_demand_plot <- ggplot(chicago_minus_demand_melt, aes(x=Year, value / (10^6), color=Type, shape=Type)) + 
  geom_point(size=2) + 
  ggtitle("Chicago") + 
  xlab("Year") + 
  ylab("Retailer Order Quantities - Demand") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14)) + 
  ylim(0,35)

dc_all_east <- data.frame(matrix(NA, nrow=nrow(full_west), ncol=5))
colnames(dc_all_east) <- c("year", "Full", "Linear", "Quadratic", "ARMA")
dc_all_east$year <- demand$year
dc_all_east$Full <- nonid_full_east[,12]
dc_all_east$Linear <- nonid_linear_east[,12]
dc_all_east$Quadratic <- nonid_quad_east[,12]
dc_all_east$ARMA <- nonid_arma_east[,12]

dc_all_west <- data.frame(matrix(NA, nrow=nrow(full_west), ncol=5))
colnames(dc_all_west) <- c("year", "Full", "Linear", "Quadratic", "ARMA")
dc_all_west$year <- demand$year
dc_all_west$Full <- nonid_full_west[,12]
dc_all_west$Linear <- nonid_linear_west[,12]
dc_all_west$Quadratic <- nonid_quad_west[,12]
dc_all_west$ARMA <- nonid_arma_west[,12]

dc_total_orders <- dc_all_east + dc_all_west
dc_total_orders$year <- dc_all_east$year

dc_minus_demand <- dc_total_orders
dc_minus_demand$Full <- dc_total_orders$Full - (demand$DC)
dc_minus_demand$Linear <- dc_total_orders$Linear - (demand$DC)
dc_minus_demand$Quadratic <- dc_total_orders$Quadratic - (demand$DC)
dc_minus_demand$ARMA <- dc_total_orders$ARMA - (demand$DC)
dc_minus_demand_melt <- melt(dc_minus_demand, id.vars=1)
colnames(dc_minus_demand_melt) <- c("Year", "Type", "value")

dc_minus_demand_plot <- ggplot(dc_minus_demand_melt, aes(x=Year, value / (10^6), color=Type, shape=Type)) + 
  geom_point(size=2) + 
  ggtitle("Washington D.C.") + 
  xlab("Year") + 
  ylab("Retailer Order Quantities - Demand") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14)) + 
  ylim(0,18)

houston_all_east <- data.frame(matrix(NA, nrow=nrow(full_west), ncol=5))
colnames(houston_all_east) <- c("year", "Full", "Linear", "Quadratic", "ARMA")
houston_all_east$year <- demand$year
houston_all_east$Full <- nonid_full_east[,10]
houston_all_east$Linear <- nonid_linear_east[,10]
houston_all_east$Quadratic <- nonid_quad_east[,10]
houston_all_east$ARMA <- nonid_arma_east[,10]

houston_all_west <- data.frame(matrix(NA, nrow=nrow(full_west), ncol=5))
colnames(houston_all_west) <- c("year", "Full", "Linear", "Quadratic", "ARMA")
houston_all_west$year <- demand$year
houston_all_west$Full <- nonid_full_west[,10]
houston_all_west$Linear <- nonid_linear_west[,10]
houston_all_west$Quadratic <- nonid_quad_west[,10]
houston_all_west$ARMA <- nonid_arma_west[,10]

houston_total_orders <- houston_all_east + houston_all_west
houston_total_orders$year <- houston_all_east$year

houston_minus_demand <- houston_total_orders
houston_minus_demand$Full <- houston_total_orders$Full - (demand$Houston)
houston_minus_demand$Linear <- houston_total_orders$Linear - (demand$Houston)
houston_minus_demand$Quadratic <- houston_total_orders$Quadratic - (demand$Houston)
houston_minus_demand$ARMA <- houston_total_orders$ARMA - (demand$Houston)
houston_minus_demand_melt <- melt(houston_minus_demand, id.vars=1)
colnames(houston_minus_demand_melt) <- c("Year", "Type", "value")

houston_minus_demand_plot <- ggplot(houston_minus_demand_melt, aes(x=Year, value / (10^6), color=Type, shape=Type)) + 
  geom_point(size=2) + 
  ggtitle("Houston") + 
  xlab("Year") + 
  ylab("Retailer Order Quantities - Demand") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14)) + 
  ylim(0,15)

la_all_east <- data.frame(matrix(NA, nrow=nrow(full_west), ncol=5))
colnames(la_all_east) <- c("year", "Full", "Linear", "Quadratic", "ARMA")
la_all_east$year <- demand$year
la_all_east$Full <- nonid_full_east[,13]
la_all_east$Linear <- nonid_linear_east[,13]
la_all_east$Quadratic <- nonid_quad_east[,13]
la_all_east$ARMA <- nonid_arma_east[,13]

la_all_west <- data.frame(matrix(NA, nrow=nrow(full_west), ncol=5))
colnames(la_all_west) <- c("year", "Full", "Linear", "Quadratic", "ARMA")
la_all_west$year <- demand$year
la_all_west$Full <- nonid_full_west[,13]
la_all_west$Linear <- nonid_linear_west[,13]
la_all_west$Quadratic <- nonid_quad_west[,13]
la_all_west$ARMA <- nonid_arma_west[,13]

la_total_orders <- la_all_east + la_all_west
la_total_orders$year <- la_all_east$year

la_minus_demand <- la_total_orders
la_minus_demand$Full <- la_total_orders$Full - (demand$Los.Angeles)
la_minus_demand$Linear <- la_total_orders$Linear - (demand$Los.Angeles)
la_minus_demand$Quadratic <- la_total_orders$Quadratic - (demand$Los.Angeles)
la_minus_demand$ARMA <- la_total_orders$ARMA - (demand$Los.Angeles)
la_minus_demand_melt <- melt(la_minus_demand, id.vars=1)
colnames(la_minus_demand_melt) <- c("Year", "Type", "value")

la_minus_demand_plot <- ggplot(la_minus_demand_melt, aes(x=Year, value / (10^6), color=Type, shape=Type)) + 
  geom_point(size=2) + 
  ggtitle("Los Angeles") + 
  xlab("Year") + 
  ylab("Retailer Order Quantities - Demand") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14)) #+ 
  #ylim(0,85)

nonid_minus_demand <- grid.arrange(chicago_minus_demand_plot, dc_minus_demand_plot, houston_minus_demand_plot, la_minus_demand_plot, nrow=2, ncol=2)
ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/Results Visualizations/strawberries_dual_nonid_excess_orders.jpg", 
       nonid_minus_demand, width=12, height=8, dpi=300)

## plot retailers' ordering decisions (multi-sourcing and identical retailers)
multi_all_west <- data.frame(matrix(NA, nrow=nrow(multi_full_west), ncol=5))
colnames(multi_all_west) <- c("year", "Full", "Linear", "Quadratic", "ARMA")
multi_all_west$year <- demand$year
multi_all_west$Full <- multi_full_west[,1]
multi_all_west$Linear <- multi_linear_west[,1]
multi_all_west$Quadratic <- multi_quad_west[,1]
multi_all_west$ARMA <- multi_arma_west[,1]

multi_all_p_sw <- data.frame(matrix(NA, nrow=nrow(multi_full_p_sw), ncol=5))
colnames(multi_all_p_sw) <- c("year", "Full", "Linear", "Quadratic", "ARMA")
multi_all_p_sw$year <- demand$year
multi_all_p_sw$Full <- multi_full_p_sw[,1]
multi_all_p_sw$Linear <- multi_linear_p_sw[,1]
multi_all_p_sw$Quadratic <- multi_quad_p_sw[,1]
multi_all_p_sw$ARMA <- multi_arma_p_sw[,1]

multi_all_mw_se <- data.frame(matrix(NA, nrow=nrow(multi_full_mw_se), ncol=5))
colnames(multi_all_mw_se) <- c("year", "Full", "Linear", "Quadratic", "ARMA")
multi_all_mw_se$year <- demand$year
multi_all_mw_se$Full <- multi_full_mw_se[,1]
multi_all_mw_se$Linear <- multi_linear_mw_se[,1]
multi_all_mw_se$Quadratic <- multi_quad_mw_se[,1]
multi_all_mw_se$ARMA <- multi_arma_mw_se[,1]

multi_all_a_ne <- data.frame(matrix(NA, nrow=nrow(multi_full_a_ne), ncol=5))
colnames(multi_all_a_ne) <- c("year", "Full", "Linear", "Quadratic", "ARMA")
multi_all_a_ne$year <- demand$year
multi_all_a_ne$Full <- multi_full_a_ne[,1]
multi_all_a_ne$Linear <- multi_linear_a_ne[,1]
multi_all_a_ne$Quadratic <- multi_quad_a_ne[,1]
multi_all_a_ne$ARMA <- multi_arma_a_ne[,1]

multi_total_orders <- multi_all_west + multi_all_mw_se + multi_all_a_ne # + multi_all_p_sw
multi_total_orders$year <- multi_all_west$year

multi_minus_demand <- multi_total_orders
multi_minus_demand$Full <- multi_total_orders$Full - (demand$Average)
multi_minus_demand$Linear <- multi_total_orders$Linear - (demand$Average)
multi_minus_demand$Quadratic <- multi_total_orders$Quadratic - (demand$Average)
multi_minus_demand$ARMA <- multi_total_orders$ARMA - (demand$Average)
multi_minus_demand_melt <- melt(multi_minus_demand, id.vars=1)
colnames(multi_minus_demand_melt) <- c("Year", "Type", "value")

multi_minus_demand_plot <- ggplot(multi_minus_demand_melt, aes(x=Year, value / (10^6), color=Type, shape=Type)) + 
  geom_point(size=3) + 
  ggtitle("Total Orders - Demand") + 
  xlab("Year") + 
  ylab("Retailer Order Quantities - Demand") + 
  theme(plot.title = element_text(size = 24, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=20),
        axis.title.x = element_text(color = "grey20", size=20),
        axis.text.y = element_text(color = "grey20", size=20),
        axis.title.y = element_text(color = "grey20", size=20),
        legend.title = element_text(color = "grey20", size=20),
        legend.text = element_text(color = "grey20", size=20))

ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/Results Visualizations/strawberries_multi_id_excess_orders.jpg", 
       multi_minus_demand_plot, width=12, height=8, dpi=300)


