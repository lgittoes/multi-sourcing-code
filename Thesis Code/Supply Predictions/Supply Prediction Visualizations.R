library(reshape2)
library(ggplot2)

# full information
strawberries_full <- read.csv("Desktop/Senior Thesis/Supply Data/strawberries_supply.csv")
melons_full <- read.csv("Desktop/Senior Thesis/Supply Data/melons_supply.csv")
apples_full <- read.csv("Desktop/Senior Thesis/Supply Data/apples_supply.csv")
citrus_full <- read.csv("Desktop/Senior Thesis/Supply Data/citrus_supply.csv")

# linear predictions
strawberries_linear <- read.csv("Desktop/Senior Thesis/Linear Predicted Supply/strawberries_linear_predict.csv")
melons_linear <- read.csv("Desktop/Senior Thesis/Linear Predicted Supply/melons_linear_predict.csv")
apples_linear <- read.csv("Desktop/Senior Thesis/Linear Predicted Supply/apples_linear_predict.csv")
citrus_linear <- read.csv("Desktop/Senior Thesis/Linear Predicted Supply/citrus_linear_predict.csv")

# quadratic predictions
strawberries_quad <- read.csv("Desktop/Senior Thesis/Quadratic Predicted Supply/strawberries_quadratic_predict.csv")
melons_quad <- read.csv("Desktop/Senior Thesis/Quadratic Predicted Supply/melons_quadratic_predict.csv")
apples_quad <- read.csv("Desktop/Senior Thesis/Quadratic Predicted Supply/apples_quadratic_predict.csv")
citrus_quad <- read.csv("Desktop/Senior Thesis/Quadratic Predicted Supply/citrus_quadratic_predict.csv")

# ARMA predictions
strawberries_arma <- read.csv("Desktop/Senior Thesis/ARMA Predicted Supply/strawberries_arma_predict.csv")
melons_arma <- read.csv("Desktop/Senior Thesis/ARMA Predicted Supply/melons_arma_predict.csv")
apples_arma <- read.csv("Desktop/Senior Thesis/ARMA Predicted Supply/apples_arma_predict.csv")
citrus_arma <- read.csv("Desktop/Senior Thesis/ARMA Predicted Supply/citrus_arma_predict.csv")

## strawberries Supply Prediction Visualizations
west <- data.frame(matrix(NA, nrow=nrow(strawberries_full)-1, ncol=6))
colnames(west) <- c("year", "Full", "Linear", "Quadratic", "ARMA", "VAR")
west$year <- strawberries_linear$year
west$Full <- strawberries_full[2:nrow(strawberries_full),]$west_lbs
west$Linear <- strawberries_linear$west_lbs
west$Quadratic <- strawberries_quad$west_lbs
west$ARMA <- strawberries_arma$west_lbs
west$VAR <- strawberries_var$west_lbs

p_sw <- data.frame(matrix(NA, nrow=nrow(strawberries_full)-1, ncol=6))
colnames(p_sw) <- c("year", "Full", "Linear", "Quadratic", "ARMA", "VAR")
p_sw$year <- strawberries_linear$year
p_sw$Full <- strawberries_full[2:nrow(strawberries_full),]$p_sw_lbs
p_sw$Linear <- strawberries_linear$p_sw_lbs
p_sw$Quadratic <- strawberries_quad$p_sw_lbs
p_sw$ARMA <- strawberries_arma$p_sw_lbs
p_sw$VAR <- strawberries_var$p_sw_lbs

mw_se <- data.frame(matrix(NA, nrow=nrow(strawberries_full)-1, ncol=6))
colnames(mw_se) <- c("year", "Full", "Linear", "Quadratic", "ARMA", "VAR")
mw_se$year <- strawberries_linear$year
mw_se$Full <- strawberries_full[2:nrow(strawberries_full),]$mw_se_lbs
mw_se$Linear <- strawberries_linear$mw_se_lbs
mw_se$Quadratic <- strawberries_quad$mw_se_lbs
mw_se$ARMA <- strawberries_arma$mw_se_lbs
mw_se$VAR <- strawberries_var$mw_se_lbs

a_ne <- data.frame(matrix(NA, nrow=nrow(strawberries_full)-1, ncol=6))
colnames(a_ne) <- c("year", "Full", "Linear", "Quadratic", "ARMA", "VAR")
a_ne$year <- strawberries_linear$year
a_ne$Full <- strawberries_full[2:nrow(strawberries_full),]$a_ne_lbs
a_ne$Linear <- strawberries_linear$a_ne_lbs
a_ne$Quadratic <- strawberries_quad$a_ne_lbs
a_ne$ARMA <- strawberries_arma$a_ne_lbs
a_ne$VAR <- strawberries_var$a_ne_lbs

west_half <- data.frame(matrix(NA, nrow=nrow(strawberries_full)-1, ncol=6))
colnames(west_half) <- c("year", "Full", "Linear", "Quadratic", "ARMA", "VAR")
west_half$year <- strawberries_linear$year
west_half$Full <- strawberries_full[2:nrow(strawberries_full),]$west_half_lbs
west_half$Linear <- strawberries_linear$west_half_lbs
west_half$Quadratic <- strawberries_quad$west_half_lbs
west_half$ARMA <- strawberries_arma$west_half_lbs
west_half$VAR <- strawberries_var$west_half_lbs

east_half <- data.frame(matrix(NA, nrow=nrow(strawberries_full)-1, ncol=6))
colnames(east_half) <- c("year", "Full", "Linear", "Quadratic", "ARMA", "VAR")
east_half$year <- strawberries_linear$year
east_half$Full <- strawberries_full[2:nrow(strawberries_full),]$east_half_lbs
east_half$Linear <- strawberries_linear$east_half_lbs
east_half$Quadratic <- strawberries_quad$east_half_lbs
east_half$ARMA <- strawberries_arma$east_half_lbs
east_half$VAR <- strawberries_var$east_half_lbs

west_melt <- melt(west, id.vars=1)
colnames(west_melt) <- c("Year", "Type", "value")

p_sw_melt <- melt(p_sw, id.vars=1)
colnames(p_sw_melt) <- c("Year", "Type", "value")

mw_se_melt <- melt(mw_se, id.vars=1)
colnames(mw_se_melt) <- c("Year", "Type", "value")

a_ne_melt <- melt(a_ne, id.vars=1)
colnames(a_ne_melt) <- c("Year", "Type", "value")

west_half_melt <- melt(west_half, id.vars=1)
colnames(west_half_melt) <- c("Year", "Type", "value")

east_half_melt <- melt(east_half, id.vars=1)
colnames(east_half_melt) <- c("Year", "Type", "value")

west_plot <- ggplot(west_melt, aes(x=Year, value / (10^8), color=Type, shape=Type)) + 
  geom_point(size=2) + 
  ggtitle("West Region") + 
  xlab("Year") + 
  ylab("Supply Predictions") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

p_sw_plot <- ggplot(p_sw_melt, aes(x=Year, value / (10^8), color=Type, shape=Type)) + 
  geom_point(size=2) + 
  ggtitle("Mountain Plains Region") + 
  xlab("Year") + 
  ylab("Supply Predictions") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

mw_se_plot <- ggplot(mw_se_melt, aes(x=Year, value / (10^8), color=Type, shape=Type)) + 
  geom_point(size=2) + 
  ggtitle("Midwest Region") + 
  xlab("Year") + 
  ylab("Supply Predictions") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

a_ne_plot <- ggplot(a_ne_melt, aes(x=Year, value / (10^8), color=Type, shape=Type)) + 
  geom_point(size=2) + 
  ggtitle("Mid-Atlantic Region") + 
  xlab("Year") + 
  ylab("Supply Predictions") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=14),
        axis.title.x = element_text(color = "grey20", size=14),
        axis.text.y = element_text(color = "grey20", size=14),
        axis.title.y = element_text(color = "grey20", size=14),
        legend.title = element_text(color = "grey20", size=14),
        legend.text = element_text(color = "grey20", size=14))

west_half_plot <- ggplot(west_half_melt, aes(x=Year, value / (10^8), color=Type, shape=Type)) + 
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

east_half_plot <- ggplot(east_half_melt, aes(x=Year, value / (10^8), color=Type, shape=Type)) + 
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

strawberries_pred <- grid.arrange(southeast_plot, southwest_plot, west_plot, nrow=2, ncol=2)
ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Predictions/strawberries_pred.jpg", 
       strawberries_pred, width=12, height=8, dpi=300)

strawberries_dual_pred <- grid.arrange(west_half_plot, east_half_plot, nrow=1, ncol=2)
ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/Supply Data/Predictions/strawberries_dual_pred.jpg", 
       strawberries_dual_pred, width=12, height=4, dpi=300)

west_plot <- ggplot(west_melt, aes(x=Year, value / (10^8), color=Type, shape=Type)) + 
  geom_point(size=3) + 
  ggtitle("West Region Predictions") + 
  xlab("Year") + 
  ylab("Supply Predictions (in 10^8 lbs)") + 
  theme(plot.title = element_text(size = 32, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=28),
        axis.title.x = element_text(color = "grey20", size=28),
        axis.text.y = element_text(color = "grey20", size=28),
        axis.title.y = element_text(color = "grey20", size=28),
        legend.title = element_text(color = "grey20", size=28),
        legend.text = element_text(color = "grey20", size=28))

p_sw_plot <- ggplot(p_sw_melt, aes(x=Year, value / (10^8), color=Type, shape=Type)) + 
  geom_point(size=3) + 
  ggtitle("Mountain Plains Region Predictions") + 
  xlab("Year") + 
  ylab("Supply Predictions (in 10^8 lbs)") + 
  theme(plot.title = element_text(size = 32, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=28),
        axis.title.x = element_text(color = "grey20", size=28),
        axis.text.y = element_text(color = "grey20", size=28),
        axis.title.y = element_text(color = "grey20", size=28),
        legend.title = element_text(color = "grey20", size=28),
        legend.text = element_text(color = "grey20", size=28))

mw_se_plot <- ggplot(mw_se_melt, aes(x=Year, value / (10^8), color=Type, shape=Type)) + 
  geom_point(size=3) + 
  ggtitle("Midwest-Southeast Region Predictions") + 
  xlab("Year") + 
  ylab("Supply Predictions (in 10^8 lbs)") + 
  theme(plot.title = element_text(size = 32, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=28),
        axis.title.x = element_text(color = "grey20", size=28),
        axis.text.y = element_text(color = "grey20", size=28),
        axis.title.y = element_text(color = "grey20", size=28),
        legend.title = element_text(color = "grey20", size=28),
        legend.text = element_text(color = "grey20", size=28))

a_ne_plot <- ggplot(a_ne_melt, aes(x=Year, value / (10^8), color=Type, shape=Type)) + 
  geom_point(size=3) + 
  ggtitle("Atlantic-Northeast Predictions") + 
  xlab("Year") + 
  ylab("Supply Predictions (in 10^8 lbs)") + 
  theme(plot.title = element_text(size = 32, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=28),
        axis.title.x = element_text(color = "grey20", size=28),
        axis.text.y = element_text(color = "grey20", size=28),
        axis.title.y = element_text(color = "grey20", size=28),
        legend.title = element_text(color = "grey20", size=28),
        legend.text = element_text(color = "grey20", size=28))

west_half_plot <- ggplot(west_half_melt, aes(x=Year, value / (10^8), color=Type, shape=Type)) + 
  geom_point(size=3) + 
  ggtitle("West Half of U.S. Predictions") + 
  xlab("Year") + 
  ylab("Supply Predictions (in 10^8 lbs)") + 
  theme(plot.title = element_text(size = 32, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=28),
        axis.title.x = element_text(color = "grey20", size=28),
        axis.text.y = element_text(color = "grey20", size=28),
        axis.title.y = element_text(color = "grey20", size=28),
        legend.title = element_text(color = "grey20", size=28),
        legend.text = element_text(color = "grey20", size=28))

east_half_plot <- ggplot(east_half_melt, aes(x=Year, value / (10^8), color=Type, shape=Type)) + 
  geom_point(size=3) + 
  ggtitle("East Half of U.S. Predictions") + 
  xlab("Year") + 
  ylab("Supply Predictions (in 10^8 lbs)") + 
  theme(plot.title = element_text(size = 32, face = "bold", hjust=0.5),
        axis.text.x = element_text(color = "grey20", size=28),
        axis.title.x = element_text(color = "grey20", size=28),
        axis.text.y = element_text(color = "grey20", size=28),
        axis.title.y = element_text(color = "grey20", size=28),
        legend.title = element_text(color = "grey20", size=28),
        legend.text = element_text(color = "grey20", size=28))

ggsave("/Users/liliannagittoes/Desktop/Senior Thesis/Presentation Figures/strawberries_west_multi_pred.jpg", 
       west_plot, width=12, height=8, dpi=300)

