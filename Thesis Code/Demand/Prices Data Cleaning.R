# data cleaning for wholesale prices

# read in files
wholesale_prices <- read.csv("Desktop/Senior Thesis/wholesale_prices.csv")
fresh_supply <- read.csv("Desktop/Senior Thesis/fresh_production.csv")
exports <- read.csv("Desktop/Senior Thesis/all_exports.csv")
imports <- read.csv("Desktop/Senior Thesis/all_imports.csv")

# read in WWEIA data with WWEIA codes (to get list of desired foods)
wweia_codes <- read_csv("Desktop/Senior Thesis/WWEIA_codes.csv")
food_list <- colnames(wweia_codes)

# convert empty spaces to NAs
wholesale_prices[wholesale_prices==""] <- NA
fresh_supply[fresh_supply==""] <- NA
exports[exports==""] <- NA
imports[imports==""] <- NA

# create prices data frame for foods in WWEIA list
wweia_prices <- data.frame(matrix(NA, nrow=nrow(wholesale_prices), ncol=length(food_list)+1))
colnames(wweia_prices) <- c("year", food_list)
wweia_prices$year <- wholesale_prices$year

# now transfer over wholesale prices that we do not need to make any calculations for
food_list_nc <- food_list[food_list %in% colnames(wholesale_prices)]
wweia_prices[,food_list_nc] <- wholesale_prices[,food_list_nc]

wweia_prices$PeachesNectarines <- wholesale_prices$Peaches
wweia_prices$OtherBerries <- wholesale_prices$Blueberries
wweia_prices$StringBeans <- wholesale_prices$SnapBeans
wweia_prices$Citrus <- wholesale_prices$Oranges

# Melons
cantaloupe_supply <- fresh_supply$Cantaloupe[2:nrow(fresh_supply)]
cantaloupe_supply <- as.numeric(cantaloupe_supply) * 10^6
honeydew_supply <- fresh_supply$Honeydew[2:nrow(fresh_supply)]
honeydew_supply <- as.numeric(honeydew_supply) * 10^6
watermelon_supply <- fresh_supply$Watermelon[2:nrow(fresh_supply)]
watermelon_supply <- as.numeric(watermelon_supply) * 10^6

cantaloupe_prices <- wholesale_prices$Cantaloupe[2:nrow(wholesale_prices)]
cantaloupe_prices <- as.numeric(cantaloupe_prices) / 100
honeydew_prices <- wholesale_prices$Honeydew[2:nrow(wholesale_prices)]
honeydew_prices <- as.numeric(honeydew_prices) / 100
watermelon_prices <- wholesale_prices$Watermelon[2:nrow(wholesale_prices)]
watermelon_prices <- as.numeric(watermelon_prices) / 100

wweia_prices$Melons[1] <- "cwt"
wweia_prices$Melons[2:nrow(wweia_prices)] <- 100 * ((cantaloupe_supply * cantaloupe_prices) + (honeydew_supply * honeydew_prices) + (watermelon_supply * watermelon_prices)) / (cantaloupe_supply + honeydew_supply + watermelon_supply)

# Other fruit
other_fruit <- c("SweetCherries", "TartCherries", "Kiwis")
SweetCherries_supply <- fresh_supply$SweetCherries[2:nrow(fresh_supply)]
SweetCherries_supply <- as.numeric(SweetCherries_supply) * 2000
TartCherries_supply <- fresh_supply$TartCherries[2:nrow(fresh_supply)]
TartCherries_supply <- as.numeric(TartCherries_supply) * 10^6 
Kiwis_supply <- fresh_supply$Kiwis[2:nrow(fresh_supply)]
Kiwis_supply <- as.numeric(Kiwis_supply) * 2000

SweetCherries_prices <- wholesale_prices$SweetCherries[2:nrow(wholesale_prices)]
SweetCherries_prices <- as.numeric(SweetCherries_prices) / 2000
TartCherries_prices <- wholesale_prices$TartCherries[2:nrow(wholesale_prices)]
TartCherries_prices <- as.numeric(TartCherries_prices) / 100
Kiwis_prices <- wholesale_prices$Kiwis[2:nrow(wholesale_prices)]
Kiwis_prices <- as.numeric(Kiwis_prices) / 2000

wweia_prices$OtherFruit[1] <- "cwt"
wweia_prices$OtherFruit[2:nrow(wweia_prices)] <- 100 * ((SweetCherries_supply * SweetCherries_prices) + (TartCherries_supply * TartCherries_prices) + (Kiwis_supply * Kiwis_prices)) / (SweetCherries_supply + TartCherries_supply + Kiwis_supply)

# Lettuce
HeadLettuce_supply <- fresh_supply$HeadLettuce[2:nrow(fresh_supply)]
HeadLettuce_supply <- as.numeric(HeadLettuce_supply) * 10^6
LeafRomaine_supply <- fresh_supply$LeafRomaine[2:nrow(fresh_supply)]
LeafRomaine_supply <- as.numeric(LeafRomaine_supply) * 10^6

HeadLettuce_prices <- wholesale_prices$HeadLettuce[2:nrow(wholesale_prices)]
HeadLettuce_prices <- as.numeric(HeadLettuce_prices) / 100
LeafRomaine_prices <- wholesale_prices$LeafRomaine[2:nrow(wholesale_prices)]
LeafRomaine_prices <- as.numeric(LeafRomaine_prices) / 100

wweia_prices$Lettuce[1] <- "cwt"
wweia_prices$Lettuce[2:nrow(wweia_prices)] <- 100 * ((HeadLettuce_supply * HeadLettuce_prices) + (LeafRomaine_supply * LeafRomaine_prices)) / (HeadLettuce_supply + LeafRomaine_supply)

# Other Vegetables
other_veg <- c("Artichokes", "Asparagus", "BrusselsSprouts", "Celery", "Cucumber", "Eggplant", 
               "Mushrooms", "BellPeppers", "Pumpkins", "Radishes", "Squash", "SweetPotatoes")

Artichokes_supply <- fresh_supply$Artichokes[2:nrow(fresh_supply)]
Artichokes_supply <- as.numeric(Artichokes_supply) * 10^6
Asparagus_supply <- fresh_supply$Asparagus[2:nrow(fresh_supply)]
Asparagus_supply <- as.numeric(Asparagus_supply) * 10^6
BrusselsSprouts_supply <- fresh_supply$BrusselsSprouts[2:nrow(fresh_supply)]
BrusselsSprouts_supply <- as.numeric(BrusselsSprouts_supply) * 10^6
Celery_supply <- fresh_supply$Celery[2:nrow(fresh_supply)]
Celery_supply <- as.numeric(Celery_supply) * 10^6
Cucumber_supply <- fresh_supply$Cucumber[2:nrow(fresh_supply)]
Cucumber_supply <- as.numeric(Cucumber_supply) * 10^6
Eggplant_supply <- fresh_supply$Eggplant[2:nrow(fresh_supply)]
Eggplant_supply <- as.numeric(Eggplant_supply) * 10^6
Mushrooms_supply <- fresh_supply$Mushrooms[2:nrow(fresh_supply)]
Mushrooms_supply <- as.numeric(Mushrooms_supply) * 10^6
BellPeppers_supply <- fresh_supply$BellPeppers[2:nrow(fresh_supply)]
BellPeppers_supply <- as.numeric(BellPeppers_supply) * 10^6
Pumpkins_supply <- fresh_supply$Pumpkins[2:nrow(fresh_supply)]
Pumpkins_supply <- as.numeric(Pumpkins_supply) * 10^6
Radishes_supply <- fresh_supply$Radishes[2:nrow(fresh_supply)]
Radishes_supply <- as.numeric(Radishes_supply) * 10^6
Squash_supply <- fresh_supply$Squash[2:nrow(fresh_supply)]
Squash_supply <- as.numeric(Squash_supply) * 10^6
SweetPotatoes_supply <- fresh_supply$SweetPotatoes[2:nrow(fresh_supply)]
SweetPotatoes_supply <- as.numeric(SweetPotatoes_supply) * 10^6

Artichokes_prices <- wholesale_prices$Artichokes[2:nrow(wholesale_prices)]
Artichokes_prices <- as.numeric(Artichokes_prices) / 100
Asparagus_prices <- wholesale_prices$Asparagus[2:nrow(wholesale_prices)]
Asparagus_prices <- as.numeric(Asparagus_prices) / 100
BrusselsSprouts_prices <- wholesale_prices$BrusselsSprouts[2:nrow(wholesale_prices)]
BrusselsSprouts_prices <- as.numeric(BrusselsSprouts_prices) / 100
Celery_prices <- wholesale_prices$Celery[2:nrow(wholesale_prices)]
Celery_prices <- as.numeric(Celery_prices) / 100
Cucumber_prices <- wholesale_prices$Cucumber[2:nrow(wholesale_prices)]
Cucumber_prices <- as.numeric(Cucumber_prices) / 100
Eggplant_prices <- wholesale_prices$Eggplant[2:nrow(wholesale_prices)]
Eggplant_prices <- as.numeric(Eggplant_prices) / 100
Mushrooms_prices <- wholesale_prices$Mushrooms[2:nrow(wholesale_prices)]
Mushrooms_prices <- as.numeric(Mushrooms_prices) / 100
BellPeppers_prices <- wholesale_prices$BellPeppers[2:nrow(wholesale_prices)]
BellPeppers_prices <- as.numeric(BellPeppers_prices) / 100
Pumpkins_prices <- wholesale_prices$Pumpkins[2:nrow(wholesale_prices)]
Pumpkins_prices <- as.numeric(Pumpkins_prices) / 100
Radishes_prices <- wholesale_prices$Radishes[2:nrow(wholesale_prices)]
Radishes_prices <- as.numeric(Radishes_prices) / 100
Squash_prices <- wholesale_prices$Squash[2:nrow(wholesale_prices)]
Squash_prices <- as.numeric(Squash_prices) / 100
SweetPotatoes_prices <- wholesale_prices$SweetPotatoes[2:nrow(wholesale_prices)]
SweetPotatoes_prices <- as.numeric(SweetPotatoes_prices) / 100

wweia_prices$OtherVeg[1] <- "cwt"
wweia_prices$OtherVeg[2:nrow(wweia_prices)] <- 100 * ((Artichokes_supply * Artichokes_prices) + (Asparagus_supply * Asparagus_prices) + (BrusselsSprouts_supply * BrusselsSprouts_prices) + 
  (Celery_supply * Celery_prices) + (Cucumber_supply * Cucumber_prices) + (Eggplant_supply * Eggplant_prices) + 
  (Mushrooms_supply * Mushrooms_prices) + (BellPeppers_supply * BellPeppers_prices) + (Pumpkins_supply * Pumpkins_prices) + 
  (Radishes_supply * Radishes_prices) + (Squash_supply * Squash_prices) + (SweetPotatoes_supply * SweetPotatoes_prices)) / 
  (Artichokes_supply + Asparagus_supply + BrusselsSprouts_supply + Celery_supply + Cucumber_supply + Eggplant_supply + 
     Mushrooms_supply + BellPeppers_supply + Pumpkins_supply + Radishes_supply + Squash_supply + SweetPotatoes_supply)

# convert units so that all prices are in terms of cwt
wweia_prices$Citrus[1] <- "cwt"
citrus_prices <- as.numeric(wweia_prices$Citrus[2:nrow(wweia_prices)])
wweia_prices$Citrus[2:nrow(wweia_prices)] <- citrus_prices * 100 / 80

wweia_prices$Pears[1] <- "cwt"
Pears_prices <- as.numeric(wweia_prices$Pears[2:nrow(wweia_prices)])
wweia_prices$Pears[2:nrow(wweia_prices)] <- Pears_prices * 100 / 2000

wweia_prices$Grapes[1] <- "cwt"
Grapes_prices <- as.numeric(wweia_prices$Grapes[2:nrow(wweia_prices)])
wweia_prices$Grapes[2:nrow(wweia_prices)] <- Grapes_prices * 100 / 2000

# fill in NA values with simple linear interpolation
banana_prices <- as.numeric(wweia_prices$Bananas[2:nrow(wweia_prices)])
banana_prices[23] <- 0.5 * (banana_prices[22] + banana_prices[24])
wweia_prices$Bananas[2:nrow(wweia_prices)] <- banana_prices

# write to csv
write.csv(wweia_prices, "Desktop/Senior Thesis/wweia_prices.csv", row.names = FALSE)


