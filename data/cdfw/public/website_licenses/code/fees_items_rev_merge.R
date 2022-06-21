
# Packages
library(tidyverse)
library(dplyr)

# Directories
datadir <- "data/cdfw/public/website_licenses/data/intermediate/combined_decades/"


#############################
# Fish Business

# Read data
data_fees <- read.csv(file.path(datadir, "AllDecadesFeesFishBusiness"))
data_items <- read.csv(file.path(datadir, "AllDecadesItemsFishBusiness"))
data_rev <- read.csv(file.path(datadir, "AllDecadesRevenueFishBusiness"))

# Combine data
fish_data <- cbind(data_fees, data_items$items, data_rev$revenues_usd, row.names = NULL) %>%
  rename("items"="data_items$items", "revenues_usd"="data_rev$revenues_usd")

# Save to csv
write.csv(fish_data, "CDFWFishBusinessRecords")

#############################
# Commercial Fishing

# Read data
data_fees <- read.csv(file.path(datadir, "AllDecadesFeesCommercialFishing"))
data_items <- read.csv(file.path(datadir, "AllDecadesItemsCommercialFishing"))
data_rev <- read.csv(file.path(datadir, "AllDecadesRevenueCommercialFishing"))

# Combine data
comm_data <- data_items %>%
  mutate(fees_usd=data_fees$fees_usd) %>%
  mutate(revenues_usd=data_rev$revenues_usd) 
  
# Save to csv
path <- "data/cdfw/public/website_licenses/data/processed/"
write.csv(comm_data, file.path(path, "CDFWCommercialFishingRecords"))

#############################
# Hunting

# Read data
data_fees <- read.csv(file.path(datadir, "AllDecadesFeesHunting"))
data_items <- read.csv(file.path(datadir, "AllDecadesItemsHunting"))
data_rev <- read.csv(file.path(datadir, "AllDecadesRevenueHunting"))

# Combine data
hunting_data <- data_items %>%
  mutate(fees_usd=data_fees$fees_usd) %>%
  mutate(revenues_usd=data_rev$revenues_usd) 

# Save to csv
path <- "data/cdfw/public/website_licenses/data/processed/"
write.csv(hunting_data, file.path(path, "CDFWHuntingRecords"))

#############################
# Special Permits

# Read data
data_fees <- read.csv(file.path(datadir, "AllDecadesFeesSpecialPermits"))
data_items <- read.csv(file.path(datadir, "AllDecadesItemsSpecialPermits"))
data_rev <- read.csv(file.path(datadir, "AllDecadesRevenueSpecialPermits"))

# Combine data
special_data <- cbind(data_fees, data_items$items, data_rev$revenues_usd, row.names = NULL) %>%
  rename("items"="data_items$items", "revenues_usd"="data_rev$revenues_usd")

# Save to csv
path <- "data/cdfw/public/website_licenses/data/processed/"
write.csv(special_data, file.path(path, "CDFWSpecialPermitsRecords"))

#############################
# Sport Fishing

# Read data
data_fees <- read.csv(file.path(datadir, "AllDecadesFeesSportFishing"))
data_items <- read.csv(file.path(datadir, "AllDecadesItemsSportFishing"))
data_rev <- read.csv(file.path(datadir, "AllDecadesRevenueSportFishing"))

# Combine data
sport_data <- cbind(data_fees, data_items$items, data_rev$revenues_usd, row.names = NULL) %>%
  rename("items"="data_items$items", "revenues_usd"="data_rev$revenues_usd")

# Save to csv
write.csv(sport_data, "CDFWSportFishingRecords")

