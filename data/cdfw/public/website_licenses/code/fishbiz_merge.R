
# Packages
library(tidyverse)
library(dplyr)

# Directories
datadir <- "data/cdfw/public/website_licenses/data/intermediate/combined_decades/"

# Read data
data_fees <- read.csv(file.path(datadir, "AllDecadesFeesFishBusiness"))
data_items <- read.csv(file.path(datadir, "AllDecadesItemsFishBusiness"))
data_rev <- read.csv(file.path(datadir, "AllDecadesRevenueFishBusiness"))

all_data <- cbind(data_fees, data_items$items, data_rev$revenues_usd, row.names = NULL) %>%
  rename("items"="data_items$items", "revenues_usd"="data_rev$revenues_usd")

write.csv("CDFWFishBusinessRecords")