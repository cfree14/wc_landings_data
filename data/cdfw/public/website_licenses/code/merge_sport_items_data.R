
# Packages
library(tidyverse)
library(dplyr)

# Directories
datadir <- "data/cdfw/public/website_licenses/data/intermediate/csvs/"

# Read data
data_1970 <- read.csv(file.path(datadir, "tabula-70s Items Sport Fishing.csv"), na.strings=c("N/A", "", "Not Avail."))
data_1980 <- read.csv(file.path(datadir, "tabula-80s Items Sport Fishing.csv"), na.strings=c("N/A", "", "Not Avail."))
data_1990 <- read.csv(file.path(datadir, "tabula-90s Items Sport Fishing.csv"), na.strings=c("N/A", "", "Not Avail."))
data_2000 <- read.csv(file.path(datadir, "tabula-2000s Items Sport Fishing.csv"), na.strings=c("N/A", "", "Not Avail."))
data_2010 <- read.csv(file.path(datadir, "tabula-2010s Items Sport Fish.csv"), na.strings=c("N/A", "", "Not Avail."))
data_2020 <- read.csv(file.path(datadir, "tabula-2020s Items Sport FishingADA.csv"), na.strings=c("N/A", "", "-"))

# Format 1970s data
data70 <- data_1970 %>%
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) %>%
  # Create new columns to label license categories
  mutate(category=ifelse(substr(license, 1, 3)=="Sub", substr(license,13,nchar(license)), NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category upwards
  fill(category, .direction="up") %>% 
  # Change category for sub total rows
  mutate(category=ifelse(substr(license, 1, 3)=="Sub", "Subtotal", category)) %>% 
  # Remove totals row
  filter(!grepl("TOTAL SPORT FISHING", license)) %>%
  # Add some useful columns
  mutate(decade="1970s",
         filename="tabula-70s Items Sport Fishing.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="items", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert items to numeric
  mutate(items=gsub(",|-", "", items) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

# Format 1980s data
data80 <- data_1980 %>%
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) %>%
  # Create new columns to label license categories
  mutate(category=ifelse(substr(license, 1, 3)=="Sub", substr(license,13,nchar(license)), NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category upwards
  fill(category, .direction="up") %>% 
  # Change category for sub total rows
  mutate(category=ifelse(substr(license, 1, 3)=="Sub", "Subtotal", category)) %>% 
  # Remove totals row
  filter(!grepl("TOTAL SPORT FISHING", license)) %>%
  # Add some useful columns
  mutate(decade="1980s",
         filename="tabula-80s Items Sport Fishing.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="items", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert items to numeric
  mutate(items=gsub(",|-", "", items) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

# Format 1990s data
data90 <- data_1990 %>%
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) %>%
  # Create new columns to label license categories
  mutate(category=ifelse(substr(license, 1, 3)=="Sub", substr(license,13,nchar(license)), NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category upwards
  fill(category, .direction="up") %>% 
  # Change category for sub total rows
  mutate(category=ifelse(substr(license, 1, 3)=="Sub", "Subtotal", category)) %>% 
  # Remove totals row
  filter(!grepl("TOTAL SPORT FISHING", license)) %>%
  # Add some useful columns
  mutate(decade="1990s",
         filename="tabula-90s Items Sport Fishing.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="items", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert items to numeric
  mutate(items=gsub(",|-", "", items) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

# Format 2000s data
data00 <- data_2000 %>% 
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) %>%
  # Create new columns to label license categories
  mutate(category=ifelse(substr(license, 1, 3)=="Sub", substr(license,13,nchar(license)), NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category upwards
  fill(category, .direction="up") %>% 
  # Change category for sub total rows
  mutate(category=ifelse(substr(license, 1, 3)=="Sub", "Subtotal", category)) %>% 
  # Remove totals row
  filter(!grepl("TOTAL SPORT FISHING", license)) %>%
  # Add some useful columns
  mutate(decade="2000s",
         filename="tabula-2000s Items Sport Fishing.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="items", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert items to numeric
  mutate(items=gsub(",|-", "", items) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

# Format 2010s data
data10 <- data_2010 %>%
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) %>%
  # Create new columns to label license categories
  mutate(category=ifelse(substr(license, 1, 3)=="Sub", substr(license,13,nchar(license)), NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category upwards
  fill(category, .direction="up") %>% 
  # Change category for sub total rows
  mutate(category=ifelse(substr(license, 1, 3)=="Sub", "Subtotal", category)) %>% 
  # Remove totals row
  filter(!grepl("TOTAL SPORT FISHING", license)) %>%
  # Add some useful columns
  mutate(decade="2010s",
         filename="tabula-2010s Items Sport Fish.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="items", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert items to numeric
  mutate(items=gsub(",|-", "", items) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

# Format 2020s data
data20 <- data_2020 %>%
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) %>%
  # Create new columns to label license categories
  mutate(category=ifelse(substr(license, 1, 3)=="Sub", substr(license,13,nchar(license)), NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category upwards
  fill(category, .direction="up") %>% 
  # Change category for sub total rows
  mutate(category=ifelse(substr(license, 1, 3)=="Sub", "Subtotal", category)) %>% 
  # Remove totals row
  filter(!grepl("TOTAL SPORT FISHING", license)) %>%
  # Add some useful columns
  mutate(decade="2020s",
         filename="tabula-2020s Items Sport FishingADA.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="items", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert items to numeric
  mutate(items=gsub(",|-", "", items) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

# Merge dataframes by rowbinding (since all columns are same)
all_data <- rbind(data70, data80, data90, data00, data10, data20)

# Clean data and save to csv
all_data <- all_data %>%
  # Fix spacing for license names
  mutate(license=recode(license, "Non-Resident Fishing     (Annual)"="Non-Resident Fishing (Annual)",
                        "Non-Resident Fishing (l0 Day)"="Non-Resident Fishing (10 Day)",
                        "Pacific Ocean   (1 Day)"="Pacific Ocean (1 Day)",
                        "Pacific Ocean   (3 Day)"="Pacific Ocean (3 Day)",
                        "Pacific Ocean  (5 Day)"="Pacific Ocean (5 Day)",
                        "Resident Fishing     (Annual)"="Resident Fishing (Annual)",
                        "Non-Resident Fishing     (l0 Day)"="Non-Resident Fishing (10 Day)")) %>%
  # Fix "Stamps" category names
  mutate(category=recode(category, "Stamps"="Validations & Report Cards",
                         "Stamps & Report Cards"="Validations & Report Cards")) %>%
  filter(category!="Subtotal")
write.csv(all_data, "AllDecadesItemsSportFishing")
