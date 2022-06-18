
# Packages
library(tidyverse)
library(dplyr)

# Directories
datadir <- "data/cdfw/public/website_licenses/data/intermediate/csvs/"

# Read data
data_1970 <- read.csv(file.path(datadir, "tabula-70s Fees Sport Fishing.csv"), na.strings=c("N/A", ""))
data_1980 <- read.csv(file.path(datadir, "tabula-80s Fees Sport Fishing.csv"), na.strings=c("N/A", ""))
data_1990 <- read.csv(file.path(datadir, "tabula-90s Fees Sport Fishing.csv"), na.strings=c("N/A", ""))
data_2000 <- read.csv(file.path(datadir, "tabula-2000s Fees Sport Fishing.csv"), na.strings=c("N/A", ""))
data_2010 <- read.csv(file.path(datadir, "tabula-2010s Fees Sport Fishing.csv"), na.strings=c("N/A", ""))
data_2020 <- read.csv(file.path(datadir, "tabula-2020s Fees Sport FishingADA.csv"), na.strings=c("N/A", ""))

# Read category data 
cat70 <- read.csv(file.path(datadir, "tabula-70s Revenue Sport Fishing.csv"), na.strings=c("N/A", ""))
cat80 <- read.csv(file.path(datadir, "tabula-80s Revenue Sport Fishing.csv"), na.strings=c("N/A", ""))
cat90 <- read.csv(file.path(datadir, "tabula-90s Revenue Sport Fishing.csv"), na.strings=c("N/A", ""))
cat00 <- read.csv(file.path(datadir, "tabula-2000s Revenue Sport Fishing.csv"), na.strings=c("N/A", ""))
cat10 <- read.csv(file.path(datadir, "tabula-2010s Revenue Sport Fish.csv"), na.strings=c("N/A", ""))
cat20 <- read.csv(file.path(datadir, "tabula-2020s Revenue Sport FishingADA.csv"), na.strings=c("N/A", "", "-"))


# Process revenue data so categories are accessible 
cat7 <- cat70 %>%
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
  # Remove subtotals and totals rows
  filter(!grepl("Subtotal", category)) %>%
  filter(!grepl("TOTAL SPORT FISHING", license)) 
cat8 <- cat80 %>%
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
  # Remove subtotals and totals rows
  filter(!grepl("Subtotal", category)) %>%
  filter(!grepl("TOTAL SPORT FISHING", license)) 
cat9 <- cat90 %>%
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
  # Remove subtotals and totals rows
  filter(!grepl("Subtotal", category)) %>%
  filter(!grepl("TOTAL SPORT FISHING", license)) 
cat0 <- cat00 %>%
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
  # Remove subtotals and totals rows
  filter(!grepl("Subtotal", category)) %>%
  filter(!grepl("TOTAL SPORT FISHING", license)) 
cat1 <- cat10 %>%
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
  # Remove subtotals and totals rows and extra row that isn't in fees df
  filter(!grepl("Subtotal", category)) %>%
  filter(!grepl("TOTAL SPORT FISHING", license)) %>%
  filter(!grepl("Lifetime Fishing Privilege Package", license))
cat2 <- cat20 %>%
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) %>%
  # Remove extra columns
  select(-c(X:X.10)) %>%
  # Create new columns to label license categories
  mutate(category=ifelse(substr(license, 1, 3)=="Sub", substr(license,13,nchar(license)), NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category upwards
  fill(category, .direction="up") %>% 
  # Change category for sub total rows
  mutate(category=ifelse(substr(license, 1, 3)=="Sub", "Subtotal", category)) %>% 
  # Remove subtotals and totals rows
  filter(!grepl("Subtotal", category)) %>%
  filter(!grepl("TOTAL SPORT FISHING", license)) 

# Format 1970s data
data70 <- data_1970 %>%
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) %>%
  # Extract category column from revenue dataframe
  mutate(category=cat7$category) %>%
  # Remove totals row
  filter(!grepl("TOTAL SPORT FISHING", license)) %>%
  # Add some useful columns
  mutate(decade="1970s",
         filename="tabula-70s Fees Sport Fishing.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="fees_usd", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert fees to numeric
  mutate(fees_usd=gsub("\\$|,|-", "", fees_usd) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

# Format 1980s data
data80 <- data_1980 %>%
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) %>%
  # Extract category column from revenue dataframe
  mutate(category=cat8$category) %>%
  # Remove totals row
  filter(!grepl("TOTAL SPORT FISHING", license)) %>%
  # Add some useful columns
  mutate(decade="1980s",
         filename="tabula-80s Fees Sport Fishing.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="fees_usd", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert fees to numeric
  mutate(fees_usd=gsub("\\$|,|-", "", fees_usd) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

# Format 1990s data
data90 <- data_1990 %>%
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) %>%
  # Extract category column from revenue dataframe
  mutate(category=cat9$category) %>%
  # Remove totals row
  filter(!grepl("TOTAL SPORT FISHING", license)) %>%
  # Add some useful columns
  mutate(decade="1990s",
         filename="tabula-90s Fees Sport Fishing.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="fees_usd", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert fees to numeric
  mutate(fees_usd=gsub("\\$|,|-", "", fees_usd) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

# Format 2000s data
data00 <- data_2000 %>%
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) %>%
  # Extract category column from revenue dataframe
  mutate(category=cat0$category) %>%
  # Remove totals row
  filter(!grepl("TOTAL SPORT FISHING", license)) %>%
  # Add some useful columns
  mutate(decade="2000s",
         filename="tabula-2000s Fees Sport Fishing.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="fees_usd", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert fees to numeric
  mutate(fees_usd=gsub("\\$|,|-", "", fees_usd) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

# Format 2010s data
data10 <- data_2010 %>%
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) %>%
  # Extract category column from revenue dataframe
  mutate(category=cat1$category) %>%
  # Remove totals row
  filter(!grepl("TOTAL SPORT FISHING", license)) %>%
  # Add some useful columns
  mutate(decade="2010s",
         filename="tabula-2010s Fees Sport Fishing.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="fees_usd", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert fees to numeric
  mutate(fees_usd=gsub("\\$|,|-", "", fees_usd) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

# Format 2020s data
data20 <- data_2020 %>%
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) %>%
  # Remove extra columns 
  select(-c(X:X.9)) %>%
  # Extract category column from revenue dataframe
  mutate(category=cat2$category) %>%
  # Remove totals row
  filter(!grepl("TOTAL SPORT FISHING", license)) %>%
  # Add some useful columns
  mutate(decade="2020s",
         filename="tabula-2020s Fees Sport Fishing.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="fees_usd", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert fees to numeric
  mutate(fees_usd=gsub("\\$|,|-", "", fees_usd) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

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
  # Fix "Stamp" category names
  mutate(category=recode(category, "Stamps"="Stamps & Report Cards")) %>%
  filter(category!="Subtotal")
write.csv(all_data, "AllDecadesRevenueSportFishing")


