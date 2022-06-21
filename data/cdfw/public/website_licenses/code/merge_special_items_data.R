# Packages
library(tidyverse)
library(dplyr)

# Directories
datadir <- "data/cdfw/public/website_licenses/data/intermediate/csvs/"

# Read data
data_1970 <- read.csv(file.path(datadir, "tabula-70s Items Special Permits.csv"), na.strings=c("ea N/A","N/A", ""))
data_1980 <- read.csv(file.path(datadir, "tabula-80s Items Special Permits.csv"), na.strings=c("N/A", ""))
data_1990 <- read.csv(file.path(datadir, "tabula-90s Items Special Permits.csv"), na.strings=c("N/A", "", "No Data"))
data_2000 <- read.csv(file.path(datadir, "tabula-2000s Items Special Permits.csv"), na.strings=c("N/A", ""))
data_2010 <- read.csv(file.path(datadir, "tabula-2010s Items Special Permits.csv"), na.strings=c("N/A", ""))
data_2020 <- read.csv(file.path(datadir, "tabula-2020s Items Special PermitsADA.csv"), na.strings=c("N/A", "", "-"))

# Format 1970s data
data70 <- data_1970 %>%
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) %>%
  # Create license category
  mutate(category="Licenses") %>% 
  # Remove totals row
  filter(!grepl("TOTAL MISCELLANEOUS", license)) %>%
  # Add some useful columns
  mutate(decade="1970s",
         filename="tabula-70s Items Special Permits.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="items", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert items to numeric
  mutate(items=gsub(",|-", "", items) %>% trimws() %>% as.numeric()) # | = or, so replacing both - and ,

# Format 1980s data
data80 <- data_1980 %>%
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) %>%
  # Create license category
  mutate(category="Licenses") %>% 
  # Remove totals row
  filter(!grepl("TOTAL MISCELLANEOUS", license)) %>%
  # Add some useful columns
  mutate(decade="1980s",
         filename="tabula-80s Items Special Permits.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="items", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert items to numeric
  mutate(items=gsub(",|-", "", items) %>% trimws() %>% as.numeric()) # | = or, so replacing both - and ,

# Format 1990s data
data90 <- data_1990 %>%
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) %>%
  # Create license category
  mutate(category="Licenses") %>% 
  # Remove totals row
  filter(!grepl("TOTAL MISCELLANEOUS", license)) %>%
  # Add some useful columns
  mutate(decade="1990s",
         filename="tabula-90s Items Special Permits.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="items", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert items to numeric
  mutate(items=gsub(",|-", "", items) %>% trimws() %>% as.numeric()) # | = or, so replacing both - and ,


# Format 2000s data
data00 <- data_2000 %>%
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) %>%
  # Create license category
  mutate(category="Licenses") %>% 
  # Remove totals row
  filter(!grepl("TOTAL MISCELLANEOUS", license)) %>%
  # Add some useful columns
  mutate(decade="2000s",
         filename="tabula-2000s Items Special Permits.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="items", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert items to numeric
  mutate(items=gsub(",|-", "", items) %>% trimws() %>% as.numeric()) # | = or, so replacing both - and ,

# Format 2010s data
data10 <- data_2010 %>%
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) 
  # Add missing rows to match revenue dataframe
  newrow <- c("Restricted Species Permit - Breeding", rep(NA,10))
  data10 <- rbind(data10[1:45,], newrow, data10[-(1:45),])
  newrow2 <- c("Scientific Collecting Permit General Use  - Marine (Student)", rep(NA,10))
  data10 <- rbind(data10[1:91,], newrow2, data10[-(1:91),]) %>%
  # Create license category
  mutate(category="Licenses") %>% 
  # Remove totals row
  filter(!grepl("TOTALSPECIAL PERMITS", license)) %>%
  # Add some useful columns
  mutate(decade="2010s",
         filename="tabula-2010s Items Special Permits.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="items", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert items to numeric
  mutate(items=gsub(",|-", "", items) %>% trimws() %>% as.numeric()) # | = or, so replacing both - and ,

# Format 2020s data
data20 <- data_2020 %>%
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) 
  # Add rows to match items dataframe
  newrow <- c("Scientific Collecting Permit General Use Application Fee Inland Fisheries", rep(NA,10))
  data20 <- rbind(data20[1:66,], newrow, data20[-(1:66),]) %>%
  # Create license category
  mutate(category="Licenses") %>% 
  # Remove totals row
  filter(!grepl("TOTAL MISCELLANEOUS", license)) %>%
  # Add some useful columns
  mutate(decade="2020s",
         filename="tabula-2020s Items Special Permits.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="items", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert items to numeric
  mutate(items=gsub(",|-", "", items) %>% trimws() %>% as.numeric()) # | = or, so replacing both - and ,


# Merge dataframes by rowbinding (since all columns are same)
all_data <- rbind(data70, data80, data90, data00, data10, data20)

# Fixing cut-off license names
all_data <- all_data %>%
  mutate(license=recode(license, "Licensed Domesticated Migratory Game Bird Shooting A"=
                          "Licensed Domesticated Migratory Game Bird Shooting Area",
                        "Aquaculture Registration - Interest (per month )"=
                          "Aquaculture Registration - Interest (per month)"))

# Save to csv
path <- "data/cdfw/public/website_licenses/data/intermediate/combined_decades/"
write.csv(all_data, file.path(path, "AllDecadesItemsSpecialPermits"))
