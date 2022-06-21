
# Packages
library(tidyverse)

# Directories
datadir <- "data/cdfw/public/website_licenses/data/intermediate/csvs/"

# Read data
data_1970 <- read.csv(file.path(datadir, "tabula-70s Items Hunting.csv"), na.strings=c("N/A", ""))
data_1980 <- read.csv(file.path(datadir, "tabula-80s Items Hunting.csv"), na.strings=c("N/A", ""))
data_1990 <- read.csv(file.path(datadir, "tabula-90s Items Hunting.csv"), na.strings=c("N/A", "", "Not Avail."))
data_2000 <- read.csv(file.path(datadir, "tabula-2000s Items Hunting.csv"), na.strings=c("N/A", ""))
data_2010 <- read.csv(file.path(datadir, "tabula-2010s Items Hunting.csv"), na.strings=c("N/A", "", "See Above"))
data_2020 <- read.csv(file.path(datadir, "tabula-2020s Items HuntingADA.csv"), na.strings=c("N/A", "","-"))


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
  filter(!grepl("TOTAL", license)) %>%
  # Add some useful columns
  mutate(decade="1970s",
         filename="tabula-70s Items Hunting.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="items", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert items to numeric
  mutate(items=gsub("\\$|,|-", "", items) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

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
  filter(!grepl("TOTAL HUNTING", license)) %>%
  # Add some useful columns
  mutate(decade="1980s",
         filename="tabula-80s Items Hunting.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="items", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert items to numeric
  mutate(items=gsub("\\$|,|-", "", items) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

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
  filter(!grepl("TOTAL HUNTING", license)) %>%
  # Add some useful columns
  mutate(decade="1990s",
         filename="tabula-90s Items Hunting.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="items", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert items to numeric
  mutate(items=gsub("\\$|,|-", "", items) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

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
  filter(!grepl("TOTAL HUNTING", license)) %>%
  # Add some useful columns
  mutate(decade="2000s",
         filename="tabula-2000s Items Hunting.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="items", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert items to numeric
  mutate(items=gsub("\\$|,|-", "", items) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

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
  filter(!grepl("TOTAL HUNTING", license)) %>%
  # Add some useful columns
  mutate(decade="2010s",
         filename="tabula-2010s Items Hunting.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="items", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert items to numeric
  mutate(items=gsub("\\$|,|-", "", items) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,


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
  filter(!grepl("TOTAL HUNTING", license)) %>%
  # Add some useful columns
  mutate(decade="2020s",
         filename="tabula-2020s Items Hunting.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="items", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert items to numeric
  mutate(items=gsub("\\$|,|-", "", items) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

# Merge dataframes by rowbinding (since all columns are same)
all_data <- rbind(data70, data80, data90, data00, data10, data20)

# Filter data
all_data <- all_data %>%
  filter(category!="Subtotal") %>%
  
  # Fix spacing in license column
  mutate(license=recode(license, "Junior Hunting     (Annual)"="Junior Hunting (Annual)",
                        "Non-Resident Hunting     (Annual)"="Non-Resident Hunting (Annual)",
                        "Non-Resident SecondDeer Tag"="Non-Resident Second Deer Tag",
                        "Resident Hunting     (Annual)"= "Resident Hunting (Annual)", 
                        "Resident Hunting(Annual)"= "Resident Hunting (Annual)",
                        "Junior Hunting(Annual)"="Junior Hunting (Annual)",
                        "Non-Resident Hunting(Annual)"="Non-Resident Hunting (Annual)",
                        "Non-Resident Hunting     (1 Day)"= "Non-Resident Hunting (1 Day)")) %>%
  # Correct "Bear" category 
  mutate(category=recode(category, "Bear"="Bear Tags", "Bobcat tags"="Bobcat Tags"))

# Save as csv
write.csv(all_data, "AllDecadesItemsHunting")
