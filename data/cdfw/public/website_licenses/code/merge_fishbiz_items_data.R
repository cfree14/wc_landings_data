
# Packages
library(tidyverse)

# Directories
datadir <- "data/cdfw/public/website_licenses/data/intermediate/csvs/"

# Read data
data_1970 <- read.csv(file.path(datadir, "tabula-70s Items Fish Business.csv"), na.strings=c("N/A", ""))
data_1980 <- read.csv(file.path(datadir, "tabula-80s Items Fish Business.csv"), na.strings=c("N/A", ""))
data_1990 <- read.csv(file.path(datadir, "tabula-90s Items Fish Business.csv"), na.strings=c("N/A", ""))
data_2000 <- read.csv(file.path(datadir, "tabula-2000s Items Fish Business.csv"), na.strings=c("N/A", ""))
data_2010 <- read.csv(file.path(datadir, "tabula-2010s Items Fish Business.csv"), na.strings=c("N/A", ""))
data_2020 <- read.csv(file.path(datadir, "tabula-2020s Items Fish BusinessADA.csv"), na.strings=c("N/A", ""))

# Format 1970s data
data70 <- data_1970 %>%
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) %>%
  # Create new columns to label license categories
  mutate(category=ifelse(license==stringr::str_to_title(license), license, NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category downwards
  fill(category, .direction="down") %>% 
  # Fill in missing licence category
  mutate(category=ifelse(is.na(category), "Licenses", category)) %>% 
  # Remove header rows
  filter(license!=stringr::str_to_title(license)) %>% 
  # Remove total rows 
  filter(!grepl("TOTAL COMMERCIAL FISH BUSINESS", license)) %>%
  # Add some useful columns
  mutate(decade="1970s",
         filename="tabula-70s Items Fish Business.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="items", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year),
         year=recode(year, 
                     "1975.1"="1976",
                     "1977.1"="1979",
         ) %>% as.numeric()) %>% 
  # Convert items to numeric
  mutate(items=gsub("\\$|,|-", "", items) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

# Format 1980s data
data80 <- data_1980 %>%
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) %>%
  # Create new columns to label license categories
  mutate(category=ifelse(license==stringr::str_to_title(license), license, NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category downwards
  fill(category, .direction="down") %>% 
  # Fill in missing licence category
  mutate(category=ifelse(is.na(category), "Licenses", category)) %>% 
  # Remove header rows
  filter(license!=stringr::str_to_title(license)) %>% 
  # Remove total rows 
  filter(!grepl("TOTAL COMMERCIAL FISH BUSINESS", license)) %>%
  # Add some useful columns
  mutate(decade="1980s",
         filename="tabula-80s Items Fish Business.csv") %>% 
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
  mutate(category=ifelse(license==stringr::str_to_title(license), license, NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category downwards
  fill(category, .direction="down") %>% 
  # Fill in missing licence category
  mutate(category=ifelse(is.na(category), "Licenses", category)) %>% 
  # Remove header rows
  filter(license!=stringr::str_to_title(license)) %>% 
  # Remove total rows 
  filter(!grepl("TOTAL COMMERCIAL FISH BUSINESS", license)) %>%
  # Add some useful columns
  mutate(decade="1990s",
         filename="tabula-90s Items Fish Business.csv") %>% 
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
  mutate(category=ifelse(license==stringr::str_to_title(license), license, NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category downwards
  fill(category, .direction="down") %>% 
  # Fill in missing licence category
  mutate(category=ifelse(is.na(category), "Licenses", category)) %>% 
  # Remove header rows
  filter(license!=stringr::str_to_title(license)) %>% 
  # Remove total rows 
  filter(!grepl("TOTAL COMMERCIAL FISH BUSINESS", license)) %>%
  # Add some useful columns
  mutate(decade="2000s",
         filename="tabula-2000s Items Fish Business.csv") %>% 
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
  mutate(category=ifelse(license==stringr::str_to_title(license), license, NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category downwards
  fill(category, .direction="down") %>% 
  # Fill in missing licence category
  mutate(category=ifelse(is.na(category), "Licenses", category)) %>% 
  # Remove header rows
  filter(license!=stringr::str_to_title(license)) %>% 
  # Remove total rows 
  filter(!grepl("TOTAL COMMERCIAL FISH BUSINESS", license)) %>%
  # Add some useful columns
  mutate(decade="2010s",
         filename="tabula-2010s Items Fish Business.csv") %>% 
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
  mutate(category=ifelse(license==stringr::str_to_title(license), license, NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category downwards
  fill(category, .direction="down") %>% 
  # Fill in missing licence category
  mutate(category=ifelse(is.na(category), "Licenses", category)) %>% 
  # Remove header rows
  filter(license!=stringr::str_to_title(license)) %>% 
  # Remove total rows 
  filter(!grepl("TOTAL COMMERCIAL FISH BUSINESS", license)) %>%
  # Add some useful columns
  mutate(decade="2020s",
         filename="tabula-2020s Items Fish BusinessADA.csv") %>% 
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

# Save as csv
write.csv(all_data, "AllDecadesItemsFishBusiness")

