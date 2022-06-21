
# Packages
library(tidyverse)

# Directories
datadir <- "data/cdfw/public/website_licenses/data/intermediate/csvs/"

# Read data
data_1970 <- read.csv(file.path(datadir, "tabula-70s Items Commercial Fishing.csv"), na.strings=c("N/A", "", NA))
data_1980 <- read.csv(file.path(datadir, "tabula-80s Items Commercial Fishing.csv"), na.strings=c("N/A", "", NA))
data_1990 <- read.csv(file.path(datadir, "tabula-90s Items Commercial Fishing.csv"), na.strings=c("N/A", "", NA))
data_2000 <- read.csv(file.path(datadir, "tabula-2000s Items Commercial Fishing.csv"), na.strings=c("N/A", "", NA))
data_2010 <- read.csv(file.path(datadir, "tabula-2010s Items Commercial Fish.csv"), na.strings=c("N/A", "", NA))
data_2020 <- read.csv(file.path(datadir, "tabula-2020s Items Commercial FishingADA.csv"), na.strings=c("N/A", "", NA))

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
  # Remove totals row
  filter(!grepl("TOTAL", license)) %>%
  # Add some useful columns
  mutate(decade="1970s",
         filename="tabula-70s Items Commercial Fishing.csv") %>% 
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
  # Create new columns to label license categories
  mutate(category=ifelse(license==stringr::str_to_title(license), license, NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category downwards
  fill(category, .direction="down") %>% 
  # Fill in missing licence category
  mutate(category=ifelse(is.na(category), "Licenses", category)) %>% 
  # Remove header rows
  filter(license!=stringr::str_to_title(license)) %>% 
  # Remove totals row
  filter(!grepl("TOTAL COMMERCIAL FISHING", license)) %>%
  # Add some useful columns
  mutate(decade="1980s",
         filename="tabula-80s Items Commercial Fishing.csv") %>% 
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
  # Create new columns to label license categories
  mutate(category=ifelse(license==stringr::str_to_title(license), license, NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category downwards
  fill(category, .direction="down") %>% 
  # Fill in missing licence category
  mutate(category=ifelse(is.na(category), "Licenses", category)) %>% 
  # Remove header rows
  filter(license!=stringr::str_to_title(license)) %>% 
  # Remove totals row
  filter(!grepl("TOTAL COMMERCIAL FISHING", license)) %>%
  # Add some useful columns
  mutate(decade="1990s",
         filename="tabula-90s Items Commercial Fishing.csv") %>% 
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
  # Create new columns to label license categories
  mutate(category=ifelse(license==stringr::str_to_title(license), license, NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category downwards
  fill(category, .direction="down") %>% 
  # Fill in missing licence category
  mutate(category=ifelse(is.na(category), "Licenses", category)) %>% 
  # Remove header rows
  filter(license!=stringr::str_to_title(license)) %>% 
  # Remove totals row
  filter(!grepl("TOTAL COMMERCIAL FISHING", license)) %>%
  # Add some useful columns
  mutate(decade="2000s",
         filename="tabula-2000s Items Commercial Fishing.csv") %>% 
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
  # Remove totals row
  filter(!grepl("TOTAL COMMERCIAL FISHING", license)) %>%
  # Add some useful columns
  mutate(decade="2010s",
         filename="tabula-2010s Items Commercial Fishing.csv") %>% 
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
  filter_all(any_vars(!is.na(.))) %>%
  # Remove extra columns 
  select(-c(X, X.1, X.2, X.3, X.4, X.5, X.6)) %>%
  # Remove 2029 rows
  filter_all(any_vars(license!="2029"&license!="-")) %>%
  # Create new columns to label license categories
  mutate(category=ifelse(license==stringr::str_to_title(license), license, NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category downwards
  fill(category, .direction="down") %>% 
  # Fill in missing licence category
  mutate(category=ifelse(is.na(category), "Licenses", category)) %>% 
  # Remove header rows
  filter(license!=stringr::str_to_title(license)) %>% 
  # Remove totals row
  filter(!grepl("Total Commercial Fishing", license)) %>%
  # Add some useful columns
  mutate(decade="2020s",
         filename="tabula-2020s Items Commercial Fishing.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="items", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert items to numeric
  mutate(items=gsub(",|-", "", items) %>% trimws() %>% as.numeric()) # | = or, so replacing both - and ,

# Merge dataframes by rowbinding (since all columns are same)
all_data <- rbind(data70, data80, data90, data00, data10, data20) %>%

# Fix misspellings in license category
mutate(license=recode(license, 
                      "HERRING GILL NET PERMIT(R)"="HERRING GILL NET PERMIT (R)",
                      "HERRING GILL NET PERMIT(NR)"="HERRING GILL NET PERMIT (NR)",
                      "CALIFORNIA HALIBUT BOTTOM TRAWL VESSEL PERMI"=
                        "CALIFORNIA HALIBUT BOTTOM TRAWL VESSEL PERMIT",
                      "HERRING GILL NET PERMIT(R)"="HERRING GILL NET PERMIT (R)",
                      "HERRING GILL NET PERMIT(NR)"="HERRING GILL NET PERMIT (NR)",
                      "MARKET SQUID VESSSEL PERMIT, EXPERIMENTAL (NT"= 
                        "MARKET SQUID VESSSEL PERMIT, EXPERIMENTAL (NT)",
                      "NEARSHORE TRAP ENDORSEMENT - N. CENTRAL COA"=
                        "NEARSHORE TRAP ENDORSEMENT - N. CENTRAL COAST",
                      "NEARSHORE TRAP ENDORSEMENT - S. CENTRAL COA"=
                        "NEARSHORE TRAP ENDORSEMENT - S. CENTRAL COAST",
                      "SPOT PRAWN OBSERVER 1,000-9,999 POUNDS (TRAWL"=
                        "SPOT PRAWN OBSERVER 1,000-9,999 POUNDS (TRAWL)",
                      "NEARSHORE FISHERY TRAP ENDORSEMENT TRANSFE"=
                        "NEARSHORE FISHERY TRAP ENDORSEMENT TRANSFER",
                      "NORTHERN PINK SHRIMP TRAWL (VESSEL) (NEW OWN"=
                        "NORTHERN PINK SHRIMP TRAWL (VESSEL) (NEW OWNER)",
                      "NORTHERN PINK SHRIMP TRAWL (VESSEL) (SAME OW"=
                        "NORTHERN PINK SHRIMP TRAWL (VESSEL) (SAME OWNER)")) 
# Save to csv
path <- "data/cdfw/public/website_licenses/data/intermediate/combined_decades/"
write.csv(all_data, file.path(path, "AllDecadesItemsCommercialFishing"))

