
# Packages
library(tidyverse)

# Directories
datadir <- "data/cdfw/public/website_licenses/data/intermediate/csvs/"

# Read data
data_1970 <- read.csv(file.path(datadir, "tabula-70s Fees Commercial Fishing.csv"), na.strings=c("N/A", "", NA))
data_1980 <- read.csv(file.path(datadir, "tabula-80s Fees Commercial Fishing.csv"), na.strings=c("N/A", "", NA))
data_1990 <- read.csv(file.path(datadir, "tabula-90s Fees Commercial Fishing.csv"), na.strings=c("N/A", "", NA))
data_2000 <- read.csv(file.path(datadir, "tabula-2000s Fees Commercial Fishing.csv"), na.strings=c("N/A", "", NA))
data_2010 <- read.csv(file.path(datadir, "tabula-2010s Fees Commercial Fishing.csv"), na.strings=c("N/A", "", NA))
data_2020 <- read.csv(file.path(datadir, "tabula-2020s Fees Commercial FishingADA.csv"), na.strings=c("N/A", "", NA))

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
  # Add some useful columns
  mutate(decade="1970s",
         filename="tabula-70s Fees Commercial Fishing.csv") %>% 
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
  # Create new columns to label license categories
  mutate(category=ifelse(license==stringr::str_to_title(license), license, NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category downwards
  fill(category, .direction="down") %>% 
  # Fill in missing licence category
  mutate(category=ifelse(is.na(category), "Licenses", category)) %>% 
  # Remove header rows
  filter(license!=stringr::str_to_title(license)) %>% 
  # Fix license names that got cut-off
  mutate(license=recode(license, 
                        "HERRING GILL NET PERMIT(R)"="HERRING GILL NET PERMIT (R)",
                        "HERRING GILL NET PERMIT(NR)"="HERRING GILL NET PERMIT (NR)")) %>%
  # Add some useful columns
  mutate(decade="1980s",
         filename="tabula-80s Fees Commercial Fishing.csv") %>% 
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
  filter_all(any_vars(!is.na(.))) 
  # Insert row so dataframe matches items dataframe
  newrow <- c("SALMON VESSEL LATE FEE", rep(NA, 10))
  data90 <-rbind(data90[1:44,],newrow,data90[-(1:44),]) %>%
  # Create new columns to label license categories
  mutate(category=ifelse(license==stringr::str_to_title(license), license, NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category downwards
  fill(category, .direction="down") %>% 
  # Fill in missing licence category
  mutate(category=ifelse(is.na(category), "Licenses", category)) %>% 
  # Remove header rows
  filter(license!=stringr::str_to_title(license)) %>% 
  # Fix license names that got cut-off
  mutate(license=recode(license, 
                        "HERRING GILL NET PERMIT(R)"="HERRING GILL NET PERMIT (R)",
                        "HERRING GILL NET PERMIT(NR)"="HERRING GILL NET PERMIT (NR)")) %>%
  # Add some useful columns
  mutate(decade="1990s",
         filename="tabula-90s Fees Commercial Fishing.csv") %>% 
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
  # Create new columns to label license categories
  mutate(category=ifelse(license==stringr::str_to_title(license), license, NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category downwards
  fill(category, .direction="down") %>% 
  # Fill in missing licence category
  mutate(category=ifelse(is.na(category), "Licenses", category)) %>% 
  # Remove header rows
  filter(license!=stringr::str_to_title(license)) %>% 
  # Fix license names that got cut-off
  mutate(license=recode(license, 
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
                          "NORTHERN PINK SHRIMP TRAWL (VESSEL) (SAME OWNER)")) %>%
  # Add some useful columns
  mutate(decade="2000s",
         filename="tabula-2000s Fees Commercial Fishing.csv") %>% 
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
  # Create new columns to label license categories
  mutate(category=ifelse(license==stringr::str_to_title(license), license, NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category downwards
  fill(category, .direction="down") %>% 
  # Fill in missing licence category
  mutate(category=ifelse(is.na(category), "Licenses", category)) %>% 
  # Remove header rows
  filter(license!=stringr::str_to_title(license)) %>% 
  # Add some useful columns
  mutate(decade="2010s",
         filename="tabula-2010s Fees Commercial Fishing.csv") %>% 
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
  filter_all(any_vars(!is.na(.))) 
  # Insert row so dataframe matches items dataframe
  newrow <- c("SPOT PRAWN TRAP VESSEL PERMIT TIER 3", rep(NA, 10))
  data20 <-rbind(data20[1:96,],newrow,data20[-(1:96),]) %>%
  # Create new columns to label license categories
  mutate(category=ifelse(license==stringr::str_to_title(license), license, NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category downwards
  fill(category, .direction="down") %>% 
  # Fill in missing licence category
  mutate(category=ifelse(is.na(category), "Licenses", category)) %>% 
  # Remove header rows
  filter(license!=stringr::str_to_title(license)) %>%
  # Add some useful columns
  mutate(decade="2020s",
         filename="tabula-2020s Fees Commercial Fishing.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="fees_usd", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert fees to numeric
  mutate(fees_usd=gsub("\\$|,|-", "", fees_usd) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

# Merge dataframes by rowbinding (since all columns are same)
all_data <- rbind(data70, data80, data90, data00, data10, data20)

# Save to csv
path <- "data/cdfw/public/website_licenses/data/intermediate/combined_decades/"
write.csv(all_data, file.path(path, "AllDecadesFeesCommercialFishing"))


