
# Packages
library(tidyverse)
library(dplyr)

# Directories
datadir <- "data/cdfw/public/website_licenses/data/intermediate/csvs/"

# Read data
data_1970 <- read.csv(file.path(datadir, "tabula-70s Fees Special Permits.csv"), na.strings=c("N/A", "", "ea N/A"))
data_1980 <- read.csv(file.path(datadir, "tabula-80s Fees Special Permits.csv"), na.strings=c("N/A", ""))
data_1990 <- read.csv(file.path(datadir, "tabula-90s Fees Special Permits.csv"), na.strings=c("N/A", ""))
data_2000 <- read.csv(file.path(datadir, "tabula-2000s Fees Special Permits.csv"), na.strings=c("N/A", ""))
data_2010 <- read.csv(file.path(datadir, "tabula-2010s Fees Special Permits.csv"), na.strings=c("N/A", ""))
data_2020 <- read.csv(file.path(datadir, "tabula-2020s Fees Special PermitsADA.csv"), na.strings=c("N/A", ""))

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
         filename="tabula-70s Fees Special Permits.csv") %>% 
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
  # Create license category
  mutate(category="Licenses") %>% 
  # Remove totals row
  filter(!grepl("TOTAL MISCELLANEOUS", license)) %>%
  # Add some useful columns
  mutate(decade="1980s",
         filename="tabula-80s Fees Special Permits.csv") %>% 
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
  # Add rows to match items dataframe
  newrow <- c("Restricted Species Permit - Inspection Fee (Except Aquaculture & Fish)", rep(NA,10))
  data90 <- rbind(data90[1:41,], newrow, data90[-(1:41),]) %>%
  # Create license category
  mutate(category="Licenses") %>% 
  # Remove totals row
  filter(!grepl("TOTAL MISCELLANEOUS", license)) %>%
  # Add some useful columns
  mutate(decade="1990s",
         filename="tabula-90s Fees Special Permits.csv") %>% 
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
  filter_all(any_vars(!is.na(.))) 
  # Add rows to match items dataframe
  newrow <- c("Restricted Species Permit - Breeding", rep(NA,10))
  data00 <- rbind(data00[1:37,], newrow, data00[-(1:37),]) %>%
  # Create license category
  mutate(category="Licenses") %>% 
  # Remove totals row
  filter(!grepl("TOTAL MISCELLANEOUS", license)) %>%
  # Add some useful columns
  mutate(decade="2000s",
         filename="tabula-2000s Fees Special Permits.csv") %>% 
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
  filter_all(any_vars(!is.na(.))) 
  # Add rows to match items dataframe
  newrow <- c("Scientific Collecting Permit Specific Use (Student)", rep(NA,10))
  data10 <- rbind(data10[1:95,], newrow, data10[-(1:95),]) %>%
  # Create license category
  mutate(category="Licenses") %>% 
  # Remove totals row
  filter(!grepl("TOTAL MISCELLANEOUS", license)) %>%
  # Add some useful columns
  mutate(decade="2010s",
         filename="tabula-2010s Fees Special Permits.csv") %>% 
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
  # Create license category
  mutate(category="Licenses") %>% 
  # Remove totals row
  filter(!grepl("TOTAL MISCELLANEOUS", license)) %>%
  # Add some useful columns
  mutate(decade="2020s",
         filename="tabula-2020s Fees Special Permits.csv") %>% 
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

# Fixing cut-off license names
all_data <- all_data %>%
  mutate(license=recode(license, "Restricted Species Permit - Application Fee (Amended or Rene"=
                          "Restricted Species Permit - Application Fee (Amended or Renewed)",
                        "Native Reptile & Amphibian Permit (Take by Biol. Supply Hous"=
                          "Native Reptile & Amphibian Permit (Take by Biol. Supply House",
                        "Aquaculture Registration - Interest (per month )"=
                          "Aquaculture Registration - Interest (per month)"))

# Save to csv
path <- "data/cdfw/public/website_licenses/data/intermediate/combined_decades/"
write.csv(all_data, file.path(path, "AllDecadesFeesSpecialPermits"))
