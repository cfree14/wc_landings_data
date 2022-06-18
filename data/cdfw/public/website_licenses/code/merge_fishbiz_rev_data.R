
# Packages
library(tidyverse)

# Directories
datadir <- "data/cdfw/public/website_licenses/data/intermediate/csvs/"

# Read data
data_1970 <- read.csv(file.path(datadir, "tabula-70s Revenue Fish Business.csv"), na.strings=c("N/A", ""))
data_1980 <- read.csv(file.path(datadir, "tabula-80s Revenue Fish Business.csv"), na.strings=c("N/A", ""))
data_1990 <- read.csv(file.path(datadir, "tabula-90s Revenue Fish Business.csv"), na.strings=c("N/A", ""))
data_2000 <- read.csv(file.path(datadir, "tabula-2000s Revenue Fish Business.csv"), na.strings=c("N/A", ""))
data_2010 <- read.csv(file.path(datadir, "tabula-2010s Revenue Fish Business.csv"), na.strings=c("N/A", ""))
data_2020 <- read.csv(file.path(datadir, "tabula-2020s Revenue Fish BusinessADA.csv"), na.strings=c("N/A", ""))

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
         filename="tabula-70s Revenue Fish Business.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="revenues_usd", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year),
         year=recode(year, 
                     "1975.1"="1976",
                     "1977.1"="1979",
                     ) %>% as.numeric()) %>% 
  # Convert revenues to numeric
  mutate(revenues_usd=gsub("\\$|,|-", "", revenues_usd) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

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
         filename="tabula-80s Revenue Fish Business.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="revenues_usd", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert revenues to numeric
  mutate(revenues_usd=gsub("\\$|,|-", "", revenues_usd) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

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
         filename="tabula-90s Revenue Fish Business.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="revenues_usd", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert revenues to numeric
  mutate(revenues_usd=gsub("\\$|,|-", "", revenues_usd) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

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
         filename="tabula-2000s Revenue Fish Business.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="revenues_usd", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert revenues to numeric
  mutate(revenues_usd=gsub("\\$|,|-", "", revenues_usd) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

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
         filename="tabula-2010s Revenue Fish Business.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="revenues_usd", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert revenues to numeric
  mutate(revenues_usd=gsub("\\$|,|-", "", revenues_usd) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

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
         filename="tabula-2020s Revenue Fish BusinessADA.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="revenues_usd", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert revenues to numeric
  mutate(revenues_usd=gsub("\\$|,|-", "", revenues_usd) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,

# Merge dataframes by rowbinding (since all columns are same)
all_data <- rbind(data70, data80, data90, data00, data10, data20)

# Plot data
all_data <- all_data %>%
  filter(category=="Licenses")
ggplot(data=all_data, mapping=aes(x=decade, y=revenues_usd/1e6, fill=license)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Decade", y="Revenues (USD millions)", title="Revenues from fish business licenses, 1970-2022") +
  # Legend
  scale_fill_discrete(name="License") +
  # Theme
  theme_classic()

# Filter data to remove subtotal rows

# Save as csv
write.csv(all_data, "AllDecadesRevenueFishBusiness")

