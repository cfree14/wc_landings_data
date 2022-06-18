
# Packages
library(tidyverse)

# Directories
datadir <- "data/cdfw/public/website_licenses/data/intermediate/csvs/"

# Read data
data_1970 <- read.csv(file.path(datadir, "tabula-70s Fees Hunting.csv"), na.strings=c("N/A", ""))
data_1980 <- read.csv(file.path(datadir, "tabula-80s Fees Hunting.csv"), na.strings=c("N/A", ""))
data_1990 <- read.csv(file.path(datadir, "tabula-90s Fees Hunting.csv"), na.strings=c("N/A", ""))
data_2000 <- read.csv(file.path(datadir, "tabula-2000s Fees Hunting.csv"), na.strings=c("N/A", ""))
data_2010 <- read.csv(file.path(datadir, "tabula-2010s Fees Hunting.csv"), na.strings=c("N/A", ""))
data_2020 <- read.csv(file.path(datadir, "tabula-2020s Fees HuntingADA.csv"), na.strings=c("N/A", ""))

# Read category data 
cat70 <- read.csv(file.path(datadir, "tabula-70s Revenue Hunting.csv"), na.strings=c("N/A", ""))
cat80 <- read.csv(file.path(datadir, "tabula-80s Revenue Hunting.csv"), na.strings=c("N/A", ""))
cat90 <- read.csv(file.path(datadir, "tabula-90s Revenue Hunting.csv"), na.strings=c("N/A", ""))
cat00 <- read.csv(file.path(datadir, "tabula-2000s Revenue Hunting.csv"), na.strings=c("N/A", ""))
cat10 <- read.csv(file.path(datadir, "tabula-2010s Revenue Hunting.csv"), na.strings=c("N/A", ""))
cat20 <- read.csv(file.path(datadir, "tabula-2020s Revenue HuntingADA.csv"), na.strings=c("N/A", "", "-"))

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
  filter(!grepl("TOTAL", license)) 
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
  filter(!grepl("TOTAL", license)) 
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
  filter(!grepl("TOTAL", license)) 
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
  filter(!grepl("TOTAL", license)) 
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
  # Remove subtotals and totals rows and lifetime packages rows
  filter(!grepl("Subtotal", category)) %>%
  filter(!grepl("TOTAL", license)) %>%
  filter(!grepl("Lifetime Packages", category))
cat2 <- cat20 %>%
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) %>%
  # Remove empty columns
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
  filter(!grepl("TOTAL HUNTING", license)) %>%
  # Remove missing rows again
  filter_all(any_vars(!is.na(.)))
  
# Format 1970s data
data70 <- data_1970 %>%
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) %>%
  # Extract category column from revenue dataframe
  mutate(category=cat7$category) %>%
  # Remove totals row
  filter(!grepl("TOTAL", license)) %>%
  # Add some useful columns
  mutate(decade="1970s",
         filename="tabula-70s Fees Hunting.csv") %>% 
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
  filter(!grepl("TOTAL", license)) %>%
  # Add some useful columns
  mutate(decade="1980s",
         filename="tabula-80s Fees Hunting.csv") %>% 
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
  filter(!grepl("TOTAL", license)) %>%
  # Add some useful columns
  mutate(decade="1990s",
         filename="tabula-90s Revenue Hunting.csv") %>% 
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
  # Insert row so dataframe matches revenue dataframe
  newrow <- c("Lifetime Wild Pig Tags", rep(NA, 10))
  data00 <-rbind(data00[1:25,],newrow,data00[-(1:25),]) %>%
  # Extract category column from revenue dataframe
  mutate(category=cat0$category) %>%
  # Remove totals row
  filter(!grepl("TOTAL", license)) %>%
  # Add some useful columns
  mutate(decade="2000s",
         filename="tabula-2000s Fees Hunting.csv") %>% 
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
  filter(!grepl("TOTAL", license)) %>%
  # Add some useful columns
  mutate(decade="2010s",
         filename="tabula-2010s Fees Hunting.csv") %>% 
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
  # Remove empty columns
  select(-c(X:X.9)) %>% 
  # Remove missing rows
  filter_all(any_vars(!is.na(.))) %>%
  # Extract category column from revenue dataframe
  mutate(category=cat2$category) %>%
  # Remove totals row
  filter(!grepl("TOTAL", license)) %>%
  # Add some useful columns
  mutate(decade="2020s",
         filename="tabula-2020s Fees HuntingADA.csv") %>% 
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

# Filter data
all_data <- all_data %>%
  filter(category!="Subtotal") %>%
  
# Fix spacing in license column 
mutate(license=recode(license, "Junior Hunting     (Annual)"="Junior Hunting (Annual)",
                        "Non-Resident Hunting     (Annual)"="Non-Resident Hunting (Annual)",
                        "Non-Resident SecondDeer Tag"="Non-Resident Second Deer Tag",
                        "Resident Hunting     (Annual)"= "Resident Hunting (Annual)")) %>%
# Fix "Bear" and "Bobcat tags" categories
mutate(category=recode(category, "Bear"="Bear Tags", "Bobcat tags"="Bobcat Tags",
                       "Antelope, Bighorn Sheep, Elk T"="Antelope, Bighorn Sheep, Elk Tags"))

# Save as csv
write.csv(all_data, "AllDecadesFeesHunting")
