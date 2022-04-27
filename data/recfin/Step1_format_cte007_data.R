
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(RSelenium)
library(tidyverse)

# Directories
inputdir <- "data/recfin/raw/cte007"
outputdir <- "data/recfin/processed"

# Read data
data_orig <- read.csv(file=file.path(inputdir, "CTE007--1976---2020.csv"), as.is=T)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(year=recfin_year, 
         month=recfin_month, 
         state=state_name,
         area=catch_area,
         water_area_code=recfin_water_area_code,
         water_area=recfin_water_area_name,
         mode=recfin_mode,
         spp_code=recfin_species_code,
         comm_name=species_name,
         sci_name=scientific_name,
         retained_n=retained_num,
         download_date=recfin_vdate) %>% 
  # Format columns
  mutate(state=stringr::str_to_title(state),
         agency=recode(agency, "C"="CDFW"),
         comm_name=stringr::str_to_sentence(comm_name)) %>% 
  # Add level
  mutate(level="species") %>% 
  # Remove useless columns
  select(-c(download_date)) %>% 
  # Arrange
  select(year:sci_name, level, everything())
         

# Inspect
str(data)
freeR::complete(data)

# Inspect
table(data$year)
table(data$month)
table(data$state)
table(data$area)
table(data$water_area)
table(data$water_area_code)
table(data$mode)
table(data$comm_name)
table(data$sci_name)

# Inspect species
data %>% count(spp_code, comm_name, sci_name)

# Inspect agency/state
data %>% count(agency_code, agency, state)

# Export
saveRDS(data, file=file.path(outputdir, "RECFIN_1976_2020_CTE007_ca_salmon.Rds"))


