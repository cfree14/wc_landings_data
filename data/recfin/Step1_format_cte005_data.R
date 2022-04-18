
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(RSelenium)
library(tidyverse)

# Directories
inputdir <- "data/recfin/raw/cte005"
outputdir <- "data/recfin/processed"

# Read data
data_or_orig <- read.csv(file=file.path(inputdir, "CTE005-O2001---2021.csv"), as.is=T)
data_wa_orig <- read.csv(file=file.path(inputdir, "CTE005-W2001---2021.csv"), as.is=T)


# Format data
################################################################################

# Format data
data <- bind_rows(data_or_orig, data_wa_orig) %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(year=recfin_year, 
         month=recfin_month, 
         week=recfin_week,
         state=agency_code,
         district=district_name, 
         mode=recfin_mode_name,
         water_area=recfin_water_area_name,
         subregion=recfin_subregion_name,
         trip_type=recfin_trip_type_name,
         comm_name=species_name, 
         retained_n=retained_num,
         released_n=released_num) %>% 
  # Format state
  mutate(state=recode(state,
                      "O"="Oregon",
                      "W"="Washington")) %>% 
  # Format district
  mutate(district=recode(district,
                         "Wa Central Coast"="Central Washington Coast",
                         "Wa Northern Coast"="Northern Washington Coast",
                         "Wa Southern Coast"="Southern Washington Coast")) %>% 
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name))
         

# Inspect
str(data)
freeR::complete(data)

# Inspect
table(data$year)
table(data$month)
table(data$week)
table(data$state)
table(data$district)
table(data$water_area)
table(data$mode)
table(data$comm_name)
table(data$trip_type)

# Export
saveRDS(data, file=file.path(outputdir, "RECFIN_2001_2021_CTE005_or_wa_salmon.Rds"))


