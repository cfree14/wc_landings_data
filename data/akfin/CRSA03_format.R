
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/akfin/raw"
outdir <- "data/akfin/processed"

# Read data
list.files(indir)
data_orig <- read.csv(file.path(indir, "CRSAFEEXEC03-2012---2020.csv"), as.is=T)


# Format data
################################################################################

# Finish renaming columns

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(fishery=fishery_name) %>% 
  # Format fishery name 
  mutate(fishery=stringr::str_to_title(fishery)) %>% 
  # Add area/species
  mutate(area=recode(fishery,
                     "Aleutian Islands Golden King Crab"="Aleutian Islands",     
                     "Bering Sea Snow Crab"="Bering Sea",                 
                     "Bering Sea Tanner Crab"="Bering Sea",                
                     "Bristol Bay Red King Crab"="Bristol Bay",               
                     "Pribilof Islands Red And Blue King Crab"="Pribilof Islands",  
                     "St. Matthew Island Blue King Crab"="St. Matthew Island",   
                     "Western Aleutian Islands Red King Crab"="Western Aleutian Islands"),
         species=recode(fishery,
                         "Aleutian Islands Golden King Crab"="Golden king crab",     
                         "Bering Sea Snow Crab"="Snow crab",                 
                         "Bering Sea Tanner Crab"="Tanner crab",                
                         "Bristol Bay Red King Crab"="Red king crab",               
                         "Pribilof Islands Red And Blue King Crab"="Red and blue king crab",  
                         "St. Matthew Island Blue King Crab"="Blue king crab",   
                         "Western Aleutian Islands Red King Crab"="Red king crab")) %>% 
  # Arrange
  select(fishery_code, fishery, area, species, year, everything()) %>% 
  arrange(fishery_code, year)

# Inspect
str(data)
sort(unique(data$fishery))
table(data$area)
table(data$species)

# Export data
saveRDS(data, file=file.path(outdir, "CRSA03_2012_2020_crab_sector_lease_activity.Rds"))


