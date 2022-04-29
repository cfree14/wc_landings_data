
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
data_orig <- read.csv(file.path(indir, "GFSAFE007-2003-2020.csv"), as.is=T)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(value_usd=exves_val,
         area_name=fmp_area)

# Inspect
str(data)
freeR::complete(data)

# Inspect
table(data$area_name)
range(data$year)
table(data$gear)
table(data$sector)
table(data$species)



# Export data
saveRDS(data, file=file.path(outdir, "GFSA07_2003_2020_BSAI_groundfish_landings_by_sector_gear_species.Rds"))


