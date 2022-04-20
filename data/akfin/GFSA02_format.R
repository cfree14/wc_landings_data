
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
data_orig <- read.csv(file.path(indir, "GFSAFE002-2003-2020.csv"), as.is=T)


# Format data
################################################################################

# Does ALL = BSAI + GOA?

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(sector=harvest_sector,
         landings_mt=retained_catch_mt,
         value_usd=exvessel_value)
 
# Inspect
str(data)
range(data$year)
table(data$fmp_area)

# Export data
saveRDS(data, file=file.path(outdir, "GFSA01_2003_2020_groundfish_landings_by_area_sector_group.Rds"))


