
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
data_orig <- read.csv(file.path(indir, "GFSAFE008-2003-2020.csv"), as.is=T)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(value_usd=exves_val,
         area=fmp_area)

# Inspect
str(data)
freeR::complete(data)

# Inspect
table(data$area)
range(data$year)
table(data$gear)
table(data$subarea)
table(data$species)



# Export data
saveRDS(data, file=file.path(outdir, "GFSA08_2003_2020_GOA_groundfish_landings_by_gear_subarea_species.Rds"))


