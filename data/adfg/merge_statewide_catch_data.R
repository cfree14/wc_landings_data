
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/adfg/intermediate"
outdir <- "data/adfg/processed"

# Read data
catch <- readRDS(file.path(indir, "ADFG_1996_2000_statewide_catch.Rds"))
harvest <- readRDS(file.path(indir, "ADFG_1996_2000_statewide_harvest.Rds"))

# Read species key
spp_key <- readxl::read_excel(file.path(outdir, "ADFG_species_key.xlsx"))
str(spp_key)

# Merge data
################################################################################

# Merge data
data <- catch %>% 
  # Merge
  left_join(harvest, by=c("comm_name", "year")) %>% 
  # Rename
  rename(catch_n=catch_n.x, retained_n=catch_n.y) %>% 
  # Discards
  mutate(discards_n=catch_n-retained_n) %>% 
  # Add species
  left_join(spp_key) %>% 
  # Arrange
  select(mgmt_group, comm_name, sci_name, everything())

# Inspect
freeR::complete(data)

# Export
saveRDS(data, file=file.path(outdir, "ADFG_1996_2000_statewide_landings_discards.Rds"))



