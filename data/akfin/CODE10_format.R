
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
data_orig <- read.csv(file.path(indir, "CODE010.csv"), as.is=T)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(spp_code=species_code, comm_name_orig=species_common_name, mgmt_group_code=akfin_species_code) %>% 
  # Format common name
  mutate(comm_name=wcfish::convert_names(comm_name_orig, "regular") %>% stringr::str_to_sentence(.)) %>% 
  # Arrange
  select(spp_code, mgmt_group_code, comm_name_orig, comm_name, everything())

# Inspect
str(data)

# Export data
saveRDS(data, file=file.path(outdir, "CODE10_species_key.Rds"))


