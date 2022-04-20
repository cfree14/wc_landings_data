
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/akfin/raw"
outdir <- "data/akfin/processed"

# NOAA data
noaa_orig <- wcfish::noaa

# Read data
list.files(outdir)
crabs_orig <- readRDS(file.path(outdir, "CRSA01_1998_2020_crab_sector_output.Rds"))
groundfish_orig <- readRDS(file.path(outdir, "GFSA01_2003_2020_groundfish_landings_by_group_area.Rds"))


# Format NOAA data
################################################################################

# Format NOAA data
noaa_ak <- noaa_orig %>% 
  filter(fishery=='Commercial' & state %in% c("Alaska") & year >=2003) %>% 
  group_by(year) %>% 
  summarise(landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(landings_kg=measurements::conv_unit(landings_lb, "lbs", "kg"),
         landings_mt=landings_kg/1000)


# Format AKFIN data
################################################################################

# Crabs
crabs_tot <- crabs_orig %>% 
  group_by(year, species) %>% 
  summarize(landings_mt=sum(sold_mt)) %>% 
  ungroup()

# Groundfish
groundfish_tot <- groundfish_orig %>% 
  filter(species_group!="All Groundfish" & fmp_area!="All Alaska") %>% 
  group_by(year, species_group) %>% 
  summarize(landings_mt=sum(catch_mt)) %>% 
  ungroup() %>% 
  rename(species=species_group)

# Merge
akfin <- bind_rows(crabs_tot, groundfish_tot)

# plot
ggplot(akfin, aes(x=year, y=landings_mt, fill=species)) +
  geom_bar(stat="identity") +
  geom_line(data=noaa_ak, mapping=aes(x=year, y=landings_mt), inherit.aes=F)






