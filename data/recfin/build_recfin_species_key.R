
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(RSelenium)
library(tidyverse)

# Directories
outdir <- "data/recfin/processed"

# Directories
cte2 <- readRDS(file=file.path(outdir, "RECFIN_2001_2021_CTE002_rec_mort_by_state.Rds"))
cte3 <- readRDS(file=file.path(outdir, "RECFIN_2001_2021_CTE003_rec_mort_by_mode.Rds"))

# PACFIN species key
pacfin_spp <- wcfish::pacfin_species %>% 
  rename(sci_name_pacfin=sci_name)


# Build species key
################################################################################

# Same number of species
n_distinct(cte3$comm_name)
n_distinct(cte2$comm_name)

# Build species
cte2_spp <- sort(unique(cte2$comm_name))
cte3_spp <- sort(unique(cte3$comm_name))
recfin_spp <- c(cte2_spp, cte3_spp) %>% unique() %>% sort()

# Build key
spp_key1 <- tibble(comm_name_orig=recfin_spp) %>% 
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name_orig),
         comm_name=recode(comm_name, 
                          'Albacore'='Albacore tuna', 
                          'Barred sandbass'='Barred sand bass', 
                          'Black and yellow rockfish'='Black-and-yellow rockfish',  
                          'Black skipjack'='Black skipjack tuna', 
                          'Bocaccio'='Bocaccio rockfish', 
                          'Brown smoothhound'='Brown smoothhound shark', 
                          'C-o sole'='C-O sole', 
                           'California moray'='California moray eel', 
                          'Chilipepper'='Chilipepper rockfish',
                          'Cowcod'='Cowcod rockfish', 
                          'Giant seabass'='Giant sea bass',  
                          'Gray smoothhound'='Gray smoothhound shark',
                          'Pacific ocean perch'='Pacific ocean perch rockfish', 
                          'Seven gill shark'='Sevengill shark',  
                          'Spotted sandbass'='Spotted sand bass',  
                          'Thornback'='Thornback skate', 
                          'Treefish'='Treefish rockfish', 
                          'Wolf-eel'='Wolf eel', 
                          'Yelllow rock crab'='Yellow rock crab')) %>% 
  # Add scientific name
  mutate(sci_name=wcfish::harmonize_names(comm_name, "comm", "sci")) %>% 
  # Add PACFIN scientific name
  left_join(pacfin_spp %>% select(comm_name, sci_name_pacfin)) %>% 
  mutate(sci_name=ifelse(is.na(sci_name), sci_name_pacfin, sci_name)) %>%
  # Mark species/other
  mutate(level=ifelse(grepl("order|family|genus|class|unidentified|unknown", tolower(comm_name)), "general", "species")) %>% 
  select(-sci_name_pacfin)

# Unmatched species
spp_unmatched <- spp_key1 %>% 
  filter(level=="species" & is.na(sci_name)) %>% 
  pull(comm_name)
wcfish::check_names(spp_unmatched)

# Export
write.csv(spp_key1, file=file.path(outdir, "RECFIN_species_key_raw.csv"), row.names=F)

# Check species key
################################################################################

# Read formatted key
spp_key2 <- readxl::read_excel(file.path(outdir, "RECFIN_species_key.xlsx"))

# Any wrong? 
# These are all correct: 
# "Beringraja binoculata"  "Beringraja inornata"    "Beringraja rhina"       "Kyphosus azureus"       "Metacarcinus gracilis" 
# "Pseudobatos productus"  "Salvelinus alpinus"     "Sebastes diaconus"      "Tetronarce californica" "Urobatis halleri"
freeR::check_names(spp_key2$sci_name)

# Any duplicated?
anyDuplicated(spp_key2$sci_name[!is.na(spp_key2$sci_name)])



