
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
data_orig <- read.csv(file.path(indir, "CRSAFEEXEC01-1998---2020.csv"), as.is=T)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(fishery=fishery_name,
         harvest_vessels_n=hpy_vescount,
         catcher_vessels_n=hpy_cvcount,
         processor_vessels_n=hpy_cpcount, 
         cfec_permits_n=hpy_permcount, 
         buyers_n=hpy_buyercount,
         processors_n=hpy_proccount,
         sold_mt=hpy_soldmt,
         sold_lb=hpy_soldlb ,
         sold_lb_med=hpy_soldlbmed,
         sold_lb_med_prop=hpy_soldlbmed_share,
         sold_gini=hpy_soldlbgini,
         purchased_lb_med=hpy_purchlbmed,
         purchased_lb_med_prop=hpy_purchlbmed_share,
         revenue_usd=hpy_exv_nom, 
         revenue_usd_real=hpy_exv_real,
         price_usd_lb=hpy_exvpr_nom,
         price_usd_lb_real=hpy_exvpr_real,
         product_mt=hpy_prodmt ,
         product_lb=hpy_prodlb,
         wholesale_usd=hpy_wsv_nom,
         wholesale_usd_real=hpy_wsv_real,
         wholesale_price_usd_lb=hpy_wspr_nom,
         wholesale_price_usd_lb_real=hpy_wspr_real, 
         inflation_index_year=adjust_yr) %>% 
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

# Export data
saveRDS(data, file=file.path(outdir, "CRSA01_1998_2020_crab_sector_output.Rds"))


