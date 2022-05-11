

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/pacfin/raw/ALL/ALL002"
outputdir <- "data/pacfin/processed"


# Read codes
port_key <- read.csv(file.path(outputdir, "pacfin_port_codes_clean.csv"), as.is=T)
spp_key <- read.csv(file.path(outputdir, "pacfin_species_codes_clean.csv"), as.is=T)

# Formatting steps overview
# Step 1. Format each state's data and merge into single table
# Step 2. Add meta-data associated with codes


# Loop and merge
################################################################################

# List files
files2merge <- list.files(datadir, pattern="xlsx")

# Loop and merge
x <- files2merge[1]
data_orig <- purrr::map_df(files2merge, function(x){
  
  # Read data
  fdata_orig <- readxl::read_excel(file.path(datadir, x), skip=8, na=c("-"))
  
  # Column names
  col_names1 <- c("year", "agency", "mgmt_group", "complex", "comm_name", "spp_code")
  gears <- c("dredge", "hookline", "net", "pottrap", "troll", "trawl", "shrimptrawl", "nontrawl", "othergear", "total")
  values <- c("confidential", "weightmt", "priceusd", "revenue")
  col_names2 <- lapply(gears, function(x) paste(x, values, sep="_")) %>% unlist()
  col_names <- c(col_names1, col_names2)
  
  # Format data
  fdata <- fdata_orig %>% 
    # Rename
    setNames(col_names) %>% 
    # Remove totals
    filter(!grepl("TOTAL", agency) & !grepl("TOTAL", mgmt_group) & !grepl("TOTAL", complex)) %>% 
    # Remove empty rows
    filter(!is.na( agency))
    
})


# Format data
################################################################################

# Format data
data_all <- data_orig %>% 
  # Format year
  mutate(year=as.numeric(year)) %>% 
  # Format state
  rename(state=agency) %>% 
  mutate(state=recode(state, 
                      "AT-SEA"="At-Sea", 
                      "C"="California",
                      "O"="Oregon",
                      "W"="Washington")) %>% 
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name)) %>% 
  # Gather
  gather(key="metric", value="value", 7:ncol(.)) %>% 
  # Separate
  separate(col=metric, into=c("gear", "metric"), sep = "_") %>% 
  # Format gear
  mutate(gear=stringr::str_to_sentence(gear),
         gear=recode(gear, 
                     "Hookline"="Hook and line",
                     "Nontrawl"="Non-trawl",
                     "Othergear"="Other gear",
                     "Pottrap"="Pot and trap",
                     "Shrimptrawl"="Shrimp trawl")) %>% 
  # Spread metric
  spread(key="metric", value="value") %>% 
  rename(price_usd_lb=priceusd,
         catch_mt=weightmt,
         revenue_usd=revenue) %>% 
  # Remove empty rows
  filter(!is.na(catch_mt) | !is.na(revenue_usd) | !is.na(price_usd_lb)) %>% 
  # Convert to numeric
  mutate(catch_mt=as.numeric(catch_mt),
         revenue_usd=as.numeric(revenue_usd),
         price_usd_lb=as.numeric(price_usd_lb)) %>% 
  # Arrange
  select(state, mgmt_group, complex, comm_name, spp_code, gear, 
         year, catch_mt, revenue_usd, price_usd_lb, confidential, everything()) %>%
  arrange(state, state, mgmt_group, complex, comm_name, spp_code, gear, year)
  

# Inspect
str(data_all)
range(data_all$year)
freeR::complete(data_all)

# Inspect
table(data_all$gear)

# Remove totals
data <- data_all %>% 
  filter(gear!="Total")


# Export data
################################################################################

# Export
saveRDS(data, file.path(outputdir, "PACFIN_ALL002_1980_2022_all_species_landings_by_gear.Rds"))







