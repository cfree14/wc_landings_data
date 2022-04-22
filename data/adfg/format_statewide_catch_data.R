
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(RSelenium)
library(tidyverse)

# Which to do?
catch_type <- "harvest"

# Directories
outdir <- "data/adfg/intermediate"
indir <- paste0("data/adfg/raw/statewide_", catch_type)

# NOTE: There is a test below that must have 0 rows to pass.

# Merge data
################################################################################

# Files to merge
files2merge <- list.files(indir)

# Loop through files to merge
x <- files2merge[15]
data_orig <- purrr::map_df(files2merge, function(x){
  
  # Read data
  fdata_orig <- XLConnect::readWorksheetFromFile(file.path(indir, x), 1)
  
  # File info
  filename <- x
  period <- substr(filename, 1, 9)
  yr1 <- substr(period, 1, 4) %>% as.numeric()
  yr2 <- substr(period, 6, 9) %>% as.numeric()
  years <- yr1:yr2
  
  # Format data
  fdata <- fdata_orig %>% 
    # Rename
    setNames(c("comm_name", years)) %>% 
    # Filter
    filter(!is.na(comm_name)) %>% 
    # Gather
    gather(key="year", value="catch_n", 2:ncol(.)) %>% 
    # Period
    mutate(period=period) %>% 
    # Arrange
    select(period, comm_name, year, catch_n) %>% 
    # Filter
    filter(!comm_name %in% c("SALMON SPECIES", "Salmon Total", "OTHER SPECIES", "Other Species Total", "Grand Total")) %>% 
    # Convert all to character
    mutate(across(.cols=everything(), as.character))
  
  
})


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Convert to numeric
  mutate(year=as.numeric(year),
         catch_n=as.numeric(catch_n)) %>% 
  # Replace NAs with zeros
  mutate(catch_n=ifelse(is.na(catch_n), 0, catch_n)) %>% 
  # Unique
  select(comm_name, year, catch_n) %>% 
  unique()


# Inspect
freeR::complete(data)
table(data$comm_name)

# Confirm that there aren't duplicated rows
n_check <- count(data, comm_name, year) %>% 
  arrange(desc(n)) %>% 
  filter(n!=1)
n_check # must be 0 rows

# Export data
outfile <- paste0("ADFG_1996_2000_statewide_", catch_type, ".Rds")
saveRDS(data, file=file.path(outdir, outfile))


