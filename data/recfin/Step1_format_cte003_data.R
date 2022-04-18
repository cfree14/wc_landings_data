
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(RSelenium)
library(tidyverse)

# Directories
inputdir <- "data/recfin/raw/cte003"
outputdir <- "data/recfin/processed"

# Merge data
################################################################################

# Files 2 merge
files2merge <- list.files(inputdir)

# Loop through files and merge
x <- files2merge[1]
data_orig <- purrr::map_df(files2merge, function(x){
  
  # Read data
  fdata <- read.csv(file.path(inputdir, x), as.is=T) %>% 
    # Add filename
    mutate(filename=x)
  
})

# Format data
data <- data_orig %>% 
  # Rename
  rename(year=filename, comm_name=SPECIES) %>% 
  # Arrange
  select(year, everything()) %>% 
  # Gather 
  gather(key="data_long", value="catch", 3:ncol(.)) %>%
  # Format year
  mutate(year=year %>% gsub("CTE003-|.csv", "", .) %>% as.numeric()) %>% 
  # Get state/mode
  mutate(data_long=gsub("_TOTAL_MORTALITY", "", data_long)) %>% 
  separate(col=data_long, into=c("state", "mode", "units"), sep="_", remove=T) %>% 
  # Format state/mode
  mutate(mode=stringr::str_to_title(mode),
         state=recode(state,
                      "CA"="California",
                      "OR"="Oregon",
                      "WA"="Washington")) %>% 
  # Reduce
  filter(!is.na(catch)) %>% 
  # Spread
  spread(key="units", value="catch") %>% 
  rename(catch_n=NUM, catch_mt=MT) %>% 
  # Arrange
  select(state, mode, comm_name, year, catch_n, catch_mt, everything()) %>% 
  arrange(state, mode, comm_name, year)

# Export data
saveRDS(data, file=file.path(outputdir, "RECFIN_2001_2021_CTE003_rec_mort_by_mode.Rds"))



