
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(RSelenium)
library(tidyverse)

# Directories
inputdir <- "data/recfin/raw/cte002"
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
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  rename(year=filename, comm_name=SPECIES) %>% 
  # Arrange
  select(year, everything()) %>% 
  # Gather 
  gather(key="data_long", value="catch", 3:ncol(.)) %>%
  # Format year
  mutate(year=year %>% gsub("CTE002-|.csv", "", .) %>% as.numeric()) %>% 
  # Get state/mode
  mutate(data_long=gsub("RELEASED_DEAD", "RELEASED-DEAD", data_long)) %>% 
  separate(col=data_long, into=c("state", "status", "units"), sep="_", remove=T) %>% 
  # Format state/status
  mutate(state=stringr::str_to_title(state),
         status=stringr::str_to_title(status),
         status=recode(status, 
                       "Released-Dead"="Released (dead)")) %>% 
  # Reduce
  filter(!is.na(catch)) %>% 
  # Spread
  spread(key="units", value="catch") %>% 
  rename(catch_n=NUM, catch_mt=MT) %>% 
  # Arrange
  select(state, comm_name, year, status, catch_n, catch_mt, everything()) %>% 
  arrange(state, comm_name, year, status)

# Inspect
table(data$state)
table(data$status)

# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outputdir, "RECFIN_2001_2021_CTE002_rec_mort_by_state.Rds"))


# Plot data
################################################################################

# Plot data
g <- ggplot(data, aes(x=year, y=catch_n/1e6, fill=status)) +
  facet_wrap(~state, scales="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Catch (millions of fish)") +
  # Theme
  theme_bw()
g
