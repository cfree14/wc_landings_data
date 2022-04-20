

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
inputdir <- "data/noaa/raw"
outputdir <- "data/noaa/processed"
plotdir <- "data/noaa/figures"

# Read data
data_orig <- read.csv(file.path(inputdir, "foss_landings.csv"), as.is=T, na.strings="")

# The common/scientific names could be formatted way more rigourously.

# Source: https://www.fisheries.noaa.gov/national/sustainable-fisheries/commercial-fisheries-landings
# Asterisks indcate groups that are not species-specific

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename column
  janitor::clean_names("snake") %>% 
  rename(comm_name_orig=nmfs_name, 
         sci_name_orig=scientific_name,
         landings_lb=pounds,
         value_usd=dollars,
         fishery=collection) %>% 
  # Arrange columns
  select(region, source, state, fishery, tsn, comm_name_orig, sci_name_orig, year, landings_lb, value_usd, confidentiality, everything()) %>% 
  arrange(region, state, fishery, year, comm_name_orig) %>% 
  # Format state
  mutate(state=stringr::str_to_title(state)) %>% 
  # Format numeric values
  mutate(value_usd=value_usd %>% gsub(",", "", .) %>% as.numeric(),
         landings_lb=landings_lb %>% gsub(",", "", .) %>% as.numeric()) %>% 
  # Format species
  mutate(comm_name_orig=comm_name_orig %>% stringr::str_trim(),
         sci_name_orig=sci_name_orig %>% stringr::str_trim()) %>% 
  # Mark whether species specific
  mutate(level=ifelse(grepl("\\*", comm_name_orig), "generic", "species"))
  
# Inspect data
str(data)
freeR::complete(data)
range(data$year)
table(data$state)
table(data$fishery) 
table(data$confidentiality)
table(data$summary_type)


# Build species key
################################################################################

# Build species key
tsns <- sort(unique(data$tsn))

# Look up tsns
x <- tsns[3]
taxa_key_orig <- purrr::map_df(tsns, function(x){
  
  # Look up
  info_list <- taxize::classification(sci_id=x, db="itis")
  
  # Level
  level <- try(taxize::itis_taxrank(x) %>% as.character())
  if(level=="character(0)" | inherits(level, "try-error")){level <- NA}
  
  # Format
  # If it worked
  info_df_orig <- info_list[[1]]
  if( length(info_df_orig) > 1 ){
    sci_name <- info_df_orig$name[nrow(info_df_orig)]
    info_df <- info_df_orig %>% 
      mutate(tsn=x, 
             level=level,
             sci_name=sci_name) %>% 
      select(-id) %>% 
      spread(key="rank", value="name")
  }else{
    sci_name <- NA
    info_df <- tibble(tsn=x,
                      level=level,
                      sci_name=NA)
  }
  
  # Return
  info_df
  
})

# Format key
taxa_key <- taxa_key_orig %>% 
  # Simplify
  select(tsn, level, sci_name, kingdom, phylum, class, order, family, genus, species) %>% 
  # Fill in gaps
  mutate(level=ifelse(tsn=="167680,167682", "Genus", level),
         sci_name=ifelse(tsn=="167680,167682", "Morone", sci_name),
         level=ifelse(tsn=="0", "Unknown", level),
         sci_name=ifelse(tsn=="0", "Unknown", sci_name))

# Inspect
freeR::complete(taxa_key)

# Build common name key
comm_name_key <- data %>% 
  # Reduce to TSNs with common name
  filter(!is.na(comm_name_orig)) %>% 
  # Format common name (pass 1)
  mutate(comm_name=comm_name_orig %>% gsub("\\*", "", .) %>% stringr::str_trim()) %>% 
  # Unique
  select(comm_name, tsn) %>% 
  unique() %>% 
  # Format common name again
  mutate(comm_name=comm_name %>% wcfish::convert_names(., "regular"))

# Unique
freeR::which_duplicated(comm_name_key$tsn)


# Final formatting
################################################################################

# Format data
data_out <- data %>% 
  # Fix unknown TSN
  mutate(tsn=ifelse(is.na(tsn), 0, tsn)) %>% 
  # Add scientific name
  left_join(taxa_key %>% select(tsn, sci_name), by="tsn") %>% 
  # Add common name
  left_join(comm_name_key, by="tsn") %>% 
  # Fill in a few missing common names
  mutate(comm_name=ifelse(sci_name=="Cichlasoma urophthalma", "Mayan cichlid", comm_name),
         comm_name=ifelse(sci_name=="Euleptorhamphus viridis", "Ribbon halfbeak", comm_name),
         comm_name=ifelse(sci_name=="Icelinus borealis", "Northenr sculpin", comm_name),
         comm_name=ifelse(sci_name=="Clinocottus analis", "Woolly sculpin", comm_name),
         comm_name=ifelse(sci_name=="Archistes", "Archistes sculpins", comm_name),
         comm_name=ifelse(sci_name=="Raja stellulata", "Pacific starry skate", comm_name),
         comm_name=ifelse(sci_name=="Parupeneus chrysonemus", "Yellow-threaded goatfish", comm_name)) %>% 
  # Arrange
  select(region:tsn, comm_name, sci_name, level, comm_name_orig, sci_name_orig, everything())

# Which scientific names don't have common names?
data_out %>% filter(is.na(comm_name) & !is.na(sci_name)) %>% pull(sci_name) %>% unique()

# Inspect
freeR::complete(data_out)


# Export data
################################################################################

# Export data
saveRDS(data_out, file.path(outputdir, "NOAA_1950_2019_usa_landings_by_state_species.Rds"))

# Export species key
write.csv(taxa_key, file=file.path(outputdir, "NOAA_usa_commercial_species_key.csv"), row.names=F)
write.csv(taxa_key_orig, file=file.path(outputdir, "NOAA_usa_commercial_species_key_full.csv"), row.names=F)





