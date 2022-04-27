
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
indir <- "data/cdfw/public/website/cpfv/processed/"
outdir <- "data/cdfw/public/merged/data"
plotdir <- "data/cdfw/public/merged/figures"

# Read data by state
data_orig <- read.csv(file=file.path(indir, "CDFW_2000_2019_cpfv_landings_statewide.csv"), as.is=T)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Calculate USA landings
  spread(key="region", value="landings_n") %>% 
  mutate(USA=All-Mexico) %>% 
  select(-All) %>% 
  gather(key="waters", value="landings_n", 4:5) %>% 
  # Add common name
  rename(comm_name_orig=species) %>% 
  mutate(comm_name=wcfish::convert_names(comm_name_orig, to="regular"),
         comm_name=recode(comm_name,
                          "King (chinook) salmon"="Chinook salmon",
                          "Silver (coho) salmon"="Coho salmon",
                          "Bonito (mako) shark"="Shortfin mako shark",
                          "Dolphinfish"="Common dolphinfish",
                          "Jack mackerel"="Pacific jack mackerel",
                          "White croaker"="White croaker (kingfish)",
                          "Yellowtail"="Yellowtail (amberjack)",
                          "Kelp (calico) bass"="Kelp bass",
                          "Giant (black) sea bass"="Giant sea bass", 
                          "Unspecified salmon"="Salmon",
                          "Unspecified shark"="Shark",
                          "Unspecified sturgeon"="Sturgeon",
                          "Unspecified rockfishes"="Rockfish",
                          "Unspecified flatfishes"="Flatfish",
                          "Unspecified fishes"="Fish",
                          "Unspecified croaker"="Croaker",
                          "Unspecified invertebrates"="Invertebrate",
                          "Unspecified (jumbo squid included) fishes"="Fish")) %>% 
  # Add scientific name
  mutate(sci_name=harmonize_names(comm_name, "comm", "sci")) %>% 
  # Add level
  mutate(level=ifelse(grepl("spp", sci_name), "group", "species")) %>% 
  # Arrange
  select(-filename) %>% 
  select(waters, comm_name, comm_name_orig, sci_name, level, year, landings_n, everything()) %>% 
  arrange(waters, comm_name, year)

# Inspect
freeR::complete(data)
range(data$year)

# Inspect species
spp_key <- data %>% 
  select(comm_name, comm_name_orig, sci_name, level) %>% unique() %>% 
  arrange(comm_name) #%>% 
  #filter(is.na(sci_name)) %>% pull(comm_name)


# Export data
################################################################################

# Export data
write.csv(data, file=file.path(outdir, "CDFW_2000_2019_annual_cpfv_landings_by_species.csv"), row.names = F)
saveRDS(data, file=file.path(outdir, "CDFW_2000_2019_annual_cpfv_landings_by_species.Rds"))

