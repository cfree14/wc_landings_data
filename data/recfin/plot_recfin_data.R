
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(RSelenium)
library(tidyverse)

# Directories
outputdir <- "data/recfin/processed"
list.files(outputdir)

# Read data
cte002_orig <- readRDS(file.path(outputdir, "RECFIN_2001_2021_CTE002_rec_mort_by_state.Rds"))
cte003_orig <- readRDS(file.path(outputdir, "RECFIN_2001_2021_CTE003_rec_mort_by_mode.Rds"))
cte005_orig <- readRDS(file.path(outputdir, "RECFIN_2001_2021_CTE005_or_wa_salmon.Rds"))
cte007_orig <- readRDS(file.path(outputdir, "RECFIN_1976_2020_CTE007_ca_salmon.Rds"))

# Read NOAA data
noaa_orig <- wcfish::noaa

# Read CDFW data
cdfw_cpfv1_orig <- wcfish::cdfw_cpfv
cdfw_cpfv2_orig <- wcfish::cdfw_cpfv_port


# Format CA data
################################################################################

# CA HMS statewide
cdfw_cpfv1 <- cdfw_cpfv1_orig %>% 
  group_by(year) %>%
  summarize(landings_n=sum(landings_n, na.rm = T))

# CA HMS by port
cdfw_cpfv2 <- cdfw_cpfv2_orig %>% 
  group_by(year, port_complex) %>%
  summarize(landings_n=sum(landings_n, na.rm = T))
  
# Plot data
g <- ggplot(cdfw_cpfv2, aes(x=year, y=landings_n/1e6, fill=port_complex)) +
  geom_bar(stat="identity") +
  geom_line(data=cdfw_cpfv1, aes(x=year, y=landings_n/1e6), inherit.aes = F) +
  theme_bw()
g


# Format data
################################################################################

# Build CTE002
cte002 <- cte002_orig %>% 
  group_by(state, status, year) %>% 
  summarize(catch_n=sum(catch_n, na.rm=T),
            catch_mt=sum(catch_mt, na.rm=T)) %>% 
  ungroup()

# Build CTE003
cte003 <- cte003_orig %>% 
  group_by(state, year) %>% 
  summarize(catch_n=sum(catch_n, na.rm=T),
            catch_mt=sum(catch_mt, na.rm=T)) %>% 
  ungroup()

# Build NOAA data
noaa <- noaa_orig %>% 
  # Reduce
  filter(fishery=="Recreational" & state %in% c("California", "Oregon", "Washington")) %>% 
  # Summarize
  group_by(state, year) %>% 
  summarize(catch_lb=sum(landings_lb, na.rm=T)) %>% 
  ungroup() %>% 
  # COnvert to mt
  mutate(catch_kg=measurements::conv_unit(catch_lb, "lbs", "kg"),
         catch_mt=catch_kg/1000)


# Plot data
################################################################################

# Plot data
g <- ggplot(cte002, aes(x=year, y=catch_mt, fill=status)) +
  facet_wrap(~state, scales="free_y") +
  geom_bar(stat="identity") +
  # Plot other
  geom_line(data=cte003, aes(x=year, y=catch_mt), inherit.aes = F) +
  geom_line(data=noaa, aes(x=year, y=catch_mt), inherit.aes = F, color="red") +
  # Labels
  labs(x="Year", y="Catch (lbs)") +
  # Theme
  theme_bw()
g



