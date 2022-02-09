
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
outdir <- "data/cdfw/public/merged/data"
plotdir <- "calfish_paper/figures"


# Build data
################################################################################

# Get data
pacfin_orig <- wcfish::pacfin_all6

# Format data
pacfin <- pacfin_all6 %>% 
  # Reduce to CA
  filter(state=="California") %>% 
  # Summarize
  group_by(port_name, year) %>% 
  summarize(landings_mt=sum(landings_mt)) %>% 
  ungroup() %>% 
  # Calculate proportion
  group_by(year) %>% 
  mutate(plandings=landings_mt/sum(landings_mt)) %>% 
  ungroup() %>% 
  # Format port name
  mutate(port_name=gsub(" Area ports", "", port_name),
         port_name=recode(port_name, "Other/Unknown California ports 2"="Unknown"),
         port_name=factor(port_name,
                          levels=c("San Diego", "Los Angeles", "Santa Barbara", 
                                   "Morro Bay", "Monterey", "San Francisco",
                                   "Bodega Bay", "Fort Bragg", "Eureka", "Crescent City") %>% rev()))


# Plot data
################################################################################

# Theme
base_theme <-   theme(axis.text=element_text(size=6),
                      axis.title=element_text(size=8),
                      plot.title=element_text(size=9),
                      legend.text=element_text(size=6),
                      legend.title=element_text(size=8),
                      # Gridlines
                      panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(), 
                      axis.line = element_line(colour = "black"),
                      # Legend
                      legend.key.size = unit(0.2, "cm"),
                      legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(pacfin, aes(x=year, y=landings_mt/1000, fill=port_name)) +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="Year", y="Landings (1000s mt)", title="PACFIN data (1981-present)") +
  # Legend
  scale_fill_discrete(name="Port complex") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.85, 0.75))
g1

# Plot data
g2 <- ggplot(pacfin, aes(x=year, y=plandings, fill=port_name)) +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="Year", y="Landings (1000s mt)", title="") +
  # Legend
  scale_fill_discrete(name="Port complex") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        plot.title = element_blank())
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, ncol=1, heights=c(0.7, 0.3))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "pacfin_port_data_for_reviewer.png"), 
       width=6.5, height=5, units="in", dpi=600)

