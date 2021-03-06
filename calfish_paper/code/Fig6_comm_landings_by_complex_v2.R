
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

# Read data
data_orig <- readRDS(file=file.path(outdir, "CDFW_1941_2019_landings_by_port_complex.Rds"))
totals_orig <- readRDS(file=file.path(outdir, "CDFW_1916_1999_total_landings_shipments.Rds"))


# Build data
################################################################################

# Annual landings by taxa
data_use <- data_orig %>%
  # Landings
  filter(type %in% c("Landings", "Landings/Shipments")) %>%
  # Annual sum by group
  group_by(year, port_complex2) %>%
  summarize(landings_lb=sum(landings_lb, na.rm=T)) %>% 
  ungroup() %>% 
  # Remove years without data
  filter(landings_lb!=0)
  
# Extract "totals" time series to supplement
totals_use <- totals_orig %>% 
  filter(!year %in% data_use$year) %>% 
  # Add port complex
  mutate(port_complex2="Unknown") %>% 
  select(year, port_complex2, seafood_lb) %>% 
  # Rename
  rename(landings_lb=seafood_lb)
  
# Complexes 2
complexes2 <- c("Eureka", "San Francisco", "Sacramento Delta", 
                "Monterey", "Santa Barbara", 'Los Angeles', "San Diego", "Inland Waters")

# Build final dataset
data <- bind_rows(data_use, totals_use) %>% 
  # Arrange
  arrange(year, port_complex2) %>% 
  select(year, port_complex2, landings_lb) %>%  
  # Props
  group_by(year) %>% 
  mutate(plandings=landings_lb/sum(landings_lb)) %>% 
  ungroup() %>% 
  # Format port complex
  mutate(port_complex2=ifelse(port_complex2=="Unknown", NA, port_complex2)) %>% 
  mutate(port_complex2=factor(port_complex2, levels=c(complexes2)))

# Plot data
################################################################################

# Theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=8),
                    strip.text=element_text(size=8),
                    plot.title=element_text(size=10),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"))

# Params
line_y1 <- 1790
ymax <- 1850

# Plot data
g1 <- ggplot(data, aes(x=year, y=landings_lb/1e6, fill=port_complex2)) +
  geom_bar(stat="identity", color='grey10', lwd=0.1) +
  # Labels
  labs(x="Year", y="Landings\n(millions of lbs)") +
  # Limits
  lims(y=c(0, ymax)) +
  scale_x_continuous(breaks=seq(1910,2020,10)) +
  # Mark availaibility of port-level data
  annotate(geom="text", x=1941, y=line_y1, hjust=0, vjust=-0.6, label="Port- and species-level landings data", size=2) +
  geom_segment(mapping=aes(x=1941, xend=1947, y=line_y1, yend=line_y1), lwd=0.3, linetype="dotted") +
  geom_segment(mapping=aes(x=1947, xend=1976, y=line_y1, yend=line_y1), lwd=0.3) +
  geom_segment(mapping=aes(x=2000, xend=2019, y=line_y1, yend=line_y1), lwd=0.3) +
  # Legend
  scale_fill_discrete(name="Port complex\n(north to south)", na.value="grey80") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.9,0.63),
        legend.key.size = unit(0.3, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)))
g1

# Plot data
g2 <- ggplot(data, aes(x=year, y=plandings, fill=port_complex2)) +
  geom_bar(stat="identity", color='grey10', lwd=0.1) +
  # Labels
  labs(x="Year", y="Proportion\nof landings") +
  scale_x_continuous(breaks=seq(1910,2020,10)) +
  # Legend
  scale_fill_discrete(name="Source", na.value="grey80", drop=F) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, ncol=1, heights=c(0.75, 0.25))

# Export
ggsave(g, filename=file.path(plotdir, "Fig6_comm_landings_by_port_complex_v2.png"), 
       width=6.5, height=4, units="in", dpi=600)


