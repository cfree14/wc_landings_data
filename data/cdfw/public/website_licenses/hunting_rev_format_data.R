


# Packages
library(tidyverse)

# Directories
datadir <- "data/cdfw/public/website_licenses/"

# Read data
data_orig <- read.csv(file.path(datadir, "tabula-2000s Revenue Hunting.csv"), na.strings=c("N/A", "", "Not  Avail."))

# Inspect data
head(data_orig)
tail(data_orig)
str(data_orig)
nrow(data_orig)
ncol(data_orig)

# Inspect a column of data
data_orig$Licenses
table(data_orig$Licenses)
unique(data_orig$Licenses)

# Illustrating pipes
data <- mutate(data_orig, decade="2000s")
data <- mutate(data, filename="tabula-2000s Revenue hunting.csv")
data <- data_orig %>% 
  mutate(decade="2000s") %>% 
  mutate(filename="tabula-2000s Revenue Hunting.csv")
data_orig %>% mutate(., decade="2000s")

# Format data
data <- data_orig %>% 
  # Rename a column
  rename(license=Licenses) %>% 
  # Remove missing rows
  remove_missing() %>%
  # Create new columns to label license categories
  mutate(category=ifelse(substr(license, 1, 3)=="Sub", substr(license,13,nchar(license)), NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category upwards
  fill(category, .direction="up") %>% 
  # Change category for sub total rows
  mutate(category=ifelse(substr(license, 1, 3)=="Sub", "Subtotal", category)) %>% 
  # Remove totals row
  filter(!grepl("TOTAL HUNTING", license)) %>%
  # Add some useful columns
  mutate(decade="2000s",
         filename="tabula-2000s Revenue Hunting.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="revenues_usd", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert revenues to numeric
  mutate(revenues_usd=gsub("\\$|,|-", "", revenues_usd) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,



# Format data for plotting
############################################

# Format data
data1 <- data %>% 
  filter(category=="Subtotal")


# Plot data
ggplot(data=data1, mapping=aes(x=year, y=revenues_usd/1e6, fill=license)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Revenues (USD millions)", title="Revenues from hunting licenses, 2000-2009") +
  # Axes
  # c(2000, 2005, 2010); seq(2000, 2010, 2)
  scale_x_continuous(breaks=2000:2010) +
  # Legend
  scale_fill_discrete(name="License Subtotal") +
  # Theme
  theme_classic()


