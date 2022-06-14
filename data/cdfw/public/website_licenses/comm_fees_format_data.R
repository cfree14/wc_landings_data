


# Packages
library(tidyverse)

# Directories
datadir <- "data/cdfw/public/website_licenses/"

# Read data
data_orig <- read.csv(file.path(datadir, "tabula-2000s Fees Commercial Fishing.csv"), na.strings=c("N/A", "", NA))

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
data <- mutate(data, filename="tabula-2000s Fees Commercial Fishing.csv")
data <- data_orig %>% 
  mutate(decade="2000s") %>% 
  mutate(filename="tabula-2000s Fees Commercial Fishing.csv")
data_orig %>% mutate(., decade="2000s")

# Format data
data <- data_orig %>% 
  # Rename a column
  rename(license=Licenses) %>% 
  # Create new columns to label license categories
  mutate(category=ifelse(license==stringr::str_to_title(license), license, NA)) %>% 
  select(category, everything()) %>% 
  # Fill license category downwards
  fill(category, .direction="down") %>% 
  # Fill in missing licence category
  mutate(category=ifelse(is.na(category), "Licenses", category)) %>% 
  # Remove header rows
  filter(license!=stringr::str_to_title(license)) %>% 
  # Add some useful columns
  mutate(decade="2000s",
         filename="tabula-2000s Fees Commercial Fishing.csv") %>% 
  # Rearrange the columns
  select(filename, decade, category, everything()) %>% 
  # Gather
  gather(key="year", value="fees_usd", 5:ncol(.)) %>% 
  # Convert year to numeric
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Convert fees to numeric
  mutate(fees_usd=gsub("\\$|,|-", "", fees_usd) %>% trimws() %>% as.numeric()) # | = or, so replacing both $ and ,



# Format data for plotting
############################################

# Format data
data1 <- data %>% 
  filter(category=="Licenses")


# Plot data
ggplot(data=data1, mapping=aes(x=year, y=fees_usd/1e3, fill=license)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Fees (USD thousands)", title="Fees for comm. fishing licenses, 2000-2009") +
  # Axes
  # c(2000, 2005, 2010); seq(2000, 2010, 2)
  scale_x_continuous(breaks=2000:2010) +
  # Legend
  scale_fill_discrete(name="License") +
  # Theme
  theme_classic()












