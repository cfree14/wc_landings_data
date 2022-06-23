# Packages
library(tidyverse)
library(dplyr)
library(purrr)

# Directories
datadir <- "data/cdfw/public/website_licenses/data/processed/"

comm <- read.csv(file.path(datadir, "CDFWCommercialFishingRecords")) 
fishbiz <- read.csv(file.path(datadir, "CDFWFishBusinessRecords"))
hunting <- read.csv(file.path(datadir, "CDFWHuntingRecords"))
special <- read.csv(file.path(datadir, "CDFWSpecialPermitsRecords"))
sport <- read.csv(file.path(datadir, "CDFWSportFishingRecords"))

################### # Functions
# Function for finding sum of license fees for that year
fees_sum <- function(dataframe, y) {
  subdf <- dataframe %>%
    filter(grepl(y, year))
  return(sum(subdf$fees_usd, na.rm=TRUE))
}
# Function for finding sum of license items for that year
items_sum <- function(dataframe, y) {
  subdf <- dataframe %>%
    filter(grepl(y, year))
  return(sum(subdf$items, na.rm=TRUE))
}
# Function for finding sum of license revenue for that year
rev_sum <- function(dataframe, y) {
  subdf <- dataframe %>%
    filter(grepl(y, year))
  return(sum(subdf$revenues_usd, na.rm=TRUE))
}
# Make totals dataframe  (helper method)
make_df <- function(wholedf, totdf) {
  f <-map(years, function(x) fees_sum(dataframe=wholedf, y=x))
  i <-map(years, function(x) items_sum(dataframe=wholedf, y=x))
  r <-map(years, function(x) rev_sum(dataframe=wholedf, y=x))
  totdf$fees_total <- f
  totdf$items_total <- i
  totdf$rev_total <- r
}
# Function that makes a dataframe with totals for fees, items, revenue for all 
  #  fifty years for dataframe in argument
check_totals <- function(df) {
  years<-c(1970, 1971, 1972, 1973,1974, 1975, 1976, 1977, 1978, 1979)
  check_70 <- data.frame(years)
  make_df(df, check_70)
  years <- map(years, function(x) x+10)
  check_80 <- data.frame(years)
  make_df(df, check_80)
  years <- map(years, function(x) x+10)
  check_90 <- data.frame(years)
  make_df(df, check_90)
  years <- map(years, function(x) x+10)
  check_00 <- data.frame(years)
  make_df(df, check_00)
  years <- map(years, function(x) x+10)
  check_10 <- data.frame(years)
  make_df(df, check_10)
  years <- map(years, function(x) x+10)
  check_20 <- data.frame(years)
  make_df(df, check_20)
}
#############################

check_totals(comm)
check_totals(fishbiz)
check_totals(hunting)
check_totals(special)
check_totals(sport)

