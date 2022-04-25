
# Clear workspace
# BEWARE - lots of users hate this, I don't
rm(list = ls())

# Setup
################################################################################

# Packages
library(RSelenium)
library(tidyverse)

# Directories
downloaddir <- "/Users/cfree/Downloads"

# Erase downloads folder
# BEWARE! This will delete everything in your downloads folder (you'll see why below)
unlink(paste0(downloaddir, "/*"))

# Loop through
################################################################################

# Launch RSelenium server and driver
rD <- rsDriver(browser=c("firefox"))
remDr <- rD[["client"]]
remDr$open(silent=T)

# Base URL
base_url <- "http://www.adfg.alaska.gov/sf/sportfishingsurvey/index.cfm?ADFG=region.home"

# Navigate to page
remDr$navigate(base_url)

# Get YEARS to loop through
year_menu <- remDr$findElement(using="name", "year")
years_list <- year_menu$getElementText()
years <- strsplit(years_list[[1]], split="\n") %>% unlist()

# Harvest or catch?
catch_type <- "Catch"

# Loop through years
i <- 5
for(i in 1:length(years)){
  
  # Year
  year <- years[i]
  yr2 <- substr(year, 6, 10)
  print(year)

  # Select year
  xpath <- paste0("//select[@name = 'year']/option[@value='",   yr2, "']")
  yr_select <- remDr$findElement(using="xpath", xpath)
  yr_select$clickElement()
  
  # Click catch type
  if(catch_type=="Catch"){
    catch_radio <- remDr$findElement(using="id", "optRecTypeCatch")
    catch_radio$clickElement()
  }else{
    catch_radio <- remDr$findElement(using="id", "optRecTypeHarvest")
    catch_radio$clickElement()
  }
  
  # Click "Get Data"
  get_button <- remDr$findElement(using="css selector", "[value='Get Data >']")
  get_button$clickElement()
  
  # Click "Download As Spreadsheet"
  download_button <- remDr$findElement(using="class name", "exportLink")
  download_button$clickElement()
  
  # Rename and move file
  filename_orig <- list.files(downloaddir)
  outputdir <- paste0("data/adfg/raw/statewide_", catch_type)
  outfile <- paste0(year, "_statewide_", tolower(catch_type), ".xls")
  file.rename(from=file.path(downloaddir, filename_orig), to=file.path(outputdir, outfile))
  
  # Navigate to page
  remDr$navigate(base_url)

}



