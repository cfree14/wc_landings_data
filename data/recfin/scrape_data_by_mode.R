
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(RSelenium)
library(tidyverse)

# Directories
outputdir <- "data/recfin/raw/cte003"

# Loop through
################################################################################

# Firefox profile
# Based on this link:
# https://yizeng.me/2014/05/23/download-pdf-files-automatically-in-firefox-using-selenium-webdriver/
fprof <- makeFirefoxProfile(list(
  browser.download.folderList = 2L,
  browser.download.dir = outputdir,
  browser.helperApps.neverAsk.saveToDisk = "application/pdf",
  pdfjs.disabled = TRUE,
  plugin.scan.plid.all = FALSE,
  plugin.scan.Acrobat = "99.0"
))

# Firefox profile
# Based on this link:
# https://www.seanwarlick.com/post/2016-11-19-rselenium/
# fprof <- makeFirefoxProfile(list(
#   browser.download.folderList = 2L,
#   browser.download.manager.showWhenStarting = FALSE,
#   browser.download.dir = outputdir,
#   browser.helperApps.neverAsk.openFile = "application/pdf",
#   browser.helperApps.neverAsk.saveToDisk = "application/pdf",
#   browser.helperApps.alwaysAsk.force = FALSE,
#   browser.download.manager.showAlertOnComplete = FALSE,
#   browser.download.manager.closeWhenDone = TRUE
# ))

# Launch RSelenium server and driver
rD <- rsDriver(browser=c("firefox"), extraCapabilities = fprof)
remDr <- rD[["client"]]
remDr$open(silent=T)

# Base URL
base_url <- "https://reports.psmfc.org/recfin/f?p=601:1000:5937884473268:::::"

# Navigate to page
remDr$navigate(base_url)

# Loop through years
years <- c(2015, 2004)
for(i in 1:length(years)){
  
  # Year
  year <- years[i]
  year_chr <- as.character(year)
  print(year)

  # Click CTE003 Total Mortality by Mode
  submit_button <- remDr$findElement(using="link text", "CTE003 Total Mortality by Mode")
  submit_button$clickElement()
  Sys.sleep(3)
  
  # Click Filter button
  filter_button <- remDr$findElement(using="id", "B88333917457936928")
  filter_button$clickElement()
  Sys.sleep(3)
  
  # Enter year 1 
  # I'm saving this as a reminder for how you build the select code
  # yr1_entry <- remDr$findElement(using="xpath", "//select[@id = 'P3_MIN_YEAR']/option[@value='2013']")
  xpath <- paste0("//select[@id = 'P3_MIN_YEAR']/option[@value='", year_chr, "']")
  yr1_entry <- remDr$findElement(using="xpath", xpath)
  yr1_entry$clickElement()
  Sys.sleep(1)
  
  # Enter year 2
  xpath2 <- paste0("//select[@id = 'P3_MAX_YEAR']/option[@value='", year_chr, "']")
  yr2_entry <- remDr$findElement(using="xpath", xpath2)
  yr2_entry$clickElement()
  
  # Press Apply button
  apply_button <- remDr$findElement(using="id", "filters-submit")
  apply_button$clickElement()
  
  # Press Download button
  download_button <- remDr$findElement(using="id", "B88334031602936929")
  download_button$clickElement()
  
  # Press CSV button
  csv_button <- remDr$findElement(using="class", "t-Card-wrap")
  csv_button$clickElement()
  
  # Return to base URL
  remDr$navigate(base_url)
  
  # Pause for a second
  Sys.sleep(3)

}


