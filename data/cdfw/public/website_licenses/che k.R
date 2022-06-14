#checking data

# Read data
comm_fees <- read.csv("AllDecadesFeesCommercialFishing", na.strings=c("N/A", ""))
comm_rev <- read.csv("AllDecadesRevenueCommercialFishing", na.strings=c("N/A", ""))
fishbiz <- read.csv("AllDecadesRevenueFishBusiness", na.strings=c("N/A", ""))
hunting <- read.csv("AllDecadesRevenueHunting", na.strings=c("N/A", ""))
sport <- read.csv("AllDecadesRevenueSportFishing", na.strings=c("N/A", ""))
special <- read.csv("AllDecadesRevenueSpecialPermits", na.strings=c("N/A", ""))

