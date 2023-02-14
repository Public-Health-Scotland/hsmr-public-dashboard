###############################################.
## HSMR public dashboard ----
# This script reads the latest version of each file and saves it in your local repository.
###############################################.

###############################################.
## Set up environment ----
###############################################.

library(hsmr)

# Setting file permissions to anyone to allow writing/overwriting of project files
Sys.umask("006")

# Define Whether Running on Server or Locally
# Covers both the old server and the pro one
if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)",
                                  "x86_64-pc-linux-gnu (64-bit)")) {
  platform <- "server"
} else {
  platform <- "locally"
}

# Define file path for data
data_folder <- dplyr::if_else(platform == "server",
                              '/conf/quality_indicators/hsmr/quarter_cycle/data/',
                              '//stats/quality_indicators/hsmr/quarter_cycle/data/')

lookup_folder <- dplyr::if_else(platform == "server",
                              '/conf/quality_indicators/hsmr/quarter_cycle/Dashboard/lookups/',
                              '//stats/quality_indicators/hsmr/quarter_cycle/Dashboard/lookups/')


###############################################.
## Extract dates ----
###############################################.

# Start and end date of the HSMR period (i.e progress both dates by 3 months each publication)
start_date        <- lubridate::dmy(01102021) # UPDATE
end_date          <- lubridate::dmy(30092022) # UPDATE

# Publication dates
pub_day <- pub_date(end_date = end_date, "current")
next_pub <- pub_date(end_date = end_date, "next")

# Create a file that can be picked up in shiny app scripts
dates <- data.frame(start_date, end_date, pub_day, next_pub)
saveRDS(dates, paste0("shiny_app/data/pub_dates.rds"))



###############################################.
## Saving data ----
###############################################.

# Read in data that has been produced by the publication RAP process
smr <- readRDS(paste0(data_folder, pub_day, "/output/", pub_day, "_SMR_data_public_dashboard.rds"))
trend <- readRDS(paste0(data_folder, pub_day, "/output/", pub_day, "_trend_data_public_dashboard.rds"))
lookup <- readRDS(paste0(lookup_folder, "geo_lookup.rds"))

# Save copies of data files in shiny folder
saveRDS(smr, paste0("shiny_app/data/", pub_day,"-smr-data.rds"))
saveRDS(trend, paste0("shiny_app/data/", pub_day,"-trend-data.rds"))
saveRDS(lookup, paste0("shiny_app/lookups/geo_lookup.rds"))


# END OF SCRIPT
