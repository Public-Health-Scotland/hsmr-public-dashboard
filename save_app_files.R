###############################################.
## HSMR public dashboard ----
# This script reads the latest version of each file and saves it in your local repository.
###############################################.

###############################################.
## Set up environment ----
###############################################.

#Source functions

source("R/pub_date.R")

# Setting file permissions to anyone to allow writing/overwriting of project files
Sys.umask("006")

# Define file path for data
data_folder <- '/conf/quality_indicators/hsmr/quarter_cycle/data/'
lookup_folder <- '/conf/quality_indicators/hsmr/quarter_cycle/Dashboard/lookups/'

###############################################.
## Extract dates ----
###############################################.

# Start and end date of the HSMR reporting period (i.e for the May publication 
# this should be from 1 January to 31 December)

start_date        <- lubridate::dmy(01012024) # UPDATE
end_date          <- lubridate::dmy(31122024) # UPDATE

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
