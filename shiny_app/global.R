# Global - HSMR public dashboard

library(shiny)
library(plotly)           # for charts
library(shinyWidgets)     # for dropdowns
library(dplyr)            # for data manipulation
library(DT)               # for data table
library(shinycssloaders)  # for loading icons, see line below
# it uses github version devtools::install_github("andrewsali/shinycssloaders")
# This is to avoid issues with loading symbols behind charts and perhaps with bouncing of app
library(shinyjs)          # for enable/disable functions
library(readr)            # for writing/reading csvs
library(stringr)          # for manipulating strings
library(flextable)
library(shinyBS)          # for collapsible panels in commentary
library(zoo)
library(magrittr)
library(shinymanager)
library(lubridate)
library(readxl)
library(phsstyles)
library(formattable)
library(scales)
library(shinydashboard)

###############################################.
## Set up environment ----
###############################################.

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

###############################################.
## Things to update each publication ----
###############################################.

# Define publication date
pub_day <-lubridate::dmy(09112021)

# Define the HSMR period
latest_hsmr <- c("July 2020 to June 2021")

# Also check that the list of locations (~line 135) does not need updated for this publication.

###############################################.
## Data ----
###############################################.

# Read in data that has been prepared by the hsmr_data_prep.R script
trend <- readRDS(paste0("data/", pub_day, "-hsmr-trend-data.rds"))
hsmr <- readRDS(paste0("data/", pub_day, "-hsmr-data.rds"))

# Sort quarters in chronological order
hsmr <- hsmr %>% arrange(hsmr[,"q_num"])

# Read in lookups that are used
geo_lookup <- readRDS("data/geo_lookup.rds")
geo_lookup_hb <- readRDS("data/geo_lookup.rds") %>%
  filter(areatype %in% c("Scotland", "NHS Board of treatment"))


###############################################.
## Objects, names, lists ----
###############################################.

# List of indicator options on further analysis tab
indicator_list_fa <- c("Crude mortality (%) within 30 days of discharge" = "Discharge",
                       "Crude mortality per 1,000 population" = "Population")

# List of sub groups for Crude trends tab
subgroup_list <- c("All admissions", "Admission type", "Age group", "Deprivation",
                   "Sex", "Place of death", "Specialty")

# List of sections in Home tab
home_list <- c("About HSMR" = "about",
               "Using the dashboard" = "use",
               "Further information" = "info",
               "Accessibility" = "accessibility")

# List of quarters for HSMR time period drop-down
timeperiod_list <- c(unique(hsmr$period_label))

# List of HBs, used in Further analysis tab
hb_list <- c("Scotland",
             "NHS Ayrshire & Arran",
             "NHS Borders",
             "NHS Dumfries & Galloway",
             "NHS Fife",
             "NHS Forth Valley",
             "NHS Grampian",
             "NHS Greater Glasgow & Clyde",
             "NHS Highland",
             "NHS Lanarkshire",
             "NHS Lothian",
             "NHS Orkney",
             "NHS Shetland",
             "NHS Tayside",
             "NHS Western Isles",
             "Golden Jubilee")

# List of HBs and codes - required for the HSMR funnel and table
hsmr_hb_list <- c("Scotland" = "Scotland",
             "NHS Ayrshire & Arran" = "S08000015",
             "NHS Borders" = "S08000016",
             "NHS Dumfries & Galloway" = "S08000017",
             "NHS Fife" = "S08000029",
             "NHS Forth Valley" = "S08000019",
             "NHS Grampian" = "S08000020",
             "NHS Greater Glasgow and Clyde" = "S08000031",
             "NHS Highland" = "S08000022",
             "NHS Lanarkshire" = "S08000032",
             "NHS Lothian" = "S08000024",
             "NHS Orkney" = "S08000025",
             "NHS Shetland" = "S08000026",
             "NHS Tayside" = "S08000030",
             "NHS Western Isles" = "S08000028",
             "Golden Jubilee" = "S08100001")


# List of locations for Crude trends tab
# IMPORTANT: update this list if there are any changes to the hospitals included
location_list <- list(
  "Scotland" = c("Scotland"),
  "NHS Ayrshire & Arran" = c("NHS Ayrshire & Arran", "Arran War Memorial Hospital",
                             "University Hospital Ayr", "University Hospital Crosshouse"),
  "NHS Borders" = c("NHS Borders", "Borders General Hospital"),
  "NHS Dumfries & Galloway" = c("NHS Dumfries & Galloway", "Dumfries & Galloway Royal Infirmary",
                                "Galloway Community Hospital"),
  "NHS Fife" = c("NHS Fife", "Victoria Hospital"),
  "NHS Forth Valley" = c("NHS Forth Valley", "Forth Valley Royal Hospital"),
  "NHS Grampian" = c("NHS Grampian", "Aberdeen Royal Infirmary", "Dr Gray's Hospital"),
  "NHS Greater Glasgow & Clyde" = c("NHS Greater Glasgow & Clyde", "Glasgow Royal Infirmary",
                                    "Inverclyde Royal Hospital", "Queen Elizabeth University Hospital",
                                    "Royal Alexandra/Vale of Leven"),
  "NHS Highland" = c("NHS Highland", "Belford Hospital", "Caithness General Hospital",
                     "Lorn & Islands Hospital", "Raigmore Hospital"),
  "NHS Lanarkshire" = c("NHS Lanarkshire", "University Hospital Hairmyres",
                        "University Hospital Monklands", "University Hospital Wishaw"),
  "NHS Lothian" = c("NHS Lothian", "Royal Infirmary of Edinburgh at Little France",
                    "St John's Hospital", "Western General Hospital"),
  "NHS Orkney" = c("NHS Orkney", "The Balfour"),
  "NHS Shetland" = c("NHS Shetland", "Gilbert Bain Hospital"),
  "NHS Tayside" = c("NHS Tayside", "Ninewells Hospital", "Perth Royal Infirmary",
                  "Stracathro Hospital"),
  "NHS Western Isles" = c("NHS Western Isles", "Western Isles Hospital"),
  "Golden Jubilee" = c("Golden Jubilee National Hospital"))

# List of variables to be renamed for the csv data download
trend_variable_names <- c("period_label" = "label_short",
                    "period_number" = "mth_qtr",
                    "subgroup" = "sub_grp",
                    "group" = "label",
                    "patients" = "pats",
                    "crude_rate" = "crd_rate",
                    "scotland_deaths" = "scot_deaths",
                    "scotland_patients" = "scot_pats",
                    "scotland_crude_rate" = "scot_crd_rate")

# List of variables to be renamed for the csv data download
hsmr_variable_names <- c("predicted_deaths" = "pred",
                    "patients" = "pats",
                    "crude_rate" = "crd_rate",
                    "standardised_mortality_ratio" = "smr",
                    "scotland_standardised_mortality_ratio" = "smr_scot",
                    "scotland_deaths" = "death_scot",
                    "scotland_patients" = "pats_scot",
                    "upper_warning_limit" = "uwl",
                    "upper_control_limit" = "ucl",
                    "lower_control_limit" = "lcl",
                    "lower_warning_limit" = "lwl")

###############################################.
## Palettes and plot parameters ----
###############################################.

## Colour palettes
# PHS colour palette from phsstyles package
chart_colours <- as.character(phs_colours()[1:8])

# PHS colour palette for 2 categories
palette2 <- phs_colours(c("phs-magenta", "phs-teal-80"))

# PHS colour palette for 5 categories
palette5 <- c("#391E4F","#3F3685", "#6861A2", "#938DBE", "#BEBAD9")


## Chart parameters
# Style of x and y axis
xaxis_plots <- list(title = FALSE, fixedrange=TRUE, ticks="outside",
                     rangemode="tozero", dtick = 2, range = FALSE)


yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE,
                    ticks = "outside", showline=TRUE, range = FALSE)

# Buttons to remove
bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d',
                     'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',
                     'hoverClosestCartesian', 'zoom2d', 'pan2d', 'resetScale2d')


## END

