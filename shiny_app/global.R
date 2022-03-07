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

# Define publication date
pub_day <-lubridate::dmy(09112021)


###############################################.
## Data ----
###############################################.

trend <- readRDS(paste0("data/", pub_day, "-hsmr-trend-data.rds"))
hsmr <- readRDS(paste0("data/", pub_day, "-hsmr-data.rds")) %>% arrange(hsmr[,"q_num"])


geo_lookup <- readRDS("data/geo_lookup.rds")
geo_lookup_hb <- readRDS("data/geo_lookup.rds") %>%
  filter(areatype %in% c("Scotland", "NHS Board of treatment"))


###############################################.
## Objects, names, lists ----
###############################################.

# List of indicator options on further analysis tab
indicator_list_fa <- c("Crude mortality (%) within 30 days of discharge" = "Discharge",
                       "Crude population mortality per 1,000 population" = "Population")

subgroup_list <- c("All Admissions", "Admission Type", "Age Group", "Deprivation", "Sex",
                   "Place of Death", "Specialty")

timeperiod_list <- c(unique(hsmr$period_label))

hb_list <- c("Scotland" = "Scotland",
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


###############################################.
## Palettes and plot parameters ----
###############################################.

# PHS colour palette from phsstyles package
chart_colours <- as.character(phs_colours()[1:8])

# chart_colours <- c('#9B4393','#83BB26','#C73918',
#                    '#948DA3','#0078D4','#1E7F84',
#                    '#6B5C85')

# #Style of x and y axis
xaxis_plots <- list(title = FALSE, fixedrange=TRUE, ticks="outside", tickangle = 270,
                     rangemode="tozero")

yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE,
                    ticks = "outside", showline=TRUE, range = FALSE)

# Buttons to remove
bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d',
                     'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',
                     'hoverClosestCartesian', 'zoom2d', 'pan2d', 'resetScale2d')

## END

