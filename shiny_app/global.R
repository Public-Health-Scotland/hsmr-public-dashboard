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

#smr <- read.csv(paste0(data_folder, pub_day, "/output/", pub_day, "_SMR-data_dashboard.csv"))
trend <- readRDS(paste0("data/", pub_day, "-hsmr-trend-data.rds"))

geo_lookup <- readRDS("data/geo_lookup.rds")
geo_lookup_fa <- readRDS("data/geo_lookup.rds") %>%
  filter(areatype %in% c("Scotland", "NHS Board of treatment"))




###############################################.
## Objects, names, lists ----
###############################################.

# List of indicator options on further analysis tab
indicator_list_fa <- c("Crude mortality (%) within 30 days of discharge" = "Discharge",
                       "Crude population mortality per 1,000 population" = "Population")


###############################################.
## Palettes and plot parameters ----
###############################################.

#Palette for 7 series in a gradient
pal_age <- c('#543005', '#8c510a', '#bf812d',  '#d0d1e6',
            '#74add1', '#4575b4', '#313695')

# pal_eth <- c('#3F3685', '#7872A9', '#C5C3DA', '#9B4393', '#B97BB3', '#E1C7DF',
#              '#1E7F84', '#61A5A8', '#BCD9DA', '#0078D4', '#4CA0E0', '#B2D6F2',
#              '#4A405D', '#978CA9')

pal_eth <- c('#3F3685', '#7872A9', '#C5C3DA', '#9B4393', '#B97BB3', '#E1C7DF',
             '#1E7F84', '#61A5A8', '#BCD9DA', '#0078D4', '#4CA0E0', '#B2D6F2',
             '#4A405D', '#978CA9')



# #Style of x and y axis
# xaxis_plots <- list(title = FALSE, tickfont = list(size=14), titlefont = list(size=14),
#                     showline = TRUE, fixedrange=TRUE)
#
# yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE, size = 4,
#                     tickfont = list(size=14), titlefont = list(size=14))

# Buttons to remove
bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d',
                     'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',
                     'hoverClosestCartesian', 'zoom2d', 'pan2d', 'resetScale2d')

## END

