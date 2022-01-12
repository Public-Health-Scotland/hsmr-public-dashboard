# Data prep - HSMR public dashboard

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
trend <- read.csv(paste0(data_folder, pub_day, "/output/", pub_day, "_trends-data-level1.csv"))



