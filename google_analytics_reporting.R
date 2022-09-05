# Extracting google analytics data for wider impacts
# From https://code.markedmondson.me/googleAnalyticsR/articles/setup.html
# Google Analytics API guide: https://ga-dev-tools.web.app/dimensions-metrics-explorer/

###############################################.
## Packages ----
###############################################.

library(googleAnalyticsR) #to extract google analytics data
library(plotly) # for charts
library(dplyr) #for data manipulation
library(magrittr) # for pipe operators
library(lubridate) #for date operations
library(rmarkdown) # for running report
library(janitor) #to clean names

###############################################.
## Lookups/filepaths ----
###############################################.

data_folder <- "/conf/quality_indicators/hsmr/quarter_cycle/Dashboard/google_analytics/"
# Lookup of event names and their equivalent tab name
list_events <- data.frame( stringsAsFactors = F,
                           eventlabel = c( "hsmr", "fa", "home", "crude"),
                           tabname = c("HSMR", "Further analysis", "Home", "Crude trends"))

###############################################.
## Connecting to GA and extracting data ----
###############################################.

# Select 1: Yes to say you wish to keep your OAuth access credentials.
# The library should then launch a browser window and ask you to login to Google - 
# log in with an email that has access to your Google Analytics - it will take you 
# to a screen with an OOB token. Copy-paste that token back into RStudio:
# email_login is an object with the email address of the account with access to GA
readRenviron(".env")
email_login <- Sys.getenv("email_login")
ga_auth(email = email_login)

# See list of views/tables you have access to
View(ga_account_list("ga4"))

## View account_list and pick the viewId you want to extract data from. 
ga_id <- 319028171 
start_date <- "2022-06-10"
last_date <- paste0(Sys.Date())
# last_year <- paste0(Sys.Date() - 365)

# View(ga_meta("data", propertyId = ga_id))

## Query getting sessions data from first day we got data from to today
sessions_ga <- ga_data(ga_id,
                       date_range = c(start_date, last_date),
                       metrics = c("sessions", "averageSessionDuration"),
                       dimensions = c("date"))

## Query to obtain where people comes from and what they use
source_ga <- ga_data(ga_id,
                     date_range = c(start_date, last_date),
                     metrics = c("sessions", "averageSessionDuration"),
                     dimensions = c('sessionSource','sessionMedium', "deviceCategory"))

## Query to obtain country/city of users
geo_ga <- ga_data(ga_id,
                  date_range = c(start_date, last_date),
                  metrics = c("sessions", "averageSessionDuration"),
                  dimensions = c('city','region', 'country'))

## Query getting events data from first day we got data from to today
events_ga <- ga_data(ga_id,
                     date_range = c(start_date, last_date),
                     metrics = c("eventCount"),
                     dimensions = c("date", "customEvent:event_label")) %>% 
  clean_names() %>%  rename(eventlabel = custom_event_event_label)

###############################################.
## Formatting data ----
###############################################.
# Session data 

# Calculating week ending and aggregating to obtain totals
sessions <- sessions_ga %>% 
  mutate(week_ending = ceiling_date(date, "week", change_on_boundary = F),
         tot_session = sessions * averageSessionDuration) %>% 
  group_by(week_ending) %>% 
  summarise(count = sum(sessions, na.rm = T),
            tot_session = sum(tot_session, na.rm = T)) %>% ungroup() %>% 
  mutate(session_ave = round(tot_session/count/60, 1))

saveRDS(sessions, paste0(data_folder, "sessions.rds"))

###############################################.
# Events data 

# Joining to obtain names
events <- left_join(events_ga, list_events) 

# Filtering out tab menus and not sets (these are internal links with no value)
events %<>% filter(!eventlabel %in% c("(not set)"))

# Creating yearly totals
events_year <- events %>% 
  group_by(tabname) %>% 
  summarise(count = sum(event_count, na.rm = T)) %>% ungroup()

saveRDS(events_year, paste0(data_folder, "tabvisits_yearly.rds"))

# Calculating week ending and aggregating to obtain totals
events %<>% 
  mutate(week_ending = ceiling_date(date, "week", change_on_boundary = F)) %>% 
  group_by(week_ending, tabname) %>% 
  summarise(count = sum(event_count, na.rm = T)) %>% ungroup()

saveRDS(events, paste0(data_folder, "tabvisits.rds"))

###############################################.
# Source data
source_agg <- source_ga %>% 
  mutate(tot_session = sessions * averageSessionDuration) %>% 
  group_by(sessionSource, sessionMedium) %>% 
  summarise(count = sum(sessions),
            tot_session = sum(tot_session, na.rm = T)) %>% ungroup() %>% 
  mutate(session_ave = round(tot_session/count/60, 1)) %>% 
  # slice_max(count, n = 20) %>% # selecting only top 20
  select(Source = sessionSource, Sessions = count, 
         "Average session length (minutes)" = session_ave)

saveRDS(source_agg, paste0(data_folder, "source.rds"))

###############################################.
# Device + session duration 
device <- source_ga %>% 
  mutate(tot_session = sessions * averageSessionDuration) %>% 
  group_by(deviceCategory) %>% 
  summarise(count = sum(sessions),
            sessionDuration = sum(tot_session)) %>% ungroup() %>% 
  mutate(session_ave = round(sessionDuration/count/60, 1),
         device = make_clean_names(deviceCategory, case = "title")) 

tot <- device %>%
  mutate(device = "Total") %>% 
  group_by(device) %>% 
  summarise(count = sum(count),
            sessionDuration = sum(sessionDuration)) %>% ungroup() %>% 
  mutate(session_ave = round(sessionDuration/count/60, 1))

device <- bind_rows(tot, device) %>% 
  select(Device = device, Sessions = count, 
         "Average session length (minutes)" = session_ave)

saveRDS(device, paste0(data_folder, "device.rds"))

###############################################.
# Geography of users - Not adding this at the moment to the report
city_top <- geo_ga %>% 
  mutate(tot_session = sessions * averageSessionDuration) %>% 
  group_by(city) %>% 
  summarise(count = sum(sessions),
            sessionDuration = sum(tot_session)) %>% ungroup() %>% 
  mutate(session_ave = round(sessionDuration/count/60, 1)) 

country_top <- geo_ga %>% 
  mutate(tot_session = sessions * averageSessionDuration) %>% 
  group_by(country) %>% 
  summarise(count = sum(sessions),
            sessionDuration = sum(tot_session)) %>% ungroup() %>% 
  mutate(session_ave = round(sessionDuration/count/60, 1)) 

###############################################.
## Creating report ----
###############################################.
rmarkdown::render("google_analytics/ga_report.Rmd")

## END