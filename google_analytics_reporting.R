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

data_folder <- "/conf/PHSCOVID19_Analysis/shiny_input_files/google_analytics/"

# Lookup of event names and their equivalent tab name
# list_events <- data.frame( stringsAsFactors = F,
#                            eventlabel = c("apgar",  "booking", "breastfeeding", "cancer", 
#                                           "cardio",  "child_dev", "child_health", 
#                                           "comment", "drugs", "gestation", "imm", "inductions", "injuries", "intro", 
#                                           "mentalhealth", "mod", "perinatal_mortality",  "preterm", 
#                                           "sact", "summary", "table", "tears", "terminations"),
#                            tabname = c("Apgar scores", "Antenatal bookings", "Breastfeeding", "Cancer pathology",
#                                        "Cardiovascular", "Child development", "Child health reviews", "Commentary",
#                                        "Substance use", "Gestation at delivery", "Immunisations", "Induction of labour", "Injuries", "Home page",
#                                        "Mental health", "Mode of delivery", "Stillbirths and infant deaths",
#                                        "Location of extremely preterm deliveries", "SACT", "Summary trends",
#                                        "Data", "Perineal tears", "Termination of pregnancy"),
#                            tabarea = c("Pregnancy/births/babies", "Pregnancy/births/babies", "Child health", 
#                                        "Cancer", "Others", "Child health", "Child health", "Others",
#                                        "Substance use", "Pregnancy/births/babies", "Child health", "Pregnancy/births/babies", "Others", "Others",
#                                        "Others", "Pregnancy/births/babies", "Pregnancy/births/babies",
#                                        "Pregnancy/births/babies", "Cancer", "Others",
#                                        "Others", "Pregnancy/births/babies", "Pregnancy/births/babies"))
# 

###############################################.
## Connecting to GA and extracting data ----
###############################################.

# Select 1: Yes to say you wish to keep your OAuth access credentials.
# The library should then launch a browser window and ask you to login to Google - 
# log in with an email that has access to your Google Analytics - it will take you 
# to a screen with an OOB token. Copy-paste that token back into RStudio:
ga_auth()

# See list of views/tables you have access to
View(ga_account_list("ga4"))

## View account_list and pick the viewId you want to extract data from. 
ga_id <- 319028171
start_date <- "2022-06-10"
last_date <- paste0(Sys.Date())
# last_year <- paste0(Sys.Date() - 365)

View(ga_meta("data", propertyId = ga_id))

## Query getting sessions data from first day we got data from to today
sessions_ga <- ga_data(ga_id,
                       date_range = c(start_date, last_date),
                       metrics = c("sessions", "averageSessionDuration"),
                       dimensions = c("date"))

## Query to obtain where people comes from and what they use
source_ga <- ga_data(ga_id,
                     date_range = c(start_date, last_date),
                     metrics = c("sessions", "averageSessionDuration"),
                     dimensions = c('source','medium', "deviceCategory"))

## Query to obtain country/city of users
geo_ga <- ga_data(ga_id,
                  date_range = c(start_date, last_date),
                  metrics = c("sessions", "averageSessionDuration"),
                  dimensions = c('city','region', 'metro', 'country'))

## Query getting events data from first day we got data from to today
events_ga <- ga_data(ga_id,
                     date_range = c(start_date, last_date),
                     metrics = c("eventCount"),
                     dimensions = c("date", "customEvent:event_label"))

# metrics = c("customEvent:credits_spent"),
# dimensions = c("date","customUser:last_level","customEvent:achievement_id"),
###############################################.
## Formatting data ----
###############################################.
# Session data 

# Calculating week endding and aggregating to obtain totals
sessions <- sessions_ga %>% 
  mutate(week_ending = ceiling_date(date, "week", change_on_boundary = F)) %>% 
  group_by(week_ending) %>% 
  summarise(count = sum(sessions, na.rm = T),
            sessionDuration = sum(sessionDuration)) %>% ungroup() %>% 
  mutate(session_ave = round(sessionDuration/count/60, 1))

saveRDS(sessions, paste0(data_folder, "sessions.rds"))

###############################################.
# Events data 

# Joining to obtain names
events <- left_join(events_ga, list_events) 

# Filtering out tab menus and not sets (these are internal links with no value)
events %<>% 
  filter(!eventlabel %in% c("(not set)", "Births and babies", "Cancer",
                            "Child health", "Pregnancy", "intro"))

# Creating yearly totals
events_year <- events %>% 
  group_by(tabname, tabarea) %>% 
  summarise(count = sum(totalEvents, na.rm = T)) %>% ungroup()

saveRDS(events_year, paste0(data_folder, "tabvisits_yearly.rds"))

# Calculating week ending and aggregating to obtain totals
events %<>% 
  mutate(week_ending = ceiling_date(date, "week", change_on_boundary = F)) %>% 
  group_by(week_ending, tabname, tabarea) %>% 
  summarise(count = sum(totalEvents, na.rm = T)) %>% ungroup()

# Splitting data for each tab area to help with report/plotting
events <- split.data.frame(events, events$tabarea)

saveRDS(events, paste0(data_folder, "tabvisits.rds"))

###############################################.
# Source data
source_agg <- source_ga %>% 
  group_by(source, medium) %>% 
  summarise(count = sum(sessions),
            sessionDuration = sum(sessionDuration)) %>% ungroup() %>% 
  mutate(session_ave = round(sessionDuration/count/60, 1)) %>% 
  slice_max(count, n = 20) %>% 
  select(Source = source, Sessions = count, 
         "Average session length (minutes)" = session_ave)

saveRDS(source_agg, paste0(data_folder, "source.rds"))

###############################################.
# Device + session duration 
device <- source_ga %>% 
  group_by(deviceCategory) %>% 
  summarise(count = sum(sessions),
            sessionDuration = sum(sessionDuration)) %>% ungroup() %>% 
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
  group_by(city) %>% 
  summarise(count = sum(sessions),
            sessionDuration = sum(sessionDuration)) %>% ungroup() %>% 
  mutate(session_ave = round(sessionDuration/count/60, 1)) 

country_top <- geo_ga %>% 
  group_by(country) %>% 
  summarise(count = sum(sessions),
            sessionDuration = sum(sessionDuration)) %>% ungroup() %>% 
  mutate(session_ave = round(sessionDuration/count/60, 1)) 

###############################################.
## Creating report ----
###############################################.
rmarkdown::render("google_analytics/ga_report.Rmd")

## END