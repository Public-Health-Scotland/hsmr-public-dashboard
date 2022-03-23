# Data prep for Shiny HSMR public dashboard
# Output from HSMR publication currently needs reconfigured to be more effective
# to be used as input for Shiny dashboard

library(dplyr)            # for data manipulation
library(readr)            # for writing/reading csvs
library(stringr)          # for manipulating strings
library(lubridate)
library(janitor)


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

### TREND

trend <- read_csv(paste0(data_folder, pub_day, "/output/", pub_day, "_trends-data-level1.csv"),
                  col_types = cols(
                    quarter = col_double(),
                    quarter_short = col_character(),
                    quarter_full = col_character()))


#create scotland crude rate
trend %<>% mutate(scot_crd_rate = (scot_deaths/scot_pats)*100)

#create generic labels and remove redundant variables
trend %<>%
  mutate(label_short = case_when(time_period == "Quarter" ~ quarter_short,
                                 time_period == "Month" ~ month_label),
         mth_qtr = case_when(time_period == "Quarter" ~ quarter,
                             time_period == "Month" ~ month)) %>%
  select(hb, location, location_name, agg_label, time_period, mth_qtr, label_short,
         sub_grp, label, deaths, pats, crd_rate, scot_deaths, scot_pats,
         scot_crd_rate, completeness_date)

saveRDS(trend, file = paste0("shiny_app/data/", pub_day, "-hsmr-trend-data.rds"))


### HSMR

# read data
hsmr <- read_csv(paste0(data_folder, pub_day, "/output/", pub_day, "_SMR-data_dashboard.csv"))

# List of locations included in the Excel tables/dashboard files/markdown
hosp_filter = c('A101H', 'A111H', 'A210H', 'B120H', 'D102H', 'F704H',
                'G107H', 'C313H', 'G405H', 'C418H', 'H212H', 'H103H', 'C121H',
                'H202H', 'L302H', 'L106H', 'L308H', 'N101H', 'N411H', 'R103H',
                'S314H', 'S308H', 'S116H', 'T101H', 'T202H', 'T312H', 'V217H',
                'W107H', 'Y146H', 'Y144H', 'Z102H', 'Scot')

# Need to update the HB codes to be the 2019 codes - this could be added to RAP process
hsmr %<>% mutate(hb = case_when(hb == "S08000018" ~ "S08000029",
                           hb == "S08000027" ~ "S08000030",
                           hb == "S08000021" ~ "S08000031",
                           hb == "S08000023" ~ "S08000032",
                           TRUE ~ hb))

# Create a quarter number to enable sorting - this could be added to RAP process
hsmr %<>% mutate(q_num = case_when(period_label == "April 2018 to March 2019" ~ 1,
                                   period_label == "July 2018 to June 2019" ~ 2,
                                   period_label == "October 2018 to September 2019" ~ 3,
                                   period_label == "January 2019 to December 2019" ~ 4,
                                   period_label == "April 2019 to March 2020" ~ 5,
                                   period_label == "July 2019 to June 2020" ~ 6,
                                   period_label == "October 2019 to September 2020" ~ 7,
                                   period_label == "January 2020 to December 2020" ~ 8,
                                   period_label == "April 2020 to March 2021"~ 9,
                                   period_label == "July 2020 to June 2021" ~ 10))


# create warning and control confidence limits for funnel plot
hsmr %<>%
  filter(period == 3 & location %in% c(hosp_filter)) %>%
  mutate(st_err = round_half_up(sqrt(1/round_half_up(pred, 8)), 8),
         z = if_else(location_type == "hospital",
                     round_half_up(((round_half_up(smr, 8) - 1)/round_half_up(st_err,8)), 8),
                     0)) %>%
  mutate(
    z_max = max(z),
    z_min = min(z),
    z_flag = case_when(z == z_max ~ 1,
                       z == z_min ~ -1,
                       TRUE ~ 0),
    z = if_else(z == z_max | z == z_min, 0, z),
    z_max = max(z),
    z_min = min(z),
    z = case_when(z_flag == 1 ~ z_max,
                  z_flag == -1 ~ z_min,
                  TRUE ~ z),
    z_flag = if_else(z != 0, 1, 0),
    w_score = round_half_up(sqrt(sum(round_half_up(z * z, 8))/sum(z_flag)),8)) %>%
  # Calculate funnel limits for funnel plot
  mutate(uwl = 1 + 1.96 * round_half_up(st_err * w_score,8),
         ucl = 1 + 3.09 * round_half_up(st_err * w_score,8),
         lwl = 1 - 1.96 * round_half_up(st_err * w_score,8),
         lcl = 1 - 3.09 * round_half_up(st_err * w_score,8)) %>%

  # Create flag for where hospital sits on funnel plot
  mutate(flag = case_when(smr > ucl ~ "2",
                          smr > uwl & smr <= ucl ~ "1",
                          smr <lcl ~ "3",
                          smr <lwl & smr >= lcl ~ "4",
                          TRUE ~ "0"))


         #  flag_above_ucl = case_when(smr > ucl ~ TRUE,
         #                        TRUE ~ FALSE),
         # flag_below_lcl = case_when(smr < lcl ~ TRUE,
         #                        TRUE ~ FALSE))


hsmr %<>% select(hb, location, location_name, q_num, period_label, deaths, pred, pats, smr, crd_rate, smr_scot, death_scot, pats_scot,
                 uwl, ucl, lwl, lcl, flag, completeness_date)


saveRDS(hsmr, file = paste0("shiny_app/data/", pub_day, "-hsmr-data.rds"))

# END
