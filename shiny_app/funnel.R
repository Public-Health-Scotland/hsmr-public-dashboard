###############################################.
## HSMR public dashboard ----
## Function for the dynamic funnel plot text ----
###############################################.

# A flag was created in the publication process for where hospital sits on funnel plot:
# smr > ucl: 1
# smr < lcl: 2
# smr > uwl & smr <= ucl: 3
# smr < lwl & smr >= lcl: 4
# within limits: 0


funnel_text <- function(hsmr_data, indicator = c("above", "below")){

## Outliers - above UCL ----

if(indicator == "above"){

  flag_hosps <- hsmr_data %>%
    filter(flag == "1")

  # Number of hospitals above UCL
  n_hosps <- nrow(flag_hosps)

  # Names of hospitals above UCL
  hosp_names <- flag_hosps$location_name

  # Update text depending on whether number of flagged hospitals is singular or plural
  hosp_s <- if(n_hosps > 1) {"hospitals"} else {"hospital"}

  # No outliers
  if(n_hosps == 0){

    output_1 <- paste0("For the period ", unique(hsmr_data$period_label),
                       ", no hospitals had a significantly higher standardised ",
                       "mortality ratio than the national average.")
    }


  # One or more outliers above UCL
  if(n_hosps > 0){

    output_1 <- paste0("For the period ", unique(hsmr_data$period_label), ", ", as.english(n_hosps),
                       " ", hosp_s, " had a significantly higher standardised mortality ",
                       "ratio than the national average: ",
                       paste0(flag_hosps$location_name, " (", sprintf("%.2f", flag_hosps$smr), ")",
                              collapse = ", "), ".")
    }

}


## Outliers - below LCL ----

if(indicator == "below"){

  flag_hosps <- hsmr_data %>%
    filter(flag == "2")

  # Number of hospitals below LCL
  n_hosps <- nrow(flag_hosps)

  # Names of hospitals below LCL
  hosp_names <- flag_hosps$location_name

  # Update text depending on whether number of flagged hospitals is singular or plural
  hosp_s <- if(n_hosps > 1) {"hospitals"} else {"hospital"}

  # No outliers
  if(n_hosps == 0){

    output_1 <- paste0("For the period ", unique(hsmr_data$period_label),
                       ", no hospitals had a significantly lower ",
                       "standardised mortality ratio than the national average.")
    }


  # One or more outliers above LCL
  if(n_hosps > 0){

    output_1 <- paste0("For the period ", unique(hsmr_data$period_label), ", ",
                       as.english(n_hosps), " ", hosp_s,
                       " had a significantly lower standardised mortality ratio than ",
                       "the national average: ",
                       paste0(hosp_names, " (", sprintf("%.2f", flag_hosps$smr), ")",
                              collapse = ", "), ".")
    }
}

return(c(output_1))

}

# END