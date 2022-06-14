# HSMR public dashboard
# Function for the dynamic funnel plot text.

# A flag was created in the publication process for where hospital sits on funnel plot:
# smr > ucl: 1
# smr < lcl: 2
# smr > uwl & smr <= ucl: 3
# smr <lwl & smr >= lcl: 4
# within limits: 0

####################################################

funnel_text <- function(hsmr_data, indicator = c("above", "below")){

if(indicator == "above"){

flag_hosps <- hsmr_data %>%
    filter(flag == "1")

  n_hosps <- nrow(flag_hosps)

  if(n_hosps == 0){

    output_1 <- paste0("For the period ",
                       latest_hsmr, ", no hospitals had a significantly higher ",
                       "standardised mortality ratio than the national average.")

  }

  if(n_hosps == 1){

    output_1 <- paste0("For the period ",
                       latest_hsmr, ", one hospital had a significantly higher ",
                       "standardised mortality ratio  than the national average",
                       ": ", paste0(flag_hosps$location_name, " (",
                                    sprintf("%.2f", flag_hosps$smr), ")",
                                    collapse = ", "), ".")
  }

  if(n_hosps > 1){

    output_1 <- paste0("For the period ", latest_hsmr, ", ", n_hosps, " had a significantly higher ",
                       "standardised mortality ratio ",
                       latest_hsmr, " than the national average",
                       ": ", paste0(flag_hosps$location_name, " (",
                                    sprintf("%.2f", flag_hosps$smr),
                                    ")", collapse = ", "), ".")
  }

}

if(indicator == "below"){

  flag_hosps <- hsmr_data %>%
    filter(flag == "2")

  n_hosps <- nrow(flag_hosps)

  if(n_hosps == 0){

    output_1 <- paste0("For the period ",
                      latest_hsmr, ", no hospitals had a significantly lower ",
                       "standardised mortality ratio than the national average.")

  }

  if(n_hosps == 1){

    output_1 <- paste0("For the period ",
                       latest_hsmr, ", one hospital had a significantly lower ",
                       "standardised mortality ratio than the national average",
                       ": ", paste0(flag_hosps$location_name, " (",
                                    sprintf("%.2f", flag_hosps$smr), ")",
                                    collapse = ", "), ".")

  }

  if(n_hosps > 1){

    output_1 <- paste0("For the period ",
                       latest_hsmr, ", ", n_hosps, " had a significantly lower ",
                       "standardised mortality ratio "," than the national average",
                       ": ", paste0(flag_hosps$location_name, " (",
                                    sprintf("%.2f", flag_hosps$smr), ")",
                                    collapse = ", "), ".")

  }
}

return(c(output_1))

}
