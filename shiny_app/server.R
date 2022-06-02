# Server - HSMR public dashboard

function(input, output, session) {


  # For debugging
  # observeEvent(input$browser, browser())
  ###############################################.
  ## Reactive controls  ----
  ###############################################.

  # This updates the location options in the drop downs depending on the sub group selection
  observeEvent(input$subgroup_select, {
    x <- input$subgroup_select

    if (x == "All admissions") {
      trend_label = "Step 2. Select locations of interest."
      trend_choices = location_list
      #shinyjs::show("geoname_ui")
      enable("geotype")
    }

    if (x != "All admissions") {
      trend_label = "Step 2. Scotland level data only for this subgroup."
      trend_choices = c("Scotland")
      #hide("geoname_ui")
      disable("geotype")
    }

    updateSelectizeInput(session, "geotype",
                         label = trend_label,
                         choices = trend_choices,
                         selected = trend_choices[1]
    )

  }) #observeEvent


  # This updates the time period options depending on the sub group selection i.e.
  # monthly data is only available for Scotland
  observeEvent(input$subgroup_select, {
    req(input$subgroup_select)

    toggleState ("timeperiod", condition =
                   input$subgroup_select == "All admissions")
    if (input$subgroup_select != "All admissions") {

      updateRadioGroupButtons(session, "timeperiod",
                              label = "Step 3. Quarterly data only for this subgroup.",
                              choices = c("Quarter"))
    }

    else {
      updateRadioGroupButtons(session, "timeperiod",
                              label = "Step 3. Select to view trends by month or quarter.",
                              choices = c("Quarter","Month"))

      enable("timeperiod")
    }
  })


  ###############################################.
  ## Modals  ----
  ###############################################.

  # Text for 'What is a funnel plot?' info button
  funnel_modal <- modalDialog(
    p("A funnel plot is a type of ‘Statistical Process Control’ chart that helps to show
      data at a particular point in time. Funnel plots in this report allow comparisons
      to be made between each hospital and the average for Scotland for a particular period."),
    p(tags$b("Centre line (dashed blue)"), "- Scottish average"),
    p(tags$b("Control limits (red)"), "- plotted at 3 standard deviations from the Scottish average"),
    p(tags$b("Warning limits (orange)"), "- plotted at 2 standard deviations from the Scottish average"),
    p("The limits are wider at the left hand side of the graph because the data points plotted
      here represent smaller hospitals which are made up of fewer observations and subject to
      greater variability. This means that smaller hospitals will appear towards the left
      hand side of the graph and larger hospitals towards the right."),
    p("An overdispersion factor has been applied to the funnel plot limits to reduce
      the effect of possibly false outliers. This is discussed in more detail in
      the ", tags$a(href="https://www.isdscotland.org/Health-Topics/Quality-Indicators/HSMR/Methodology/_docs/HSMR-2019-Technical-Specification.pdf",
                    "Scottish HSMR Technical Document.", target="_blank")),

    p(tags$b("How to interpret a funnel plot")),
    p("Data points outwith the control limits (referred to here as ‘outliers’) are
      said to exhibit ‘special cause variation’. Variations may reflect a number
      of factors, such as characteristics of the patients being cared for (case-mix),
      the quality of clinical care, errors in the data submitted by hospitals or
      even variation by chance. A single apparently high value of the HSMR is not
      sufficient evidence on which to conclude that a poor quality or unsafe service
      is being provided. This is why it is important not to focus solely on ‘outliers’
      when making reliable judgements about the quality of patient care."),
    size = "l",
    easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
    )
  # Link action button click to modal launch
  observeEvent(input$funnel_info, { showModal(funnel_modal) })


  # Text for 'What are the sub groups'' info button
  subgroup_modal <- modalDialog(
    p("The subgroups that we have included are as follows:"),
    p(tags$b("Age group: "),"this refers to the patient's age on admission. The ages are grouped into 20 year age bands."),
    p(tags$b("Admission type: "), "this refers to whether the patient was an elective (planned) admission to hopsital, or
      non-elective (emergency) admission to hospital."),
    p(tags$b("Deprivation: "), "this is based on the patient's postcode of residence using the Scottish Index of Multiple
      Deprivation (SIMD). The SIMD is calculated by the Scottish Government and is a
      relative measure of deprivation across 6,976 small areas (called data zones). If an area is identified as ‘deprived’,
      this can relate to people having a low income but it can also mean fewer resources or opportunities. SIMD looks at the
      extent to which an area is deprived across seven domains: income, employment, education, health, access to services,
      crime and housing."),
    p(tags$b("Sex: "), "refers to the sex recorded on the episode of admission to hospital."),
    p(tags$b("Place of death: "), "this is the location where the patient died; in hospital or in the community (e.g residential
      home, care home, etc.)"),
    p(tags$b("Specialty: "), "we classify specialty group according to the specialty of the consultant/GP/healthcare
    professional who was in charge of the patient episode. The specialties are grouped into seven distinct specialty
    groups: Community, Dental, Emergency, Medical, Paediatrics, Surgery, and Women & Newborn."),
    size = "l",
    easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
    )
  # Link action button click to modal launch
  observeEvent(input$subgroup_info, { showModal(subgroup_modal) })



  ###############################################.
  ## Reactive datasets  ----
  ###############################################.

  # HSMR
  hsmr_data <- reactive({

    hsmr %>%
      select(-completeness_date, -location) %>%
      filter(period_label == input$timeperiod_hsmr & location_name != "Scotland")
  })

  # HSMR - chart highlight
  hsmr_highlight <- reactive({

    hsmr %>%
      select(-completeness_date, -location) %>%
      filter(period_label == input$timeperiod_hsmr & hb == input$hb_hsmr)
  })


  # Crude trends
  trend_data <- reactive({

    trend %>%
      select(-completeness_date, -hb, -location) %>%
      filter(location_name %in% input$geotype &
               time_period == input$timeperiod &
               sub_grp == input$subgroup_select)
  })


  # Further analysis
  fa_data <- reactive({

    trend_fa <- trend %>%
      select(-completeness_date, -hb) %>%
      filter(location_name %in% input$geotype_fa &
               time_period == "Quarter" &
               sub_grp == input$indicator_select_fa)
  })





  ###############################################.
  ##  Reactive layout  ----
  ###############################################.

  # HSMR
  output$hsmr <- renderUI({

    hsmr_chart_title <- paste0("HSMR for deaths within 30 days of admission by hospital: ", input$timeperiod_hsmr)

    main_points <- if(input$timeperiod_hsmr == latest_hsmr) {
          tagList(
          h4(tags$b("Main points for this publication")),

          # This could be automated from the funnel_text function
          p(tags$li("For the period ", input$timeperiod_hsmr, " no hospitals had a
                    significantly higher standardised mortality ratio than the national average."),
            tags$li("For the period ", input$timeperiod_hsmr, " one hospital had a significantly
                    lower standardised mortality ratio than the national average: Western General Hospital (0.75)."))
        )}

    else {}

    tagList(
      # p("HSMR is presented using a 12 month reporting period when making comparisons
      #   against the national average. This is advanced by three months with each
      #   quarterly update."),
      main_points, br(),
      fluidRow(column(9, h4(tags$b(paste0(hsmr_chart_title)))),
               column(3, div(actionButton("funnel_info","What is a funnel plot?",
                                      icon = icon('question-circle')), style = "float: right"))),
      fluidRow(column(12, withSpinner(plotlyOutput("hsmr_chart")))),
      fluidRow(column(3, downloadButton('download_hsmr_data', 'Download data'))),
      fluidRow(column(12, dataTableOutput("hsmr_table"))), br(), br()
          ) #tagList

  })


  # Crude trends
  output$crude_trends <- renderUI({

    trend_chart_title <- paste0("Crude mortality (%) within 30 days of admission: ",
                                input$subgroup_select)

    tagList(
    p("This section presents crude mortality rates for deaths within 30 days of
      admission for the previous 5 years. Since crude mortality rates do not account for differences in case mix, they are less appropriate
      for comparing individual hospitals to Scotland. The data is also split by some
      of the factors that affect the underlying risk of death such as age, admission type
      and deprivation."),

    fluidRow(column(9, h4(tags$b(paste0(trend_chart_title)))),
             column(3, div(actionButton("subgroup_info","What are the subgroups?",
                                    icon = icon('question-circle'), style = "float: right"))),
             column(12, withSpinner(plotlyOutput("trend_chart"))), br(),
             column(3, downloadButton('download_trend_data', 'Download data')),
             column(12, dataTableOutput("trend_table"))), br(), br()
    ) #tagList

  })
  #fluidRow(column(12, div(submitButton(), style = "float: right")))

  # Further analysis
  output$further_analysis <- renderUI({

    # dynamic chart title
    fa_chart_title <- case_when(input$indicator_select_fa == "Discharge" ~
                                  paste0("Crude mortality (%) within 30 days of discharge"),
                                input$indicator_select_fa == "Population" ~
                                  paste0("Crude mortality per 1,000 population"))

    # Indicator description to update depending on selection
    fa_indicator_desc <- case_when(
      input$indicator_select_fa == "Discharge" ~
      paste0("This chart shows the trend in mortality for Scotland and NHS Boards of treatment using a definition
      similar to the Summary Hospital-level Mortality Indicator (SHMI) in England. SHMI takes
      account of inpatient mortality and deaths within 30 days of discharge.
      The Scottish HSMR does not include patients that die in hospital more than
      30 days from admission. In Scotland, we associate the outcome
      with decisions made at the point of admission. More information can be found in the full report."),
      input$indicator_select_fa == "Population" ~
      paste0("An alternative measure of mortality is to compare the total number of deaths within the population
      in a given time period and area. This approach provides an overall picture of the mortality in the
      population. Crude population mortality rates for Scotland and NHS Boards of residence are given
      in this chart using the total number of deaths in each quarter and mid-year population estimates."))

    tagList(fluidRow(column(12, fa_indicator_desc)), br(),
            fluidRow(column(12, h4(tags$b(paste0(fa_chart_title)))),
                     column(12, withSpinner(plotlyOutput("fa_chart")))) %>%
              fluidRow(column(3, downloadButton('download_fa_data', 'Download data'))),
              fluidRow(column(12, dataTableOutput("fa_table"))), br(), br()
    )
  })



  ###############################################.
  ## Charts ----
  ###############################################.


  # HSMR - funnel plot
  output$hsmr_chart <- renderPlotly({

    hsmr <- hsmr_data() %>%
      filter(location_name != "Scotland")

    highlight <- hsmr_highlight() %>%
      filter(location_name != "Scotland")


    # Information to be displayed in tooltip
    tooltip_hsmr <- c(paste0(hsmr$location_name, "<br>",
                             hsmr$period_label, "<br>",
                             "HSMR: ", sprintf("%.2f", hsmr$smr), "<br>",
                             "Predicted deaths: ", format(round(hsmr$pred,0), big.mark=","),"<br>",
                            # "Predicted deaths: ", round(hsmr$pred,0), "<br>",
                            "Observed deaths: ", format(hsmr$deaths, big.mark=",")))
                            #"Observed deaths: ", hsmr$deaths))

     # Assign colours for hospital data points depending on flag
    hosp_colour <- case_when(hsmr$flag == 2 ~ "#FF0000", # ucl
                             hsmr$flag == 3 ~ "#FF0000", # lcl
                             hsmr$flag == 1 ~ "#FF6700", # uwl
                             hsmr$flag == 4 ~ "#FF6700", # lwl
                             TRUE ~ "#0078D4")

    # Assign colours for highlighting selected hospitals
    hosp_highlight <- case_when(highlight$flag == 2 ~ "#FF9999", # ucl
                                highlight$flag == 3 ~ "#FF9999", # lcl
                                highlight$flag == 1 ~ "#FFCD99", # uwl
                                highlight$flag == 4 ~ "#FFCD99", # lwl
                                TRUE ~ "#80BCEA")

    # Define formats
    yaxis_plots[["range"]] <- c(0, 2)
    yaxis_plots[["title"]] <- "HSMR"
    xaxis_plots[["title"]] <- "Predicted deaths"
    xaxis_plots[["dtick"]] <- FALSE

    plot <- plot_ly() %>%

      # Highlight HBs
      add_trace(data=highlight, x=~pred, y = ~smr, hoverinfo='skip',
                type = 'scatter', mode = 'markers', marker = list(color = hosp_highlight, size=20), showlegend=F) %>%
      # Hospital markers
      add_trace(data=hsmr, x=~pred, y = ~smr, text=tooltip_hsmr, hoverinfo="text", name = 'Hospital',
                type = 'scatter', mode = 'markers', marker=list(color = hosp_colour, size=10)) %>%
      # Scotland line
      add_lines(data=hsmr, x=~pred, y = ~smr_scot, mode='line', type='scatter',
                line = list(color = '#003399', dash ='dash'),
                hoverinfo='skip', name = "Scotland") %>%
      # ucl line
      add_lines(data=hsmr, x=~pred, y = ~ucl, mode='line', type='scatter', line = list(color = 'FF0000'),
                hoverinfo='skip', name = "Upper/lower control limit") %>%
      # uwl line
      add_lines(data=hsmr, x=~pred, y = ~uwl, mode='line', type='scatter', line = list(color = '#FFA500'),
                hoverinfo='skip', name = "Upper/lower warning limit") %>%
      # lwl line
      add_lines(data=hsmr, x=~pred, y = ~lwl, mode='line', type='scatter', line = list(color = '#FFA500'),
                hoverinfo='skip', showlegend=F) %>%
      # lcl line
      add_lines(data=hsmr, x=~pred, y = ~lcl, mode='line', type='scatter', line = list(color = 'FF0000'),
                hoverinfo='skip', showlegend=F) %>%
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots,
             xaxis = xaxis_plots,
             legend = list(orientation = "h", x=0, y=1.2)) %>% #position of legend
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
  }
  )#plotly end



  # Crude trends
  output$trend_chart <- renderPlotly({

    trend <- trend_data() %>%
      mutate(label_short = factor(label_short, levels = unique(trend_data()$label_short))) %>%
      filter(label != "Unknown")

    # Information to be displayed in tooltip
    tooltip_trend <- c(paste0(input$timeperiod, ": ", trend$label_short, "<br>",
                              trend$location_name, "<br>",
                              trend$sub_grp, ": ", trend$label, "<br>",
                              "Crude mortality rate (%): ", sprintf("%.1f", trend$crd_rate), "<br>",
                              "Deaths: ", format(trend$deaths, big.mark = ","), "<br>",
                              "Patients: ", format(trend$pats, big.mark = ",")))

    # Titles for axes
    yaxis_plots[["title"]] <- "Crude mortality rate (%)"
    xaxis_plots[["title"]]<- input$timeperiod

    if(input$subgroup_select == "All admissions") {

      group_num <- length(unique(trend$location_name))

      plot <- plot_ly(data=trend, x=~label_short) %>%

        # location line
        add_lines(y = ~crd_rate, color= ~location_name, colors = chart_colours[1:group_num],
                  text=tooltip_trend, hoverinfo="text", name = ~location_name,
                  mode = 'lines+markers', symbol= ~location_name,
                  symbols = list('circle','square','triangle-down', 'x', 'diamond', 'star-square', 'cross'),
                  marker = list(size= 8)) %>%
        #Layout
        layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
               yaxis = yaxis_plots,
               xaxis = xaxis_plots, list(categoryorder = "array", categoryarray = arrange(trend[,"mth_qtr"])),
               legend = list(orientation = "h", x=0, y=1.2)) %>% #position of legend
        # leaving only save plot button
        config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    }

    else {

      group_num <- length(unique(trend$label))

      # assign colour palette depending on subgroup chosen
      if (input$subgroup_select %in% c("Sex", "Admission type", "Place of death")) {
        chart_colours <- palette2
        }

      else if (input$subgroup_select %in% c("Age group", "Deprivation")) {
        chart_colours <- palette5
        }

      else {
        chart_colours
        }

      plot <- plot_ly(data=trend, x=~label_short) %>%

        # location line
        add_lines(y = ~crd_rate, color = ~label,
                  colors = chart_colours[1:group_num],
                  #colors = chart_colours,
                  text=tooltip_trend, hoverinfo="text", name = ~label,
                  mode = 'lines+markers', symbol= ~label,
                  symbols = list('circle','square','triangle-down', 'x', 'diamond', 'star-square', 'cross'),
                  marker = list(size= 8)) %>%
        #Layout
        layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
               yaxis = yaxis_plots,
               xaxis = xaxis_plots, list(categoryorder = "array",
                                         categoryarray = arrange(trend[,"mth_qtr"])),
               legend = list(orientation = "h", x=0, y=1.2)) %>% #position of legend


        # add_lines(y = ~crd_rate, color = ~label,
        #           colors = chart_colours[1:group_num],
        #           #colors = chart_colours,
        #           text=tooltip_trend, hoverinfo="text", name = ~label) %>%
        # add_markers(y = ~crd_rate, x=~label_short, symbol= 'triangle', symbols = ~label) %>%
        # #Layout
        # layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
        #        yaxis = yaxis_plots,
        #        xaxis = xaxis_plots, list(categoryorder = "array",
        #                                  categoryarray = arrange(trend[,"mth_qtr"])),
        #        legend = list(orientation = "h", x=0, y=1.2)) %>% #position of legend


        # leaving only save plot button
        config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)

    }

  }
  )#plotly end


  # Further analysis
  output$fa_chart <- renderPlotly({

    fa <- fa_data() %>%
      mutate(label_short = factor(label_short, levels = unique(fa_data()$label_short)))

    # Titles for axes
    xaxis_plots[["title"]] <- "Quarter"

    if (input$indicator_select_fa == "Discharge") {
      # Information to be displayed in tooltip - discharge indicator
      tooltip_fa <- c(paste0(input$timeperiod, ": ", fa$label_short, "<br>",
                             fa$location_name, "<br>",
                             "Crude mortality rate (%): ", sprintf("%.1f", fa$crd_rate), "<br>",
                             "Deaths: ", format(fa$deaths, big.mark = ","), "<br>",
                             "Patients: ", format(fa$pats, big.mark = ",")))

      yaxis_plots[["title"]] <- "Crude mortality rate (%)"


    } else {
      # Information to be displayed in tooltip - population rate indicator
      tooltip_fa <- c(paste0(input$timeperiod, ": ", fa$label_short, "<br>",
                             fa$location_name, "<br>",
                             "Crude mortality per 1,000 population: ", sprintf("%.1f", fa$crd_rate), "<br>",
                             "Deaths: ", format(fa$deaths, big.mark = ","), "<br>",
                             "Population: ", format(fa$pats, big.mark = ",")))

      yaxis_plots[["title"]] <- "Crude mortality"

    }

    # To ensure correct colours
    group_num <- length(unique(fa$location_name))

    plot <- plot_ly(data=fa, x=~label_short) %>%

      # location line
      add_lines(y = ~crd_rate, color= fa$location_name, colors = chart_colours[1:group_num], text=tooltip_fa, hoverinfo="text",
                name = ~location_name,
                mode = 'lines+markers', symbol= ~location_name,
                symbols = list('circle','square','triangle-down', 'x', 'diamond', 'star-square', 'cross'),
                marker = list(size= 8)) %>%
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots,
             xaxis = xaxis_plots, list(categoryorder = "array", categoryarray = arrange(fa[,"mth_qtr"])),
             legend = list(orientation = "h", x=0, y=1.2)) %>% #position of legend
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)

  }
  )#plotly end


  ###############################################.
  ## Table ----
  ###############################################.

  # HSMR
  output$hsmr_table <- renderDataTable({

    table <- hsmr_highlight() %>%
      filter(hb == input$hb_hsmr) %>%
      mutate(crd_rate = sprintf("%.1f", crd_rate),
             smr = sprintf("%.1f", smr),
             pred = format(round(pred,0), big.mark=","),
             pats = format(pats, big.mark=","),
             deaths = format(deaths, big.mark=",")) %>%
      select(location_name, period_label, deaths, pred,
             pats, smr, crd_rate) %>%
      rename(Location = location_name, "Period" = period_label, "Observed deaths" = deaths,
             "Predicted deaths" = pred, Patients = pats, "Crude mortality rate (%)" = crd_rate,
             "Hospital standardised mortality ratio (HSMR)" = smr)

    datatable(table,
              style = 'bootstrap',
              class = 'table-bordered table-condensed',
              rownames = FALSE,
              options = list(pageLength = 20,
                             dom = 't',
                             autoWidth = TRUE,
                             columnDefs = list(list(className = 'dt-right', targets = 2:6))),
              filter = "none")

  })




  # Crude trends
  output$trend_table <- renderDataTable({

    table <- trend_data() %>% select(location_name, label_short, sub_grp, label, deaths,
                                     pats, crd_rate) %>%
      mutate(crd_rate = sprintf("%.1f", crd_rate),
             pats = format(pats, big.mark=","),
             deaths = format(deaths, big.mark=",")) %>%
      rename(Location = location_name, "Time period" = label_short, Subgroup = sub_grp,
             Group = label, "Deaths" = deaths, Patients = pats, "Crude mortality rate (%)" = crd_rate)



    datatable(table,
              style = 'bootstrap',
              class = 'table-bordered table-condensed',
              rownames = FALSE,
              options = list(pageLength = 20,
                             dom = 't',
                             autoWidth = TRUE,
                             columnDefs = list(list(className = 'dt-right', targets = 4:6))),
              filter = "none")

  })


  # Further analysis
  output$fa_table <- renderDataTable({

    fa <- fa_data() %>% select(location_name, label_short, deaths,
                               pats, crd_rate) %>%
      mutate(crd_rate = sprintf("%.1f", crd_rate),
             pats = format(pats, big.mark=","),
             deaths = format(deaths, big.mark=",")) %>%
      rename(Location = location_name, Quarter = label_short, Deaths = deaths)

    if (input$indicator_select_fa == "Discharge") {
      fa %<>% rename(Patients = pats,
                     "Crude mortality rate (%)" = crd_rate)}
    else {
      fa %<>% rename(Population = pats,
                     "Crude mortality per 1,000 population" = crd_rate) }


    datatable(fa,
              style = 'bootstrap',
              class = 'table-bordered table-condensed',
              rownames = FALSE,
              options = list(pageLength = 20,
                             dom = 't',
                             autoWidth = TRUE,
                             columnDefs = list(list(className = 'dt-right', targets = 2:4))),
              filter = "none")
              #colnames = table_colnames)


  })



###############################################.
## Data downloads ----
###############################################.
# This section prepares the data in each tab for download by
# renaming variables where necessary.

# rename variables - trend and further analysis



# HSMR
  hsmr_download <- reactive({

  hsmr_extract <- hsmr_data() %>%
    select(-hb, - q_num, -flag) %>%
    rename(hsmr_variable_names)
  })


  output$download_hsmr_data <- downloadHandler(
    filename ="hsmr.csv",
    content = function(file) {
      write_csv(hsmr_download(), file) }
  )



# Crude trends
  trends_download <- reactive({

    trend_extract <- trend_data() %>%
      select(-agg_label) %>%
      rename(all_of(trend_variable_names))
    })

  output$download_trend_data <- downloadHandler(
    filename ="crude_mortality_trends.csv",
    content = function(file) {
      write_csv(trends_download(), file) }
  )


# Further analysis
  fa_download <- reactive({

    fa_extract <- fa_data() %>%
      rename(all_of(trend_variable_names)) %>%
      rename("indicator_label" = "group") %>%
      select(-location, -agg_label, -subgroup)
    })

  output$download_fa_data <- downloadHandler(
    filename ="further_analysis.csv",
    content = function(file) {
      write_csv(fa_download(), file) }
  )





  } # server end

  ##END

