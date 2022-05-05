# server - HSMR public dashboard

function(input, output, session) {


  # For debugging
   observeEvent(input$browser, browser())
  ###############################################.
  ## Reactive controls  ----
  ###############################################.

  # Crude trends tab
  # Show list of area names depending on areatype selected
  # output$geoname_ui <- renderUI({
  #   areas_summary <- sort(geo_lookup$areaname[geo_lookup$areatype %in% input$geotype])
  #   selectizeInput("geoname", label = NULL,
  #                  choices = areas_summary,
  #                  multiple = TRUE,
  #                  selected = "Scotland",
  #                  options = list(maxItems = 8))
  # })



  # This updates the location options in the drop downs depending on the sub group selection
  observeEvent(input$subgroup_select, {
    x <- input$subgroup_select

    if (x == "All admissions") {
      trend_label = "Step 2. Select locations of interest"
      trend_choices = location_list
      #shinyjs::show("geoname_ui")
      enable("geotype")
    }

    if (x != "All admissions") {
      trend_label = "Step 2. Scotland level data only for this subgroup"
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



  observeEvent(input$subgroup_select, {
    req(input$subgroup_select)

    toggleState ("timeperiod", condition =
                   input$subgroup_select == "All admissions")
    if (input$subgroup_select != "All admissions") {

      updateRadioGroupButtons(session, "timeperiod",
                              label = "Step 3. Quarterly data only for this subgroup",
                              choices = c("Quarter"))
    }

    else {
      updateRadioGroupButtons(session, "timeperiod",
                              label = "Step 3. Select to view trends by month or quarter",
                              choices = c("Quarter","Month"))

      enable("timeperiod")
    }
  })


  ###############################################.
  ## Modals  ----
  ###############################################.

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
  observeEvent(input$funnel_help, { showModal(funnel_modal) })



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

  # Home
  output$home <- renderUI({

   if (input$home_select == "about") {
     tagList(
       h3(tags$b("Hospital Standardised Mortality Ratios")),
       h4(tags$b(latest_hsmr)),
       p(tags$b(paste0("Publication Date: ", format(pub_day, "%d %B %Y")))),br(),
       p(paste0("This dashboard, which accompanies the quarterly Hospital Standardised
           Mortality Ratio (HSMR) publication, presents the latest HSMR for the period ",
                latest_hsmr, " for hospitals in Scotland. In addition crude mortality
        trends are presented by month and quarter for the last five years. Hospital
        mortality measures have an important role to play in stimulating reflection
        on the quality and safety of patient care.")),
       p(tags$b("What is a Hospital Standardised Mortality Ratio?")),
       p("Hospital Standardised Mortality Ratios adjust death data, also known as
         mortality data, to take account of some of the factors known to affect
         the underlying risk of death. The mix of patients seen varies between
         hospitals, and the mortality rate may be higher or lower at a hospital
         for many reasons; the patients seen, for example, may be more seriously
         ill than average. The adjusted mortality at individual hospitals
         can then be compared to the Scottish average. This approach provides a better
         starting point for investigating hospital mortality than crude mortality rates,
         which do not provide a fair comparison."),
       p(tags$b("How is the HSMR calculated?")),
       p("We calculate HSMRs using information from validated SMR01 records (acute
         inpatient and day case admissions only) which includes patients admitted
         to all medical and surgical specialties in NHS Scotland apart from obstetrics
         and psychiatry. Our calculation takes account of patients who died within
         30 days of hospital admission. This means that HSMR values also include
         some deaths that occurred outside hospital, and excludes deaths that occurred
         in hospital more than 30 days after admission."),
       p(tags$b("What does the HSMR value mean?")),
       p("The Scottish HSMR is 1.00. If an HSMR value for a hospital is less than one,
         this means the number of deaths within 30 days of admission for this hospital is
         fewer than predicted. If an HSMR value for a hospital is greater than one, this
         means the number of deaths within 30 days for this hospital is more than predicted.
         If the number of deaths is more than predicted this does not necessarily mean that
         these were avoidable deaths (i.e. that they should not have happened), or that
         they were unexpected, or were attributable to failings in the quality of care."),

       p(tags$b("Next publication")),
       p("The next release of this publication will be ", tags$b("10 May 2022"), ".")


       ) # tagList
   }




    else if(input$home_select == "use") {
      tagList(
        h3(tags$b("Using the dashboard")),
        p("The dashboard has 4 tabs across the top which can be selected:
          Home, HSMR, Crude trends, and Further analysis."),
        p(tags$li(tags$b("Home: "), "includes sub-sections on the left hand side
          which provide an introduction to the HSMR publication, accessibility
          information and suggested resources to find out more.")),
        p(tags$li(tags$b("HSMR: "), "view the HSMR for each hospital in Scotland for the
          latest 12 month period, and the previous publications.")),
        p(tags$li(tags$b("Crude trends: "), "view the crude mortality trends for all admissions
          and key subgroups including age, sex, deprivation, admission type, specialty
          and place of death by NHS Board of treatment and hospital.")),
        p(tags$li(tags$b("Further analysis: "), "additional data is provided including crude
          mortality trends within 30 days of discharge by NHS Board of treatment
          and population mortality rates by NHS Board of residence.")), br(),

        p(tags$b("Interacting with the dashboard")),
        p("On each tab there are drop-down menus which allow the user to update
          the charts and data tables for a specific NHS Board, hospital,
          or subgroup. On the Crude trends and Further Analysis tabs,
          the location drop-down allows multiple locations to be added to the chart and table.
          These can easily be removed by highlighting the location and deleting."),

        p(tags$b("Downloading data")),
        p(tags$li("There is the option to download data as a csv file by clicking the
        'Download data' button which can be found above the table on each tab.")),
        p(tags$li("To download an image of a chart, click the camera icon in the top-right
        corner of any chart in the dashboard and a png image file will automatically download."))


      ) #tagList
    }





    else if(input$home_select == "info") {
      tagList(
        h3(tags$b("Further information")),
        p("The summary report and full report for this publication can be found
          on the ", tags$a(href="https://publichealthscotland.scot/publications/hospital-standardised-mortality-ratios/",
        "HSMR publication page", target="_blank"), ". For more detailed information about the HSMR publication, visit the ",
          tags$a(href="https://www.isdscotland.org/Health-Topics/Quality-Indicators/HSMR/",
                                                    "HSMR webpages.", target="_blank")),
        p("The ", tags$a(href="https://www.isdscotland.org/Health-Topics/Quality-Indicators/HSMR/Methodology/_docs/HSMR-2019-Technical-Specification.pdf",
                         "Technical Document", target="_blank"), " explains in more detail about how the HSMRs
          are calculated and the ",
          tags$a(href="https://www.isdscotland.org/Health-Topics/Quality-Indicators/HSMR/FAQ/_docs/HSMR-2019-FAQs.pdf",
          "Frequently Asked Questions", target="_blank"), "document answers common
          questions about the HSMR publication."),

        p(tags$b("Data files")),
        p("The data from this dashboard can be downloaded by clicking the 'Download data'
          button above each of the tables, or it is available to download from the ",
          tags$a(href="https://publichealthscotland.scot/publications/hospital-standardised-mortality-ratios/",
                 "publication data files.", target="_blank")),
        p(tags$b("Open data")),
        p("Open data from this publication is available from the ",
          tags$a(href="https://www.opendata.nhs.scot/dataset/hospital-standardised-mortality-ratios",
                 "Scottish Health and Social Care Open Data platform.", target="_blank")),
        p("The code used to produce this publication can be accessed in this ",
        tags$a(href= "https://github.com/Public-Health-Scotland/hsmr", "GitHub repository.",
               target="_blank")),
        p(tags$b("Data sources")),
        p(tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=1&SubID=5",
        "General Acute Inpatient and Day Case - Scottish Morbidity Record (SMR01)", target="_blank")),
        p(tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=3&SubID=13",
                 "National Records of Scotland (NRS) - Deaths Data", target="_blank")),
        p(tags$b("Data completeness")),
        p("Information about the completeness of the SMR01 dataset at the time of this
          publication can be found on the ", tags$a(href="https://beta.isdscotland.org/products-and-services/data-management-hospital-activity/smr-completeness/",
        "SMR completeness webpage.", target="_blank")),
        p(tags$b("Contact us")),
        p("Please contact the ", tags$a(href="mailto:phs.qualityindicators@phs.scot",
        "Quality Indicators team"), "if you have any questions about this publication or dashboard.")

      ) #tagList


    }

    else if (input$home_select == "accessibility") {
      tagList(
        h3(tags$b("Accessibility")),
        p("This website is run by ", tags$a(href="https://www.publichealthscotland.scot/",
                                            "Public Health Scotland", target="_blank"),
          ", Scotland's national organisation for public health. As a new organisation formed
        on 1 April 2020, Public Health Scotland is currently reviewing its web estate. Public
        Health Scotland is committed to making its website accessible, in accordance with
        the Public Sector Bodies (Websites and Mobile Applications) (No. 2) Accessibility
        Regulations 2018. This accessibility statement applies to the dashboard that accompanies
        the HSMR quarterly publication."),
        p(tags$a(href="https://mcmw.abilitynet.org.uk/", "AbilityNet", target="_blank"),
          " has advice on making your device easier to use if you have a disability."),
        p(tags$b("Compliance status")),
        p("This site has not yet been evaluated against Web Content Accessibility Guidelines
        version 2.1 level AA standard."),
        p(tags$b("Reporting any accessibility problems with this website")),
        p("If you wish to contact us about any accessibility issues you encounter on this
        site, please email ", tags$a(href="mailto:phs.qualityindicators@phs.scot", "phs.qualityindicators@phs.scot", ".")),
        p(tags$b("Enforcement procedure")),
        p("The Equality and Human Rights Commission (EHRC) is responsible for enforcing the
        Public Sector Bodies (Websites and Mobile Applications) (No. 2) Accessibility Regulations
        2018 (the ‘accessibility regulations’)."),
        p("If you’re not happy with how we respond to your complaint, ",
          tags$a(href="https://www.equalityadvisoryservice.com/", "contact the Equality Advisory and Support Service (EASS).",
                 target = "_blank")),
        p(tags$b("Preparation of this accessibility statement")),
        p("This statement was prepared on DD MMM 2022. It was last reviewed on DD MMM 2022.")

      )
    }

        })


  # HSMR
  output$hsmr <- renderUI({

    hsmr_chart_title <- paste0("HSMR for deaths within 30 days of admission by hospital; ", input$timeperiod_hsmr)

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
               column(3, actionButton("funnel_help","What is a funnel plot?",
                                      icon = icon('question-circle')))),
      fluidRow(column(12, withSpinner(plotlyOutput("hsmr_chart")))),
      fluidRow(column(3, downloadButton('download_hsmr_data', 'Download data'))),
      fluidRow(column(12, dataTableOutput("hsmr_table"))), br(), br()
          ) #tagList

  })


  # Crude trends
  output$crude_trends <- renderUI({

    trend_chart_title <- paste0("Crude mortality within 30 days of admission; ",
                                input$subgroup_select)

    tagList(
    p("This section presents crude mortality rates for deaths within 30 days of
      admission, which can be used to show trends through time. Since crude mortality
      rates do not account for differences in case mix, they are less appropriate
      for comparing individual hospitals to Scotland. The data is also split by some
      of the factors that affect the underlying risk of death such as age, admission type
      and deprivation."),

    fluidRow(column(12, h4(tags$b(paste0(trend_chart_title)))),
             column(12, withSpinner(plotlyOutput("trend_chart")))) %>%
      br() %>%
      fluidRow(column(3, downloadButton('download_trend_data', 'Download data'))),
      fluidRow(column(12, dataTableOutput("trend_table"))), br(), br()
    ) #tagList

  })


  # Further analysis
  output$further_analysis <- renderUI({

    # dynamic chart title
    fa_chart_title <- case_when(input$indicator_select_fa == "Discharge" ~
                                  paste0("Crude mortality (%) within 30 days of discharge"),
                                input$indicator_select_fa == "Population" ~
                                  paste0("Crude population mortality per 1,000 population"))

    # Indicator description to update depending on selection
    fa_indicator_desc <- case_when(
      input$indicator_select_fa == "Discharge" ~
      paste0("This chart shows the trend in mortality at Scotland and NHS Board of Treatment level according to a definition
      similar to the Summary Hospital-level Mortality Indicator in England. SHMI takes
      account of in-patient mortality and deaths within 30 days of discharge.
      The Scottish HSMR does not include patients that die in hospital more than
      30 days from admission. In Scotland, the decision was made to associate the outcome
      with decisions made at the point of admission - more information can be found in the full report."),
      input$indicator_select_fa == "Population" ~
      paste0("Mortality rates can also be examined by comparing the total number of deaths within the population
      in a given time period and area. This approach provides an overall picture of the mortality in the
      population. Crude population mortality rates for Scotland and NHS Boards of Residence are given
      in this chart using the total number of deaths in each quarter and mid-year population estimates."))

    tagList(fluidRow(column(12, fa_indicator_desc)),
            fluidRow(column(12, h4(tags$b(paste0(fa_chart_title)))),
                     column(12, withSpinner(plotlyOutput("fa_chart")))) %>% br() %>%
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
                             "HSMR: ", round(hsmr$smr,2), "<br>",
                             "Predicted deaths: ", round(hsmr$pred,0)))


    #  hosp_colour <- case_when(hsmr$flag_above_ucl == TRUE ~ "#FF0000",
    #                           hsmr$flag_below_lcl == TRUE ~ "#FF0000",
    #                           TRUE ~ "#0078D4")

    hosp_colour <- case_when(hsmr$flag == 2 ~ "#FF0000", # ucl
                             hsmr$flag == 3 ~ "#FF0000", # lcl
                             hsmr$flag == 1 ~ "#FF6700", # uwl
                             hsmr$flag == 4 ~ "#FF6700", # lwl
                             TRUE ~ "#0078D4")


    # hosp_highlight <- case_when(highlight$flag_above_ucl == TRUE ~ "#FF9999",
    #                          highlight$flag_below_lcl == TRUE ~ "#FF9999",
    #                          TRUE ~ "#80BCEA")

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

  #legend=list(orientation="h",xanchor="center",x=0,y=1)) , x = 0, y = 1


  # Crude trends
  output$trend_chart <- renderPlotly({

    trend <- trend_data() %>%
      mutate(label_short = factor(label_short, levels = unique(trend_data()$label_short)))

    # Information to be displayed in tooltip
    tooltip_trend <- c(paste0(input$timeperiod, ": ", trend$label_short, "<br>",
                              trend$location_name, "<br>",
                              trend$sub_grp, " : ", trend$label, "<br>",
                              "Crude mortality rate (%): ", round(trend$crd_rate,1)))

    # Titles for axes
    yaxis_plots[["title"]] <- "Crude rate (%)"
    xaxis_plots[["title"]]<- input$timeperiod

    if(input$subgroup_select == "All admissions") {

      group_num <- length(unique(trend$location_name))

      plot <- plot_ly(data=trend, x=~label_short) %>%

        # location line
        add_lines(y = ~crd_rate, color= ~location_name, colors = chart_colours[1:group_num],
                  text=tooltip_trend, hoverinfo="text", name = ~location_name) %>%
        #Layout
        layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
               yaxis = yaxis_plots,
               xaxis = xaxis_plots, list(categoryorder = "array", categoryarray = arrange(trend[,"mth_qtr"])),
               legend = list(orientation = "h", x=0, y=1.2)) %>% #position of legend
        #add_annotations(text = "hello", x = -0.1, xref = 'paper', y = 0.5, yref = 'paper', showarrow = FALSE)) %>%
        # leaving only save plot button
        config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
      #legend = list(x = 100, y = 0.5)
      #, xanchor = "left", x=0.55, y=1.2
    }

    else {

      group_num <- length(unique(trend$label))

      plot <- plot_ly(data=trend, x=~label_short) %>%

        # location line
        add_lines(y = ~crd_rate, color = ~label, colors = chart_colours[1:group_num],
                  text=tooltip_trend, hoverinfo="text", name = ~label) %>%
        #Layout
        layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
               yaxis = yaxis_plots,
               xaxis = xaxis_plots, list(categoryorder = "array",
                                         categoryarray = arrange(trend[,"mth_qtr"])),
               legend = list(orientation = "h", x=0, y=1.2)) %>% #position of legend
        # leaving only save plot button
        config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
      #
    }

  }
  )#plotly end


  # Further analysis
  output$fa_chart <- renderPlotly({

    fa <- fa_data() %>%
      mutate(label_short = factor(label_short, levels = unique(fa_data()$label_short)))

    # Information to be displayed in tooltip
    tooltip_fa <- c(paste0(input$timeperiod, ": ", fa$label_short, "<br>",
                           fa$location_name, "<br>",
                           "Crude mortality rate: ", round(fa$crd_rate,1)))

    # Titles for axes
    yaxis_plots[["title"]] <- "Crude rate (%)"
    xaxis_plots[["title"]] <- "Quarter"

    # To ensure correct colours
    group_num <- length(unique(fa$location_name))

    plot <- plot_ly(data=fa, x=~label_short) %>%

      # location line
      add_lines(y = ~crd_rate, color= fa$location_name, colors = chart_colours[1:group_num], text=tooltip_fa, hoverinfo="text",
                name = ~location_name) %>%
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
      mutate(pred = round(pred,0),
             crd_rate = round(crd_rate,1),
             smr = round(smr, 2)) %>%
      select(location_name, period_label, deaths, pred,
             pats, smr, crd_rate) %>%
      rename(Location = location_name, "Period" = period_label, Deaths = deaths,
             "Predicted deaths" = pred, Patients = pats, Crude_rate = crd_rate,
             "Standardised Mortality Ratio (SMR)" = smr)

    table_colnames  <-  gsub("_", " ", colnames(table))

    # have tried to create a vector to reduce repetition, but it hasn't worked.
    datatable(table,
              style = 'bootstrap',
              class = 'table-bordered table-condensed',
              rownames = FALSE,
              options = list(pageLength = 20,
                             dom = 't',
                             autoWidth = TRUE),
              filter = "none",
              colnames = table_colnames)

  })




  # Crude trends
  output$trend_table <- renderDataTable({

    table <- trend_data() %>% select(location_name, label_short, sub_grp, label, deaths,
                                     pats, crd_rate, scot_crd_rate) %>%
      rename(Location = location_name, Time_period = label_short, Subgroup = sub_grp,
             Group = label, Deaths = deaths, Patients = pats, Crude_rate = crd_rate,
             Scotland_crude_rate = scot_crd_rate) %>%
      mutate_if(is.numeric, round, 1)

    table_colnames  <-  gsub("_", " ", colnames(table))

    datatable(table,
              style = 'bootstrap',
              class = 'table-bordered table-condensed',
              rownames = FALSE,
              options = list(pageLength = 20,
                             dom = 't',
                             autoWidth = TRUE),
              filter = "none",
              colnames = table_colnames)

  })


  # Further analysis
  output$fa_table <- renderDataTable({

    fa <- fa_data() %>% select(location_name, label_short, deaths,
                               pats, crd_rate, scot_crd_rate) %>%
      rename(Location = location_name, Quarter = label_short, Deaths = deaths,
             Patients = pats, Crude_rate = crd_rate, Scotland_crude_rate = scot_crd_rate) %>%
      mutate_if(is.numeric, round, 1)

    table_colnames  <-  gsub("_", " ", colnames(fa))

    datatable(fa,
              style = 'bootstrap',
              class = 'table-bordered table-condensed',
              rownames = FALSE,
              options = list(pageLength = 20,
                             dom = 't',
                             autoWidth = TRUE),
              filter = "none",
              colnames = table_colnames)


  })



###############################################.
## Data downloads ----
###############################################.
# This section prepares the data in each tab for download by
# renaming variables where necessary.

# HSMR
hsmr_download <- reactive({

  hsmr_extract <- hsmr_data() %>% select(-hb, - q_num, -flag)
})


  output$download_hsmr_data <- downloadHandler(
    filename ="hsmr_data.csv",
    content = function(file) {
      write_csv(hsmr_download(),
                file) }
  )


# Crude trends
  trends_download <- reactive({

    trend_extract <- trend_data() %>%
      select(-agg_label) %>%
      rename("period_num" = mth_qtr)

  })

  output$download_trend_data <- downloadHandler(
    filename ="crude_mortality_trends_data.csv",
    content = function(file) {
      write_csv(trends_download(),
                file) }
  )

# Further analysis
  fa_download <- reactive({

    fa_extract <- fa_data() %>%
      select(-location, -agg_label, -sub_grp) %>%
      rename("quarter_num" = mth_qtr,
             "quarter_label" = label_short,
             "indicator_label" = label)

  })

  output$download_fa_data <- downloadHandler(
    filename ="further_analysis_data.csv",
    content = function(file) {
      write_csv(fa_download(),
                file) }
  )





  } # server end

  ##END

