# server - HSMR public dashboard

function(input, output, session) {


# For debugging
observeEvent(input$browser, browser())
###############################################.
## Reactive controls  ----
###############################################.

# Crude trends tab
# Show list of area names depending on areatype selected
output$geoname_ui <- renderUI({
  areas_summary <- sort(geo_lookup$areaname[geo_lookup$areatype %in% input$geotype])
  selectizeInput("geoname", label = NULL,
                 choices = areas_summary,
                 multiple = TRUE,
                 selected = "Scotland")
})


# This updates the location options in the drop downs depending on the sub group selection
observeEvent(input$subgroup_select, {
  x <- input$subgroup_select

  if (x == "All Admissions") {
    trend_label = "Step 2. Select a geography and area of interest"
    trend_choices = c("Scotland", "NHS Board of treatment", "Hospital")
    shinyjs::show("geoname_ui")
    enable("geotype")
  }

  if (x != "All Admissions") {
    trend_label = "Step 2. Scotland level data only for this subgroup"
    trend_choices = c("Scotland")
    hide("geoname_ui")
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
               input$subgroup_select == "All Admissions")
  if (input$subgroup_select != "All Admissions") {

     updateSelectizeInput(session, "timeperiod",
                         label = "Step 3. Quarterly data available only",
                         choices = c("Quarter"))
                         }

  else {
    updateSelectizeInput(session, "timeperiod",
                         label = "Step 3. Select to view trends by month or quarter",
                         choices = c("Quarter","Month"))


    enable("timeperiod")
    }
})


# Further analysis tab
# Show list of area names depending on areatype selected
output$geoname_fa_ui <- renderUI({
  areas_summary <- sort(geo_lookup_hb$areaname[geo_lookup_hb$areatype %in% input$geotype_fa])
  selectizeInput("geoname_fa", label = NULL,
                 choices = areas_summary,
                 multiple = TRUE,
                 selected = "Scotland")
})


###############################################.
## Modals  ----
###############################################.

funnel_modal <- modalDialog(
  h5(tags$b("Interpretation of this chart")),
  p("HSMRs are based on all acute inpatient and day case patients admitted to all
    specialties in hospital (apart from obstetrics and psychiatry which are
    excluded). The calculation takes account of patients who died within 30 days
    from admission and includes deaths that occurred in the community as well as
    those occurring in hospitals."),
  p("The Scottish HSMR is 1.00. If an HSMR value for a hospital is less than one,
  this means the number of deaths within 30 days of admission for this hospital is
  fewer than predicted. If an HSMR value for a hospital is greater than one, this
  means the number of deaths within 30 days for this hospital is more than predicted.
  If the number of deaths is more than predicted this does not necessarily mean that
  these were avoidable deaths (i.e. that they should not have happened), or that
  they were unexpected, or were attributable to failings in the quality of care."),
  p("Centre line (dashed blue) - Scottish average"),
  p("Control limits (red) - plotted at 3 standard deviations from the Scottish average"),
  p("Warning limits (orange) - plotted at 2 standard deviations from the Scottish average"),
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
    filter(period_label == input$timeperiod_hsmr)
})

hsmr_highlight <- reactive({

  hsmr %>%
    select(-completeness_date, -location) %>%
      filter(period_label == input$timeperiod_hsmr & hb == input$hb_hsmr)
})


# Crude trends
trend_data <- reactive({

  trend %>%
    select(-completeness_date, -hb, -location) %>%
    filter(location_name %in% input$geoname &
             time_period == input$timeperiod &
             sub_grp == input$subgroup_select)
})


# Further analysis
fa_data <- reactive({

  trend_fa <- trend %>%
    select(-completeness_date, -hb, -location) %>%
    filter(location_name %in% input$geoname_fa &
             time_period == "Quarter" &
             sub_grp == input$indicator_select_fa)
})





###############################################.
##  Reactive layout  ----
###############################################.

# HSMR
output$hsmr <- renderUI({

  hsmr_chart_title <- paste0("HSMR for deaths within 30-days of admission by hospital; ", input$timeperiod_hsmr)


    tagList(p("HSMR is presented using a 12 month reporting period when
making comparisons against the national average. This is advanced by three
months with each quarterly update."),

    fluidRow(column(6, h4(paste0(hsmr_chart_title))),
           column(12, withSpinner(plotlyOutput("hsmr_chart")))) %>% br() %>%

    fluidRow(column(6, h4(paste0(hsmr_chart_title))),
             column(12, dataTableOutput("hsmr_table"))))

})




# Crude trends
output$crude_trends <- renderUI({

  actionButton("jump_commentary_hsmr", "Interpretation of this chart",
               icon = icon('fas fa-exclamation-circle'))

  trend_chart_title <- paste0("Crude mortality within 30 days of admission; ", input$subgroup_select)

  fluidRow(column(6, h4(paste0(trend_chart_title))),
           column(12, withSpinner(plotlyOutput("trend_chart")))) %>% br() %>%

  fluidRow(column(6, h4(paste0(trend_chart_title))),
           column(12, dataTableOutput("trend_table")))

  })


# Further analysis
output$further_analysis <- renderUI({

  # dynamic chart title
  fa_chart_title <- case_when(
    input$indicator_select_fa == "Discharge" ~
      paste0("Crude mortality (%) within 30 days of discharge"),
    input$indicator_select_fa == "Population" ~
      paste0("Crude population mortality per 1,000 population"))

    fluidRow(column(6, h4(paste0(fa_chart_title))),
           column(12, withSpinner(plotlyOutput("fa_chart")))) %>% br() %>%

    fluidRow(column(6, h4(paste0(fa_chart_title))),
             column(12, dataTableOutput("fa_table")))

})



###############################################.
## Charts ----
###############################################.

# HSMR - funnel plot
output$hsmr_chart <- renderPlotly({

  hsmr <- hsmr_data() %>%
    filter(location_name != "Scotland")

  highlight <- hsmr_highlight()


  # Information to be displayed in tooltip
  tooltip_hsmr <- c(paste0(hsmr$location_name, "<br>",
                           hsmr$period_label, "<br>",
                           "HSMR: ", round(hsmr$smr,2)))

  plot <- plot_ly(data=hsmr, x=~pred) %>%

    add_trace(y = ~smr, text=tooltip_hsmr, hoverinfo="text", name = 'Hospital',
              type = 'scatter', mode = 'markers', size=3) %>%


    # Scotland line
    add_lines(y = ~smr_scot, mode='line', type='scatter', line = list(color = '#0B0B45', dash ='dash'),
              text=tooltip_hsmr, hoverinfo="text", name = "Scotland") %>%

    # uwl line
    add_lines(y = ~uwl, mode='line', type='scatter', line = list(color = '#FFA500'),
              text=tooltip_hsmr, hoverinfo="text",
              name = "UWL") %>%
    # ucl line
    add_lines(y = ~ucl, mode='line', type='scatter', line = list(color = 'FF0000'),
              text=tooltip_hsmr, hoverinfo="text",
              name = "UCL") %>%
    # lwl line
    add_lines(y = ~lwl, mode='line', type='scatter', line = list(color = '#FFA500'),
              text=tooltip_hsmr, hoverinfo="text",
              name = "LWL") %>%
    # lcl line
    add_lines(y = ~lcl, mode='line', type='scatter', line = list(color = 'FF0000'),
              text=tooltip_hsmr, hoverinfo="text",
              name = "LCL") %>%
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = list(title = "HSMR", rangemode="tozero", fixedrange=TRUE, range = c(0, 2)),
           xaxis = list(title = "Predicted deaths", fixedrange=TRUE, ticks=2,
                        tickangle = 270, rangemode="tozero"),
           legend = list(x = 100, y = 0.5)) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
}
)#plotly end




# Crude trends
output$trend_chart <- renderPlotly({

  trend <- trend_data() %>%
    mutate(label_short = factor(label_short, levels = unique(trend_data()$label_short)))

  # Information to be displayed in tooltip
  tooltip_trend <- c(paste0(input$timeperiod, ": ", trend$label_short, "<br>",
                            trend$location_name, "<br>",
                            trend$sub_grp, " : ", trend$label, "<br>",
                            "Crude mortality rate: ", round(trend$crd_rate,1)))


  if(input$subgroup_select == "All Admissions") {

   plot <- plot_ly(data=trend, x=~label_short) %>%

    # location line
    add_lines(y = ~crd_rate, color= ~location_name, colors = chart_colours,
              text=tooltip_trend, hoverinfo="text",
              name = ~location_name) %>%
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = list(title = "Crude rate (%)", rangemode="tozero", fixedrange=TRUE),
           xaxis = list(title = input$timeperiod,  fixedrange=TRUE, ticks=2, tickangle = 270,
                        automargin = TRUE,categoryorder = "array", categoryarray = arrange(trend[,"mth_qtr"])),
           legend = list(x = 100, y = 0.5)) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
  }

  else {

    plot <- plot_ly(data=trend, x=~label_short) %>%

      # location line
      add_lines(y = ~crd_rate, color = ~label, colors = chart_colours,
                text=tooltip_trend, hoverinfo="text",
                name = ~label) %>%
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = list(title = "Crude rate (%)", rangemode="tozero", fixedrange=TRUE),
             xaxis = list(title = input$timeperiod,  fixedrange=TRUE, ticks=2, tickangle = 270,
                          automargin = TRUE, categoryorder = "array", categoryarray = arrange(trend[,"mth_qtr"])),
             legend = list(x = 100, y = 0.5)) %>% #position of legend
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
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

  plot <- plot_ly(data=fa, x=~label_short) %>%

    # location line
    add_lines(y = ~crd_rate, colors = chart_colours,
              text=tooltip_fa, hoverinfo="text",
              name = fa$location_name) %>%
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = list(title = "Crude rate", rangemode="tozero", fixedrange=TRUE),
           xaxis = list(title = "Quarter",  fixedrange=TRUE, ticks=2, tickangle = 270,
                        automargin = TRUE, categoryorder = "array", categoryarray = arrange(fa[,"mth_qtr"])),
           legend = list(x = 100, y = 0.5)) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )

}
)#plotly end








###############################################.
## Table ----
###############################################.

# HSMR
output$hsmr_table <- renderDataTable({

  table <- hsmr_highlight() %>%
    filter(hb == input$hb_hsmr) %>%
    select(location_name, period_label, deaths, pred,
                                  pats, smr, crd_rate) %>%
    rename(Location = location_name, "Time period" = period_label, Deaths = deaths,
           "Predicted deaths" = pred, Patients = pats, Crude_rate = crd_rate,
           "Standardised Mortality Ratio (SMR)" = smr) %>%
    mutate_if(is.numeric, round, 2)

  table_colnames  <-  gsub("_", " ", colnames(table))

  datatable(table,
            style = 'bootstrap',
            class = 'table-bordered table-condensed',
            rownames = FALSE,
            options = list(pageLength = 20,
                           dom = 'tip',
                           autoWidth = TRUE),
            filter = "top",
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
                dom = 'tip',
                autoWidth = TRUE),
  filter = "top",
  colnames = table_colnames)

})


# Further analysis
output$fa_table <- renderDataTable({

  fa <- fa_data() %>% select(location_name, label_short, deaths,
                                   pats, crd_rate, scot_crd_rate) %>%
    rename(Location = location_name, Time_period = label_short, Deaths = deaths,
           Patients = pats, Crude_rate = crd_rate, Scotland_crude_rate = scot_crd_rate) %>%
    mutate_if(is.numeric, round, 1)

  table_colnames  <-  gsub("_", " ", colnames(fa))

  datatable(fa,
            style = 'bootstrap',
            class = 'table-bordered table-condensed',
            rownames = FALSE,
            options = list(pageLength = 20,
                           dom = 'tip',
                           autoWidth = TRUE),
            filter = "top",
            colnames = table_colnames)

})





} # server end

##END

