# server - HSMR public dashboard

function(input, output, session) {


# For debugging
observeEvent(input$browser, browser())
###############################################.
## Reactive controls  ----
###############################################.

# Crude trends tab - Show list of area names depending on areatype selected
output$geoname_ui <- renderUI({
  areas_summary <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype])
  selectizeInput("geoname", label = NULL,
                 choices = areas_summary,
                 multiple = TRUE,
                 selected = "Scotland")
})


# Further analysis tab - Show list of area names depending on areatype selected
output$geoname_fa_ui <- renderUI({
  areas_summary <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_fa])
  selectizeInput("geoname_fa", label = NULL,
                 choices = areas_summary,
                 multiple = TRUE,
                 selected = "Scotland")
})



###############################################.
## Reactive datasets  ----
###############################################.

# Crude trends
trend_data <- reactive({

  trend %>%
    select(-completeness_date, -label, -hb, -location) %>%
    filter(location_name %in% input$geoname &
             time_period == input$timeperiod &
             sub_grp == "All Admissions")
})

# Further analysis
fa_data <- reactive({

  trend_fa <- trend %>%
    select(-completeness_date, -label, -hb, -location) %>%
    filter(location_name %in% input$geoname_fa &
             time_period == "Quarter" &
             sub_grp == input$indicator_select_fa)
})





###############################################.
##  Reactive layout  ----
###############################################.
# Crude trends
output$crude_trends <- renderUI({

  trend_chart_title <- paste0("Crude mortality within 30 days of admission; ", input$geoname)

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
      paste0("Crude mortality (%) within 30 days of discharge; "),#, input$geoname_fa),
    input$indicator_select_fa == "Population" ~
      paste0("Crude population mortality per 1,000 population; "))#, input$geoname_fa))

    fluidRow(column(6, h4(paste0(fa_chart_title))),
           column(12, withSpinner(plotlyOutput("fa_chart")))) %>% br() %>%

    fluidRow(column(6, h4(paste0(fa_chart_title))),
             column(12, dataTableOutput("fa_table")))

})


###############################################.
## Charts ----
###############################################.

# Crude trends
output$trend_chart <- renderPlotly({

  trend <- trend_data() %>%
    mutate(label_short = factor(label_short, levels = unique(trend_data()$label_short)))

  # Information to be displayed in tooltip
  tooltip_trend <- c(paste0(input$timeperiod, ": ", trend$label_short, "<br>",
                            trend$location_name, "<br>",
                            "Crude mortality rate: ", round(trend$crd_rate,1)))

  plot <- plot_ly(data=trend, x=~label_short) %>%

    # location line
    add_lines(y = ~crd_rate, line = list(color = pal_eth),
              text=tooltip_trend, hoverinfo="text",
              name = trend$location_name) %>%
    # Scotland line
    add_lines(y = ~scot_crd_rate, line = list(color = '#3F3685', dash = 'dash'),
              text=tooltip_trend, hoverinfo="text",
              name = "Scotland") %>%
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = list(title = "Crude rate (%)", rangemode="tozero", fixedrange=TRUE),
           xaxis = list(title = input$timeperiod,  fixedrange=TRUE, ticks=2, tickangle = 270,
                        categoryorder = "array", categoryarray = sort(trend[,"mth_qtr"])),
           legend = list(x = 100, y = 0.5)) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )

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
    add_lines(y = ~crd_rate, line = list(color = pal_eth),
              text=tooltip_fa, hoverinfo="text",
              name = fa$location_name) %>%
    # Scotland line
    add_lines(y = ~scot_crd_rate, line = list(color = '#3F3685', dash = 'dash'),
              text=tooltip_fa, hoverinfo="text",
              name = "Scotland") %>%
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = list(title = "Crude rate", rangemode="tozero", fixedrange=TRUE),
           xaxis = list(title = input$timeperiod,  fixedrange=TRUE, ticks=2, tickangle = 270,
                        categoryorder = "array", categoryarray = sort(fa[,"mth_qtr"])),
           legend = list(x = 100, y = 0.5)) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )

}
)#plotly end





###############################################.
## Table ----
###############################################.

# Crude trends
output$trend_table <- renderDataTable({

  table <- trend_data() %>% select(location_name, label_short, deaths,
                                pats, crd_rate, scot_crd_rate) %>%
    rename(Location = location_name, Time_period = label_short, Deaths = deaths,
           Patients = pats, Crude_rate = crd_rate, Scotland_crude_rate = scot_crd_rate) %>%
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

