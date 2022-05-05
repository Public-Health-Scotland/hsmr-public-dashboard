# UI - HSMR public dashboard

#secure_app( #uncomment if needing password protection
 tagList( #needed for shinyjs
   useShinyjs(),  # Include shinyjs
   navbarPage(id = "intabset", # id used for jumping between tabs
                 div(
                  tags$a(img(src="phs-logo.png", width=120, alt = "Public Health Scotland logo"),
                                 href= "https://www.publichealthscotland.scot/",
                                 target = "_blank"),
                         style = "position: relative; top: -12px;"),
              windowTitle = "Hospital Standardised Mortality Ratios", #title for browser tab
              header = tags$head(includeCSS("www/styles.css"), # CSS styles
                                 HTML("<html lang='en'>"),
                                 tags$link(rel="shortcut icon", href="favicon_phs.ico"), #Icon for browser tab
                                 #Including Google analytics
                                 includeScript("google-analytics.js")),

###############################################.
### Home ----
###############################################.
tabPanel(title = "Home", icon = icon("info-circle"), value = "home",

         sidebarLayout(
           sidebarPanel(width = 3,
                        radioGroupButtons("home_select",
                                          choices = home_list, status = "primary",
                                          direction = "vertical", justified = T)),
           mainPanel(width = 9,
                     uiOutput("home"))
           ) # sidebarLayout
         ), # tabPanel


###############################################.
### HSMR ----
###############################################.

tabPanel(title = "HSMR", value = "hsmr", icon = icon("bed"),
         wellPanel(actionButton("browser", "browser"),
                   column(4, div(title="Select NHS Board of treatment to highlight on chart",
                                 selectInput("hb_hsmr",
                                             label = "Step 1. Select NHS Board to highlight on chart.",
                                             choices = hb_list,
                                             selected =  "Scotland"),
                                 uiOutput("hb_hsmr_ui"))),
                   column(4, div(title="Select a time period.",
                                 selectizeInput("timeperiod_hsmr",
                                                label = "Step 2. Select a time period.",
                                                choices= timeperiod_list,
                                                selected = "July 2020 to June 2021")),
                          uiOutput("timeperiod_hsmr_ui"))
                   ), # wellPanel
         mainPanel(width = 12,
                   uiOutput("hsmr")
         ) # mainPanel
), # tabPanel

###############################################.
### Crude trends tab ----
##############################################.
tabPanel(title = "Crude trends", value = "crude", icon = icon("area-chart"),


           wellPanel(
                   # column(4, div(title="Select the subgroup you wish to explore.", # tooltip
                   #               radioGroupButtons("subgroup_select",
                   #                                 label= "Step 1. Select the subgroup you want to explore.",
                   #                                 choices = subgroup_list, status = "primary",
                   #                                 direction = "vertical", justified = T))),

                   column(4, div(title="Select the subgroup you wish to explore.", # tooltip
                                 selectizeInput("subgroup_select",
                                                label= "Step 1. Select the subgroup you want to explore.",
                                                choices = subgroup_list, selected = "All admissions",
                                                multiple = F))),

                   column(4, div(title="Select a location",
                                 pickerInput("geotype", label = "Select a location",
                                        choices = location_list,
                                        selected = "Scotland", multiple = T)),
                         uiOutput("geoname_ui")),

                  column(4, div(title="Select frequency",
                      radioGroupButtons("timeperiod", label = "Step 3. Select frequency", choices=c("Month", "Quarter"),
                                        status = "primary", direction = "horizontal", justified = T)),
                  uiOutput("timeperiod_ui"))
                  #downloadButton("download_hsmr_data", "Download data", style = "float: left"))

                 ), # wellPanel
        mainPanel(
          width = 12,
                  uiOutput("crude_trends")
         )# mainPanel
), # tabPanel



###############################################.
### Further analysis tab ----
##############################################.
tabPanel(title = "Further analysis", value = "fa", icon = icon("chart-bar"),
         wellPanel(column(4, div(title="Select indicator.", # tooltip
                                 radioGroupButtons("indicator_select_fa",
                                                   label= "Step 1. Select the data you want to explore.",
                                                   choices = indicator_list_fa, status = "primary",
                                                   direction = "vertical", justified = T))),
                   column(4, div(title="Select a location",
                                 selectizeInput("geotype_fa", label = "Step 2. Select NHS Board/s of interest.",
                                             choices= hb_list,
                                             selected = "Scotland", multiple = TRUE)),
                          uiOutput("geoname_fa_ui"))
                   ), # wellPanel
         mainPanel(width = 12,
                   uiOutput("further_analysis")
                   )# mainPanel bracket

    ) # tabPanel
  ) # navbarPage
) # tagList

# END
