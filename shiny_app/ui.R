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
#             mainPanel(

               mainPanel(width = 9,
                         uiOutput("home"))


      #
      #    # h4(tags$b("Main points")),
      #    #
      #    #  # This could be automated from the funnel_text function
      #    # p(tags$li("For the period July 2020 to July 2021 no hospitals had a
      #    #   significantly higher standardised mortality ratio than the national average."),
      #    #  tags$li("For the period July 2020 to July 2021 one hospital had a significantly
      #    #   lower standardised mortality ratio than the national average: Western General Hospital (0.75).")), br(),

      #
      # ) # mainPanel
  ) # sidebarLayout
), # tabPanel



###############################################.
### HSMR ----
###############################################.

tabPanel(title = "HSMR", value = "hsmr", icon = icon("bed"),
         wellPanel(#actionButton("browser", "browser"),
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
                         selectizeInput("geotype", label = NULL,
                                        choices = location_list,
                                        #choices= c("Scotland", "NHS Board of treatment", "Hospital"),
                                        selected = "Scotland", multiple = TRUE)),
                         uiOutput("geoname_ui")),

                  column(4, div(title="Select frequency",
                      radioGroupButtons("timeperiod", label = "Step 3. Select frequency", choices=c("Month", "Quarter"),
                                        status = "primary", direction = "horizontal", justified = T)),
                  uiOutput("timeperiod_ui"))
                  #downloadButton("download_hsmr_data", "Download data", style = "float: left"))

                 # fluidRow(
                    # column(9, div(actionButton("btn_methodology", "Learn more",
                    #            icon = icon('th'),
                    #            #style = "float: right",
                    #            onclick = "window.open('https://www.isdscotland.org/Health-Topics/Quality-Indicators/HSMR/', '_blank')")),


                              #  )
           ), #wellPanel
        mainPanel(
          width = 12,
                  uiOutput("crude_trends")
         )# mainPanel
), # tabPanel



###############################################.
### Further analysis tab ----
##############################################.
tabPanel(title = "Further analysis", value = "fa", icon = icon("chart-bar"),
         wellPanel(actionButton("browser", "browser"),
                   column(4, div(title="Select indicator.", # tooltip
                                 radioGroupButtons("indicator_select_fa",
                                                   label= "Step 1. Select the data you want to explore.",
                                                   choices = indicator_list_fa, status = "primary",
                                                   direction = "vertical", justified = T))),
                   column(4, div(title="Select a location",
                                 selectizeInput("geotype_fa", label = "Step 2. Select NHS Board/s of interest.",
                                             choices= hb_list,
                                             selected = "Scotland", multiple = TRUE)),
                          uiOutput("geoname_fa_ui"))


         ), #well panel
         mainPanel(width = 12,
                   uiOutput("further_analysis")
         )# mainPanel bracket

)#, #tab panel



) # navbarPage

) #tagList
