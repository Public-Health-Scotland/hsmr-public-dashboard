# UI - HSMR public dashboard

#secure_app( #uncomment if needing password protection
 tagList( #needed for shinyjs
   useShinyjs(),  # Include shinyjs
   navbarPage(id = "intabset", # id used for jumping between tabs
              title = div(tags$a(img(src="phs-logo.png", width=120, alt = "Public Health Scotland logo"),
                                 href= "https://www.publichealthscotland.scot/",
                                 target = "_blank"),
                          style = "position: relative; top: -10px;"),
              windowTitle = "Hospital Standardised Mortality Ratios", #title for browser tab
              header = tags$head(includeCSS("www/styles.css"), # CSS styles
                                 HTML("<html lang='en'>"),
                                 tags$link(rel="shortcut icon", href="favicon_phs.ico"), #Icon for browser tab
                                 #Including Google analytics
                                 includeScript("google-analytics.js")),


###############################################.
### Contents ----
###############################################.
# tabPanel(title = "Contents", icon = icon("list-ul"), value = "contents",
#
#          h3("Hospital Standardised Mortality Ratios"),
#          h4(paste0("Publication Date: ", pub_day))
# ), # tabPanel



###############################################.
### Crude trends tab ----
##############################################.
#navbarMenu("Crude trends", icon = icon("area-chart"),
tabPanel(title = "Crude trends", value = "crude", icon = icon("area-chart"),
         wellPanel(actionButton("browser", "browser"),
           column(4, div(title="Select a location",
                         p(tags$b("Step 1. Select a geography level and then an area of interest.")),
                         selectInput("geotype", label = NULL, choices= c("Scotland", "NHS Board of treatment", "Hospital"),
                                     selected = "Scotland")),
                  uiOutput("geoname_ui")),
           column(4, div(title="Select frequency",
                         p(tags$b("Step 2. Select to see trends by month or quarter.")),
                         selectInput("timeperiod", label =NULL, choices= c("Month", "Quarter"),
                                     selected =  "Quarter")),
                  uiOutput("timeperiod_ui")),

           column(4,
                  actionButton("btn_data_source_modal", "Data source: SMR01 and NRS deaths data", icon = icon('question-circle')),
                  fluidRow(br()),
                  downloadButton("download_hsmr_data", "Download data"),
                  fluidRow(br()),
                  actionButton("jump_commentary_hsmr","Go to methodology"))
         ), #well panel
        mainPanel(width = 12,
                  uiOutput("crude_trends")
         )# mainPanel bracket
), #tab panel
#) #navbarMenu


###############################################.
### Further analysis tab ----
##############################################.
#navbarMenu("Further analysis", icon = icon("chart-bar"),
tabPanel(title = "Further analysis", value = "fa", icon = icon("chart-bar"),
         wellPanel(actionButton("browser", "browser"),
                   column(4, div(title="Select indicator.", # tooltip
                                            radioGroupButtons("indicator_select_fa",
                                                              label= "Step 1. Select the data you want to explore.",
                                                              choices = indicator_list_fa, status = "primary",
                                                              direction = "vertical", justified = T))),
                   column(4, div(title="Select a location",
                                 p(tags$b("Step 2. Select a geography level and then area of interest.")),
                                 selectInput("geotype_fa", label = NULL, choices= c("Scotland", "NHS Board of treatment"),
                                             selected = "Scotland")),
                          uiOutput("geoname_fa_ui"))#,
                   # column(4,
                   #        actionButton("btn_data_source_modal", "Data source: SMR01 and NRS deaths data", icon = icon('question-circle')),
                   #        fluidRow(br()),
                   #        downloadButton("download_hsmr_data", "Download data"),
                   #        fluidRow(br()),
                   #        actionButton("jump_commentary_hsmr","Go to methodology"))
         ), #well panel
         mainPanel(width = 12,
                   uiOutput("further_analysis")
         )# mainPanel bracket
)#, #tab panel
#) #navbarMenu

###############################################.
### HSMR ----
###############################################.

# tabPanel(title = "HSMR", icon = icon("hospital"), value = "hsmr"),






) # navbarPage
) #tagList
