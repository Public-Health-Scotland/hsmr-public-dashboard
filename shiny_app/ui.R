# UI - HSMR public dashboard

#secure_app( #uncomment if needing password protection
 tagList( #needed for shinyjs
   useShinyjs(),  # Include shinyjs
   navbarPage(id = "intabset", # id used for jumping between tabs
                div(tags$a(img(src="phs-logo.png", width=120, alt = "Public Health Scotland logo"),
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
tabPanel(title = "Introduction", icon = icon("list-ul"), value = "intro",

         h3(tags$b("Hospital Standardised Mortality Ratios")),
         h4(tags$b(paste0("Publication Date: ", format(pub_day, "%d %B %Y")))),br(),

         p("This dashboard, updated quarterly, presents the Hospital Standardised
           Mortality Ratios (HSMRs) for the latest 12 month period for hospitals in Scotland.
          In addition crude mortality trends are presented by month and quarter
           for the last five years."),
         p("Hospital mortality measures have an important role to play in stimulating
           reflection on the quality and safety of patient care. "), br(),

         h4(tags$b("Main points")),

          # This could be automated from the funnel_text function
         p(tags$li("For the period July 2020 to July 2021 no hospitals had a
           significantly higher standardised mortality ratio than the national average."),
          tags$li("For the period July 2020 to July 2021 one hospital had a significantly
           lower standardised mortality ratio than the national average: Western General Hospital (0.75).")), br(),
         p("The data from this publication is available to download from the ",
           tags$a(href="https://publichealthscotland.scot/publications/hospital-standardised-mortality-ratios/",
         "publication data files.", target="_blank"), "Open data from this publication is available from the ",
         tags$a(href="https://www.opendata.nhs.scot/dataset/hospital-standardised-mortality-ratios",
         "Scottish Health and Social Care Open Data platform.", target="_blank"))


), # tabPanel


###############################################.
### HSMR ----
###############################################.

tabPanel(title = "HSMR", value = "hsmr", icon = icon("hospital"),
         wellPanel(#actionButton("browser", "browser"),
                   column(4, div(title="Select NHS Board of treatment to highlight on chart",
                                 selectInput("hb_hsmr",
                                             label = "Step 1. Select NHS Board to highlight on chart.",
                                             choices = hb_list,
                                             selected =  "Scotland"),
                                 uiOutput("hb_hsmr_ui"))),
                   column(3, div(title="Select a time period.",
                                 selectizeInput("timeperiod_hsmr",
                                                label = "Step 2. Select a time period.",
                                                choices= timeperiod_list,
                                                selected = "July 2020 to June 2021")),
                          uiOutput("timeperiod_hsmr_ui"))
                   ), #well panel
         mainPanel(width = 12,
                   uiOutput("hsmr")
         )# mainPanel bracket
), #tab panel

###############################################.
### Crude trends tab ----
##############################################.
tabPanel(title = "Crude trends", value = "crude", icon = icon("area-chart"),
         wellPanel(#actionButton("browser", "browser"),
                   column(4, div(title="Select the subgroup you wish to explore.", # tooltip
                                 radioGroupButtons("subgroup_select",
                                                   label= "Step 1. Select the subgroup you want to explore.",
                                                   choices = subgroup_list, status = "primary",
                                                   direction = "vertical", justified = T))),

                   column(4, div(title="Select a location",
                         selectizeInput("geotype", label = NULL,
                                        choices= c("Scotland", "NHS Board of treatment", "Hospital"),
                                        selected = "Scotland", multiple = TRUE)),
                         uiOutput("geoname_ui"),

                  div(title="Select frequency",
                         selectInput("timeperiod", label =NULL, choices= c("Month", "Quarter"),
                                     selected =  "Quarter"),
                      uiOutput("timeperiod_ui"))),

           column(3, div(actionButton("btn_methodology", "Learn more",
                               icon = icon('th'),
                               #style = "float: right",
                               onclick = "window.open('https://www.isdscotland.org/Health-Topics/Quality-Indicators/HSMR/', '_blank')"),

                  fluidRow(br()),
                  downloadButton("download_hsmr_data", "Download data"#, style = "float: right"
                                 )))

         ), #well panel
        mainPanel(width = 10,
                  uiOutput("crude_trends")
         )# mainPanel bracket
), #tab panel



###############################################.
### Further analysis tab ----
##############################################.
tabPanel(title = "Further analysis", value = "fa", icon = icon("chart-bar"),
         wellPanel(#actionButton("browser", "browser"),
                   column(4, div(title="Select indicator.", # tooltip
                                 radioGroupButtons("indicator_select_fa",
                                                   label= "Step 1. Select the data you want to explore.",
                                                   choices = indicator_list_fa, status = "primary",
                                                   direction = "vertical", justified = T))),
                   column(4, div(title="Select a location",
                                 p(tags$b("Step 2. Select a geography level and then area of interest (max 8).")),
                                 selectInput("geotype_fa", label = NULL,
                                             choices= c("Scotland", "NHS Board of treatment"),
                                             selected = "Scotland", multiple = TRUE)),
                          uiOutput("geoname_fa_ui"))
         ), #well panel
         mainPanel(width = 10,
                   uiOutput("further_analysis")
         )# mainPanel bracket
)#, #tab panel



) # navbarPage
) #tagList
