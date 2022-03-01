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
tabPanel(title = "Contents", icon = icon("list-ul"), value = "contents",

         h3(tags$b("Hospital Standardised Mortality Ratios")),
         h4(tags$b(paste0("Publication Date: ", format(pub_day, "%d %B %Y")))),br(),

         p("This dashboard, updated quarterly, presents the Hospital Standardised
           Mortality Ratios (HSMRs) for the latest 12 month period for hospitals in Scotland.
          In addition crude mortality trends are presented by month and quarter
           for the last five years."),
         p("Hospital mortality measures have an important role to play in stimulating
           reflection on the quality and safety of patient care. "),
         p("The data from this publication is available to download from the ",
           tags$a(href="https://publichealthscotland.scot/publications/hospital-standardised-mortality-ratios/",
                  "publication data filess.", target="_blank"), "Open data from this publication is available from the ",
           tags$a(href="https://www.opendata.nhs.scot/dataset/hospital-standardised-mortality-ratios",
                  "Scottish Health and Social Care Open Data platform.", target="_blank"))


), # tabPanel


###############################################.
### HSMR ----
###############################################.

tabPanel(title = "HSMR", value = "hsmr", icon = icon("hospital"),
         wellPanel(actionButton("browser", "browser"),
                   column(3, div(title="Select NHS Board of treatment to highlight on chart",
                                 #p(tags$b("Step 3. Select to see trends by month or quarter.")),
                                 selectInput("hb_hsmr",
                                             label = "Step 1. Select NHS Board of treatment to highlight hospitals on the chart.",
                                             choices= geo_lookup_hb$code,
                                             selected =  "Scotland"),
                                 uiOutput("hb_hsmr_ui"))),
                   column(3, div(title="Select a time period.",
                                 selectizeInput("timeperiod_hsmr",
                                                label = "Step 2. Select a time period.",
                                                choices= timeperiod_list,
                                                selected = "July 2020 to June 2021")),
                          uiOutput("timeperiod_hsmr_ui")),
                   column(3,
                          actionButton("funnel_help","Interpretation of this chart",
                                       icon = icon('question-circle')))
                   ), #well panel
         mainPanel(width = 10,
                   uiOutput("hsmr")
         )# mainPanel bracket
), #tab panel
#) #navbarMenu


###############################################.
### Crude trends tab ----
##############################################.
tabPanel(title = "Crude trends", value = "crude", icon = icon("area-chart"),
         wellPanel(#actionButton("browser", "browser"),
                   column(3, div(title="Select the subgroup you wish to explore.", # tooltip
                                 radioGroupButtons("subgroup_select",
                                                   label= "Step 1. Select the subgroup you want to explore.",
                                                   choices = subgroup_list, status = "primary",
                                                   direction = "vertical", justified = T))),

                   column(3, div(title="Select a location",
                         #p(tags$b("Step 2. Select a geography level and then an area of interest.")),
                         selectizeInput("geotype", label = NULL,
                                        choices= c("Scotland", "NHS Board of treatment", "Hospital"),
                                        selected = "Scotland", multiple = TRUE)),
                         uiOutput("geoname_ui"),

                  div(title="Select frequency",
                         #p(tags$b("Step 3. Select to see trends by month or quarter.")),
                         selectInput("timeperiod", label =NULL, choices= c("Month", "Quarter"),
                                     selected =  "Quarter"),
                      uiOutput("timeperiod_ui"))),

           column(3, div(style = "float: right",
                  actionButton("btn_methodology", "Learn more",
                               icon = icon('th'),
                               style = "float: right",
                               onclick = "window.open('https://www.isdscotland.org/Health-Topics/Quality-Indicators/HSMR/', '_blank')"),

                  fluidRow(br()),
                  downloadButton("download_hsmr_data", "Download data", style = "float: right")))

         ), #well panel
        mainPanel(width = 10,
                  uiOutput("crude_trends")
         )# mainPanel bracket
), #tab panel



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
                                 p(tags$b("Step 2. Select a geography level and then area of interest.")),
                                 selectInput("geotype_fa", label = NULL,
                                             choices= c("Scotland", "NHS Board of treatment"),
                                             selected = "Scotland", multiple = TRUE)),
                          uiOutput("geoname_fa_ui"))
         ), #well panel
         mainPanel(width = 10,
                   uiOutput("further_analysis")
         )# mainPanel bracket
)#, #tab panel
#) #navbarMenu




) # navbarPage
) #tagList
