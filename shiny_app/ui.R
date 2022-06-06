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
         #actionButton("browser", "browser"),
         sidebarLayout(
           sidebarPanel(width = 3,
                        radioGroupButtons("home_select",
                                          choices = home_list, status = "primary",
                                          direction = "vertical", justified = T)),
           mainPanel(width = 9,
                     # About
                     conditionalPanel(
                       condition= 'input.home_select == "about"',
                       tagList(h3(tags$b("Hospital Standardised Mortality Ratios")),
                               h4(tags$b(latest_hsmr)),
                               p(tags$b(paste0("Publication date: ", format(pub_day, "%d %B %Y")))),br(),
                               p(paste0("This dashboard, which accompanies the quarterly Hospital Standardised
                               Mortality Ratio (HSMR) publication, presents the latest HSMR for the period ",
                               latest_hsmr, " for hospitals in Scotland. HSMR is presented using a 12 month reporting
                               period when making comparisons against the national average. This is advanced by
                               three months with each quarterly update. In addition crude mortality
                               trends are presented by month and quarter for the last five years. Hospital
                               mortality measures have an important role to play in stimulating reflection
                               on the quality and safety of patient care.")),
                               p("Hospital Standardised Mortality Ratios adjust death data, also known as
                               mortality data, to take account of some of the factors known to affect
                               the underlying risk of death. The mix of patients seen varies between
                               hospitals, and the mortality rate may be higher or lower at a hospital
                               for many reasons; the patients seen, for example, may be more seriously
                               ill than average. The adjusted mortality at individual hospitals
                               can then be compared to the Scottish average. This approach provides a better
                               starting point for investigating hospital mortality than crude mortality rates,
                               which do not provide a fair comparison. Trends in crude mortality (unadjusted) are provided
                               to allow individual hospitals to monitor variation over time."),

                               p(tags$b("How is the HSMR calculated?")),
                               p("We calculate HSMRs using information from validated SMR01 records (acute
                               inpatient and day case admissions only) which includes patients admitted
                               to all medical and surgical specialties in NHS Scotland, apart from obstetrics
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
                       ), # conditionalPanel

                    # Using the dashboard
                     conditionalPanel(
                       condition= 'input.home_select == "use"',
                       tagList(h3(tags$b("Using the dashboard")),
                               p("The dashboard has 4 tabs across the top which can be selected:
                                 Home, HSMR, Crude trends, and Further analysis."),
                               p(tags$li(tags$b("Home: "), "includes sub-sections on the left hand side
                                         which provide an introduction to the HSMR publication, accessibility
                                         information and suggested resources to find out more.")),
                               p(tags$li(tags$b("HSMR: "), "view the key points from the publication and the funnel plot for the
                                         latest 12 month period, and use the time period drop-down to view the previous publications.")),
                               p(tags$li(tags$b("Crude trends: "), "view the crude mortality trends for all admissions
                                         and key subgroups including age, sex, deprivation, admission type, specialty
                                         and place of death by NHS Board of treatment and hospital.")),
                               p(tags$li(tags$b("Further analysis: "), "alternative crude mortality measures are provided including crude
                                         mortality trends within 30 days of discharge by NHS Board of treatment
                                         and population mortality rates by NHS Board of residence.")), br(),

                               p(tags$b("Interacting with the dashboard")),
                               p("On each tab there are drop-down menus which allow the user to update
                               the charts and data tables for their specific NHS Board, hospital,
                               or subgroup of interest. On the Crude trends and Further analysis tabs,
                               the location drop-down allows multiple locations to be added to the chart and table.
                               These can easily be removed by selecting the location and deleting."), br(),

                               p(tags$b("Downloading data")),
                               p(tags$li("There is the option to download data as a csv file by clicking the
                                         'Download data' button which can be found above the table on each tab.")),
                               p(tags$li("To download an image of a chart, click the camera icon in the top-right
                                         corner of any chart in the dashboard and a png image file will automatically download."))
                               ) #tagList
                       ), # condtionalPanel

                     # Further information
                     conditionalPanel(
                       condition= 'input.home_select == "info"',
                       tagList(h3(tags$b("Further information")),
                               p(tags$b("Publication")),
                               p(tags$li("The summary report and full report for this quarterly publication can be found
                                 on the ", tags$a(href="https://publichealthscotland.scot/publications/hospital-standardised-mortality-ratios/",
                                 "HSMR publication page", target="_blank"), ".")),
                                 p(tags$li("For more detailed information about the HSMR publication, visit the ",
                                 tags$a(href="https://www.isdscotland.org/Health-Topics/Quality-Indicators/HSMR/",
                                        "HSMR webpages (external website).", target="_blank"))),
                               p(tags$li("The ", tags$a(href="https://www.isdscotland.org/Health-Topics/Quality-Indicators/HSMR/Methodology/_docs/HSMR-2019-Technical-Specification.pdf",
                                "Technical Document (external website)", target="_blank"), " explains in more detail about how the HSMRs
                                are calculated.")),
                                p(tags$li("The ",
                                tags$a(href="https://www.isdscotland.org/Health-Topics/Quality-Indicators/HSMR/FAQ/_docs/HSMR-2019-FAQs.pdf",
                                 "Frequently Asked Questions (external website)", target="_blank"), "document answers common
                                questions about the HSMR publication.")),

                               p(tags$b("Data files")),
                               p(tags$li("The data can be downloaded as csv files by clicking the 'Download data'
                                 button above each of the tables in this dashboard.")),
                              p(tags$li("It is also available to download as excel files from the ",
                                 tags$a(href="https://publichealthscotland.scot/publications/hospital-standardised-mortality-ratios/",
                                  "publication data files.", target="_blank"))),
                               p(tags$b("Open data")),
                               p(tags$li("Open data from this publication is available from the ",
                                 tags$a(href="https://www.opendata.nhs.scot/dataset/hospital-standardised-mortality-ratios",
                                  "Scottish Health and Social Care Open Data platform (external website).", target="_blank"))),
                               p(tags$li("The code used to produce this publication can be accessed in this ",
                                 tags$a(href= "https://github.com/Public-Health-Scotland/hsmr", "GitHub repository (external website).",
                                        target="_blank"))),
                               p(tags$b("Data sources")),
                               p(tags$li(tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=1&SubID=5",
                                  "General Acute Inpatient and Day Case - Scottish Morbidity Record (SMR01) (external website).", target="_blank"))),
                               p(tags$li(tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=3&SubID=13",
                                  "National Records of Scotland (NRS) - Deaths Data (external website).", target="_blank"))),
                               p(tags$b("Data completeness")),
                               p("Information about the completeness of the SMR01 dataset at the time of this
                                 publication can be found on the ", tags$a(href="https://beta.isdscotland.org/products-and-services/data-management-hospital-activity/smr-completeness/",
                                                                           "SMR completeness webpage (external website).", target="_blank")),
                               p(tags$b("Contact us")),
                               p("Please contact the ", tags$a(href="mailto:phs.qualityindicators@phs.scot",
                                                               "Quality Indicators team"), "if you have any
                                 questions about this publication or dashboard.")
                               ) #tagList
                       ), # conditionalPanel

                    # Accessibility
                     conditionalPanel(
                       condition= 'input.home_select == "accessibility"',
                       tagList(h3(tags$b("Accessibility")),
                               p("This website is run by ", tags$a(href="https://www.publichealthscotland.scot/",
                                                                   "Public Health Scotland", target="_blank"),
                                ", Scotland's national organisation for public health. As a new organisation formed
                                on 1 April 2020, Public Health Scotland is currently reviewing its web estate. Public
                                Health Scotland is committed to making its website accessible, in accordance with
                                the Public Sector Bodies (Websites and Mobile Applications) (No. 2) Accessibility
                                Regulations 2018. This accessibility statement applies to the dashboard that accompanies
                                the HSMR quarterly publication."),
                               p(tags$a(href="https://mcmw.abilitynet.org.uk/", "AbilityNet (external website)", target="_blank"),
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
                                 2018 (the ‘accessibility regulations’). If you’re not happy with how we respond to your complaint,",
                               tags$a(href="https://www.equalityadvisoryservice.com/", "contact the Equality Advisory and Support Service (EASS) (external website).",
                                      target = "_blank")),

                               p(tags$b("Preparation of this accessibility statement")),
                               p("This statement was prepared on DD MMM 2022. It was last reviewed on DD MMM 2022.")

                       ) # tagList
                     ) #conditonalPanel
                     ) # mainPanel
           ) # sidebarLayout
         ), # tabPanel


###############################################.
### HSMR ----
###############################################.

tabPanel(title = "HSMR", value = "hsmr", icon = icon("bed"),
         wellPanel(#actionButton("browser", "browser"),
                   column(4, div(title="Select NHS Board of treatment to highlight on chart", # tooltip
                                 selectInput("hb_hsmr",
                                             label = "Step 1. Select NHS Board to highlight on chart.",
                                             choices = hsmr_hb_list,
                                             selected =  "Scotland"),
                                 uiOutput("hb_hsmr_ui"))),
                   column(4, div(title="Select a time period.",
                                 selectizeInput("timeperiod_hsmr",
                                                label = "Step 2. Select a time period.",
                                                choices= timeperiod_list,
                                                selected = latest_hsmr)),
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
                   column(4, div(title="Select the subgroup you wish to explore.", # tooltip
                                 selectizeInput("subgroup_select",
                                                label= "Step 1. Select the subgroup you want to explore.",
                                                choices = subgroup_list, selected = "All admissions",
                                                multiple = F))),

                   column(4, div(title="Select a location.",
                                 pickerInput("geotype", label = "Select a location.",
                                        choices = location_list,
                                        options = list('actions-box' = TRUE),
                                        selected = "Scotland", multiple = T)),
                         uiOutput("geoname_ui")),

                  column(4, div(title="Select frequency.",
                      radioGroupButtons("timeperiod", label = "Step 3. Select frequency.", choices=c("Month", "Quarter"),
                                        status = "primary", direction = "horizontal", justified = T)),
                  uiOutput("timeperiod_ui"))
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
         wellPanel(
                   column(4, div(title="Select the subgroup you wish to explore.", # tooltip
                                 selectizeInput("indicator_select_fa",
                                                label= "Step 1. Select the data you want to explore.",
                                                choices = indicator_list_fa, selected = "Discharge",
                                                multiple = F))),

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


### END
