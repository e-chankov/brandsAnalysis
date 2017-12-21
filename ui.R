#### shiny libraries
library(shiny)          # version 1.0.5
library(shinythemes)    # version 1.1.1
library(shinydashboard) # version 0.6.1
library(shinyBS)        # version 0.61
library(shinyWidgets)   # version 0.3.6

#### libraries to visualization
library(wordcloud2)   # version 0.2.0
library(highcharter)  # version 0.5.0
library(googleVis)    # version 0.6.2
library(visNetwork)   # version 2.0.1
library(RColorBrewer) # version 1.1-2

#### data munging libraries
library(data.table) # version 1.10.4
library(checkmate)  # version 1.8.4
library(Matrix)     # version 1.2-11 
library(igraph)     # version 1.1.2
library(stringi)    # version 1.1.5

attribute.labels.grouped <- c(fread("data/attributeLabels.txt"))
brand.labels <- readLines("data/brandLabels.txt")


binomialInfoText <- "A column is displayed in the polar chart if the compared values are significantly different with specified confidence level. <br>To compare a car with the Sample Average the binomial mid-p test is used. <br>To compare two cars the McNemar mid-p test is used."
chiSqRS2InfoText <- "To test independence between two car models within selected set of attributes the chi-squared test for multiple-response variables is used. <br>An edge connects two car models if they are not independent with specified confidence level. <br>The network can be optionally simplified to the minimum spanning tree."


navbarPage(title = 'Car ratings', theme = shinytheme('cerulean'),
############### Overiview
           tabPanel('Overview',
                    # Add CSS files
                    includeCSS(path = "AdminLTE.css"),
                    includeCSS(path = "shinydashboard.css"),
                    fluidRow(
                      column(6,
                            htmlOutput("validationOfInputData"),
                            br(),
                            wordcloud2Output("listOfBrands", height = 375, width = "90%"),
                            br(),
                            wordcloud2Output("listOfAttributes", height = 375, width = "90%")
                           ),
                      column(6, 
                             valueBoxOutput("usersCard"),
                             valueBoxOutput("brandsCard"),
                             valueBoxOutput("attributesCard"),
                             br(),
                             HTML('<h4><center>Every respondent specifies for each car model all such attributes, which, in his opinion, correspond to the car</center></h4>'),
                             br(),
                             highchartOutput("heatmap", height = 675, width = "95%")
                      )
                    )
                    ),

############### Brand Profiles
           tabPanel('Car ratings differences', 
                    fluidRow(
                      column(4,
                             highchartOutput("barChart", height = 850, width = '100%')
                      ),
                      column(6, 
                             highchartOutput("polarChart", height = 850, width = '100%')
                      ),
                      column(2,
                             pickerInput('brandToPolarChart', h4('Car model'), choices = brand.labels, 
                                         selected = "Audi A4"),
                             pickerInput('comparedWithBrand', h4('Compared with'), 
                                         choices = c("Sample Average", brand.labels), selected = "Sample Average"),
                             sliderInput('binomConfLevel', h4('Confidence level'), value = 0.95, min = 0.9, max = 0.99,
                                         step = 0.01, ticks = FALSE),
                             br(),
                             materialSwitch("groupingPolarChart",
                                            label = tags$span("Grouping border", 
                                                              style = "font-size: 18px; color: #317eac"),
                                            value = TRUE, status = "primary"),
                             actionLink("binomialInfo", "", icon = icon("info-circle", "fa-2x"), style = "float:left"),
                             bsPopover("binomialInfo", "Info", binomialInfoText,
                                       options = list(container = "body"))
                      )
                    )
           ),
############### Dependency network
           tabPanel('Dependencies between cars', 
                    fluidRow(
                      column(4, align="center",
                             htmlOutput("treeMap")),
                      column(6, align="center",
                             visNetworkOutput('chiSqRS2Network', height = 700, width = '100%')
                      ),
                      column(2,
                             pickerInput('attributesToChiSqRS2Network',
                                         label = h4('Attributes'),
                                         choices = attribute.labels.grouped,
                                         selected = c("City focus", "Comfortable",
                                                      "Practical", "Safe",
                                                      "Sustainable", "Nice design"),
                                         options = list(
                                           `actions-box` = TRUE, 
                                           `selected-text-format` = "count > 2"
                                         ), 
                                         multiple = TRUE
                             ),
                             sliderInput('chiSqRS2ConfLevel', h4('Confidence level'), value = 0.95, min = 0.9, max = 0.99,
                                         step = 0.01, ticks = FALSE),
                             br(),
                             materialSwitch('minSpanForest', label = tags$span(HTML("Minimize network &nbsp;"), 
                                                                               style = "font-size: 18px; color: #317eac"),
                                            value = TRUE, status = "primary"),
                             materialSwitch('brandLevel', label = tags$span("TreeMap: cars first", 
                                                                               style = "font-size: 18px; color: #317eac"),
                                            value = TRUE, status = "primary"),
                             actionLink("chiSqRS2Info", "", icon = icon("info-circle", "fa-2x"), style = "float:left"),
                             bsPopover("chiSqRS2Info", "Info", chiSqRS2InfoText,
                                       options = list(container = "body"))
                      )
                    )
           )
)
