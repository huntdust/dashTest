library(shiny)
library(shinythemes)
library(plotly)
library(markdown)
library(rmarkdown)
library(knitr)
library(shinywebcam)
library(bslib)
library(shinyFiles)

datapath <- "C:/Users/huntdust/Desktop/ShinyServer/"
analysispath <- "C:/home/dashTest/Analysis/"
RMDPath <- "C:/home/dashTest/rmd/"
datafiles <- list.files(datapath)
analysisfiles <- list.files(analysispath)
analysistypes <- list.files(RMDPath)

setwd("C:/home/dashTest")

ui <- 
  navbarPage("Demo", collapsible = TRUE, inverse = TRUE, theme = shinytheme("darkly"),
             tabPanel("SamplePlot",
                      fluidPage(
                        tabsetPanel(
                          tabPanel("Plot1",br(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       radioButtons(
                                         inputId = "plotSelection",
                                         label = "Choose a plot",
                                         choices = c("Temperature",
                                                     "Humidity")), br()
                                     ),
                                     
                                     mainPanel(plotlyOutput(outputId = "samplePlot",height="600px"))
                                   )),
                          
                          tabPanel("Plot2",br(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       radioButtons(
                                         inputId = "plotSelection",
                                         label = "Choose a plot",
                                         choices = c("Temperature",
                                                     "Humidity")), br()
                                     ),
                                     
                                     mainPanel(plotlyOutput(outputId = "samplePlotTwo", height = "600px"))
                                   ))
                        )
                      )),
             tabPanel("Temperature/Humidity Plots",
                      fluidPage(
                        tabsetPanel(
                          tabPanel("Temperature", br(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       radioButtons(
                                         inputId = "timescaleTemp",
                                         label = "Timescale",
                                         choices = c("1 day",
                                                     "5 days",
                                                     "1 month", 
                                                     "all")), br(),
                                       
                                       radioButtons(
                                         inputId = "tempSensors",
                                         label = "Sensor",
                                         choices = c("Ambient_SD",
                                                     "Gryffindor",
                                                     "Reliability",
                                                     "Ambient_HP",
                                                     "Leakage_HP",
                                                     "Leakage2_HP",
                                                     "All"))
                                     ),
                                     mainPanel(
                                       plotlyOutput(outputId = "tempPlot", height = "1000px", width = "900px")
                                     )
                                   )),
                          tabPanel("Humidity", br(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       radioButtons(
                                         inputId = "timescaleHum",
                                         label = "Timescale",
                                         choices = c("1 day",
                                                     "5 days",
                                                     "1 month", 
                                                     "all")), br(),
                                       
                                       radioButtons(
                                         inputId = "humSensors",
                                         label = "Sensor",
                                         choices = c("Ambient_SD",
                                                     "Gryffindor",
                                                     "Reliability",
                                                     "Ambient_HP",
                                                     "Leakage_HP",
                                                     "Leakage2_HP",
                                                     "All"))
                                     ),
                                     mainPanel(
                                       plotlyOutput(outputId = "humgraph", height = "800px", width = "900px")
                                       #imageOutput(outputId = "humPlot", height = "900px", width="900px")
                                     )
                                   ))
                          
                          
                        ))),
             tabPanel("HTML Demos",
                      fluidPage(
                        tabsetPanel(
                          tabPanel("HTML Sample", br(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput(inputId = "analysisSelection",label= "Select Analysis File", choices = analysisfiles),
                                       br()),
                                     mainPanel(
                                       fluidRow(
                                         #Can only render the first plot correctly... next selection does not load properly
                                         htmlOutput("htmlout")
                                       )
                                     )
                                   )),
                          tabPanel("Knit", br(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput(inputId = "analysisFile", label = "Select Analysis File", choices = analysistypes), br(),
                                       selectInput(inputId = "dataSelection",label= "Select dataset", choices = datafiles), br(),
                                       
                                       #reference "runAnalysis" button status with input$runAnalysis
                                       actionButton("runAnalysis", "Run Analysis",class = "btn-success"), br()),
                                     #render(input = "Gryffindor_Force_and_Position_Plots_MASTER.Rmd", "html_document", output_file = paste(input$dataSelection,".HTML")),
                                     #file to run analysis on : paste(datapath, datafiles[2])
                                     
                                     mainPanel(
                                       
                                     )
                                   ))
                        ))), 
             tabPanel("Live plot generation",
                      
                      #includeMarkdown("test.rmd")
                      uiOutput("md_file")
                      
                      
             ),
             tabPanel("S2P plot",
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            fileInput(
                              inputId = "csvFiles",
                              label = "Select an S2P file",
                              multiple = TRUE,
                              buttonLabel = "Browse...",
                              placeholder = "No file selected"
                            )
                          ),
                          mainPanel(
                            fluidRow(
                              plotlyOutput(outputId = "s2pPlot")
                            ), br(),
                            fluidRow(
                              plotlyOutput(outputId = "s2pPlot2")
                            ), br(),
                            fluidRow(
                              plotlyOutput(outputId = "timePlot")
                            )
                           
                          
                           #plotlyOutput(outputId = "s2pPlot")
                            
                          #  fluidRow(
                          #    plotlyOutput(outputId = "s2pPlot")
                          #  ), br(),
                          #  fluidRow(
                          #    plotlyOutput(outputId = "timePlot")
                          #  )
                         
                            
                          )
                        )
                      )
             ),
             tabPanel("Time domain plot",
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            fileInput(
                              inputId = "csvFiles",
                              label = "Drag and drop here",
                              multiple = TRUE,
                              buttonLabel = "Browse...",
                              placeholder = "No file selected"
                            )
                          ),
                          mainPanel(
                            #plot goes here
                            #plotlyOutput(outputId = "timePlot")
                            
                          )
                        )
                      )
             ),
             tabPanel("shinyFiles",
                      bootstrapPage(
                        shinyFilesButton('files', label='File select', title='Please select a file', multiple=FALSE)
                      ))
        
  )