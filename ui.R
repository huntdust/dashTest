library(shiny)
library(shinythemes)
library(plotly)
library(markdown)
library(rmarkdown)
library(knitr)
library(shinywebcam)
library(bslib)
library(shinyFiles)

datapath <- "/mnt/Gryffindor/Data_and_Results"
analysispath <- "/opt/shiny-server/samples/sample-apps/dashtest/analysis"
RMDPath <- "/opt/shiny-server/samples/sample-apps/dashtest/rmd"
datafiles <- list.files(datapath)
analysisfiles <- list.files(analysispath)
analysistypes <- list.files(RMDPath)

setwd("/opt/shiny-server/samples/sample-apps/dashtest")

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
                                                     "All")),
                                       dateInput("temp_date", label = h3("Select a Date"), value = "2021-11-01"),
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
<<<<<<< Updated upstream
             tabPanel("Time domain plot",
=======
             tabPanel("Advanced RF Analysis",
                      tabsetPanel(
                        tabPanel("RF Analysis Part 1",
                           fluidPage(     
                            sidebarLayout(
                              sidebarPanel(
                                   #fileInput(
                                  #   inputId = "RF1",
                                  #   label = "S2p Files",
                                  #   multiple = TRUE,
                                  #   buttonLabel = "Browse...",
                                  #   placeholder = "No file selected"
                                  #), br(), 
                                      actionButton("dir", 'select a folder'), 
                                      actionButton("runRF1", "Run Analysis",class = "btn-success"), br(), br(),
                                      actionButton("downloadRF1", "Download Report",class = "btn-success"),
                      
                                   ),
                              mainPanel(
                                textOutput(""),
                                htmlOutput("RF1_analysis")
                                #selectInput(inputId = "dataSelection",label= "Select dataset", choices = datafiles)
                                
                              ))
                                 )),
                        tabPanel("RF Anaslysis Part 2",
                            sidebarPanel(
                                  actionButton("runRF2", "Run Analysis",class = "btn-success"),
                                  selectInput(inputId = "RF1_pth",label= "", choices = getwd()),
                                  actionButton("dir", 'select a folder'),
                                  actionButton("runRF1", "Run Analysis",class = "btn-success"), br(), br(),
                                  actionButton("downloadRF1", "Download Report",class = "btn-success"),
                                  dir <- getwd(),
                                 ))
                           )
                        ),
             tabPanel("TEC Plot",
>>>>>>> Stashed changes
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
                            
<<<<<<< Updated upstream
=======
                            #tabsetPanel(id='TECTabs',type='tabs')
                           
                            #sliderInput(inputId='TECSlider','TEC cycle', min=0,5,value=2)
                          
                            uiOutput('slider'),
                            
                            fluidRow(12,
                              #uiOutput('slider')
                              column(6,plotlyOutput(outputId = "TEC_Analysis", height = "700px", width = "900px")),
                              column(6,plotlyOutput(outputId = 'pads',width='50%',height="700px"),style='padding-left:100px; padding-right:1px; padding-top:5px; padding-bottom:5px')
                            ),
                            
                            #plotlyOutput(outputId = "TEC_Analysis", height = "700px", width = "900px"),
                            #splitLayout(cellWidths = c("80%", "20%"), plotlyOutput("TEC_Stats"), plotlyOutput(outputId = 'pads',width='50%',height="auto")),
                            DTOutput("TEC_Stats", width = "90%",height = "auto")
                            #plotlyOutput(outputId = 'pads',width='50%',height="auto")
                            
>>>>>>> Stashed changes
                          )
                        )
                      )
             ),
<<<<<<< Updated upstream
             tabPanel("shinyFiles",
                      bootstrapPage(
                        shinyFilesButton('files', label='File select', title='Please select a file', multiple=FALSE)
                      ))
        
  )
=======
          tabPanel("Cycle Plot",
                   fluidPage(
                     sidebarLayout(
                       sidebarPanel(
                         fileInput(
                           inputId = "Cycles_File",
                           label = "Select Cycling Data",
                           multiple = TRUE,
                           buttonLabel = "Browse...",
                           placeholder = "No file selected"
                         ),
                         fileInput(
                           inputId = "padFile_cycles",
                           label = "Select Pad Package Input File",
                           multiple = FALSE,
                           buttonLabel = "Browse...",
                           placeholder = "No file selected"
                         ),
                         textInput("maxRC",label='Resistance Threshold',value='0.1'),
                         checkboxInput("enableForce", "Show force trace", value = FALSE)
                       ),
                       mainPanel(
                         fluidRow(12,
                            column(6,plotlyOutput(outputId = "cyclePlot")),
                            column(6,plotlyOutput(outputId = 'cycle_pads',width='50%',height="700px"))
                         ),
                         DTOutput("cycle_Stats", width = "90%",height = "auto")
                         
                       )
                     )
                   )
                   )
  )
>>>>>>> Stashed changes
