library(shiny)
library(shinythemes)
library(plotly)
library(markdown)
library(rmarkdown)
library(knitr)
library(bslib)
library(shinyFiles)
library(DT)

datapath <- "C:/Users/huntdust/Desktop/ShinyServer/"
analysispath <- "C:/home/dashTest/Analysis/"
RMDPath <- "C:/home/dashTest/rmd/"
datafiles <- list.files(datapath)
analysisfiles <- list.files(analysispath)
analysistypes <- list.files(RMDPath)

setwd("C:/home/dashTest")

ui <- 
  navbarPage("Data visualization and analysis tools",id='tabs', collapsible = TRUE, inverse = TRUE, theme = shinytheme("cyborg"),
             tabPanel("Temperature/Humidity Plots",
                      fluidPage(
                        tabsetPanel(
                          tabPanel("Temperature", br(),
                                   sidebarLayout(
                                     sidebarPanel(width=3,
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
                                       plotlyOutput(outputId = "tempPlot", height = "800px", width = "900px")
                                     )
                                   )),
                          tabPanel("Humidity", br(),
                                   sidebarLayout(
                                     sidebarPanel(width=3,
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
             tabPanel("S2P plot",
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            fileInput(
                              inputId = "s2pFiles",
                              label = "Select S2P files",
                              multiple = TRUE,
                              buttonLabel = "Browse...",
                              placeholder = "No file selected"
                            ),
                            downloadButton("report", "Generate report"), 
                            #checkboxInput("includeStats_s2p", "include statistical analysis", value = FALSE)
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
             tabPanel("Advanced RF Analysis",
                      tabsetPanel(
                        tabPanel("RF Analysis Part 1",
                           fluidPage(     
                            sidebarLayout(
                              sidebarPanel(
                                      actionButton("dir", 'select a folder'), 
                                      #shinyDirButton("dir", "Input directory", "Upload"),
                                      #shinyFilesButton('s2p_dir',"Input directory",title="Input s2p directory",multiple='True'),
                                      verbatimTextOutput("wd", placeholder = TRUE),
                                      #actionButton("runRF1", "Generate Report",class = "btn-success"), br(), br(),
                                      actionButton("runRF1", "Download Report",class = "btn-success"),
                                      downloadButton("S2p_Analysis", "Test - report generation method 2 ")
                                      
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
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(width=3,
                            fileInput(
                              inputId = "TEC_File",
                              label = "Select TEC Data",
                              multiple = FALSE,
                              buttonLabel = "Browse...",
                              placeholder = "No file selected"
                            ),
                            fileInput(
                              inputId = "padFile",
                              label = "Select Pad Package Input File",
                              multiple = FALSE,
                              buttonLabel = "Browse...",
                              placeholder = "No file selected"
                            ),
                            textInput("maxR",label='Y axis limit ',value='0.1'),
                            textInput("TEC_spec",label='DCR Specification',value='0.1'),
                            textInput("TEC_Point", label = "Enter TEC point (if unable to be detected automatically)"),
                            textInput("TEC_title", label = "Report Title", value = "TEC Report"),
                            #textInput("DCR_spec",label='DCR Specification',value='0.1'),
                            downloadButton("TECReport", "Generate report")
                            #shinyFilesButton('TEC_File',label = 'File select', title = 'Please select file', multiple = FALSE)
                          ),
                          mainPanel(
                             # do.call(tabsetPanel, c(id='tab',lapply(1:5, function(i) {
                             #     tabPanel(
                             #        title=paste0('tab ', i),
                             #        textOutput(paste0('out',i)),
                             #        #plotlyOutput(outputId = "TEC_Analysis", height = "1000px", width = "900px"),
                             #        #DTOutput("TEC_Stats", width = "100%",height = "auto")
                             #   )
                             # })))
                            
                        
                            uiOutput('slider'),
                            #verbatimTextOutput("Use this slider to select which TEC sweep you would like to view. Integers only.",placeholder=TRUE),
                            plotlyOutput(outputId = "TEC_Analysis", height = "700px", width = "900px"),
                            
                            
                            fluidRow(
                                     #uiOutput('slider')
                                     #column(6,plotlyOutput(outputId = "TEC_Analysis", height = "700px", width = "900px"),style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px'),
                                     column(6,plotlyOutput(outputId = 'pads',width='70%',height="700px"),style='padding-left:0px; padding-right:0px; padding-top:50px; padding-bottom:0px'),
                                     column(6,plotlyOutput(outputId = 'TECHist',width='100%',height="700px"),style='padding-left:10px; padding-right:0px; padding-top:50px; padding-bottom:0px')
                            ),
                            
        
                            DTOutput("TEC_Stats", width = "90%",height = "auto")
                          )
                        )
                      )   
             ),
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
                         textInput("maxRC",label='Resistance Threshold',value='0.1'),
                         textInput("DCR_spec",label='DCR Specification',value='0.1'),
                         textInput("CycleReportTitle",label='Report Title',value='Cycling Report'),
                         checkboxInput("enableForce", "Show force trace", value = FALSE), br(), br(), 
                         downloadButton("cycleReport", "Generate report")
                       ),
                       mainPanel(
                         plotlyOutput(outputId='CyclesPlot'), br(), br(), br(),
                         plotlyOutput(outputId='CyclesHist'), br(),
                         #plotlyOutput(outputId='CyclesHistSlider'), br(),
                         DTOutput("cycleStats", width = "90%",height = "auto")  
                       )
                     )
                   )
                   )
  )
