library(shiny)
library(here)
library(rmarkdown)
library(tidyverse)
library(shinywebcam)


analysispath <- "C:/home/dashTest/Analysis/"
addResourcePath("tmpuser",getwd())

server <- function(input,output) {
  
  
  basepath <- 'C:/home/dashTest/'
  ev <- reactiveValues(data=NULL)
  
  source(file.path("samplePlot.R"), local = TRUE)$value  #sample plot
  source(file.path("samplePlot2.R"), local = TRUE)$value  #sample plot
  source(file.path("tempPlot.R"), local=TRUE)$value      #Temp Plot
  source(file.path("humPlot.R"), local=TRUE)$value      #Temp Plot
  
  groupInput <- reactive({
    switch(input$group1,
           "Student Status" = survey$status, 
           "Country / Region of Origin" = survey$country, 
           "Major" = survey$major)
  })
  
  dataInput <- reactive({
    switch(input$question1, 
           "Email/chat with library staff" = survey$Q1.1_2, 
           "Find books" = survey$Q1.1_7, 
           "Search for articles" = survey$Q1.1_4, 
           "Use subject / citation guides" = survey$Q1.1_6,
           "Book a group study room" = survey$Q1.1_3)
  })
  
  plotSensors <- reactive({
    switch(input$humSensors,
           "Ambient_SD",
           "Gryffindor",
           "Reliability",
           "Ambient_HP",
           "Leakage_HP",
           "Leakage2_HP",
           "All")
  })
  
  timeScaleSelection <- reactive({
    switch(input$timescale,
           "1 day",
           "5 days",
           "1 month", 
           "all")
  })
  
  observe({
    #Need to have webcam image saving every 50msec as a png file to network drive 
    invalidateLater(50, session = getDefaultReactiveDomain())
    output$outImage <- renderImage({
      filename <- ("C:/users/huntdust/desktop/testImage.png")
      list(src=filename)
      
    },
    deleteFile=FALSE)  
    
    output$md_file <- renderUI({
      file <- "C:/home/dashTest/rmd/Gryffindor_Force_and_Position_Plots_MASTER.Rmd"
      includeMarkdown(file) 
      
    })
    
  })
  
  observeEvent(input$runAnalysis, {
    #should a seperate version of these scripts be kept with different file paths for the server? The paths are inherently different...
    input_file <- paste(basepath, 'rmd/', input$analysisFile)
    input_file <- gsub(" ", "", input_file)
    
    output_file <- paste(basepath, 'analysis/', input$dataSelection, '.html')
    output_file <- gsub(" ", "", output_file)
    
    render(input = input_file, "html_document", output_file = output_file) 
    
  })
  
  
  
  getPage <- function() {
    file <- input$analysisSelection
    pth <- paste(analysispath,file)
    pth <- gsub(" ", "", pth)
    
    #pth <- 'C:/home/dashTest/Analysis/Gryffindor_Force_and_Position_Plots_MASTER_USB1.html'
    return(includeHTML(pth))
    #return(includeHTML("C:/home/dashTest/test.html"))
  }
  
  #this style of function could be used to dynamically perform ui behaviors like rendering an RMD document... 
  output$htmlout <- renderUI({getPage()})
  
  
  output$samplePlot <- renderPlotly({
    samplePlot <- ggplot(data, aes(x=x,y=random_y)) +geom_point()
    samplePlot <- ggplotly(samplePlot)
    samplePlot
  })
  
  output$samplePlotTwo <- renderPlotly({
    gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
      geom_point(aes(col=state, size=popdensity)) + 
      geom_smooth(method="loess", se=F) + 
      xlim(c(0, 0.1)) + 
      ylim(c(0, 500000)) + 
      labs(subtitle="Area Vs Population", 
           y="Population", 
           x="Area", 
           title="Scatterplot", 
           caption = "Source: midwest")
    
    gg
  })
  
  output$tempPlot <- renderPlotly({
    dims <- dim(amb)
    dim <- dims[1]
    
    
    
    data <- switch(input$tempSensors,
                   "Ambient_SD"  = amb,
                   "Gryffindor"  = gry,
                   "Reliability" = rel,
                   "Ambient_HP"  = HPa,
                   "Leakage_HP"  = Le1,
                   "Leakage2_HP" = Le2,
                   "All"         = test)
    range <- switch(input$timescaleTemp,
                    "1 day" = 288,
                    "5 days" = 1440,
                    "1 month"= 8640, 
                    "all"    = dim)
    
    #Create datelist 
    
    #date1 <- '07/07/2021'
    #finaldate <- format(Sys.time(), "%m/%d")
    
    #construct compacted date list 
    lowerbound <- dim-range
    dates <- data[c(lowerbound:dim),c(1:1)]
    dates <- dates[seq(1, range, (range/5))]
    
    
    #scale_x_discrete w/premade label set or scale_x_continuous() for xtick spacing adjustment 
    #plot range needs to show tail end of data, not the first couple datapoints
    #
    
    tempPlot <- ggplot(data[c(lowerbound:dim),c(1:3)] , aes(x=factor(data[c(lowerbound:dim),c(1:1)]),y=data[c(lowerbound:dim),c(2:2)])) + geom_line(aes(group=1),color='red') + labs(x = "Time (days)", y = "Temperature (C)")  + scale_x_discrete(breaks = c(dates)) + theme_minimal()   
    tempPlot <- ggplotly(tempPlot)
    tempPlot
    
  })
  
  liveish_data <- reactive({
    invalidateLater(100)
    
    #photo feed code
    
    
    
  })
  
  output$humgraph <- renderPlotly({
    dims <- dim(amb)
    dim <- dims[1]
    
    data <- switch(input$humSensors,
                   "Ambient_SD"  = amb,
                   "Gryffindor"  = gry,
                   "Reliability" = rel,
                   "Ambient_HP"  = HPa,
                   "Leakage_HP"  = Le1,
                   "Leakage2_HP" = Le2,
                   "All")
    range <- switch(input$timescaleHum,
                    "1 day" = 288,
                    "5 days" = 1440,
                    "1 month"= 8640, 
                    "all"    = dim)
    
    
    #generate compactified datelist
    lowerbound <- dim-range
    dates <- data[c(lowerbound:dim),c(1:1)]
    dates <- dates[seq(1, range, (range/5))]
    
    
    humgraph <- ggplot(data[c(lowerbound:dim),c(1:3)] , aes(x=factor(data[c(lowerbound:dim),c(1:1)]),y=data[c(lowerbound:dim),c(3:3)])) + geom_line(aes(group=1),color='red') + labs(x = "Time (days)", y = "Humidity (%RH)")  + scale_x_discrete(breaks = c(dates)) + theme_minimal()   
    humpgrah <- ggplotly(humgraph)
    humgraph
  })  
  
  
  
  output$htmlout <- renderUI({getPage()})
  
  
  output$plot_access_web <- renderPlot({
    if (input$plot1 == "Stacked Bars") {
      ggplot(survey, aes(groupInput())) + 
        geom_bar(aes(fill = dataInput()), position = position_stack(reverse = TRUE), width = 0.4, alpha = 0.75) +
        scale_fill_manual(values = c(palette[[1]][4], palette[[2]][1], palette[[3]][1])) +
        scale_x_discrete(limits = rev(levels(groupInput()))) +
        coord_flip() +
        theme(axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15, margin = margin(0,3,0,0)),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.ticks.x = element_line(size = 0),
              legend.title = element_blank(),
              legend.text = element_text(size = 15),
              plot.margin = unit(c(1,2,2,3), "cm")) 
    } else if (input$plot1 == "Grouped Bar Charts") {
      ggplot(survey, aes(groupInput())) + 
        geom_bar(aes(fill = dataInput()), position = "dodge", width = 0.6, alpha = 0.75) +
        scale_fill_manual(values = c(palette[[1]][4], palette[[2]][1], palette[[3]][1])) +
        scale_x_discrete(limits = rev(levels(groupInput()))) +
        coord_flip() +
        theme(axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15, margin = margin(0,3,0,0)),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.ticks.x = element_line(size = 0),
              legend.title = element_blank(),
              legend.text = element_text(size = 15),
              plot.margin = unit(c(1,2,2,3), "cm")) 
    } else if (input$plot1 == "Stacked Bars (Percent)") {
      ggplot(survey, aes(groupInput())) + 
        geom_bar(aes(fill = dataInput()), position = "fill", width = 0.3, alpha = 0.75) +
        scale_fill_manual(values = c(palette[[1]][4], palette[[2]][1], palette[[3]][1])) +
        scale_x_discrete(limits = rev(levels(groupInput()))) +
        coord_flip() +
        theme(axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15, margin = margin(0,3,0,0)),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.ticks.x = element_line(size = 0),
              legend.title = element_blank(),
              legend.text = element_text(size = 15),
              plot.margin = unit(c(1,2,2,3), "cm")) 
    }
  })
  
}

