library(shiny)
library(reticulate)


addResourcePath("tmpuser",getwd())

server <- function(input,output) {

  ev <- reactiveValues(data=NULL)
  
  source(file.path("samplePlot.R"), local = TRUE)$value  #sample plot
  source(file.path("samplePlot2.R"), local = TRUE)$value  #sample plot
  source(file.path("tempPlot.R"), local=TRUE)$value      #Temp Plot
  source(file.path("humPlot.R"), local=TRUE)$value      #Temp Plot
  source_python("main.py")
  
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
  
  returnAnalysis <- function() {
    #return input$analysisSelection
    return (input$analysisSelection)
  }
  
  
  renderHTML <- eventReactive(input$runAnalysis, {
    render(input = "C:/home/dashTest/Gryffindor_Force_and_Position_Plots_MASTER.Rmd", "html_document", output_file = "test.html")
  
  })

  output$htmlTest <- renderUI({
    tag$div(
      class = "rmd-class",
      includeHTML("C:\\home\\dashTest\\Analysis\\test.html")
      )
  })
  
  
  #output$html_doc <- eventReactive(input$runAnalysis) {
  #  print('Running analysis...')
  #  render(input = "C:/home/dashTest/Gryffindor_Force_and_Position_Plots_MASTER.Rmd", "html_document", output_file = "test.html")
  #}
  
  #action button
  

  
  #HTML output file selection
  
#output$htmlOut <- renderUI({
 #  HTML(markdown::renderMarkdownToHTML(knit('C:/home/dashTest/Gryffindor_Force_and_Position_Plots_MASTER.Rmd')))
    
 # })

  
    

  
  output$samplePlot <- renderPlotly({
    samplePlot <- ggplot(data, aes(x=x,y=random_y)) +geom_point()
    samplePlot <- ggplotly(samplePlot)
    samplePlot
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
     
     #xticks <- data$Time
     #del xticks[::2]
     #Create datelist 
     date1 <- '07/07/2021'
     finaldate <- format(Sys.time(), "%m/%d")
     
     
     #scale_x_discrete w/premade label set or scale_x_continuous() for xtick spacing adjustment 
     tempPlot <- ggplot(data[c(1:range),c(1:3)] , aes(x=factor(Time[1:range]),y=Temperature..C.[1:range])) + geom_line(aes(group=1),color='red')  + xlab("Time") + ylab("Temperature (C)") + theme_minimal()  
     tempPlot <- ggplotly(tempPlot)
     tempPlot
     
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
    
  #  Time <- switch(range,
  #                 "Ambient_SD"
  #                 )
    
    
    humgraph <- ggplot(data[c(1:range),c(1:3)] , aes(x=factor(Time[1:range]),y=Humidity...RH.[1:range])) + geom_line(aes(group=1),color='red')  + theme_minimal() 
    humgraph <- ggplotly(humgraph)
    humgraph 
  })  
  
  
#  output$tempPlot <- renderImage({
#    list(src = "tempPlot.png")
#  })
  
  output$retPlot <- renderImage({
    list(src = "tempPlot.png")
  })
  
  output$humPlot <- renderImage({
    list(src = "humPlot.png")
  })
  
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
  

