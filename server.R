library(shiny)
library(here)
library(rmarkdown)
library(tidyverse)
library(shinyFiles)
library(fs)
library(DT)
library(plotly)
library(abind)

#test comment 

options(shiny.maxRequestSize=30*1024^2)

analysispath <- "/opt/shiny-server/samples/sample-apps/dashtest/analysis"
addResourcePath("tmpuser",getwd())
reticulate::user_virtualenv("/opt/shiny-server/samples/sample-apps/dashtest/testenv,",required=TRUE)
reticulate::use_python("/usr/bin/python2.7")

server <- function(input,output,session) {
  
  tabIndex <- reactiveVal(0)
  volumes = getVolumes()
  basepath <- 'C:/home/dashTest/dashTest/'
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
  
  
  observeEvent(input$runAnalysis, {
    #should a seperate version of these scripts be kept with different file paths for the server? The paths are inherently different...
    input_file <- paste(basepath, 'rmd/', input$analysisFile)
    input_file <- gsub(" ", "", input_file)
    
    output_file <- paste(basepath, 'analysis/', input$dataSelection, '.html')
    output_file <- gsub(" ", "", output_file)
    
    render(input = input_file, "html_document", output_file = output_file) 
    
  })
  
  # observeEvent(input$runRF1, {
  #   #should a seperate version of these scripts be kept with different file paths for the server? The paths are inherently different...
  #   input_files <- input$RF1
  #   
  #   output_file <- paste(basepath, 'analysis/', input$dataSelection, '.html')
  #   output_file <- gsub(" ", "", output_file)
  #   
  #   render(input = input_file, "html_document", output_file = output_file) 
  #   
  # })
  
  
  #S2p Drag/Drop handling 
  output$fileNames <- renderTable({
    # Return immediately if user doesn't select any files.
    if (is.null(input$csvFiles)) {
      return()
    }
    return(input$csvFiles)
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
  #shinyFileChoose(input, 'files', root=c(root='.'), filetypes=c('', '.txt', '.html', '.s2p', '.R', '.Rmd'))
  
  
  
#######S2P PLOTTING VERSION 2######################
  
  output$s2pPlot <- renderPlotly({
    skrf <- import("skrf")
    plt <- import("matplotlib.pyplot")
    file <- input$s2pFiles
    paths <- file$datapath
    numFiles <- dim(input$s2pFiles)[1]
    names <- file$name
    
    net <- skrf$Network(file$datapath[1])
    s11 <- net$s11
    db <- s11$s_db
    f <-  s11$f
    data <- data.frame(f,db)
    
    
    fig <- plot_ly(data,x=~f,y=~db,type='scatter',mode='lines',name=names[1])
    cnt<-1
    
    for (f in 2:numFiles) {
      net <- skrf$Network(paths[cnt])
      s11 <- net$s11
      f <-  s11$f
      db <- s11$s_db
      data <- data.frame(f,db)
      data$db <- unlist(data$db)
      
      #add_marker(data=Df_game, name="line2", x = ~Timestamp, y = ~CurrentRecognitionRate)
      
      fig <- fig %>% add_trace(data=data,x=~f,y=~db,mode='lines', name = names[cnt])
    #  fig <- fig %>% add_markers(data=data,x=~f,y=~db, name = names[cnt],mode='lines')
      cnt <- cnt+1
    }
    
    fig <- fig %>% layout(title='S11', legend = list(orientation="h",y=-0.3)) 
    fig
    
    #RE-WRITE WITH PLOTLY SO THAT NAMES CAN BE ASSIGNED APPROPRIATELY 
  })
  
  output$s2pPlot2 <- renderPlotly({
    
    skrf <- import("skrf")
    plt <- import("matplotlib.pyplot")
    file <- input$s2pFiles
    paths <- file$datapath
    numFiles <- dim(input$s2pFiles)[1]
    names <- file$name
    
    net <- skrf$Network(file$datapath[1])
    s12 <- net$s12
    db <- s12$s_db
    f <-  s12$f
    data <- data.frame(f,db)
    
    fig <- plot_ly(data,x=~f,y=~db,type='scatter',mode='lines',name=names[1])
    cnt<-1
    
    for (f in 2:numFiles) {
      net <- skrf$Network(paths[cnt])
      s12 <- net$s12
      f <-  s12$f
      db <- s12$s_db
      data <- data.frame(f,db)
      data$db <- unlist(data$db)
      
      fig <- fig %>% add_trace(data=data,y=~db,mode='lines', name = names[cnt])
      cnt <- cnt+1
    }
    
    
    fig <- fig %>% layout(title='S12',legend = list(orientation="h",y=-0.3)) 
    fig
  })
    
  
  output$timePlot <- renderPlotly({
    skrf <- import("skrf")
    file <- input$s2pFiles
    paths <- file$datapath
    skrf <- import("skrf")
    numFiles <- dim(input$s2pFiles)[1]
    names <- file$name
    
    net <- skrf$Network(paths[1])
    s11 <- net$s11
    s11_ext <- s11$extrapolate_to_dc()
    t <- s11_ext$step_response()[[1]]
    z <- s11_ext$step_response()[[2]]
    
    t<- t*10e9
    x <- 50*(1+z)/(1-z)
    z <- x
    data <- data.frame(t,z)
    
    tdr <- plot_ly(data,x=~t,y=~z,type='scatter',mode='lines',name=names[1])
    cnt <- 1 
    
    for (f in 2:numFiles) {
      net <- skrf$Network(paths[cnt])
      s11 <- net$s11
      s11_ext <- s11$extrapolate_to_dc()
      t <- s11_ext$step_response()[[1]]
      z <- s11_ext$step_response()[[2]]
      
      t<- t*10e9
      x <- 50*(1+z)/(1-z)
      z <- x
      data <- data.frame(t,z)
      tdr <- tdr %>% add_trace(data=data,y=~z,mode='lines', name = names[cnt])
      
      cnt <- cnt+1
    
    }
    
    tdr <- tdr %>% layout(title='Time Domain Conversion',xaxis=list(title='Time (s)'),yaxis=list(title='Impedance (Ohm)'),legend = list(orientation="h",y=-0.3)) 
    tdr
  
  })
    
  
  
  
  
############S2P PLOTTING OLD#####################
  # output$s2pPlot <- renderPlotly({
  #   file <- input$csvFiles
  #   print(file$datapath)
  #   data <- tools::file_ext(file$datapath)
  #   
  #   req(file)
  #   validate(need(data=="s2p","Please upload an s2p file"))
  #   s2pdata <- read.csv(file$datapath)
  #   print(file$datapath)
  #   
  #   skrf <- import("skrf")
  #   plt <- import("matplotlib.pyplot")
  #   net <- skrf$Network(file$datapath)
  # 
  #   s11 <- net$s11
  #   f <- s11$f
  #   db <- s11$s_db
  #   data <- data.frame(f,db)
  #   data$db <- unlist(data$db)
  #   print(db)
  #   
  #   #+ scale_x_continuous(trans = 'log10') + scale_y_continuous(trans = 'log10')
  #   plot <- ggplot(data, aes(x = f, y = db)) + geom_line(aes(group=1)) + labs(x = "Frequency", y = "Magntitude (dB)") + ggtitle('S11')
  #   plot
  #   
  # })
  # 
  # output$s2pPlot2 <- renderPlotly({
  #   file <- input$csvFiles
  #   data <- tools::file_ext(file$datapath)
  #   
  #   req(file)
  #   validate(need(data=="s2p","Please upload an s2p file"))
  #   s2pdata <- read.csv(file$datapath)
  #   print(file$datapath)
  #   
  #   skrf <- import("skrf")
  #   plt <- import("matplotlib.pyplot")
  #   net <- skrf$Network(file$datapath)
  #   
  #   s12 <- net$s12
  #   f <- s12$f
  #   db2 <- s12$s_db
  #   
  #   data2 <- data.frame(f,db2)
  #   #data2$db2 <- unlist(data2$db2)
  #   #data2$db2 <- Re(data2$db2)
  #   
  #   #Add new plot rather than overwriting 
  #   plot2 <- ggplot(data2, aes(x = f, y = db2)) + geom_line(aes(group=1)) + labs(x = "Frequency", y = "Magntitude (dB)") + ggtitle('S12')
  #   plot2
  #})
  
  # output$timePlot <- renderPlotly({
  #   file <- input$csvFiles
  #   data <- tools::file_ext(file$datapath)
  #   
  #   req(file)
  #   validate(need(data=="s2p","Please upload an s2p file"))
  #   s2pdata <- read.csv(file$datapath)
  #   print(file$datapath)
  #   
  #   skrf <- import("skrf")
  #   plt <- import("matplotlib.pyplot")
  #   net <- skrf$Network(file$datapath)
  #   
  #   s11 <- net$s11
  #   s11_ext <- s11$extrapolate_to_dc()
  #   t <- s11_ext$step_response()[[1]]
  #   z <- s11_ext$step_response()[[2]]
  #   
  #   t<- t*10e9
  #   x <- 50*(1+z)/(1-z)
  #   z <- x
  #   data <- data.frame(t,z)
  #   
  #   tdr <- ggplot(data, aes(x = t, y = z)) + geom_line(aes(group=1)) + labs(x = "Time (s)", y = "Impedance (Ohm)") + ggtitle('Time domain conversion')
  #   tdr
  #   
  #   
  # })


  
################RF1 ANALYSIS##############################
  
  output$RF1_analysis <- renderUI({
    filePth <- getwd()
    
    
    render(input = "C:/home/dashtest/RF_Analysis_Data_Aggregation.Rmd")
    #Delay(10000)
    #render(input = "C:/home/dashtest/RF_Analysis_Part1.Rmd")
    
  })

  observeEvent(input$dir, {   
    setwd(choose.dir("c:/")) #selecting a directory   
    output$wd <- renderText(getwd())

    input$RF1_pth <- getwd()
    })
  
  observeEvent(input$runRF1, {
    #Set path variable to be used in the data aggregation script 
  
  })
  
################RF2 ANALYSIS##############################  
  
  output$RF2_analysis <- renderUI({
    filePth <- getwd()
  })
  
  observeEvent(input$dir2, {   
    setwd(choose.dir("c:/")) #selecting a directory   
    output$wd <- renderText(getwd())})
  
  observeEvent(input$runRF1, {
    render(input = "C:/home/dashtest/RF_Analysis_Data_Aggregation.Rmd")
    render(input = "RF_Analysis_Part1.Rmd")
    
  }) 
  
  

###############TEC ANALYSIS##############################
  
    output$TEC_Analysis <- renderPlotly({
      
    #introduce tab/TEC number feature - TEC_Analysis needs to accept plot # argument 
     req(input$TECSlider)
     cycle <- input$TECSlider
     
    
     file <- input$TEC_File
     data <- tools::file_ext(file$datapath)
     
     d <<- read.delim(file$datapath, header = TRUE, sep = "\t", dec = ".", comment.char = "!", fill = TRUE)
     max_resistance <- as.double(input$maxR)
    
     # Get the columns
     first_resistance_column <<- which(names(d) == "Date") + 1 # used to indicate which column is the first one that contains resistacne data
     last_resistance_coluimn <<- which(names(d) == "FBSteps") - 1 # specifies the last column that contains resistance data
     Force_column <- which(names(d)=="LdCel.0")              #Load Cell data - forces
     cycleLength <- dim(d)[1]/numTabs()
     
     ay <- list(
     tickfont = list(color='red'),
     overlaying = "y",
     side = "right",
     title = "<b>Force</b> (lbs)"
     )
    
     # Create the plot
     plt <- plot_ly(data = d[(cycleLength*(cycle-1)):(cycleLength*cycle),], type = "scatter", mode = "lines")
     for(i in first_resistance_column:last_resistance_coluimn){
       plt <- plt %>% add_trace(x = ~External.Z.Delayed, y = d[(cycleLength*(cycle-1)):(cycleLength*cycle),i], name = names(d)[i])
     }
     d[((cycleLength*(cycle-1))):((cycleLength*cycle)),Force_column][1] <- 0
     plt <- plt %>% add_trace(x= ~External.Z.Delayed,y=na.omit(d[((cycleLength*(cycle-1))):((cycleLength*cycle)),Force_column]),name = "Force", yaxis="y2",line=list(width=5,color='red'))      #Force trace
         # Add labels and set range limit
     plt_title <- c("Total Electrical Compliance")
   
     plt <- plt %>% layout(xaxis = list(title = "Distance (mils)"),
                          yaxis = list(range = c(0,max_resistance),
                                       title = c("Resistance (Ohms)")),
                          title = plt_title,
                          yaxis2 = ay,
                          showlegend = FALSE,
                          margin = list(b=20,t=10,r=50,l=50)
                          )
     plt
   
  })
  
    stats <-reactive({
      #need a TEC input if going to split up the plots 
      file <- input$TEC_File
      data <- tools::file_ext(file$datapath)
      cycle <- cycle()
      cycleLength <- dim(d)[1]/numTabs() 
      
      print(cycle)
      
      d <- read.delim(file$datapath, header = TRUE, sep = "\t", dec = ".", comment.char = "!", fill = TRUE)
      max_resistance <- .1
      
      # Get the columns
      first_resistance_column <- which(names(d) == "Date") + 1 # used to indicate which column is the first one that contains resistacne data
      last_resistance_coluimn <- which(names(d) == "FBSteps") - 1 # specifies the last column that contains resistance data
      Force_column <- which(names(d)=="LdCel.0")              #Load Cell data - forces
      
       #Search through the correct rows as set by cycle variable

       ncol = last_resistance_coluimn-first_resistance_column
       for (r in (((cycle-1)*cycleLength)+1):(cycleLength*cycle)){
         for (c in 1:ncol) {
           #print(d[r,c+first_resistance_column])
           if (d[r,c+first_resistance_column]>0.1){
             row <-r
           } else {
             row <-2
           }
         }
       }
       row
       
       set <- t(d[row,c(first_resistance_column:last_resistance_coluimn)])
       subset <- set[set[,1]<max_resistance]
       #d[row,c(first_resistance_column:last_resistance_coluimn)>max_resistance] <- max_resistance 
       #TEC - point at which all pins drop below 100mOhm
       avgTEC <- mean(subset)
       stdTEC <- sd(subset)
       df_TEC <- c(avgTEC,stdTEC,4*stdTEC,5*stdTEC,6*stdTEC,7*stdTEC)
       
       #Full Compression
       avgC <- mean(as.double(d[cycle*cycleLength,c(first_resistance_column:last_resistance_coluimn)]))
       stdC <- sd(as.double(d[cycle*cycleLength,c(first_resistance_column:last_resistance_coluimn)]))
       df_Comp <- c(avgC,stdC,4*stdC,5*stdC,6*stdC,7*stdC)
       
       labels = c('mean', 'std', '4 sigma', '5 sigma', '6 sigma', '7 sigma')
       headers = c('TEC', 'Full Compression')
       
       stats <- data.frame(metrics=labels,TEC = df_TEC, COMPRESSED = df_Comp)
       #stats <- do.call(rbind.data.frame, stats)
       #stats <- as.data.frame(stats)
       
       #stats <- matrix(unlist(stats),ncol=dim(stats)[2])
       stats
  })
    

    
    output$pads <- renderPlotly({
      cycle <- cycle()
      cycleLength <- dim(d)[1]/numTabs() 
      y_scale <- as.double(input$maxR)
      
      file <- input$padFile
      pattern <- read.delim(file$datapath, header = TRUE, sep = "\t", dec = ".", comment.char = "!", fill = TRUE)
      x_column <- na.omit(pattern['X'])
      y_column <- na.omit(pattern['Y'])
      
      pattern <- pattern[0:dim(x_column)[1],]
      
      # comp <- d[dim(d)[1],first_resistance_column:last_resistance_coluimn]
      # comp <- as.numeric(comp)
      # pattern <- cbind(pattern,'res'=comp)

      #arr <- array(0,dim=c(dim(pattern)[1],(dim(pattern)[2]+1),cycle))
      
      resData <- d[((cycle-1)+1):(cycle*cycleLength),first_resistance_column:last_resistance_coluimn,] #address each res set with resData[n,]
      
      temp <- resData[1,]
      temp2 <- resData[2,]
      sheet1 <- pattern
      sheet2<- pattern
      sheet1$res <- t(temp)
      sheet2$res <- t(temp2)
      sheet1$cycle <- 1
      sheet2$cycle <- 2
  
      #arr <- abind(sheet1,sheet2,along=3)
      arr <- rbind(sheet1,sheet2)
  
      #Get pattern sheet, add one column of resistance data, and then stack sheets together. To add one column... 
      for (i in 2:dim(resData)[1]) {
        #Get resistance data for one cycle
        temp <- resData[i,]
        
        
        #Append resistance data for each cyle to the pattern data to create a sheet
        sheet <- pattern
        sheet$res <- t(temp)
        sheet$cycle <- i
        #Append sheet to three dimensional array initialized outside of the loop
        arr <- rbind(arr,sheet)
      }
      res_c <- which(names(arr)=='res')
      arr[arr$res>y_scale,res_c] <- y_scale
      
       
      #To animate this, need to have exactly one variable that corresponds to the X/Y position sheet
      #Or, could append sheets vertically and have an additional column label for cycle, and set frame to cycle 
      plt <- plot_ly(data=arr,x=~X,y=~Y,type = 'scatter',mode='markers',color=~res,frame=~cycle)
      plt <- plt %>% layout(title='Pad pattern',xaxis = list(title = ""),yaxis = list(title = ""))
      plt
      
      #Append colors based on full compression data. Need to append column of resistances at full compression -> get from the global d frame 
      
    })
    
    numTabs <- reactive({
      file <- input$TEC_File
      data <- tools::file_ext(file$datapath)
      d <- read.delim(file$datapath, header = TRUE, sep = "\t", dec = ".", comment.char = "!", fill = TRUE)
      Force_column <- which(names(d)=="LdCel.0")
      cnt<-1
      
      for (i in 1:dim(d[Force_column])[1]-1) {
        if(isTRUE(((d[i,Force_column]-d[i+1,Force_column])>2))) {
          cnt<-cnt+1
        }}
      cnt
      
    })
    
    cycle <- reactive({
      currCycle <- input$TECSlider 
      currCycle
    })
  
    observeEvent(input$TEC_File,{
      removeTab("TECTabs", target=input$TECTabs)
      for (i in 1:numTabs()) {
        appendTab("TECTabs",
                  tabPanel(i,
                           fluidPage(
                             #could maybe pass cycle #argument 
                             plotlyOutput(outputId = "TEC_Analysis", height = "1000px", width = "900px"),
                             DTOutput("TEC_Stats", width = "100%",height = "auto")
                           )), select=TRUE)
      }
    }) 
    
    output$TEC_Stats <- renderDT(
      stats()
    )
    
    #Tab handling for multiple TEC plot tabs 
    # lapply(1:5, function(j) {
    #   output[[paste0('out',j)]] <- renderPrint({
    #     paste0('generated out ', j)
    #   })
    # })
  
    currTab <- reactive({
      input$TECTabs
    })
    
    output$slider <- renderUI({
      req(numTabs())
      sliderInput(inputId='TECSlider',"TEC cycle",min=0,max=numTabs(),value=1)
    })
    
    
################CYCLE PLOTS##############################

    output$CyclesPlot <- renderPlotly({
      
      #introduce tab/TEC number feature - TEC_Analysis needs to accept plot # argument
      
      file <- input$Cycles_File
      d <<- read.delim(file$datapath, header = TRUE, sep = "\t", dec = ".", comment.char = "!", fill = TRUE)
      cycleLength <- dim(d)[1]
      
      max_resistance <- as.double(input$maxRC)
      
      
      # Get the columns
      first_resistance_column <<- which(names(d) == "Date") + 1 # used to indicate which column is the first one that contains resistacne data
      last_resistance_coluimn <<- which(names(d) == "FBSteps") - 1 # specifies the last column that contains resistance data
      Force_column <- which(names(d)=="LdCel.0")              #Load Cell data - forces
      
      ay <- list(
        tickfont = list(color='red'),
        overlaying = "y",
        side = "right",
        title = "<b>Force</b> (lbs)"
      )
      
      # Create the plot
      numCycles <- dim(d)[1]
      plt <- plot_ly(data = d[1:cycleLength,], type = "scatter", mode = "lines")
      for(i in first_resistance_column:last_resistance_coluimn){
        plt <- plt %>% add_trace(x = d[1], y = d[1:cycleLength,i], name = names(d)[i])
      }
      
      #d[((cycleLength*(cycle-1))):((cycleLength*cycle)),Force_column][1] <- 0
      if (input$enableForce) {
        plt <- plt %>% add_trace(x= d[1],y=na.omit(d[1:cycleLength,Force_column]),name = "Force", yaxis="y2",line=list(width=1,color='red'))      #Force trace
      }
      
      # Add labels and set range limit
      plt_title <- c("Cycles vs DCR")
      
      plt <- plt %>% layout(xaxis = list(title = "Distance (mils)"),
                            yaxis = list(range = c(0,max_resistance),
                                         title = c("Resistance (Ohms)")),
                            title = plt_title,
                            yaxis2 = ay,
                            showlegend = FALSE,
                            #Legend fundamentally overlaps the yaxis2 label - this is a known issue 
                            legend = list(orientation="s",xanchor="center"),
                            margin = list(b=20,t=10,r=100,l=50)
      )
      
      plt
    })

    

################TEMPERATURE PLOTS#########################
  
  output$tempPlot <- renderPlotly({
 
    data <- amb
    dims <- dim(data)
    dim <- dims[1]
    
    
    data <- switch(input$tempSensors,
                   "Ambient_SD"  = amb,
                   "Gryffindor"  = gry,
                   "Reliability" = rel,
                   "Ambient_HP"  = HPa,
                   "Leakage_HP"  = Le1,
                   "Leakage2_HP" = Le2,
                   "All"         = test)
    
    dims <- dim(data)
    dim <- dims[1]
    
    range <- switch(input$timescaleTemp,
                    "1 day" = 288,
                    "5 days" = 1440,
                    "1 month"= 8640, 
                    "all"    = dim)

    

    
    max <- as.numeric(max(data$Temperature..C.[(dim-range):dim],na.rm=TRUE))
    min <- as.numeric(min(data$Temperature..C.[(dim-range):dim],na.rm=TRUE))
    
    tempTicks <- seq(min,max,((max-min)/10))
    
    lowerbound <- dim-range
    dates <- data[c(lowerbound:dim),c(1:1)]
    dates <- dates[seq(1, range, (range/5))]
    
    
    tempPlot <- ggplot(data[c(lowerbound:dim),c(1:3)] , aes(x=factor(data[c(lowerbound:dim),c(1:1)]),y=data[c(lowerbound:dim),c(2:2)])) + 
      geom_line(aes(group=1),color='red') + labs(x = "Time (days)", y = "Temperature (C)")  + 
      scale_x_discrete(breaks = c(dates)) +
      scale_y_continuous(limits=c(tempTicks[1],tempTicks[11])) + theme_minimal()
    
  })
  
  
  output$humgraph <- renderPlotly({
    dims <- dim(data)
    dim <- dims[1]
    
    
    data <- switch(input$humSensors,
                   "Ambient_SD"  = amb,
                   "Gryffindor"  = gry,
                   "Reliability" = rel,
                   "Ambient_HP"  = HPa,
                   "Leakage_HP"  = Le1,
                   "Leakage2_HP" = Le2,
                   "All")
    
    dims <- dim(data)
    dim <- dims[1]
    
    range <- switch(input$timescaleHum,
                    "1 day" = 288,
                    "5 days" = 1440,
                    "1 month"= 8640, 
                    "all"    = dim)
    
    max <- as.numeric(max(data$Humidity...RH.[(dim-range):dim],na.rm=TRUE))
    min <- as.numeric(min(data$Humidity...RH.[(dim-range):dim],na.rm=TRUE))
    
    #generate compactified datelist
    lowerbound <- dim-range
    dates <- data[c(lowerbound:dim),c(1:1)]
    dates <- dates[seq(1, range, (range/5))]
    
    humgraph <- ggplot(data[c(lowerbound:dim),c(1:3)] , aes(x=factor(data[c(lowerbound:dim),c(1:1)]),y=data[c(lowerbound:dim),c(3:3)])) + 
      geom_line(aes(group=1),color='red') + 
      labs(x = "Time (days)", y = "Humidity (%RH)")  + 
      scale_x_discrete(breaks = c(dates)) + 
      scale_y_continuous(limits=c(humTicks[1],humTicks[11])) + theme_minimal()   
  })  
  
  #shinyFileChoose(input, 'files', root=c(root='.'), filetypes=c('', '.txt', '.html', '.s2p', '.R', '.Rmd'))
  
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
