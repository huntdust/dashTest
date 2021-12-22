library(shiny)
library(here)
library(rmarkdown)
library(tidyverse)
library(shinyFiles)
library(fs)
library(DT)
library(plotly)
library(abind)
library(e1071) 

#test comment 
#test comment 2
###############

############


options(shiny.maxRequestSize=30*1024^2)

analysispath <- "C:/home/dashTest/Analysis/"
addResourcePath("tmpuser",getwd())

server <- function(input,output,session) {
  
  tabIndex <- reactiveVal(0)
  volumes <- getVolumes()
  #volumes <- c(Home = fs::path_home())
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
    req(input$s2pFiles)
    
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
    
    
    fig <- plot_ly(data,x=~f,y=~db,type='scatter',mode='lines',name=names[1],hoverinfo = names[1])
    cnt<-2
    
    if (numFiles>1) {
      for (f in 2:numFiles) {
        #file 
        net <- skrf$Network(paths[cnt])
        s11 <- net$s11
        f <-  s11$f
        db <- s11$s_db
        data <- data.frame(f,db)
        data$db <- unlist(data$db)
        
        #add_marker(data=Df_game, name="line2", x = ~Timestamp, y = ~CurrentRecognitionRate)
        
        fig <- fig %>% add_trace(data=data,x=~f,y=~db,mode='lines', name = names[cnt],hoverinfo = names[cnt])
      #  fig <- fig %>% add_markers(data=data,x=~f,y=~db, name = names[cnt],mode='lines')
        cnt <- cnt+1
      }}
    
    fig <- fig %>% layout(title='S11',xaxis=list(title='Frequency (Ghz)'),yaxis=list(title='Magnitude of S11 (dB)'),legend = list(orientation="h",y=-0.3)) 
    fig <- fig %>% animation_opts(frame = 20)
    fig
    
    #RE-WRITE WITH PLOTLY SO THAT NAMES CAN BE ASSIGNED APPROPRIATELY 
  })
  
  output$s2pPlot2 <- renderPlotly({
    req(input$s2pFiles)
    
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
    
    fig <- plot_ly(data,x=~f,y=~db,type='scatter',mode='lines',name=names[1],hoverinfo = names[1])
    cnt<-2
    
    if (numFiles>1) {
      for (f in 2:numFiles) {
        net <- skrf$Network(paths[cnt])
        s12 <- net$s12
        f <-  s12$f
        db <- s12$s_db
        data <- data.frame(f,db)
        data$db <- unlist(data$db)
        
        fig <- fig %>% add_trace(data=data,y=~db,mode='lines', name = names[cnt], hoverinfo = names[cnt])
        cnt <- cnt+1
      }}
    
    
    fig <- fig %>% layout(title='S12',xaxis=list(title='Frequency (Ghz)'),yaxis=list(title='Magnitude of S12(dB)'),legend = list(orientation="h",y=-0.3)) 
    fig
  })
    
  
  output$timePlot <- renderPlotly({
    req(input$s2pFiles)
    
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
    
    tdr <- plot_ly(data,x=~t,y=~z,type='scatter',mode='lines',name=names[1],hoverinfo = names[1])
    cnt <- 2 
    
    if (numFiles>1) {
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
        tdr <- tdr %>% add_trace(data=data,y=~z,mode='lines', name = names[cnt], hoverinfo = names[cnt])
        
        cnt <- cnt+1
    }}
    
    tdr <- tdr %>% layout(title='Time Domain Conversion',xaxis=list(title='Time (s)'),yaxis=list(title='Impedance (Ohm)'),legend = list(orientation="h",y=-0.3)) 
    tdr
    
    #test
    # if(!is.null(input$fileOpen)){
    #   inFile <- input$s2pFiles
    #   old_name <- inFile$datapath
    #   dirstr <- dirname(inFile$datapath)
    #   new_name <- paste(dirstr, inFile$name,sep="/")
    #   file.rename(old_name, new_name)
    #   print(new_name)
    # }
  })
  
  

  ##################report handler##########################
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "SParameterReport.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "s2pRreport.Rmd")
      file.copy("s2pReport.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(s2pfile = input$s2pFiles, stats = input$includeStats_s2p)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
################RF1 ANALYSIS##############################
#first get path from path input selector, then use it to run/render aggregated data, then...
#get dir from regular fileInput
  

  
  ##############file input########################
  
   global <- reactiveValues(datapath = getwd())

   dir <- reactive(input$dir)

   output$dir <- renderText({
      global$datapath
   })

   observeEvent(ignoreNULL = TRUE,
                eventExpr = {
                  input$dir
                },
                handlerExpr = {
                  if (!"path" %in% names(dir())) return()
                  home <- normalizePath("~")
                  global$datapath <-
                  file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
  
                })

   RF1_pth <- reactive({
     dir <- input$RF1_select
     dir
   })
  
  shinyDirChoose(
    input,
    'dir',
    roots = volumes,
    filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw","s2p")
  )
  ###########################################################
  ###################aggregation############################
  
  observeEvent(input$runRF1, {
    print('Running aggregation...')
    rmarkdown::render(input = "C:/home/dashtest/RF_Analysis_Data_Aggregation.Rmd")#params=list(pth = input$dir)
    #rmarkdwon::render(input= "C:/home/dashtest/RF_Analysis_Part1.Rmd",params=list(pth = getwd()))
    
    #render(input="C:/home/dashtest/RF_Analysis_Part1.Rmd")
    
    
  })
  
  observeEvent(input$dir, {   
    setwd(choose.dir("c:/")) #selecting a directory   
    output$wd <- renderText(getwd())})
  
  ##########################################################
  
  
################RF2 ANALYSIS#################################
  
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
  
  

###############TEC ANALYSIS###########################################
  
    output$TEC_Analysis <- renderPlotly({
      
    #introduce tab/TEC number feature - TEC_Analysis needs to accept plot # argument 
     req(input$TECSlider, input$TEC_File)
     cycle <- input$TECSlider
     
    
     file <- input$TEC_File
     data <- tools::file_ext(file$datapath)
     
     d <<- read.delim(file$datapath, header = TRUE, sep = "\t", dec = ".", comment.char = "!", fill = TRUE,skip=22)
     max_resistance <- as.double(input$maxR)
    
     # Get the columns
     first_resistance_column <<- which(names(d) == "Date") + 1 # used to indicate which column is the first one that contains resistacne data
     last_resistance_column <<- which(names(d) == "FBSteps") - 1 # specifies the last column that contains resistance data
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
     for(i in first_resistance_column:last_resistance_column){     #Add one trace for each pin 
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
                          margin = list(b=20,t=50,r=50,l=50)
                          )
     plt
   
  })
  
  #Histogram plot for TEC - animated histogram vs. displacement 
  output$TECHist <- renderPlotly({
    req(input$TECSlider, input$TEC_File)
    cycle <- input$TECSlider
    
    
    file <- input$TEC_File
    data <- tools::file_ext(file$datapath)

    cycleLength <- dim(d)[1]/numTabs() 
    
    d <<- read.delim(file$datapath, header = TRUE, sep = "\t", dec = ".", comment.char = "!", fill = TRUE,skip=22)
    max_resistance <- 0.1
    first_resistance_column <<- which(names(d) == "Date") + 1 # used to indicate which column is the first one that contains resistacne data
    last_resistance_column <<- which(names(d) == "FBSteps") - 1 # specifies the last column that contains resistance data
    Force_column <- which(names(d)=="LdCel.0")              #Load Cell data - forces
    disp_col <- which(names(d)=="External.Z.Delayed")
    
    temp <- d[disp_col]
    d[d>max_resistance*2] <- NA
    d[disp_col] <- temp
    
    # create steps and plot all histograms
    steps <- list()
    aval <- list()
    
    x <- seq(0,10, length.out = 1000)
    
    for(step in 1:cycleLength){
      aval[[step]] <-list(visible = FALSE,
                          name = paste0('v = ', step),
                          x=x,
                          y=sin(step*x))
    }
    
    fig <- plot_ly()
    for (i in 1:(cycleLength)) {
      fig<- fig %>% add_trace(x=unlist(d[i,first_resistance_column:last_resistance_column],), visible = aval[i][[1]]$visible, type = 'histogram',
                              xaxis=list(c(0,0.2)),yaxis=list(c(0,60)),bingroup=1)
      
      step <- list(args = list('visible', rep(FALSE, length(aval))), method = 'restyle',label = d[i,disp_col])
      step$args[[2]][i] = TRUE  
      steps[[i]] = step 
    }  
    
    # add slider control to plot
    fig <- fig %>%layout(title = 'TEC - Histogram of Resistance vs. Displacement', sliders = list(list(active = 1,
                                                                                                       currentvalue = list(prefix = "Displacement (mils): "),
                                                                                                       steps = steps)),xaxis=list(title='Resistance (Ohms)',range=c(0.00,max_resistance)),yaxis=list(title='Frequency',range=c(0,60)))
    fig
    
  })
  
    stats <-reactive({
      #need a TEC input if going to split up the plots 
      file <- input$TEC_File
      data <- tools::file_ext(file$datapath)
      cycle <- cycle()
      cycleLength <- dim(d)[1]/numTabs() 
      
      d <- read.delim(file$datapath, header = TRUE, sep = "\t", dec = ".", comment.char = "!", fill = TRUE, skip=22)
      max_resistance <- .1
      
      # Get the columns
      first_resistance_column <- which(names(d) == "Date") + 1 # used to indicate which column is the first one that contains resistacne data
      last_resistance_column <- which(names(d) == "FBSteps") - 1 # specifies the last column that contains resistance data
      Force_column <- which(names(d)=="LdCel.0")              #Load Cell data - forces
      
       #Search through the correct rows as set by cycle variable

      row <- 1
      
      ncol = last_resistance_column-first_resistance_column
      for (r in (((cycle-1)*cycleLength)+1):(cycleLength*cycle)){
        for (c in 1:ncol) {
          if ((d[r,c+first_resistance_column] > 0.1) | is.na(d[r,c+first_resistance_column])){
            row <- r
          } 
        }
      }
      
       #filtering into a single column, and removing outliers
       set <- t(d[row,c(first_resistance_column:last_resistance_column)])
       subset <- round(set[set[,1]<max_resistance],3)
       
       #TEC - point at which all pins drop below 100mOhm
       avgTEC <- signif(mean(subset),4)*10e2
       stdTEC <- signif(sd(subset),4)*10e2
       kurtTEC <- signif(kurtosis(subset),4)
       df_TEC <- c(kurtTEC,avgTEC,stdTEC,signif(min(subset),3)*10e2,signif(max(subset),3)*10e2,4*stdTEC,5*stdTEC,6*stdTEC,7*stdTEC)
       
       #Full Compression dataframe
       d <- round(as.double(d[cycle*cycleLength,c(first_resistance_column:last_resistance_column)]),5)
       
      #Caclulate pass rate 
       #spec <- input$DCR_spec
       #rowN <- dim(d[,first_resistance_column:last_resistance_column])[1]
       #colN <- dim(d[,first_resistance_column:last_resistance_column])[2]
       #total <- colN
       #failed <- sum(d[cycleCount*cycle,first_resistance_column:last_resistance_column]>spec,na.rm=TRUE)
       #pass_rate <- (1-(failed/total))
       
       avgC <- round(mean(d),5)*10e2
       stdC <- round(sd(d),5)*10e2
       kurtC <- signif(kurtosis(d),4)
       df_Comp <- c(kurtC,avgC,stdC,signif(min(d),3)*10e2,signif(max(d),3)*10e2,4*stdC,5*stdC,6*stdC,7*stdC)
       
       labels = c('Kurtosis #','mean', 'std', 'min', 'max' ,'4 sigma', '5 sigma', '6 sigma', '7 sigma')
       headers = c('TEC', 'Full Compression')
       
       stats <- data.frame(metrics=labels,TEC_mOhms = df_TEC, COMPRESSED_mOhms = df_Comp)
  
       stats
  })
    

    #render pad package 
    output$pads <- renderPlotly({
      cycle <- cycle()
      cycleLength <- dim(d)[1]/numTabs() 
      y_scale <- as.double(input$maxR)
      
      #read the input pad pattern
      file <- input$padFile
      pattern <- read.delim(file$datapath, header = TRUE, sep = "\t", dec = ".", comment.char = "!", fill = TRUE,skip=2)
      
      
     if ("x" %in% colnames(pattern)) {
        pattern <- rename(pattern,"X.1"="X")
        pattern <- rename(pattern,"Y"="y")
        pattern <- rename(pattern,"X"="x")

     }
      x_column <- na.omit(pattern['X'])
      y_column <- na.omit(pattern['Y'])
      
      #remove junk
      pattern <- pattern[0:dim(x_column)[1],]
      
      # comp <- d[dim(d)[1],first_resistance_column:last_resistance_column]
      # comp <- as.numeric(comp)
      # pattern <- cbind(pattern,'res'=comp)

      #arr <- array(0,dim=c(dim(pattern)[1],(dim(pattern)[2]+1),cycle))
      
      resData <- d[((cycle-1)+1):(cycle*cycleLength),first_resistance_column:last_resistance_column,] #address each res set with resData[n,]
      
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
      plt <- plt %>% animation_opts(frame=20)
      plt
      
      #Append colors based on full compression data. Need to append column of resistances at full compression -> get from the global d frame 
      
    })
    
    numTabs <- reactive({
      file <- input$TEC_File
      data <- tools::file_ext(file$datapath)
      d <- read.delim(file$datapath, header = TRUE, sep = "\t", dec = ".", comment.char = "!", fill = TRUE,skip=22)
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
    
    #output$TEC_Stats <- DT::renderDataTable({
    #  #datatable(stats(), options = list(paging=FALSE)) %>% formatStyle(color='white') 
    #  stats()
    #}) 
    
    output$TEC_Stats<- DT::renderDataTable({ 
      dat <- datatable(stats(), options = list(paging=FALSE)) %>%
        formatStyle(names(stats()),color = 'white', backgroundColor = 'black', fontWeight = 'bold',target='row')
      return(dat)
    })
    
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
    
#################report handler###########################
   
     output$TECReport <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "TECReport.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "TECReport.Rmd")
        file.copy("TECReport.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(TEC_File = input$TEC_File, padFile = input$padFile, maxR = input$maxR, TECSlider=input$TECSlider,stats=stats(),numTabs = numTabs(), cycle=cycle())
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
################CYCLE PLOTS##############################

    output$CyclesPlot <- renderPlotly({
      req(input$Cycles_File)
      
      #introduce tab/TEC number feature - TEC_Analysis needs to accept plot # argument
      
      file <- input$Cycles_File
      d <<- read.delim(file$datapath, header = TRUE, sep = "\t", dec = ".", comment.char = "!", fill = TRUE, skip=22)
      cycleLength <- dim(d)[1]
      
      max_resistance <- as.double(input$maxRC)
      
      
      # Get the columns
      first_resistance_column <<- which(names(d) == "Date") + 1 # used to indicate which column is the first one that contains resistacne data
      last_resistance_column <<- which(names(d) == "FBSteps") - 1 # specifies the last column that contains resistance data
      Force_column <- which(names(d)=="LdCel.0")              #Load Cell data - forces
      
      ay <- list(
        tickfont = list(color='red'),
        overlaying = "y",
        side = "right",
        title = "<b>Force</b> (lbs)"
      )
      
      # Create the plot
      numCycles <- dim(d)[1]
      plt <- plot_ly(data = d[1:cycleLength,], type = "scatter", mode = "lines",height=400)
      for(i in first_resistance_column:last_resistance_column){
        plt <- plt %>% add_trace(x = d[1], y = d[1:cycleLength,i], name = names(d)[i])
      }
      
      #d[((cycleLength*(cycle-1))):((cycleLength*cycle)),Force_column][1] <- 0
      if (input$enableForce) {
        plt <- plt %>% add_trace(x= d[1],y=na.omit(d[1:cycleLength,Force_column]),name = "Force", yaxis="y2",line=list(width=1,color='red'))      #Force trace
      }
      
      # Add labels and set range limit
      plt_title <- c("Cycles vs DCR")
      
      plt <- plt %>% layout(xaxis = list(title = "Cycles"),
                            yaxis = list(range = c(0,max_resistance),
                                         title = c("Resistance (Ohms)")),
                            title = plt_title,
                            yaxis2 = ay,
                            showlegend = FALSE,
                            #Legend fundamentally overlaps the yaxis2 label - this is a known issue 
                            legend = list(orientation="s",xanchor="center"),
                            margin = list(b=20,t=100,r=100,l=50)
      )
      
      plt
    })

    #histogram plot for cycling data 
    
    output$CyclesHist <- renderPlotly({
      req(input$Cycles_File)
      
      file <- input$Cycles_File
      d <<- read.delim(file$datapath, header = TRUE, sep = "\t", dec = ".", comment.char = "!", fill = TRUE, skip=22)
      cycleLength <- dim(d)[1]
      
      max_resistance <- as.double(input$maxRC)
      
      set <- t(d[,c(first_resistance_column:last_resistance_column)])
      subset <- round(set[set[,1]<max_resistance],4)
      
      # Get the columns
      first_resistance_column <<- which(names(d) == "Date") + 1 # used to indicate which column is the first one that contains resistacne data
      last_resistance_column <<- which(names(d) == "FBSteps") - 1 # specifies the last column that contains resistance data
      Force_column <- which(names(d)=="LdCel.0")      
      
      fig <- plot_ly(x = subset,type="histogram")
      fig <- fig %>% layout(xaxis=list(title='Resistance (Ohms)',range=c(0,max_resistance)),yaxis=list(title='Frequency'),title='Histogram of All Cycles')
      fig
      
      #layout(title='S11',xaxis=list(title='Frequency (Ghz)'),yaxis=list(title='Magnitude of S11 (dB)'),legend = list(orientation="h",y=-0.3))
    })
    
    
    #stats for cycles data
    
    cycleStatsR <-reactive({
      file <- input$Cycles_File
      data <- tools::file_ext(file$datapath)
     
      d <- read.delim(file$datapath, header = TRUE, sep = "\t", dec = ".", comment.char = "!", fill = TRUE, skip=22)
      max_resistance <- as.double(input$maxRC)
      #max_resistance <- 0.1
      
      # Get the columns
      first_resistance_column <- which(names(d) == "Date") + 1 # used to indicate which column is the first one that contains resistacne data
      last_resistance_column <- which(names(d) == "FBSteps") - 1 # specifies the last column that contains resistance data
      Force_column <- which(names(d)=="LdCel.0")              #Load Cell data - forces
      
      #Calculate the pass rate 
      spec <- input$DCR_spec
      rowN <- dim(d[,first_resistance_column:last_resistance_column])[1]
      colN <- dim(d[,first_resistance_column:last_resistance_column])[2]
      total <- colN*rowN
      failed <- sum(d[,first_resistance_column:last_resistance_column]>spec,na.rm=TRUE)
      pass_rate <- (1-(failed/total))

      #compute avg. and stdev. 
      d <- as.double(unlist(d[,c(first_resistance_column:last_resistance_column)]))
      avgC <- signif(mean(d),3)*10e2
      stdC <- signif(sd(d),3)*10e2
      kurt <- signif(kurtosis(subset),3)
      df_Comp <- c(kurt,pass_rate,avgC,stdC,signif(min(d),3)*10e2,signif(max(d),3)*10e2,4*stdC,5*stdC,6*stdC,7*stdC)
      
      labels = c('Kurtosis #','pass rate','mean', 'std','min','max','4 sigma', '5 sigma', '6 sigma', '7 sigma')
      headers = c('Stats (mOhm)')
      
      stats <- data.frame(metrics=labels, Stats_mOhms = df_Comp)
      stats
    })
    
  
    
    output$cycleStats <- DT::renderDataTable({ 
      dat <- datatable(cycleStatsR(), options = list(paging=FALSE)) %>%
        formatStyle(names(cycleStatsR()),color = 'white', backgroundColor = 'black', fontWeight = 'bold',target='row')
      return(dat)
    })
    
  
    
  
  #################report handler###########################
    output$cycleReport <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "cyclesReport.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "cycleReport.Rmd")
        file.copy("cycleReport.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(cycle_file=input$Cycles_File,maxRC = input$maxRC,enableForce=input$enableForce,cycleStats = cycleStatsR())
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    

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
    
    humTicks <- seq(min,max,((max-min)/10))
    
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
