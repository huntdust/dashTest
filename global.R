wq_plotly <- function(path){

  #############GET TAB COUNT###########################
  d <- read.delim(path, header = TRUE, sep = "\t", dec = ".", comment.char = "!", fill = TRUE)
  Force_column <- which(names(d)=="LdCel.0")
  cnt<-0
  
  
  for (i in 1:dim(d[Force_column])[1]-1) {
    if(isTRUE(((d[i,Force_column]-d[i+1,Force_column])>2))) {
      cnt<-cnt+1
    }}
  ######################################################
  ################CREATE PLOTS##########################
  d <- read.delim(path, header = TRUE, sep = "\t", dec = ".", comment.char = "!", fill = TRUE)
  max_resistance <- as.double(0.1)
  # Get the columns
  first_resistance_column <- which(names(d) == "Date") + 1 # used to indicate which column is the first one that contains resistacne data
  last_resistance_coluimn <- which(names(d) == "FBSteps") - 1 # specifies the last column that contains resistance data
  Force_column <- which(names(d)=="LdCel.0")              #Load Cell data - forces
  loopCount <- dim(d)[1]/cnt
  
  ay <- list(
    tickfont = list(color='red'),
    overlaying = "y",
    side = "right",
    title = "<b>Force</b> plot"
    #zerolinewidth = 50
  )
  
  # Create the plots
  #Need to initialize empty array with length loopCount for each plot 
  
  vec <- vector(mode='list',cnt)
  
  
  for (p in 1:cnt){
    vec[[p]] <- local({
      plt <- plot_ly(data = d, type = "scatter", mode = "lines")
      for(i in first_resistance_column:last_resistance_coluimn){
        plt <- plt %>% add_trace(x = ~External.Z.Delayed, y = d[,i], name = names(d)[i])
      }
      plt <- plt %>% add_trace(x= ~External.Z.Delayed,y=d[,Force_column],name = "Force", yaxis="y2",line=list(width=5,color='red'))
      # Add labels and set range limit
      plt_title <- c("Total Electrical Compliance")
      
      plt <- plt %>% layout(xaxis = list(title = "Distance (mils)"),
                            yaxis = list(range = c(0,max_resistance),
                                         title = c("Resistance (Ohms)")),
                            title = plt_title,
                            yaxis2 = ay,
                            showlegend = FALSE,
                            margin = list(b=20,t=10,r=50,l=50))
      
    })
  }
}