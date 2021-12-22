library(plotly)


#pth <- "C:/users/huntdust/desktop/Sample_TEC/old_format_1.txt"
pth <- "C:/users/huntdust/desktop/Sample_TEC/data_resistance_REL_FULL_12-17-2021_16-40-04.txt"
  
d <<- read.delim(pth, header = TRUE, sep = "\t", dec = ".", comment.char = "!", fill = TRUE,skip=22)
max_resistance <- 0.1
first_resistance_column <<- which(names(d) == "Date") + 1 # used to indicate which column is the first one that contains resistacne data
last_resistance_column <<- which(names(d) == "FBSteps") - 1 # specifies the last column that contains resistance data
Force_column <- which(names(d)=="LdCel.0")              #Load Cell data - forces
disp_col <- which(names(d)=="External.Z.Delayed")
cycleCount <- 37

# temp <- d[disp_col]
# d[d>max_resistance*2] <- NA
# d[disp_col] <- temp
# 
# # create steps and plot all histograms
# steps <- list()
# aval <- list()
# 
# 
# x <- seq(0,10, length.out = 1000)
# 
# for(step in 1:cycleCount){
#   aval[[step]] <-list(visible = FALSE,
#                       name = paste0('v = ', step),
#                       x=x,
#                       y=sin(step*x))
# }
# 
# fig <- plot_ly(nbinsx = 30)
# fig <- fig %>%layout(xaxis=list(range=c(0,max_resistance)))
# for (i in 1:(cycleCount)) {
#   fig<- fig %>% add_histogram(x=unlist(d[i,first_resistance_column:last_resistance_column],), visible = aval[i][[1]]$visible, type = 'histogram',
#   xaxis=list(range=c(0,max_resistance)),yaxis=list(c(0,60)),bingroup=1,nbinsx = 30)
#   
#   step <- list(args = list('visible', rep(FALSE, length(aval))), method = 'restyle',label = d[i,disp_col])
#   step$args[[2]][i] = TRUE  
#   steps[[i]] = step 
# }  
# 
# # add slider control to plot
# fig <- fig %>%layout(title = 'TEC - Histogram of Resistance vs. Displacement', sliders = list(list(active = 1,
#                              currentvalue = list(prefix = "Displacement (mils): "),
#                              steps = steps)),xaxis=list(title='Resistance (Ohms)',range=c(0,max_resistance)),yaxis=list(title='Frequency',range=c(0,60)))
# fig
# 


spec <- 0.02
rowN <- dim(d[,first_resistance_column:last_resistance_column])[1]
colN <- dim(d[,first_resistance_column:last_resistance_column])[2]
total <- colN
failed <- sum(d[cycleCount,first_resistance_column:last_resistance_column]>spec,na.rm=TRUE)
pass_rate <- (1-(failed/total))
print(pass_rate)




# 
# pth <- "C:/users/huntdust/desktop/Sample_TEC/cycles_sample.txt"
# d <<- read.delim(pth, header = TRUE, sep = "\t", dec = ".", comment.char = "!", fill = TRUE,skip=22)
# 
