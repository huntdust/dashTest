library(ggplot2)
library(plotly)

#colours <- list(~class, ~drv, ~fl)
#for (colour in colours) {
#  print(ggplot(mpg, aes_(~ displ, ~ hwy, colour = colour)) +
#          geom_point())
#}



#boxplot(weight ~ group, data = PlantGrowth, main = "PlantGrowth data", notch = TRUE, varwidth = TRUE)



x <- c(1:100)
random_y <- rnorm(100,mean=0)
data <- data.frame(x,random_y)
samplePlot <- plot_ly(data, x = ~x, y =~random_y, type= 'scatter', mode='lines')

#export(samplePlot,file = "samplePlot.png")


#samplePlot
#samplePlot <- ggplot(aes) + geom_point()



