library(shiny)
library(reticulate)


amb <- read.csv(file="\\\\pilly\\Advanced_Signal_Delivery\\Personal_Folders\\Dustin\\tempData_ambient.txt",sep='\t')
rel <- read.csv(file="\\\\pilly\\Advanced_Signal_Delivery\\Personal_Folders\\Dustin\\tempData_reliability.txt",sep='\t')
gry <- read.csv(file="\\\\pilly\\Advanced_Signal_Delivery\\Personal_Folders\\Dustin\\tempData_gryffindor.txt",sep='\t')
HPa <- read.csv(file="\\\\pilly\\Advanced_Signal_Delivery\\Personal_Folders\\Dustin\\tempData_HPambient.txt",sep='\t')
Le1 <- read.csv(file="\\\\pilly\\Advanced_Signal_Delivery\\Personal_Folders\\Dustin\\tempData_Leak1.txt",sep='\t')
Le2 <- read.csv(file="\\\\pilly\\Advanced_Signal_Delivery\\Personal_Folders\\Dustin\\tempData_Leak2.txt",sep='\t')


#humgraph <- ggplot(data = amb, aes(x=Time,y = Humidity...RH.)) +geom_point()
#humgraph <- ggplotly(humgraph)
#humgraph


#test <- ggplot(amb, aes(y=Humidity...RH., x=Time)) +
#  geom_point()

#test2 <- ggplot(rel, aes(y=Humidity...RH., x=Time)) +
#  geom_point()





