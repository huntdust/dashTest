library(shiny)
library(reticulate)


amb <- read.csv(file="\\\\pilly\\Advanced_Signal_Delivery\\Personal_Folders\\Dustin\\tempData_ambient.txt",sep='\t', stringsAsFactors=FALSE,encoding = 'UTF-8',quote="")
rel <- read.csv(file="\\\\pilly\\Advanced_Signal_Delivery\\Personal_Folders\\Dustin\\tempData_reliability.txt",sep='\t', stringsAsFactors=FALSE,encoding = 'UTF-8',quote="")
gry <- read.csv(file="\\\\pilly\\Advanced_Signal_Delivery\\Personal_Folders\\Dustin\\tempData_gryffindor.txt",sep='\t', stringsAsFactors=FALSE,encoding = 'UTF-8',quote="")
HPa <- read.csv(file="\\\\pilly\\Advanced_Signal_Delivery\\Personal_Folders\\Dustin\\tempData_HPambient.txt",sep='\t', stringsAsFactors=FALSE,encoding = 'UTF-8',quote="")
Le1 <- read.csv(file="\\\\pilly\\Advanced_Signal_Delivery\\Personal_Folders\\Dustin\\tempData_Leak1.txt",sep='\t', stringsAsFactors=FALSE,encoding = 'UTF-8',quote="")
Le2 <- read.csv(file="\\\\pilly\\Advanced_Signal_Delivery\\Personal_Folders\\Dustin\\tempData_Leak2.txt",sep='\t', stringsAsFactors=FALSE,encoding = 'UTF-8',quote="")


test <- ggplot(amb, aes(y=Temperature..C., x=Time)) +
  geom_point()

test2 <- ggplot(rel, aes(y=Temperature..C., x=Time)) +
  geom_point()

# amb$Time <- unlist(amb$Time)
# amb$Temperature..C. <- unlist(amb$Temperature..C.)
# amb$Humidity...RH. <- unlist(amb$Humidity...RH.)
# 
# ambTime <- amb[c(1)]
# ambTemp <- amb[c(2)]
# ambHum  <- amb[c(3)]





