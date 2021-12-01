library(shiny)
library(reticulate)


amb <- read.csv(file="/mnt/Dustin/tempData_ambient.txt",sep='\t', stringsAsFactors=FALSE,encoding = 'UTF-8')
rel <- read.csv(file="/mnt/Dustin/tempData_reliability.txt",sep='\t', stringsAsFactors=FALSE,encoding = 'UTF-8')
gry <- read.csv(file="/mnt/Dustin/tempData_gryffindor.txt",sep='\t', stringsAsFactors=FALSE,encoding = 'UTF-8')
HPa <- read.csv(file="/mnt/Dustin/tempData_HPambient.txt",sep='\t', stringsAsFactors=FALSE,encoding = 'UTF-8')
Le1 <- read.csv(file="/mnt/Dustin/tempData_Leak1.txt",sep='\t', stringsAsFactors=FALSE,encoding = 'UTF-8')
Le2 <- read.csv(file="/mnt/Dustin/tempData_Leak2.txt",sep='\t', stringsAsFactors=FALSE,encoding = 'UTF-8')


test <- ggplot(amb, aes(y=Temperature..C., x=Time)) +
  geom_point()

test2 <- ggplot(rel, aes(y=Temperature..C., x=Time)) +
  geom_point()

amb$Time <- unlist(amb$Time)
amb$Temperature..C. <- unlist(amb$Temperature..C.)
amb$Humidity...RH. <- unlist(amb$Humidity...RH.)

ambTime <- amb[c(1)]
ambTemp <- amb[c(2)]
ambHum  <- amb[c(3)]





