# Analysis of Reliability Fixture Data
rm(list=ls())
library(ggplot2)
library(reshape2)
library(data.table)
library(qwraps2)

##### USER SELECTED VARIABLES #####

# path to the folder where the data is saved
pth = "E:/Utah Data/Harbor 1/"

# specific data file for analysis
file = "data_resistance_REL_FULL_12-11-2018_14-57-08_through4500.txt"

###################################

#######################
#######################

# SOLID BEGINNING BUT THE CURRENT OUTPUT MAKES LITTLE TO NO SENSE DUE TO PROBABAL CALCLATION ERRORS, AND PLOTS THAT ARE TOO DENSE


######################
######################





# get the data
full_pth <- paste(pth, file, sep = "")
d <- read.delim(file = full_pth, header = TRUE, sep = "\t", skip = 22)

# create summary of min, max, avg and quartiles
smry <- summary(d)
smry <- as.data.frame(smry)
#smry

# this super basic box plot works
bp <- d[,-c(1:3,136:141)]

#save the data in the current format (without extra variables) for use in other applications like Orange3

fwrite(bp, "E:/Utah Data/Harbor 1/orangedata.txt")


#############################



boxplot(data = bp, title = "Resistances of all paths in the Utah cables")

dt <- t(d)
dt <- dt[-c(1:3,136:141),]

mbp <- melt(dt)

boxplot(mbp, value)


bp <- ggplot(mbp, aes(Var1, value))
bp + geom_boxplot() #+ facet_wrap(~Var1, scale="free")


#data <- 



