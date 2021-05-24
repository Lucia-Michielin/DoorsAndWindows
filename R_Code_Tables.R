library(tidyverse)
#Import dataset 
MasterFile <- read_csv('CSV_Files/MasterFile.csv')
#Remove null values in width
MasterFile <- MasterFile[!(is.na(MasterFile$Width) | MasterFile$Width==""), ]
#Remove null values in Height
FullMeasures <- MasterFile[!(is.na(MasterFile$Height) | MasterFile$Height==""), ]
#Calculate W-H Ratio and Area
FullMeasures<-within(FullMeasures, Ratio <- (FullMeasures$Width)/(FullMeasures$Height))
FullMeasures<- within(FullMeasures, Surface <- ((FullMeasures$Width/100)* (FullMeasures$Height/100)))
# Calculating height and width in feet
FullMeasures<-within(Opening_Ratio_Surface, WidthPedes <- Opening_Ratio_Surface$`Width (cm)`/29.6)
FullMeasures<-within(Opening_Pedes, HeightPedes <- Opening_Pedes$`Height (cm)`/29.6)
#Question 1: Proportion of Openings. which is the level of correlation between width and height of doors and windows

#correlation coefficent
cor(y=FullMeasures$Width, x=FullMeasures$Height)
#extract doors
Doors <-subset(FullMeasures, Type=="DOORWAY")
#extract windows
Windows <-subset(FullMeasures, Type=="WINDOW")
#ONLY DOORWAY
cor(y=Doors$Width, x=Doors$Height)
#ONLY WINDOWS
cor(y=Windows$Width, x=Windows$Height)



#Table6.1
