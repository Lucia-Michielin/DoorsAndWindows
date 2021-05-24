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

#Question 1: Are the proportions and sizes of doors and window influenced by the chronology of the features 
ggplot(FullMeasures, aes(x=Surface, fill=Period))+ geom_density(alpha=0.5) +theme_bw()
ggplot(FullMeasures, aes(x=Ratio, fill=Period))+ geom_density(alpha=0.5) +theme_bw()

#Very right skewed
#check if the log willfix it 
ggplot(FullMeasures, aes(x=Surface, fill=Period))+ geom_density(alpha=0.5) +theme_bw()+facet_wrap(~Type,scales="free_x")+ coord_trans(x="log")

ggplot(FullMeasures, aes(x=Ratio, fill=Period))+ geom_density(alpha=0.5) +theme_bw()+ coord_trans(x="log")

#Perform an Anova with Tukey correction on the log
FullMeasures<-within(FullMeasures, SurfaceLog <- log(FullMeasures$Surface))

FullMeasures<-within(FullMeasures, RatioLog <- log(FullMeasures$Ratio))
#On Surface 
anovaResult1 <- aov(FullMeasures$Surface ~ FullMeasures$Period)
summary(anovaResult1)
#Now use Tukey multiple pairwise-comparisons
TukeyTest1 <- TukeyHSD(anovaResult1)
TukeyHSD(anovaResult1)
print(TukeyTest1)
plot(TukeyTest1, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#On the log 
anovaResult2 <- aov(FullMeasures$SurfaceLog ~ FullMeasures$Period)
summary(anovaResult1)
#Now use Tukey multiple pairwise-comparisons
TukeyTest2 <- TukeyHSD(anovaResult2)
TukeyHSD(anovaResult2)
print(TukeyTest2)
plot(TukeyTest2, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#Kruskal test 
kruskal.test(Surface ~ Period, data = FullMeasures)
pairwise.wilcox.test(FullMeasures$Surface, FullMeasures$Period,
                     p.adjust.method = "BH")

#on normal Surface windows
anovaResult3 <- aov(WindowsC$Surface ~ Windows$Period)
summary(anovaResult3)
#Now use Tukey multiple pairwise-comparisons
TukeyTest3 <- TukeyHSD(anovaResult3)
TukeyHSD(anovaResult3)
print(TukeyTest3)
plot(TukeyTest3, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#On the log 
anovaResult4 <- aov(Windows$SurfaceLog ~ Windows$Period)
summary(anovaResult4)
#Now use Tukey multiple pairwise-comparisons
TukeyTest4 <- TukeyHSD(anovaResult4)
TukeyHSD(anovaResult4)
print(TukeyTest4)
plot(TukeyTest4, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#Kurskal test 
kruskal.test(Surface ~ Period, data = Windows)
prova<- pairwise.wilcox.test(Windows$Surface, Windows$Period,
                     p.adjust.method = "BH")
print(prova)
#on normal Surface doors
anovaResult5 <- aov(Doors$Surface ~ Doors$Period)
summary(anovaResult5)
#Now use Tukey multiple pairwise-comparisons
TukeyTest5 <- TukeyHSD(anovaResult5)
TukeyHSD(anovaResult5)
print(TukeyTest5)
plot(TukeyTest5, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#On the log 
anovaResult6 <- aov(Doors$SurfaceLog ~ Doors$Period)
summary(anovaResult6)
#Now use Tukey multiple pairwise-comparisons
TukeyTest6 <- TukeyHSD(anovaResult6)
TukeyHSD(anovaResult6)
print(TukeyTest6)
plot(TukeyTest4, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#Kurskal test 
kruskal.test(Surface ~ Period, data = Doors)
pairwise.wilcox.test(Doors$Surface, Doors$Period,
                     p.adjust.method = "BH")


#TimesWindows
EarlyEWind <- subset(Windows,Period=="EARLY EMPIRE")
MiddleEWind<- subset(Windows,Period=="MIDDLE EMPIRE")
LateEWind<- subset(Windows,Period=="LATE EMPIRE")

mean(EarlyEWind$Surface)
mean(MiddleEWind$Surface)
mean(LateEWind$Surface)

#Timestot
EarlyE <- subset(FullMeasures,Period=="EARLY EMPIRE")
MiddleE<- subset(FullMeasures,Period=="MIDDLE EMPIRE")
LateE<- subset(FullMeasures,Period=="LATE EMPIRE")

mean(EarlyE$Surface)
mean(MiddleE$Surface)
mean(LateE$Surface)



#Timestot
EarlyEDoors <- subset(Doors,Period=="EARLY EMPIRE")
MiddleEDoors<- subset(Doors,Period=="MIDDLE EMPIRE")
LateEDoors<- subset(Doors,Period=="LATE EMPIRE")

mean(EarlyEDoors$Surface)
mean(MiddleEDoors$Surface)
mean(LateEDoors$Surface)


#question 2: Are the proportion and sizes of doors and windows changing Depending on the sites they are located in 
#Check the distribution
MasterFileAreas <- mutate(MasterFile, Area = ifelse(Town=="BRESCIA" |Town=="VICENZA"|Town=="VERONA"|Town=="RIMINI", "NORD", ifelse(Town=="ROME" |Town=="OSTIA", "CENTRE","SOUTH")))
MasterFileAreas <- subset(MasterFileAreas, !(PeculiarType == "WOLF MOUTH WINDOW" & Town == "VICENZA"))

ggplot(MasterFileAreas,aes(x=WidthLog, fill=Area))+ 
  geom_density(alpha=0.5)+theme_bw()
+facet_wrap(~Area)

+facet_grid(Area~Type,scales="free_y")

ggplot(MasterFileAreas,aes(x=WidthLog, fill=Town))+ 
  geom_histogram()+theme_bw()+facet_wrap(~Area)


library("dplyr")
MasterFileAreas %>% 
  group_by(Area, Type) %>% 
  summarize(count=n())
MasterFileAreas %>% 
  group_by(Town) %>% 
  summarize(mean(Width), sd(Width))
MasterFileAreas %>% 
  group_by(Area) %>% 
  summarize(mean(Width), sd(Width))
MasterFileAreas %>% 
  group_by(Area) %>% 
  summarize(mean(WidthLog), sd(WidthLog))


MasterFile<-within(MasterFile, WidthLog <- log(MasterFile$Width))


#On Width 
anovaResult7 <- aov(MasterFileAreas$Width ~ MasterFileAreas$Area)
summary(anovaResult7)
#Now use Tukey multiple pairwise-comparisons
TukeyTest7 <- TukeyHSD(anovaResult7)
TukeyHSD(anovaResult7)
print(TukeyTest7)
plot(TukeyTest7, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#On the log 
anovaResult8 <- aov(MasterFileAreas$WidthLog ~ MasterFileAreas$Area)
summary(anovaResult8)
#Now use Tukey multiple pairwise-comparisons
TukeyTest8<- TukeyHSD(anovaResult8)
TukeyHSD(anovaResult8)
print(TukeyTest8)
plot(TukeyTest8, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#Kruskal test 
kruskal.test(Width ~ Area, data = MasterFileAreas)
pairwise.wilcox.test(MasterFileAreas$Width,MasterFileAreas$Area,
                     p.adjust.method = "BH")


#On Width Sites
anovaResult9 <- aov(MasterFileAreas$Width ~ MasterFileAreas$Town)
summary(anovaResult9)
#Now use Tukey multiple pairwise-comparisons
TukeyTest9 <- TukeyHSD(anovaResult9)
TukeyHSD(anovaResult9)
print(TukeyTest9)
plot(TukeyTest9, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#On the log 
anovaResult10 <- aov(MasterFileAreas$WidthLog ~ MasterFileAreas$Town)
summary(anovaResult10)
#Now use Tukey multiple pairwise-comparisons
TukeyTest10<- TukeyHSD(anovaResult10)
TukeyHSD(anovaResult10)
print(TukeyTest10)
plot(TukeyTest10, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#Kruskal test 
kruskal.test(Width ~ Town, data = MasterFileAreas)
pairwise.wilcox.test(MasterFileAreas$Width,MasterFileAreas$Town,
                     p.adjust.method = "BH")

summarise <- MasterFileAreas %>% 
  group_by(Town) %>% 
  summarize(round(mean(Width),2), sd(Width), count=n())




#question 3: Are the proportion and sizes of doors and windows changing across inside and outside facing structures
#T-tests across W-H Ratio
#Total
OutsideOpening<-subset(FullMeasures, Facing =="OUTSIDE")
InsideOpening<-subset(FullMeasures, Facing =="INSIDE")
OutsideWindows<-subset(Windows, Facing =="OUTSIDE")
InsideWindows<-subset(Windows, Facing =="INSIDE")
OutsideDoors<-subset(Doors, Facing =="OUTSIDE")
InsideDoors<-subset(Doors, Facing =="INSIDE")

#On the full sample to use the width 
DoorsTot<-subset(MasterFile, Type =="DOORWAY")
WindowsTot<-subset(MasterFile, Type =="WINDOW")


OutsideOpeningTot<-subset(MasterFile, Facing =="OUTSIDE")
InsideOpeningTot<-subset(MasterFile, Facing =="INSIDE")
OutsideWindowsTot<-subset(WindowsTot, Facing =="OUTSIDE")
InsideWindowsTot<-subset(WindowsTot, Facing =="INSIDE")
OutsideDoorsTot<-subset(DoorsTot, Facing =="OUTSIDE")
InsideDoorsTot<-subset(DoorsTot, Facing =="INSIDE")

library(tidyverse)
ggplot(FullMeasures, aes(x=Surface, fill=Facing))+ geom_density(alpha=0.5) +theme_bw()
ggplot(FullMeasures, aes(x=SurfaceLog, fill=Facing))+ geom_density(alpha=0.5) +theme_bw()+facet_wrap(~Type)
ggplot(FullMeasures, aes(x=Ratio, fill=Facing))+ geom_density(alpha=0.5) +theme_bw()

#Full Sample
t.test(OutsideOpening$Ratio,InsideOpening$Ratio)
t.test(OutsideOpening$RatioLog,InsideOpening$RatioLog)

t.test(OutsideOpening$Surface,InsideOpening$Surface)
t.test(OutsideOpening$SurfaceLog,InsideOpening$SurfaceLog)

t.test(OutsideOpeningTot$Width,InsideOpeningTot$Width)
t.test(OutsideOpeningTot$WidthLog,InsideOpeningTot$WidthLog)

ks.test(OutsideOpening$Ratio,InsideOpening$Ratio)
ks.test(OutsideOpening$Surface,InsideOpening$Surface)
ks.test(OutsideOpeningTot$Width,InsideOpeningTot$Width)

#Windows
t.test(OutsideWindows$Ratio,InsideWindows$Ratio)
t.test(OutsideWindows$RatioLog,InsideWindows$RatioLog)

t.test(OutsideWindows$Surface,InsideWindows$Surface)
t.test(OutsideWindows$SurfaceLog,InsideWindows$SurfaceLog)

t.test(OutsideWindowsTot$Width,InsideWindowsTot$Width)
t.test(OutsideWindowsTot$WidthLog,InsideWindowsTot$WidthLog)

ks.test(OutsideWindows$Ratio,InsideWindows$Ratio)
ks.test(OutsideWindows$Surface,InsideWindows$Surface)
ks.test(OutsideWindowsTot$Width,InsideWindowsTot$Width)

#Doorways
t.test(OutsideDoors$Ratio,InsideDoors$Ratio)
t.test(OutsideDoors$RatioLog,InsideDoors$RatioLog)

t.test(OutsideDoors$Surface,InsideDoors$Surface)
t.test(OutsideDoors$SurfaceLog,InsideDoors$SurfaceLog)

t.test(OutsideDoorsTot$Width,InsideDoorsTot$Width)
t.test(OutsideDoorsTot$WidthLog,InsideDoorsTot$WidthLog)

ks.test(OutsideDoors$Ratio,InsideDoors$Ratio)
ks.test(OutsideDoors$Surface,InsideDoors$Surface)
ks.test(OutsideDoorsTot$Width,InsideDoorsTot$Width)


#Correction on Ratio T-test
pValue<- c(0.6026,0.8234,0.0003243,0.8954,0.0006783,7.63E-07,6.32E-05,3.99E-05,0.0001987,0.00774,0.00118,0.001548,0.6101,0.8713,0.001389,0.4686,0.6952,0.4266,0.2236,0.1168,0.06253,0.2538,0.001203,0.0005353,3.07E-07,1.09E-07,1.92E-07)
p.adjust(pValue, method = "bonferroni")

pValue1<- c(0.8954,0.0006783,7.63E-07,6.32E-05,3.99E-05,0.0001987,0.6101,0.8713,0.001389,0.4686,0.6952,0.4266,0.2538,0.001203,0.0005353,3.07E-07,1.09E-07,1.92E-07)
p.adjust(pValue1, method = "bonferroni")


summarise2 <- MasterFileAreas %>% 
  group_by(Town,Facing, Type ) %>% 
  summarize(mean(Width), count=n())


#Openings and Room typologies
#Question: are the typologies of the Rooms interconnected by the openings influencing the proportion and sizes of doors and windows?

#Generate a dataset that will record all recurrencies of room types
Singlematches <- subset(FullMeasuresForRoomType, RoomComparison =="PUBLIC"|RoomComparison =="PRIVATE"|RoomComparison =="SERVICE"|RoomComparison =="OPEN SPACES")

Singlematches <- mutate(Singlematches, RoomGeneral = ifelse(RoomComparison=="PUBLIC", "PUBLIC ROOM",ifelse(RoomComparison=="PRIVATE", "PRIVATE ROOM",ifelse(RoomComparison=="SERVICE", "SERVICE ROOM","OPEN SPACES"))))

AllOtherCombo <- subset(FullMeasuresForRoomType, RoomComparison =="PUBLIC - OPEN SPACES"|RoomComparison =="PRIVATE - PUBLIC"|RoomComparison =="SERVICE - PUBLIC"|RoomComparison =="PRIVATE - OPEN AREA"|RoomComparison =="SERVICE - OPEN AREA"|RoomComparison =="SERVICE - PRIVATE")

PuOS<- subset(AllOtherCombo, RoomComparison =="PUBLIC - OPEN SPACES")
PuOS1 <- cbind(PuOS, RoomGeneral = "PUBLIC ROOM")
PuOS2 <- cbind(PuOS, RoomGeneral = "OPEN SPACES")

PrPu<- subset(AllOtherCombo, RoomComparison =="PRIVATE - PUBLIC")
PrPu1 <- cbind(PrPu, RoomGeneral = "PRIVATE ROOM")
PrPu2 <- cbind(PrPu, RoomGeneral = "PUBLIC ROOM")

SePu<- subset(AllOtherCombo, RoomComparison =="SERVICE - PUBLIC")
SePu1 <- cbind(SePu, RoomGeneral = "SERVICE ROOM")
SePu2 <- cbind(SePu, RoomGeneral = "PUBLIC ROOM")

PrOS<- subset(AllOtherCombo, RoomComparison =="PRIVATE - OPEN AREA")
PrOS1 <- cbind(PrOS, RoomGeneral = "PRIVATE ROOM")
PrOS2 <- cbind(PrOS, RoomGeneral = "OPEN SPACES")

SeOS<- subset(AllOtherCombo, RoomComparison =="SERVICE - OPEN AREA")
SeOS1 <- cbind(SeOS, RoomGeneral = "SERVICE ROOM")
SeOS2 <- cbind(SeOS, RoomGeneral = "OPEN SPACES")

SePr<- subset(AllOtherCombo, RoomComparison =="SERVICE - PRIVATE")
SePr1 <- cbind(SePr, RoomGeneral = "SERVICE ROOM")
SePr2 <- cbind(SePr, RoomGeneral = "PRIVATE ROOM")

TotRoom <- bind_rows(Singlematches,SePr1,SePr2,SeOS2,SeOS1,PrOS2,PrOS1,SePu2,SePu1,PrPu2,PrPu1,PuOS2,PuOS1)

#Visualise
ggplot(TotRoom,aes(x=SurfaceLog, fill=RoomGeneral))+ 
  geom_density(alpha=0.5)+theme_bw()+facet_grid(~Type,scales="free_y", ncol=1)

#On Surface Area 
anovaResult11 <- aov(TotRoom$Surface ~ TotRoom$RoomGeneral)
summary(anovaResult11)
#Now use Tukey multiple pairwise-comparisons
TukeyTest11 <- TukeyHSD(anovaResult11)
TukeyHSD(anovaResult11)
Tuckey11 <- print(TukeyTest11)
plot(TukeyTest11, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#On the log 
anovaResult12 <- aov(TotRoom$SurfaceLog ~ TotRoom$RoomGeneral)
summary(anovaResult12)
#Now use Tukey multiple pairwise-comparisons
TukeyTest12<- TukeyHSD(anovaResult12)
TukeyHSD(anovaResult12)
print(TukeyTest12)
plot(TukeyTest12, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#Kruskal test 
kruskal.test(Surface ~ RoomGeneral, data = TotRoom)
pairwise.wilcox.test(TotRoom$Surface,TotRoom$RoomGeneral,
                     p.adjust.method = "BH")

summarise4 <- TotRoom %>% 
  group_by(RoomGeneral) %>% 
  summarize(mean(Surface), count=n())

#Extract windows and Doors
WindowsRoom <- subset(TotRoom, Type=="WINDOW")
DoorsRoom <- subset(TotRoom, Type=="DOORWAY")

#On Surface Area windows
anovaResult13 <- aov(WindowsRoom$Surface ~ WindowsRoom$RoomGeneral)
summary(anovaResult13)
#Now use Tukey multiple pairwise-comparisons
TukeyTest13 <- TukeyHSD(anovaResult13)
TukeyHSD(anovaResult13)
Tuckey13 <- print(TukeyTest13)
plot(TukeyTest13, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#On the log 
anovaResult14 <- aov(WindowsRoom$SurfaceLog ~ WindowsRoom$RoomGeneral)
summary(anovaResult14)
#Now use Tukey multiple pairwise-comparisons
TukeyTest14<- TukeyHSD(anovaResult14)
TukeyHSD(anovaResult14)
print(TukeyTest14)
plot(TukeyTest14, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#Kruskal test 
kruskal.test(Surface ~ RoomGeneral, data = WindowsRoom)
pairwise.wilcox.test(WindowsRoom$Surface,WindowsRoom$RoomGeneral,
                     p.adjust.method = "BH")#Benjamini & Hochberg correction

summarise5 <- TotRoom %>% 
  group_by(RoomGeneral) %>% 
  summarize(MSurf=mean(Surface), count=n())

summarise5 <- arrange(summarise5, MSurf)

#On Surface Area windows
anovaResult15 <- aov(DoorsRoom$Surface ~ DoorsRoom$RoomGeneral)
summary(anovaResult15)
#Now use Tukey multiple pairwise-comparisons
TukeyTest15 <- TukeyHSD(anovaResult15)
TukeyHSD(anovaResult15)
Tuckey15 <- print(TukeyTest15)
plot(TukeyTest15, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#On the log 
anovaResult16 <- aov(DoorsRoom$SurfaceLog ~ DoorsRoom$RoomGeneral)
summary(anovaResult16)
#Now use Tukey multiple pairwise-comparisons
TukeyTest16<- TukeyHSD(anovaResult16)
TukeyHSD(anovaResult16)
print(TukeyTest16)
plot(TukeyTest16, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#Kruskal test 
kruskal.test(Surface ~ RoomGeneral, data = DoorsRoom)
pairwise.wilcox.test(DoorsRoom$Surface,DoorsRoom$RoomGeneral,
                     p.adjust.method = "BH")

#Analysis of the atrium and perystile doors 
AtriumPeri <- subset(FullMeasures, Type=="DOORWAY" & BiggerAreaRoom == "ATRIUM" |SmallerAreaRoom == "ATRIUM"|BiggerAreaRoom == "PERISTYLE" |SmallerAreaRoom == "PERISTYLE")
Peri <- subset(AtriumPeri,BiggerAreaRoom == "PERISTYLE" |SmallerAreaRoom == "PERISTYLE") 
#Visualise
ggplot(Peri ,aes(x=Surface))+ 
  geom_density(alpha=0.5)+theme_bw()

Atrium <- subset(AtriumPeri,BiggerAreaRoom == "ATRIUM" |SmallerAreaRoom == "ATRIUM") 
#Visualise
ggplot(Atrium ,aes(x=Surface))+ 
  geom_density(alpha=0.5)+theme_bw()

#TEsts on surface
t.test(Peri$Surface, Atrium$Surface)
t.test(Peri$SurfaceLog, Atrium$SurfaceLog)
ks.test(Peri$Surface, Atrium$Surface)
pValue<- c(0.6043,0.453,0.6775)
p.adjust(pValue, method = "bonferroni")

#Tests on Height
#Add log of Height
AtriumPeri<-within(AtriumPeri, HeightLog <- log(AtriumPeri$Height))

t.test(Peri$Height, Atrium$Height)
t.test(Peri$HeightLog, Atrium$HeightLog)
ks.test(Peri$Height, Atrium$Height)
pValue<- c(0.06068,0.1318,0.2405)
p.adjust(pValue, method = "bonferroni")

#Housetype and sizes 
#CHeck the distribution
ggplot(FullMeasures,aes(x=Surface, fill=HouseType))+ 
geom_density(alpha=0.5)+theme_bw()+facet_wrap(~Type,scales="free_y", ncol=1)

FullMeasuresHouse <- mutate(FullMeasures, Housetot = ifelse(HouseType=="DOMUS", "DOMUS",ifelse(HouseType=="HOTEL", "HOTEL",ifelse(HouseType=="SHOP APARTMENT", "SHOP APARTMENT",ifelse(HouseType=="MEDIANUM APARTMENT", "MEDIANUM APARTMENT","APARTMENT")))))

#On Surface full sample 
anovaResult17 <- aov(FullMeasuresHouse$Surface ~ FullMeasuresHouse$Housetot)
summary(anovaResult17)
#Now use Tukey multiple pairwise-comparisons
TukeyTest17 <- TukeyHSD(anovaResult17)
TukeyHSD(anovaResult17)
Tuckey17 <- print(TukeyTest17)
plot(TukeyTest17, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#On the log 
anovaResult18 <- aov(FullMeasuresHouse$SurfaceLog ~ FullMeasuresHouse$Housetot)
summary(anovaResult18)
#Now use Tukey multiple pairwise-comparisons
TukeyTest18<- TukeyHSD(anovaResult18)
TukeyHSD(anovaResult18)
print(TukeyTest18)
plot(TukeyTest18, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#Kruskal test 
kruskal.test(Surface ~ Housetot, data = FullMeasuresHouse)
pairwise.wilcox.test(FullMeasuresHouse$Surface,FullMeasuresHouse$Housetot,
                     p.adjust.method = "BH")


summarise6 <- FullMeasuresHouse%>% 
  group_by(Type,Housetot) %>% 
  summarize(MSurf=mean(Surface), count=n())
summarise6 <- arrange(summarise6, Type, MSurf)

DoorsHouse<- subset(FullMeasuresHouse, Type=="DOORWAY")
WindowHouse<- subset(FullMeasuresHouse, Type=="WINDOW")

#On SURFACE window
anovaResult19 <- aov(WindowHouse$Surface ~ WindowHouse$Housetot)
summary(anovaResult19)
#Now use Tukey multiple pairwise-comparisons
TukeyTest19<- TukeyHSD(anovaResult19)
TukeyHSD(anovaResult19)
Tuckey19 <- print(TukeyTest19)
plot(TukeyTest19, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#On the log window
anovaResult20 <- aov(WindowHouse$SurfaceLog ~ WindowHouse$Housetot)
summary(anovaResult20)
#Now use Tukey multiple pairwise-comparisons
TukeyTest20<- TukeyHSD(anovaResult20)
TukeyHSD(anovaResult20)
print(TukeyTest20)
plot(TukeyTest20, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#Kruskal test  window
kruskal.test(Surface ~ Housetot, data = WindowHouse)
pairwise.wilcox.test(WindowHouse$Surface,WindowHouse$Housetot,
                     p.adjust.method = "BH")

#On SURFACE DOORS
anovaResult21 <- aov(DoorsHouse$Surface ~ DoorsHouse$Housetot)
summary(anovaResult21)
#Now use Tukey multiple pairwise-comparisons
TukeyTest21<- TukeyHSD(anovaResult21)
TukeyHSD(anovaResult21)
Tuckey21 <- print(TukeyTest21)
plot(TukeyTest21, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#On the log doors
anovaResult22 <- aov(DoorsHouse$SurfaceLog ~ DoorsHouse$Housetot)
summary(anovaResult22)
#Now use Tukey multiple pairwise-comparisons
TukeyTest22<- TukeyHSD(anovaResult22)
TukeyHSD(anovaResult22)
print(TukeyTest22)
plot(TukeyTest22, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#Kruskal test  window
kruskal.test(Surface ~ Housetot, data = DoorsHouse)
pairwise.wilcox.test(DoorsHouse$Surface,DoorsHouse$Housetot,
                     p.adjust.method = "BH")


#Relationship between size of doors and type of door
#CHeck the distribution
ggplot(DoorsType,aes(x=SurfaceLog, fill=LintelType))+ 
  geom_density(alpha=0.5)+theme_bw()

#On SURFACE 
anovaResult23 <- aov(DoorsType$Surface ~ DoorsType$LintelType)
summary(anovaResult23)
#Now use Tukey multiple pairwise-comparisons
TukeyTest23<- TukeyHSD(anovaResult23)
TukeyHSD(anovaResult23)
Tuckey23 <- print(TukeyTest23)
plot(TukeyTest23, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#On the log doors
anovaResult24 <- aov(DoorsType$SurfaceLog ~ DoorsType$LintelType)
summary(anovaResult24)
#Now use Tukey multiple pairwise-comparisons
TukeyTest24<- TukeyHSD(anovaResult24)
TukeyHSD(anovaResult24)
print(TukeyTest24)
plot(TukeyTest24, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#Kruskal test 
kruskal.test(Surface ~ LintelType, data = DoorsType)
pairwise.wilcox.test(DoorsType$Surface,DoorsType$LintelType,
                     p.adjust.method = "BH")


#Checking the connection betweewn sizes and type of windows
#On SURFACE DOORS

#Check the distribution
ggplot(WindowsF,aes(x=SurfaceLog, fill=WindTypeTot))+ 
  geom_density(alpha=0.5)+theme_bw()


anovaResult25 <- aov(WindowsF$Surface ~ WindowsF$WindTypeTot)
summary(anovaResult25)
#Now use Tukey multiple pairwise-comparisons
TukeyTest25<- TukeyHSD(anovaResult25)
TukeyHSD(anovaResult25)
Tuckey25 <- print(TukeyTest25)
plot(TukeyTest25, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#On the log 
anovaResult26 <- aov(WindowsF$SurfaceLog ~ WindowsF$WindTypeTot)
summary(anovaResult26)
#Now use Tukey multiple pairwise-comparisons
TukeyTest26<- TukeyHSD(anovaResult26)
TukeyHSD(anovaResult26)
print(TukeyTest26)
plot(TukeyTest26, las=1)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))

#Kruskal test  window
kruskal.test(Surface ~ WindTypeTot, data = WindowsF)
pairwise.wilcox.test(WindowsF$Surface, WindowsF$WindTypeTot,
                     p.adjust.method = "BH")
