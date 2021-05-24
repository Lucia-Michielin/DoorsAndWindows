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

#Question 1: Proportion of Openings. which is the level of correlation between width and height of doors and windows###########

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


# Fig. 6.1: Scatterplot of width and height of the windows and doorways using the Roman feet(29.6 cm as UM)##################

ggplot(FullMeasures, aes(x=WidthPedes, y=HeightPedes, color=Type))+ geom_point(size=3.5, alpha=0.4)+
  labs (x="WIDTH (feet)", y="HEIGHT (feet)")+theme_bw()+
  expand_limits(x=c(0,22), y=c(0,16))+
  scale_x_continuous(minor_breaks = seq(0,23, 1), breaks = seq(0,23, 2))+ 
  scale_y_continuous(minor_breaks = seq(0,17, 1), breaks = seq(0,17, 2))+
  theme(legend.position = "bottom")+
  scale_color_manual(values=c("blue", "red"))
ggsave(filename='Figure6_1.tiff', dpi=600, path='Plot600')

#Fig 6.2 Density of the openings##################
library(hexbin)
ggplot(FullMeasures, aes(x=WidthPedes, y=HeightPedes) ) +
  geom_hex(bins =30) +
  scale_fill_gradient(low = "grey91", high = "grey5")+
  theme_bw()+facet_wrap(~Type)
ggsave(filename='Figure6_2.tiff', dpi=600, path='Plot600')

# Fig. 6.3 Chronology##################
#Period as factors
FullMeasures <- FullMeasures %>% 
  mutate(Period = as.factor(Period))
levels(FullMeasures$Period)
#Reorder periods
FullMeasures$Period <- factor(FullMeasures$Period, levels(FullMeasures$Period)[c(1,3,2)])
#Plot it
ggplot(FullMeasures,aes(x=Period, y=Surface, fill=Period))+ geom_boxplot()+theme_bw()+theme(legend.position="bottom")+labs(x="Period", y="Surface of the openings (m2)")+
  scale_fill_manual(values=c("grey87", "grey55", "grey30"))
ggsave(filename='Figure6_3.tiff', dpi=600, path='Plot600')

#Fig 6.4 Surface of the doorways and windows##################
ggplot(FullMeasures,aes(x=Period, y=Surface, fill=Period))+ 
  geom_boxplot()+theme_bw()+theme(legend.position="bottom")+
  labs(x="Period", y="Surface of the openings (m2)")+
  scale_fill_manual(values=c("grey87", "grey55", "grey30"))+
  facet_wrap(~Type, scales="free_y")
ggsave(filename='Figure6_4.tiff', dpi=600, path='Plot600')

# Fig. 6.5 Proportion of the openings and Sites##################
ggplot(FullMeasures,aes(x=Town, y=Ratio, fill=Town))+ 
  geom_boxplot()+theme_bw()+theme(legend.position="bottom")+
  labs(x="Site", y="Proportion of the openings")+
  scale_fill_manual(values=c("grey90", "grey75", "grey60", "grey45","grey30"))
ggsave(filename='Figure6_5.tiff', dpi=600, path='Plot600')

# Fig. 6.6 Width of the openings and Sites##################
#Sites as factors
MasterFile <- MasterFile %>% 
  mutate(Town = as.factor(Town))
levels(MasterFile$Town)
#Reorder Sites
MasterFile$Town <- factor(MasterFile$Town, levels(MasterFile$Town)[c(2,7,8,5,6,4,1,3)])
#Plot it
ggplot(MasterFile,aes(x=Town, y=Width, fill=Town))+ 
  geom_boxplot()+theme_bw()+theme(legend.position="bottom")+
  labs(x="Site", y="Width of the openings")+
  scale_fill_manual(values=c("grey97", "grey87", "grey77", "grey67","grey57","grey47","grey37","grey27"))
ggsave(filename='Figure6_6.tiff', dpi=600, path='Plot600')

# Fig. 6.7 Area faced by the openings and size of the openings##################
ggplot(FullMeasures,aes(x=Facing, y=Surface, fill=Facing))+ geom_boxplot()+facet_wrap(~Type)+theme_bw()+theme(legend.position="bottom")+labs (x="Area Faced by the Opening", y="Surface (m2)")+
  scale_fill_manual(values=c("grey87", "grey30"))
ggsave(filename='Figure6_7.tiff', dpi=600, path='Plot600')

# Fig. 6.8 Size Openings and Type of rooms
#Remove the unknown rooms
MasterFileForRoomType <- MasterFile[!(MasterFile$BiggerAreaRoom=="UNKNOWN ROOM" |MasterFile$SmallerAreaRoom=="UNKNOWN ROOM"), ]

FullMeasuresForRoomType <- FullMeasures[!(FullMeasures$BiggerAreaRoom=="UNKNOWN ROOM" |FullMeasures$SmallerAreaRoom=="UNKNOWN ROOM"), ]

#Combine the information from bigger and smaller rooms
MasterFileForRoomType  <- mutate(MasterFileForRoomType, RoomComparison = ifelse(SmallerAreaRoomType=="SERVICE ROOM" & BiggerAreaRoomType=="SERVICE ROOM", "SERVICE", ifelse((SmallerAreaRoomType=="SERVICE ROOM" & BiggerAreaRoomType=="PUBLIC ROOM")|(BiggerAreaRoomType=="SERVICE ROOM" & SmallerAreaRoomType=="PUBLIC ROOM"), "SERVICE - PUBLIC", ifelse((SmallerAreaRoomType=="SERVICE ROOM"& BiggerAreaRoomType=="PRIVATE ROOM")|(BiggerAreaRoomType=="SERVICE ROOM" & SmallerAreaRoomType=="PRIVATE ROOM"), "SERVICE - PRIVATE", ifelse((SmallerAreaRoomType=="SERVICE ROOM" & BiggerAreaRoomType=="OPEN SPACES")|(BiggerAreaRoom=="SERVICE ROOM" & SmallerAreaRoomType=="OPEN SPACES"), "SERVICE - OPEN AREA",ifelse((SmallerAreaRoomType=="PRIVATE ROOM"& BiggerAreaRoomType=="OPEN SPACES")|(BiggerAreaRoomType=="PRIVATE ROOM" & SmallerAreaRoomType=="OPEN SPACES"), "PRIVATE - OPEN AREA", ifelse((SmallerAreaRoomType=="PRIVATE ROOM"& BiggerAreaRoomType=="PUBLIC ROOM")|(BiggerAreaRoomType=="PRIVATE ROOM" & SmallerAreaRoomType=="PUBLIC ROOM"), "PRIVATE - PUBLIC",ifelse(SmallerAreaRoomType=="PRIVATE ROOM"& BiggerAreaRoomType=="PRIVATE ROOM", "PRIVATE",ifelse((SmallerAreaRoomType=="PUBLIC ROOM"& BiggerAreaRoomType=="OPEN SPACES")|(BiggerAreaRoomType=="PUBLIC ROOM" & SmallerAreaRoomType=="OPEN SPACES"), "PUBLIC - OPEN SPACES",ifelse(SmallerAreaRoomType=="OPEN SPACES"& BiggerAreaRoomType=="OPEN SPACES", "OPEN SPACES","PUBLIC" ))))))))))

FullMeasuresForRoomType <- mutate(FullMeasuresForRoomType, RoomComparison = ifelse(SmallerAreaRoomType=="SERVICE ROOM" & BiggerAreaRoomType=="SERVICE ROOM", "SERVICE", ifelse((SmallerAreaRoomType=="SERVICE ROOM" & BiggerAreaRoomType=="PUBLIC ROOM")|(BiggerAreaRoomType=="SERVICE ROOM" & SmallerAreaRoomType=="PUBLIC ROOM"), "SERVICE - PUBLIC", ifelse((SmallerAreaRoomType=="SERVICE ROOM"& BiggerAreaRoomType=="PRIVATE ROOM")|(BiggerAreaRoomType=="SERVICE ROOM" & SmallerAreaRoomType=="PRIVATE ROOM"), "SERVICE - PRIVATE", ifelse((SmallerAreaRoomType=="SERVICE ROOM" & BiggerAreaRoomType=="OPEN SPACES")|(BiggerAreaRoom=="SERVICE ROOM" & SmallerAreaRoomType=="OPEN SPACES"), "SERVICE - OPEN AREA",ifelse((SmallerAreaRoomType=="PRIVATE ROOM"& BiggerAreaRoomType=="OPEN SPACES")|(BiggerAreaRoomType=="PRIVATE ROOM" & SmallerAreaRoomType=="OPEN SPACES"), "PRIVATE - OPEN AREA", ifelse((SmallerAreaRoomType=="PRIVATE ROOM"& BiggerAreaRoomType=="PUBLIC ROOM")|(BiggerAreaRoomType=="PRIVATE ROOM" & SmallerAreaRoomType=="PUBLIC ROOM"), "PRIVATE - PUBLIC",ifelse(SmallerAreaRoomType=="PRIVATE ROOM"& BiggerAreaRoomType=="PRIVATE ROOM", "PRIVATE",ifelse((SmallerAreaRoomType=="PUBLIC ROOM"& BiggerAreaRoomType=="OPEN SPACES")|(BiggerAreaRoomType=="PUBLIC ROOM" & SmallerAreaRoomType=="OPEN SPACES"), "PUBLIC - OPEN SPACES",ifelse(SmallerAreaRoomType=="OPEN SPACES"& BiggerAreaRoomType=="OPEN SPACES", "OPEN SPACES","PUBLIC" ))))))))))

#RoomComparison as factors
FullMeasuresForRoomType<- FullMeasuresForRoomType %>% 
  mutate(RoomComparison = as.factor(RoomComparison))
levels(FullMeasuresForRoomType$RoomComparison)
#Reorder RoomComparison
FullMeasuresForRoomType$RoomComparison <- factor(FullMeasuresForRoomType$RoomComparison, levels(FullMeasuresForRoomType$RoomComparison)[c(2,9,7,3,8,10,4,1,6,5)])

#let's plot it
ggplot(FullMeasuresForRoomType,aes(x=WidthPedes, y=HeightPedes, color=RoomComparison))+ geom_point(size=3.5, alpha=0.5)+facet_wrap(~Type)+theme_bw()+theme(legend.position="bottom")+labs (x="Width(Roman Feet)", y="Height(Roman Feet)", color= "Room Comparison")+scale_color_manual(values= rainbow(10))
ggsave(filename='Figure6_8.tiff', dpi=600, path='Plot600')

#Table6.7 Size and proportions across different typologies of rooms#######################
Table6_7 <- FullMeasuresForRoomType %>% 
  group_by(Type, RoomComparison) %>% 
  summarize(SurfaceM = mean(Surface), RatioM=mean(Ratio), count=n())

Table6_7 <-arrange(Table6_7, Type, desc(SurfaceM))
 
#Analysis correlation roomsize opening size#######################
#Duplicate the sample to have all sizes of room on a new column
SizeRoom1 <- FullMeasures %>% 
  mutate(RoomSM = `Bigger Area RoomM2`)
SizeRoom2 <- FullMeasures %>% 
  mutate(RoomSM = SmallerAreaRoomM2)
TotSizeRoom <- bind_rows(SizeRoom1, SizeRoom2)
#Remove Null Values
TotSizeRoom <-TotSizeRoom[!(is.na(TotSizeRoom$RoomSM) | TotSizeRoom$RoomSM==""), ]

#Visualise Fig 6.10 (fig6.9 is a plan)#######################
ggplot(TotSizeRoom,aes(x=Surface, y=RoomSM, shape=Type))+ geom_point(size=3.5, alpha=0.6)+theme_bw()+ theme(legend.position="bottom")+labs (x="Surface(m2)", y="Size of the Room(m2)", Shape= "Type of structure")
ggsave(filename='Figure6_10.tiff', dpi=600, path='Plot600')

#Correlationvalues Table 6.12 ####################
cor(y=TotSizeRoom$RoomSM, x=TotSizeRoom$Surface)
cor(y=TotSizeRoom$RoomSM, x=TotSizeRoom$Ratio)
#Divide doors and windows
DoorsSQM <- subset(TotSizeRoom, Type=="DOORWAY")
WindowsSQM <- subset(TotSizeRoom, Type=="WINDOW")
#Correlation Windows
cor(y=WindowsSQM$RoomSM, x=WindowsSQM$Surface)
cor(y=WindowsSQM$RoomSM, x=WindowsSQM$Ratio)
#Correlation Doors
cor(y=DoorsSQM$RoomSM, x=DoorsSQM$Surface)
cor(y=DoorsSQM$RoomSM, x=DoorsSQM$Ratio)

#Fig 6.11 Size across type of dwellings
#Type of dwellings as factors##################
FullMeasures <- FullMeasures %>% 
  mutate(HouseType = as.factor(HouseType))
levels(FullMeasures$HouseType)
#Reorder periods
FullMeasures$HouseType <- factor(FullMeasures$HouseType, levels(FullMeasures$HouseType)[c(4,3,1,2,5,6)])
#Plot it
ggplot(FullMeasures,aes(x=HouseType, y=Surface, fill=HouseType))+ geom_boxplot()+theme_bw()+theme(legend.position="bottom", axis.text.x=element_text(angle=25,hjust = 1))+
  labs(x="Dwelling Type", y="Surface of the Openings(m2)", color= "Dwelling Type")+
  scale_fill_manual(values=c("grey95", "grey85", "grey73", "grey60","grey45","grey30"))
ggsave(filename='Figure6_11.tiff', dpi=600, path='Plot600')

#Doors proportions 6.2.1###########

Doors %>% 
group_by(Town) %>% 
  summarize(count=n(), sd(Ratio), mean(Ratio))

Doors <-mutate(Doors, Doorsize = ifelse(Ratio > 1, "Wide Door", "Standard Door"))
Doors <-mutate(Doors, Doorsize2 = ifelse(Ratio <= 0.5 , "A", "B"))
Doors <-mutate(Doors, Doorsize3 = ifelse(Ratio <= 0.5 & Ratio > 0.4, "A", "B"))
Doors %>% 
  group_by(Period, Doorsize) %>% 
  summarize(count=n())

Doors %>% 
  group_by(Doorsize2) %>% 
  summarize(count=n())     

Doors %>% 
  group_by(Doorsize3) %>% 
  summarize(count=n())  
#Import dataset 
TotalFilling<- read_csv('CSV_Files/TotalFilling.csv')
#Remove null values in width
TotalFilling <- TotalFilling[!(is.na(TotalFilling$Width) | TotalFilling$Width==""), ]
#Remove null values in Height
FullMeasuresFilling <- TotalFilling[!(is.na(TotalFilling$Height) | TotalFilling$Height==""), ]

#Calculate W-H Ratio and Area
FullMeasuresFilling<-within(FullMeasuresFilling, Ratio <- (FullMeasuresFilling$Width)/(FullMeasuresFilling$Height))

FullMeasuresFilling<- within(FullMeasuresFilling, Surface <- ((FullMeasuresFilling$Width/100)* (FullMeasuresFilling$Height/100)))

DoubleDoors <- subset(FullMeasuresFilling, FillingType == "DOUBLE LEAF SHUTTER" & Type=="DOORWAY")

DoubleDoors <-mutate(DoubleDoors, Doorsize2 = ifelse(Ratio <= 0.5 , "A", "B"))
DoubleDoors <-mutate(DoubleDoors, Doorsize3 = ifelse(Ratio <= 0.5 & Ratio > 0.4, "A", "B"))

DoubleDoors %>% 
  group_by(Doorsize2) %>% 
  summarize(count=n())  

#Fig 6.12
ggplot(Doors,aes(x=WidthPedes, y=HeightPedes))+
  geom_point(alpha=0.6,size=4, shape=4)+theme_bw()+theme(legend.position="bottom")+
  geom_hline(linetype = "dashed",yintercept = 8, size=1)+
  geom_hline(linetype = "dashed",yintercept = 11, size=1)+
  labs(x="Width(Roman Feet)",y="Height (Roman Feet)")+
  expand_limits(x=c(2,18), y=c(2,16))+
  scale_x_continuous(minor_breaks = seq(2,18, 1), breaks = seq(2,18, 2))+ 
  scale_y_continuous(minor_breaks = seq(2,17, 1), breaks = seq(2,17, 2))
ggsave(filename='Figure6_12.tiff', dpi=600, path='Plot600')

#Check incidence 7+-1 pedes for heights
Doors <-mutate(Doors, DoorHeights = ifelse(HeightPedes <= 8 & HeightPedes >= 6, "A", "B"))
Doors %>% 
  group_by(DoorHeights) %>% 
  summarize(count=n())  

#Check incidence 4+-1 pedes for Widths
Doors <-mutate(Doors, DoorWidths = ifelse(WidthPedes <= 5 & WidthPedes >= 3, "A", "B"))
Doors %>% 
  group_by(DoorWidths) %>% 
  summarize(count=n())  

#Removing the doors for which we do not know the type
DoorsType <- Doors[!(is.na(Doors$LintelType) | Doors$LintelType==""), ]
DoorsType <-mutate(DoorsType, DoorTypeTot = ifelse(LintelType == "A" |LintelType == "B"|LintelType == "D", "Squared", ifelse(LintelType == "C" |LintelType == "F", "Filled Arch", "Arch")))

#Fig 6.14 (6.13 is not a graph)===============
ggplot(DoorsType, aes(x=factor(DoorTypeTot), fill=LintelType)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), color="black")+theme_bw()+ scale_y_continuous(labels = scales::percent)+ labs(y="Percentage", x="Doorway Appereance", fill="Doorway Shape")+ scale_fill_grey(start = 0, end = 1)
ggsave(filename='Figure6_14.tiff', dpi=600, path='Plot600')

#Fig 6.15===============
p <- ggplot(DoorsType, aes(x=DoorTypeTot, y=Town))+ geom_bin2d(color="black") +theme_bw()+ scale_fill_gradient(low = "grey95", high = "grey50")+ labs(y="Site", x="Doorway Appereance")
newdata <- ggplot_build(p)$data[[1]]
p + geom_text(data=newdata, aes((xmin + xmax)/2, (ymin + ymax)/2, 
                                label=count), col="Black",size=7)
ggsave(filename='Figure6_15.tiff', dpi=600, path='Plot600')

#Fig 6.16===============
p2 <- ggplot(DoorsType, aes(x=DoorTypeTot, y=Period))+ geom_bin2d(color="black") +theme_bw()+ scale_fill_gradient(low = "grey95", high = "grey50")+ labs(y="Period", x="Doorway Appereance")
newdata <- ggplot_build(p2)$data[[1]]
p2 + geom_text(data=newdata, aes((xmin + xmax)/2, (ymin + ymax)/2, 
                                 label=count), col="Black", size=7)
ggsave(filename='Figure6_16.tiff', dpi=600, path='Plot600')

#Fig 6.17===============
p3 <- ggplot(DoorsType, aes(x=DoorTypeTot, y=HouseType))+ geom_bin2d(color="black") +theme_bw()+ scale_fill_gradient(low = "grey95", high = "grey50")+ labs(y="House Type", x="Doorway Appereance")
newdata <- ggplot_build(p3)$data[[1]]
p3 + geom_text(data=newdata, aes((xmin + xmax)/2, (ymin + ymax)/2, 
                                 label=count), col="Black", size=7)
ggsave(filename='Figure6_17.tiff', dpi=600, path='Plot600')

#Fig 6.18========================
ggplot(DoorsType,aes(x=WidthPedes, y=HeightPedes))+
  geom_point(size=4, shape=21, color="black", aes(fill=LintelType))+theme_bw()+theme(legend.position="bottom")+
  geom_hline(linetype = "dashed",yintercept = 8, size=1)+
  geom_hline(linetype = "dashed",yintercept = 11, size=1)+
  labs(x="Width(Roman Feet)",y="Height (Roman Feet)")+
  expand_limits(x=c(2,18), y=c(2,16))+
  scale_x_continuous(minor_breaks = seq(2,18, 1), breaks = seq(2,18, 2))+ 
  scale_y_continuous(minor_breaks = seq(2,17, 1), breaks = seq(2,17, 2))+ scale_fill_grey(start = 0, end = 1)
ggsave(filename='Figure6_18.tiff', dpi=600, path='Plot600')

Summarise7 <- DoorsType %>% 
  group_by(LintelType, Town) %>% 
  summarize(count=n(), mean(Surface)) 

Summarise7 <-arrange(Summarise7, LintelType, desc(count))


#Filling
DoorsT<- DoorsTot[!(is.na(DoorsTot$IdThresholdSill) | DoorsTot$IdThresholdSill==""), ]
DoorsF <- subset(TotalFilling, Type=="DOORWAY")
DoorsF<- DoorsF[!(is.na(DoorsF$FillingType) | DoorsF$FillingType==""), ]

Summarise8 <- DoorsF %>% 
  group_by(FillingType) %>% 
  summarize(count=n(), mean(Surface)) 
#Fig 6.19
#filling as factors
DoorsF <- DoorsF %>% 
  mutate(FillingType = as.factor(FillingType))
levels(DoorsF$FillingType)
#Reorder periods
DoorsF$FillingType<- factor(DoorsF$FillingType, levels(DoorsF$FillingType)[c(1,7,5,4,6,2,3)])
#Plot it fig 6.19
ggplot(DoorsF, aes(x=factor(FillingType), fill=FillingType)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), color="black")+theme_bw()+ scale_y_continuous(labels = scales::percent)+ labs(y="Percentage", x="Door/Barrier Type")+ scale_fill_grey(start = 0, end = 1)+ theme(legend.position="bottom", axis.text.x=element_text(angle=40,hjust = 1))
ggsave(filename='Figure6_19.tiff', dpi=600, path='Plot600')

#Fig 6.20=============

DoorsDir<- DoorsF[!(is.na(DoorsF$DirectionOpeningRoom) | DoorsF$DirectionOpeningRoom==""), ]
Summarise9 <- DoorsDir %>% 
  group_by(DirectionOpeningRoom) %>% 
  summarize(count=n()) 

DoorsDir2 <-mutate(DoorsDir, DoorDirTot = ifelse(DirectionOpeningRoom == "SR" , SmallerAreaRoomType, BiggerAreaRoomType ))

#Room dir as factors
DoorsDir2 <- DoorsDir2 %>% 
  mutate(DoorDirTot = as.factor(DoorDirTot))
levels(DoorsDir2$DoorDirTot)
#Reorder periods
DoorsDir2$DoorDirTot<- factor(DoorsDir2$DoorDirTot, levels(DoorsDir2$DoorDirTot)[c(3,2,4,1,5)])

ggplot(DoorsDir2, aes(x=factor(DoorDirTot), fill=DoorDirTot)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), color="black")+
  theme_bw()+ 
  scale_y_continuous(labels = scales::percent)+ 
  labs(y="Percentage", x="Door the Room Open to", fill="Door the Room Open to")+ scale_fill_grey(start = 0, end = 1)+ theme(axis.text.x=element_text(angle=40,hjust = 1))
ggsave(filename='Figure6_20.tiff', dpi=600, path='Plot600')


cor(y=WindowsCleaned$WidthPedes, x=WindowsCleaned$HeightPedes)

Summarise10 <- Windows %>% 
  summarize(mean(Windows$WidthPedes),sd(Windows$Width), mean(Windows$HeightPedes),sd(Windows$Height)) 

mean(Windows$Ratio)
sd(Windows$Ratio)

mean(Doors$Ratio)
sd(Doors$Ratio)


min(Windows$WidthPedes)
min(Windows$HeightPedes)
max(Windows$HeightPedes)
sd(Windows$Width)
sd(Windows$Height)

#Fig 6.21==========================
WindowsCleaned <- subset(Windows, Width < 400)
ggplot(Windows, aes(x=WidthPedes, y=HeightPedes )) + 
  geom_point(size=4, alpha=0.5)+ geom_abline(size=1, intercept = 0.7015)+
  theme_bw()+
  labs(x="Width(Roman Feet)",y="Height (Roman Feet)")+
  expand_limits(x=c(2,23), y=c(2,12))+
  scale_x_continuous(minor_breaks = seq(0,23, 1), breaks = seq(2,23, 2))+ 
  scale_y_continuous(minor_breaks = seq(0,12, 1), breaks = seq(2,12, 2))+ scale_fill_grey(start = 0, end = 1)
ggsave(filename='Figure6_21.tiff', dpi=600, path='Plot600')

lm(WidthPedes~HeightPedes, data= Windows)

Summarise10 <- Windows %>% 
  group_by(Period) %>% 
  summarize(mean(Ratio),sd(Ratio), mean(WidthPedes),sd(WidthPedes), mean(HeightPedes),sd(HeightPedes)) 


Summarise11 <- WindowHouse %>% 
  group_by(Housetot) %>% 
  summarize(mean(Ratio),sd(Ratio), mean(WidthPedes),sd(WidthPedes), mean(HeightPedes),sd(HeightPedes)) 

#Peculiar types of Windows
WindowsTypes1 <-mutate(WindowsTot, WindowsTypes1 = ifelse(SmallerAreaRoomType== "OPEN SPACES"| BiggerAreaRoomType== "OPEN SPACES", "NO", "YES"))

Summarise12 <- WindowsTypes1 %>% 
  group_by(WindowsTypes1, Town) %>% 
  summarize(count=n()) 

WindowsTypes1 <- mutate(WindowsTypes1, Housetot = ifelse(HouseType=="DOMUS", "DOMUS",ifelse(HouseType=="HOTEL", "HOTEL",ifelse(HouseType=="SHOP APARTMENT", "SHOP APARTMENT",ifelse(HouseType=="MEDIANUM APARTMENT", "MEDIANUM APARTMENT","APARTMENT")))))

Summarise13 <- WindowsTypes1 %>% 
  group_by(WindowsTypes1, Housetot) %>% 
  summarize(count=n()) 

Summarise14 <- WindowsTypes1 %>% 
  group_by(WindowsTypes1, Period) %>% 
  summarize(count=n()) 

WindowsTypes1 %>% 
  group_by(WindowsTypes1) %>% 
  summarize(count=n()) 

#Windows type 2: medianum
WindowsTypes2 <- mutate(WindowsTypes1, MedianumW = ifelse(SmallerAreaRoom =="MEDIANUM"|BiggerAreaRoom =="MEDIANUM", "YES","NO"))

WindowsTypes2 %>% 
  group_by(Housetot,MedianumW) %>% 
  summarize(count=n()) 

#Window type 3: wolf Mouth windows
WindowsTypes3 <- mutate(WindowsTypes1, WMWindow = ifelse(PeculiarType =="WOLF MOUTH WINDOW", "YES","NO"))
Summarise15 <-WindowsTypes3 %>% 
  group_by(WMWindow, Period) %>% 
  summarize(count=n(), mean(Width) ) 

#Window Fillings
WindowsF <- subset(TotalFilling, Type=="WINDOW")
WindowsF<- WindowsF [!(is.na(WindowsF$LintelType) | WindowsF$LintelType==""), ]
WindowsF <-mutate(WindowsF, WindTypeTot = ifelse(LintelType == "A" |LintelType == "B"|LintelType == "D", "Squared", ifelse(LintelType == "C" |LintelType == "F", "Filled Arch", "Arch")))

WindowsF<-within(WindowsF, Ratio <- (WindowsF$Width)/(WindowsF$Height))

WindowsF<- within(WindowsF, Surface <- ((WindowsF$Width/100)* (WindowsF$Height/100)))
WindowsF<- within(WindowsF, SurfaceLog <- log(WindowsF$Surface))

#plot it Fig 6.23 (22 is a plan)============
ggplot(WindowsF, aes(x=factor(WindTypeTot), fill=LintelType)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), color="black")+theme_bw()+ scale_y_continuous(labels = scales::percent)+ labs(y="Percentage", x="Window Type")+ scale_fill_grey(start = 0, end = 1)+ theme(legend.position="bottom")
ggsave(filename='Figure6_23.tiff', dpi=600, path='Plot600')


Summarise16 <-WindowsF %>% 
  group_by(LintelType) %>% 
  summarize(count=n()) 

#extract all the windows for which we know the distance from ground
WindDist <- WindowsTot[!(is.na(WindowsTot$DistancefromGround) | WindowsTot$DistancefromGround==""), ] 
WindDist <- mutate(WindDist, Distance = ifelse(DistancefromGround < 90, "LW", ifelse(DistancefromGround >=90 & DistancefromGround<=150, "MW", "HW")))

Summarise17 <-WindDist  %>% 
  group_by(Distance) %>% 
  summarize(count=n(), min(DistancefromGround), max(DistancefromGround))

Summarise18 <-WindowsTot  %>% 
  group_by(Floor) %>% 
  summarize(count=n(), mean(Width))

#Fig 6.24

WindowsF<- mutate(WindowsF, Filling = ifelse(is.na(FillingType), "UNKNOWN", FillingType))
WindowsF<- mutate(WindowsF, FillingTot = ifelse(Filling=="UNKNOWN"|Filling=="NO FILLING", "NO FILLING OR UNKNOWN", "BARRIER KNOWN"))

ggplot(WindowsF, aes(x=factor(FillingTot), fill=Filling)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), color="black")+theme_bw()+ scale_y_continuous(labels = scales::percent)+ labs(y="Percentage", x="Window Filling Type")+ scale_fill_grey(start = 0, end = 1)+ theme(legend.position="bottom")
ggsave(filename='Figure6_24.tiff', dpi=600, path='Plot600')

#Between Flats
BetweenFlats <- subset(WindowsTot, PeculiarType=="BETWEEN FLATS")
Summarise19 <-BetweenFlats  %>% 
  group_by(Period) %>% 
  summarize(count=n())
BetweenFlats  %>% 
  group_by(Town) %>% 
  summarize(count=n())
BetweenFlats  %>% 
  group_by(HouseType) %>% 
  summarize(count=n())
mean(BetweenFlats$Width)

#Fig 6.25=============================================
OpeningFill<- MasterFile[!(is.na(MasterFile$IdThresholdSill) |MasterFile$IdThresholdSill==""), ]
OpeningFill<- mutate(OpeningFill, ThresholdSillMaterial = ifelse(is.na(ThresholdSillMaterial), "UNKNOWN", ifelse(ThresholdSillMaterial=="LIMESTONE AND MOSAIC"|ThresholdSillMaterial=="MARBLE AND MOSAIC", "MULTIPLE MATERIALS", ifelse(ThresholdSillMaterial=="OPUS SECTILE"|ThresholdSillMaterial=="MOSAIC", "MOSAIC OR OPUS SECTILE",ThresholdSillMaterial))))

#Material as factors
OpeningFill <- OpeningFill %>% 
  mutate(ThresholdSillMaterial = as.factor(ThresholdSillMaterial))
levels(OpeningFill$ThresholdSillMaterial)
#Material periods
OpeningFill$ThresholdSillMaterial<- factor(OpeningFill$ThresholdSillMaterial, levels(OpeningFill$ThresholdSillMaterial)[c(3,2,6,4,1,7,5,8)])

ggplot(OpeningFill, aes(x=factor(ThresholdSillMaterial), fill=ThresholdSillMaterial)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), color="black")+theme_bw()+ scale_y_continuous(labels = scales::percent)+ labs(y="Percentage", x="Threshold Material", fill= "Threshold Material")+ scale_fill_grey(start = 0, end = 1)+ theme(axis.text.x=element_text(angle=40,hjust = 1))
ggsave(filename='Figure6_25.tiff', dpi=600, path='Plot600')


#Fig 6.26=======================
OpeningFill <- OpeningFill %>% 
  mutate(Period = as.factor(Period))
levels(OpeningFill$Period)
#Reorder periods
OpeningFill$Period <- factor(OpeningFill$Period, levels(OpeningFill$Period)[c(1,3,2)])

p7<- ggplot(OpeningFill, aes(x=Period, y=ThresholdSillMaterial))+ geom_bin2d(color="black") +theme_bw()+ scale_fill_gradient(low = "grey95", high = "grey50")+ labs(y="Threshold Materials", x="Chronology")
newdata <- ggplot_build(p7)$data[[1]]
p7 + geom_text(data=newdata, aes((xmin + xmax)/2, (ymin + ymax)/2, 
                                 label=count), col="Black",size=7)
ggsave(filename='Figure6_26.tiff', dpi=600, path='Plot600')

#Fig 6.27=========================
p7<- ggplot(OpeningFill, aes(x=Town, y=ThresholdSillMaterial))+ geom_bin2d(color="black") +theme_bw()+ scale_fill_gradient(low = "grey95", high = "grey50")+ labs(y="Threshold Materials", x="Site")+ theme( axis.text.x=element_text(angle=40,hjust = 1))
newdata <- ggplot_build(p7)$data[[1]]
p7 + geom_text(data=newdata, aes((xmin + xmax)/2, (ymin + ymax)/2, 
                                 label=count), col="Black",size=7)
ggsave(filename='Figure6_27.tiff', dpi=600, path='Plot600')

#Fig 6.28=================================
p7<- ggplot(OpeningFill, aes(x=HouseType, y=ThresholdSillMaterial))+ geom_bin2d(color="black") +theme_bw()+ scale_fill_gradient(low = "grey95", high = "grey50")+ labs(y="Threshold Materials", x="House Type")+ theme( axis.text.x=element_text(angle=40,hjust = 1))
newdata <- ggplot_build(p7)$data[[1]]
p7 + geom_text(data=newdata, aes((xmin + xmax)/2, (ymin + ymax)/2, 
                                 label=count), col="Black",size=7)
ggsave(filename='Figure6_28.tiff', dpi=600, path='Plot600')


#Fig 6.29==================================
ggplot(OpeningFill, aes(x=ThresholdSillMaterial, y=Width, fill=ThresholdSillMaterial)) + 
  geom_boxplot()+ labs(y="Width (cm)", x="Threshold Materials", fill= "Threshold Materials")+ scale_fill_grey(start = 0, end = 1)+ theme(axis.text.x=element_text(angle=40,hjust = 1))+theme_bw()+ theme(legend.position="bottom", axis.text.x=element_text(angle=40,hjust = 1))
ggsave(filename='Figure6_29.tiff', dpi=600, path='Plot600')

Summarise20 <-  OpeningFill  %>% 
  group_by(Period,ThresholdSillType ) %>% 
  summarize(count=n() )


#Fig 6.31 (fig 30 not a graph)==========================
p5 <- ggplot(OpeningFill, aes(x=ThresholdSillMaterial, y=ThresholdSillType))+ geom_bin2d(color="black") +theme_bw()+ scale_fill_gradient(low = "grey95", high = "grey50")+ labs(y="Doorstop Type", x="Threshold Material")+ theme( axis.text.x=element_text(angle=40,hjust = 1))
newdata <- ggplot_build(p5)$data[[1]]
p5 + geom_text(data=newdata, aes((xmin + xmax)/2, (ymin + ymax)/2, 
                                 label=count), col="Black",size=7)
ggsave(filename='Figure6_31.tiff', dpi=600, path='Plot600')

#Fig 6.32========================
p6<- ggplot(OpeningFill, aes(x=HouseType, y=ThresholdSillType))+ geom_bin2d(color="black") +theme_bw()+ scale_fill_gradient(low = "grey95", high = "grey50")+ labs(y="Doorstop Type", x="House Type")+ theme( axis.text.x=element_text(angle=40,hjust = 1))
newdata <- ggplot_build(p6)$data[[1]]
p6 + geom_text(data=newdata, aes((xmin + xmax)/2, (ymin + ymax)/2, 
                                 label=count), col="Black",size=7)
ggsave(filename='Figure6_32.tiff', dpi=600, path='Plot600')

#Fig 6.33=========================

ggplot(OpeningFill, aes(x=ThresholdSillType, y=Width, fill=ThresholdSillType)) + 
  geom_boxplot()+ labs(y="Width (cm)", x="Threshold Type", fill= "Threshold Type")+ scale_fill_grey(start = 0, end = 1)+ theme(axis.text.x=element_text(angle=40,hjust = 1))+theme_bw()+ theme(legend.position="bottom")
ggsave(filename='Figure6_33.tiff', dpi=600, path='Plot600')

#Fig 6.34===============================
OpeningFillNo0 <- OpeningFillNo0 %>% 
  mutate(Town = as.factor(Town))
levels(OpeningFillNo0$Town)
#Reorder periods
OpeningFillNo0$Town <- factor(OpeningFillNo0$Town, levels(OpeningFillNo0$Town)[c(6,5,1,2,3,4,7,8)])

OpeningFillNo0 <- subset(OpeningFill,ThresholdSillDoorstopWidth>0)

ggplot(OpeningFillNo0, aes(x=ThresholdSillDoorstopWidth, fill=Town)) + 
  geom_histogram(color="Black", binwidth = 1)+
  labs(x="Doorstop Width", fill= "Site")+ 
  scale_fill_grey(start = 0, end = 1)+ 
  theme(axis.text.x=element_text(angle=40,hjust = 1))+theme_bw()+ theme(legend.position="bottom")+
  scale_x_continuous(minor_breaks = seq(0,50, 2), breaks = seq(0,50, 4))+
  scale_y_continuous(minor_breaks = seq(0,60,5), breaks = seq(0,60,10))
ggsave(filename='Figure6_34.tiff', dpi=600, path='Plot600')


#Fig 6.35========================
p4 <- ggplot(OpeningFill, aes(x=Period, y=ThresholdSillType))+ geom_bin2d(color="black") +theme_bw()+ scale_fill_gradient(low = "grey95", high = "grey50")+ labs(y="Doorstop Type", x="Chronology")
newdata <- ggplot_build(p4)$data[[1]]
p4 + geom_text(data=newdata, aes((xmin + xmax)/2, (ymin + ymax)/2, 
                                 label=count), col="Black",size=7)
ggsave(filename='Figure6_35.tiff', dpi=600, path='Plot600')

#Import Hinges traces
HingesTraces <- read_csv('CSV_Files/HingesTraces.csv')
#Remove null values in width
HingesTraces <- HingesTraces[!(is.na(HingesTraces$IdThresholdSill) | HingesTraces$IdThresholdSill==""), ]
HingesTraces <- HingesTraces[!(is.na(HingesTraces$Period) | HingesTraces$Period==""), ]



#Order Periods
HingesTraces <- HingesTraces %>% 
  mutate(Period = as.factor(Period))
levels(HingesTraces$Period)
#Reorder periods
HingesTraces$Period <- factor(HingesTraces$Period, levels(HingesTraces$Period)[c(1,3,2)])

#Bring together apartments and Hotel
HingesTraces <-mutate(HingesTraces, HouseTot= ifelse(HouseType== "DOMUS", "DOMUS", "APARTMENT OR HOTEL"))


#Fig 6.36=============================
p4 <- ggplot(HingesTraces, aes(x=Period, y=ThresholdSillHingeHoleShape))+ geom_bin2d(color="black") +theme_bw()+ scale_fill_gradient(low = "grey95", high = "grey50")+ labs(y="Hinge Hole Type", x="Chronology")
newdata <- ggplot_build(p4)$data[[1]]
p4 + geom_text(data=newdata, aes((xmin + xmax)/2, (ymin + ymax)/2, 
                                 label=count), col="Black",size=7)
ggsave(filename='Figure6_36.tiff', dpi=600, path='Plot600')

#Fig 6.37===============================
P4 <- ggplot(HingesTraces, aes(x=Period, y=ThresholdSillHingeHoleShape))+ geom_bin2d(color="black") +theme_bw()+ scale_fill_gradient(low = "grey95", high = "grey50")+ labs(y="Hinge Hole Type", x="Chronology")+facet_wrap(~HouseTot)
  
newdata <- ggplot_build(P4)$data[[1]]
newdata1<-subset(newdata, PANEL==1)
newdata2<-subset(newdata, PANEL==2)

P4+geom_text(data=newdata1,aes((xmin + xmax)/2,(ymin + ymax)/2,label=ifelse(PANEL==1,count," ")), col="Black",size=7)+geom_text(data=newdata2,aes((xmin + xmax)/2,(ymin + ymax)/2,label=ifelse(PANEL==2,count," ")), col="Black",size=7)
ggsave(filename='Figure6_37.tiff', dpi=600, path='Plot600')

#Extract squareHinges
squareHinges <- subset(HingesTraces,ThresholdSillHingeHoleShape=="SQUARED")

#Remove null values in width
squareHinges <- squareHinges[!(is.na(squareHinges$ThresholdSillHingeHoleWidth) | squareHinges$ThresholdSillHingeHoleWidth==""), ]
#Remove null values in Length
squareHinges <- squareHinges[!(is.na(squareHinges$ThresholdSillHingeHoleLength) | squareHinges$ThresholdSillHingeHoleLength==""), ]

#Fig 6.38=======================
library(hexbin)
ggplot(squareHinges, aes(x=ThresholdSillHingeHoleWidth, y=ThresholdSillHingeHoleLength) ) +
  geom_hex(bins =10.5) +
  scale_fill_gradient(low = "grey91", high = "grey5")+
  theme_bw()+
  scale_x_continuous(minor_breaks = seq(0,14, 1), breaks = seq(0,14, 2))+ 
  scale_y_continuous(minor_breaks = seq(0,14, 1), breaks = seq(0,14, 2))+
  expand_limits(x=c(2,14), y=c(2,14))+
  geom_abline(slope=1, size=1.5)+ 
  labs(y="Length (cm)", x="Width (cm)")
ggsave(filename='Figure6_38.tiff', dpi=600, path='Plot600')

#Add surface
squareHinges<- within(squareHinges, SurfaceHinges <- ((squareHinges$ThresholdSillHingeHoleWidth)* (squareHinges$ThresholdSillHingeHoleLength)))

cor(squareHinges$ThresholdSillHingeHoleWidth, squareHinges$ThresholdSillHingeHoleLength)

#Add ratio
squareHinges<- within(squareHinges, RatioHinges <- (squareHinges$ThresholdSillHingeHoleWidth/squareHinges$ThresholdSillHingeHoleLength))

write.csv(squareHinges,"CSV_Files\\SquareHinges.csv", row.names = FALSE)
  
#Table 6.22
Table6_22<-  squareHinges  %>% 
  group_by(Period) %>% 
  summarize(MeanSurface=round(mean(SurfaceHinges),1) )

#Extract RoundHinges
RoundHinges <- subset(HingesTraces,ThresholdSillHingeHoleShape=="ROUND")

#Remove null values radius
RoundHinges <- RoundHinges[!(is.na(RoundHinges$ThresholdSillHingeHoleRadius) | RoundHinges$ThresholdSillHingeHoleRadius==""), ]

#Table 6.23
Table6_23<-  RoundHinges  %>% 
  group_by(Period) %>% 
  summarize(MeanRadius=round(mean(ThresholdSillHingeHoleRadius),1) )

#Table 6.24
Table6_24<-  RoundHinges  %>% 
  group_by(Town) %>% 
  summarize(MeanRadius=round(mean(ThresholdSillHingeHoleRadius),1) )

#Fig 6.39================================
#Select only where traces of bolt are attested 
BoltTraces <- subset(MasterFile, ThresholdSillBoltHoleNumber>0)

Singlematches <- subset(BoltTraces, SmallerAreaRoom == BiggerAreaRoom)
Singlematches <-mutate(Singlematches, RoomGeneral= SmallerAreaRoom)

AllOTherMatch1 <- mutate(BoltTraces , RoomGeneral = ifelse(SmallerAreaRoom == BiggerAreaRoom,"TOCANCEL", SmallerAreaRoom))

AllOTherMatch2 <- mutate(BoltTraces , RoomGeneral = ifelse(SmallerAreaRoom == BiggerAreaRoom,"TOCANCEL", BiggerAreaRoom))

TotRoomBolt<- bind_rows(Singlematches,AllOTherMatch1,AllOTherMatch2)
TotRoomBolt<- TotRoomBolt[!(TotRoomBolt$RoomGeneral=="TOCANCEL"), ]

ggplot(TotRoomBolt, aes(x=factor(RoomGeneral), fill=RoomGeneral)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), color="black")+theme_bw()+ scale_y_continuous(labels = scales::percent)+ labs(y="Percentage", x="Room Type", Fill="Room Type")+ scale_fill_grey(start = 0, end = 1)+ theme(axis.text.x=element_text(angle=40,hjust = 1))
ggsave(filename='Figure6_39.tiff', dpi=600, path='Plot600')

#Order ROoms
#Order Periods
TotRoomBolt<- TotRoomBolt %>% 
  mutate(RoomGeneral = as.factor(RoomGeneral))
levels(TotRoomBolt$RoomGeneral)
#Reorder periods
TotRoomBolt$RoomGeneral <- factor(TotRoomBolt$RoomGeneral, levels(TotRoomBolt$RoomGeneral)[c(3,6,15,18,12,4,7,1,11,16,17,8,13,2,5,9,10,14)])



