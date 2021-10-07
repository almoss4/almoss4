
library(ggplot2)
library(countrycode)
library(maps)
library(ggmap)
library(mapproj)
library(dplyr)
library(countrycode)
library(RColorBrewer)
library(forcats)
library(kernlab)
library(caret)
library(e1071)
library(lubridate)
library(readxl)
library(plyr)
library(lmtest)
library(data.table)
library(neuralnet)

cityDF<- read_excel("Downloads/H2-City (1).xlsx")
resortDF <- read_excel("Downloads/H1-Resort (2).xlsx")

#clean data
ymd<-data.frame(date = cityDF$`Arrival Date`,
                ArrivalYear = as.numeric(format(cityDF$`Arrival Date`, format = "%Y")),
                ArrivalMonth = as.numeric(format(cityDF$`Arrival Date`, format = "%m")),
                ArrivalDay = as.numeric(format(cityDF$`Arrival Date`, format = "%d")))
str(ymd)
cityDF<-c(cityDF,ymd)
cityDF<-as.data.frame(cityDF)

library(lubridate)

Seasons = function(x){
  if(x %in% 3:5) return("Spring")
  if(x %in% 6:8) return("Summer")
  if(x %in% 9:11) return("Fall")
  if(x %in% c(12,1,2)) return("Winter")
  
}
cityDF$Season = sapply(cityDF$ArrivalMonth, Seasons)
cityDF<-as.data.frame(cityDF)
cityDF$Season<-as.character(cityDF$Season)
#Adding Visitor Type Column
cityDF$Children<-as.numeric(cityDF$Children)
cityDF$SumVisitors<-(cityDF$Adults+cityDF$Children+cityDF$Babies)


Visitorsfunction = function(x){
  if(x %in% 1) return("Single")
  if(x %in% 2) return("Couple")
  if(x %in% 3) return("Family")
  if(x %in% 4:100) return("Family")
  
}
cityDF$VisitorType = sapply(cityDF$SumVisitors, Visitorsfunction)
cityDF$VisitorType<-as.character(cityDF$VisitorType)
str(cityDF)

#AverageRevenuePerStay

cityDF<-cityDF%>% mutate(AverageRevenuePerStay =  ifelse(IsCanceled==0,(cityDF$StaysInWeekendNights+cityDF$StaysInWeekNights)*cityDF$ADR, 0))
names(cityDF)[3]<-paste("ArrivalDate")
str(cityDF)
View(cityDF)
head(cityDF)

#FOR RESORT
ymd<-data.frame(date = resortDF$`Arrival Date`,
                ArrivalYear = as.numeric(format(resortDF$`Arrival Date`, format = "%Y")),
                ArrivalMonth = as.numeric(format(resortDF$`Arrival Date`, format = "%m")),
                ArrivalDay = as.numeric(format(resortDF$`Arrival Date`, format = "%d")))
str(ymd)
resortDF<-c(resortDF,ymd)
resortDF<-as.data.frame(resortDF)

library(lubridate)

Seasons = function(x){
  if(x %in% 3:5) return("Spring")
  if(x %in% 6:8) return("Summer")
  if(x %in% 9:11) return("Fall")
  if(x %in% c(12,1,2)) return("Winter")
  
}
resortDF$Season = sapply(resortDF$ArrivalMonth, Seasons)
resortDF<-as.data.frame(resortDF)
resortDF$Season<-as.character(resortDF$Season)
#Adding Visitor Type Column
resortDF$Children<-as.numeric(resortDF$Children)
resortDF$SumVisitors<-(resortDF$Adults+resortDF$Children+resortDF$Babies)


Visitorsfunction = function(x){
  if(x %in% 1) return("Single")
  if(x %in% 2) return("Couple")
  if(x %in% 3) return("Family")
  if(x %in% 4:100) return("Family")
  
}
resortDF$VisitorType = sapply(resortDF$SumVisitors, Visitorsfunction)
resortDF$VisitorType<-as.character(resortDF$VisitorType)
str(resortDF)

#AverageRevenuePerStay

resortDF<-resortDF%>% mutate(AverageRevenuePerStay =  ifelse(IsCanceled==0,(resortDF$StaysInWeekendNights+resortDF$StaysInWeekNights)*resortDF$ADR, 0))
names(resortDF)[3]<-paste("ArrivalDate")
str(resortDF)
View(resortDF)
head(resortDF)

#fix empty item in resort Row 13, column 5
resortDF[13,5]="Check-Out"


#get rid of NAs. the cityDF has active reservations until 40060
shortcityDF<-cityDF[1:40060,]
View(shortcityDF)
#2a
###Descriptive Statistics of Numeric Variables
#cityDF

#ADR
summary(cityDF$ADR)

#Adults
summary(cityDF$Adults)
table(cityDF$Adults)

#Babies
summary(cityDF$Babies)
table(cityDF$Babies)

#BookingChanges
summary(cityDF$BookingChanges)
table(cityDF$BookingChanges)

#Children
cityDF$Children[is.na(cityDF$Children)]<-0 #Change 4 NA values to zero
summary(cityDF$Children)
table(cityDF$Children)

#DaysInWaitingList
summary(cityDF$DaysInWaitingList)
table(cityDF$DaysInWaitingList)

#LeadTime
summary(cityDF$LeadTime)

#PreviousBookingsNotCanceled
summary(cityDF$PreviousBookingsNotCanceled)
table(cityDF$PreviousBookingsNotCanceled)

#PreviousCancellations
summary(cityDF$PreviousCancellations)
table(cityDF$PreviousCancellations)

#RequiredCarParkingSpaces
summary(cityDF$RequiredCarParkingSpaces)
table(cityDF$RequiredCarParkingSpaces)

#StaysInWeekendsNights
summary(cityDF$StaysInWeekendNights)
table(cityDF$StaysInWeekendNights)

#StaysInWeekNights
summary(cityDF$StaysInWeekNights)
table(cityDF$StaysInWeekNights)

#TotalSpecialRequests
summary(cityDF$TotalOfSpecialRequests)
table(cityDF$TotalOfSpecialRequests)

#ArrivalYear
summary(cityDF$ArrivalYear)
table(cityDF$ArrivalYear)

#ArrivalMonth
summary(cityDF$ArrivalMonth)
table(cityDF$ArrivalMonth)

#ArrivalDay
summary(cityDF$ArrivalDay)
table(cityDF$ArrivalDay)

#SumVisitors
cityDF$SumVisitors[is.na(cityDF$SumVisitors)]<-0 #Change 4 NA values to zero
summary(cityDF$SumVisitors)
table(cityDF$SumVisitors)

#AverageRevenuePerStay
summary(cityDF$AverageRevenuePerStay[cityDF$AverageRevenuePerStay!=0]) #does not include zero values

#resortDF

#ADR
summary(resortDF$ADR)

#Adults
summary(resortDF$Adults)
table(resortDF$Adults)

#Babies
summary(resortDF$Babies)
table(resortDF$Babies)

#BookingChanges
summary(resortDF$BookingChanges)
table(resortDF$BookingChanges)

#Children
summary(resortDF$Children)
table(resortDF$Children)

#DaysInWaitingList
summary(resortDF$DaysInWaitingList)
table(resortDF$DaysInWaitingList)

#LeadTime
summary(resortDF$LeadTime)

#PreviousBookingsNotCanceled
summary(resortDF$PreviousBookingsNotCanceled)
table(resortDF$PreviousBookingsNotCanceled)

#PreviousCancellations
summary(resortDF$PreviousCancellations)
table(resortDF$PreviousCancellations)

#RequiredCarParkingSpaces
summary(resortDF$RequiredCarParkingSpaces)
table(resortDF$RequiredCarParkingSpaces)

#StaysInWeekendsNights
summary(resortDF$StaysInWeekendNights)
table(resortDF$StaysInWeekendNights)

#StaysInWeekNights
summary(resortDF$StaysInWeekNights)
table(resortDF$StaysInWeekNights)

#TotalSpecialRequests
summary(resortDF$TotalOfSpecialRequests)
table(resortDF$TotalOfSpecialRequests)

#ArrivalYear
summary(resortDF$ArrivalYear)
table(resortDF$ArrivalYear)

#ArrivalMonth
summary(resortDF$ArrivalMonth)
table(resortDF$ArrivalMonth)

#ArrivalDay
summary(resortDF$ArrivalDay)
table(resortDF$ArrivalDay)

#SumVisitors
summary(resortDF$SumVisitors)
table(resortDF$SumVisitors)

#AverageRevenuePerStay
summary(resortDF$AverageRevenuePerStay[resortDF$AverageRevenuePerStay!=0])

#cityDF Histograms and Boxplots of Numeric Variables

#ADR
hist(cityDF$ADR,xlim=c(0,350),ylim=c(0,30000),breaks=250,main='City ADR Histogram')
boxplot(cityDF$ADR,ylim=c(0,350),range=0,horizontal=TRUE,varwidth=TRUE,main='City ADR Boxplot')

#LeadTime
hist(cityDF$LeadTime,ylim=c(0,20000),breaks=25,main='City Lead Time Histogram')
boxplot(cityDF$LeadTime,horizontal=TRUE,range=0,varwidth=TRUE,main='City Lead Time Boxplot')

#StaysinWeekendNights
hist(cityDF$StaysInWeekendNights,xlim=c(0,5),ylim=c(0,50000),breaks=100,main='City Stays in Weekend Nights Histogram')
boxplot(cityDF$StaysInWeekNights,ylim=c(0,5),horizontal=TRUE,range=0,varwidth=TRUE,main='City Stays in Weekends Boxplot')

#StaysinWeekNights
hist(cityDF$StaysInWeekNights,xlim=c(0,15),ylim=c(0,30000),breaks=50,main='City Stays in Weeknights Histogram')
boxplot(cityDF$StaysInWeekNights,ylim=c(0,15),horizontal=TRUE,range=0,varwidth=TRUE,main='City Stays in Weeknights Boxplot')

#TotalSpecialRequests
hist(cityDF$TotalOfSpecialRequests,ylim=c(0,50000),breaks=20,main='City Total Special Requests Histogram')
boxplot(cityDF$TotalOfSpecialRequests,horizontal=TRUE,range=0,varwidth=TRUE,main='City Total Special Requests Boxplot')

#ArrivalYear
hist(cityDF$ArrivalYear,xlim=c(2014,2018),ylim=c(0,20000),breaks=10,main='City Arrival Year Histogram')
boxplot(cityDF$ArrivalYear,ylim=c(2014,2018),horizontal=TRUE,range=0,varwidth=TRUE,main='City Arrival Year Boxplot')

#ArrivalMonth
hist(cityDF$ArrivalMonth,xlim=c(1,12),ylim=c(0,6000),breaks=12,main='City Arrival Month Histogram')
boxplot(cityDF$ArrivalMonth,ylim=c(1,12),horizontal=TRUE,range=0,varwidth=TRUE,main='City Arrival Month Boxplot')

#ArrivalDay
hist(cityDF$ArrivalDay,xlim=c(1,31),ylim=c(0,10000),breaks=5,main='City Arrival Day Histogram')
boxplot(cityDF$ArrivalDay,ylim=c(1,31),horizontal=TRUE,range=0,varwidth=TRUE,main='City Arrival Day Boxplot')

#SumVisitors
hist(cityDF$SumVisitors,xlim=c(0,5),ylim=c(0,60000),breaks=20,main='City Total Visitors Histogram')

#ARS (excluding zeros)
hist(cityDF$AverageRevenuePerStay[cityDF$AverageRevenuePerStay!=0],xlim=c(0,2000),ylim=c(0,6000),breaks=100,main='City Average Revenue per Stay (excluding Zeros) Histogram')
boxplot(cityDF$AverageRevenuePerStay[cityDF$AverageRevenuePerStay!=0],ylim=c(0,2000),horizontal=TRUE,range=0,varwidth=TRUE,main='City Average Revenue per Stay (excluding Zeros) Boxplot')

#resortDF Histograms and Boxplots of Numeric Variables

#ADR
hist(resortDF$ADR,xlim=c(0,250),ylim=c(0,4000),breaks=100,main='Resort ADR Histogram')
boxplot(resortDF$ADR,ylim=c(0,250),range=0,horizontal=TRUE,varwidth=TRUE,main='Resort ADR Boxplot')

#LeadTime
hist(resortDF$LeadTime,ylim=c(0,20000),xlim=c(0,400),breaks=25,main='Resort Lead Time Histogram')
boxplot(resortDF$LeadTime,ylim=c(0,400),horizontal=TRUE,range=0,varwidth=TRUE,main='Resort Lead Time Boxplot')

#StaysinWeekendNights
hist(resortDF$StaysInWeekendNights,xlim=c(0,6),ylim=c(0,20000),breaks=100,main='Resort Stays in Weekend Nights Histogram')
boxplot(resortDF$StaysInWeekNights,ylim=c(0,6),horizontal=TRUE,range=0,varwidth=TRUE,main='Resort Stays in Weekends Boxplot')

#StaysinWeekNights
hist(resortDF$StaysInWeekNights,xlim=c(0,10),ylim=c(0,15000),breaks=50,main='Resort Stays in Weeknights Histogram')
boxplot(resortDF$StaysInWeekNights,ylim=c(0,10),horizontal=TRUE,range=0,varwidth=TRUE,main='Resort Stays in Weeknights Boxplot')

#TotalSpecialRequests
hist(resortDF$TotalOfSpecialRequests,ylim=c(0,25000),breaks=20,main='Resort Total Special Requests Histogram')
boxplot(resortDF$TotalOfSpecialRequests,horizontal=TRUE,range=0,varwidth=TRUE,main='Resort Total Special Requests Boxplot')

#ArrivalYear
hist(resortDF$ArrivalYear,xlim=c(2014,2018),ylim=c(0,20000),breaks=10,main='Resort Arrival Year Histogram')
boxplot(resortDF$ArrivalYear,ylim=c(2014,2018),horizontal=TRUE,range=0,varwidth=TRUE,main='Resort Arrival Year Boxplot')

#ArrivalMonth
hist(resortDF$ArrivalMonth,xlim=c(1,12),ylim=c(0,6000),breaks=12,main='Resort Arrival Month Histogram')
boxplot(resortDF$ArrivalMonth,ylim=c(1,12),horizontal=TRUE,range=0,varwidth=TRUE,main='Resort Arrival Month Boxplot')

#ArrivalDay
hist(resortDF$ArrivalDay,xlim=c(1,31),ylim=c(0,10000),breaks=5,main='Resort Arrival Day Histogram')
boxplot(resortDF$ArrivalDay,ylim=c(1,31),horizontal=TRUE,range=0,varwidth=TRUE,main='Resort Arrival Day Boxplot')

#SumVisitors
hist(resortDF$SumVisitors,xlim=c(0,5),ylim=c(0,40000),breaks=100,main='Resort Total Visitors Histogram')

#ARS (excluding zeros)
hist(resortDF$AverageRevenuePerStay[resortDF$AverageRevenuePerStay!=0],xlim=c(0,2000),ylim=c(0,8000),breaks=100,main='Resort Average Revenue per Stay (excluding Zeros) Histogram')
boxplot(resortDF$AverageRevenuePerStay[resortDF$AverageRevenuePerStay!=0],ylim=c(0,1000),horizontal=TRUE,range=0,varwidth=TRUE,main='Resort Average Revenue per Stay (excluding Zeros) Boxplot')

#2b
###Tables and Bargraphs of Categorical Variables
cityDF$ReservedRoomType<-as.factor(cityDF$ReservedRoomType)
cityDF$VisitorType[cityDF$VisitorType=='NULL']<-NA
cityDF$VisitorType<-as.factor(cityDF$VisitorType)
shortcityDF$Season<-as.factor(shortcityDF$Season)
cityDF$CustomerType<-as.factor(cityDF$CustomerType)
cityDF$Agent<-as.factor(cityDF$Agent)
cityDF$Company<-as.factor(cityDF$Company)
cityDF$DepositType<-as.factor(cityDF$DepositType)
cityDF$AssignedRoomType<-as.factor(cityDF$AssignedRoomType)
cityDF$DistributionChannel<-as.factor(cityDF$DistributionChannel)
cityDF$MarketSegment<-as.factor(cityDF$MarketSegment)
cityDF$Country<-as.factor(cityDF$Country)
cityDF$Meal<-as.factor(cityDF$Meal)
cityDF$ReservationStatus<-as.factor(cityDF$ReservationStatus)
cityDF$IsCanceled<-as.factor(cityDF$IsCanceled)
cityDF$IsRepeatedGuest<-as.factor(cityDF$IsRepeatedGuest)
resortDF$ReservedRoomType<-as.factor(resortDF$ReservedRoomType)
resortDF$VisitorType<-as.factor(resortDF$VisitorType)
resortDF$Season<-as.factor(resortDF$Season)
resortDF$CustomerType<-as.factor(resortDF$CustomerType)
resortDF$Agent<-as.factor(resortDF$Agent)
resortDF$Company<-as.factor(resortDF$Company)
resortDF$DepositType<-as.factor(resortDF$DepositType)
resortDF$AssignedRoomType<-as.factor(resortDF$AssignedRoomType)
resortDF$DistributionChannel<-as.factor(resortDF$DistributionChannel)
resortDF$MarketSegment<-as.factor(resortDF$MarketSegment)
resortDF$Country<-as.factor(resortDF$Country)
resortDF$Meal<-as.factor(resortDF$Meal)
resortDF$ReservationStatus<-as.factor(resortDF$ReservationStatus)
resortDF$IsCanceled<-as.factor(resortDF$IsCanceled)
resortDF$IsRepeatedGuest<-as.factor(resortDF$IsRepeatedGuest)

#cityDF

#Agent
table(cityDF$Agent)

#ReservedRoomType
table(cityDF$ReservedRoomType)
ggplot(cityDF,aes(x=ReservedRoomType))+geom_bar()

#VisitorType
table(cityDF$VisitorType)
ggplot(subset(cityDF,!is.na(VisitorType)),aes(x=VisitorType))+geom_bar()

#Season
table(shortcityDF$Season)
ggplot(shortcityDF,aes(x=Season))+geom_bar()

#CustomerType
table(cityDF$CustomerType)
ggplot(cityDF,aes(x=CustomerType))+geom_bar()

#Company
table(cityDF$Company)

#DepositType
table(cityDF$DepositType)
ggplot(cityDF,aes(x=DepositType))+geom_bar()

#AssignedRoomType
table(cityDF$AssignedRoomType)
ggplot(cityDF,aes(x=AssignedRoomType))+geom_bar()

#DistributionChannel
table(cityDF$DistributionChannel)
ggplot(cityDF,aes(x=DistributionChannel))+geom_bar()

#MarketSegment
table(cityDF$MarketSegment)
ggplot(cityDF,aes(x=MarketSegment))+geom_bar()+theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1))

#Country
cityDF$Country<-as.character(cityDF$Country)
cityDF$Country[cityDF$Country=='CN']<-'CAN'
cityDF$Country<-as.factor(cityDF$Country)
cityDF$Country<-countrycode(cityDF$Country,origin='iso3c',destination='country.name.en')
table(cityDF$Country)

#Meal
table(cityDF$Meal)
ggplot(cityDF,aes(x=Meal))+geom_bar()

#ReservationStatus
table(cityDF$ReservationStatus)
ggplot(cityDF,aes(x=ReservationStatus))+geom_bar()

#IsCancelled
table(cityDF$IsCanceled)
ggplot(cityDF,aes(x=IsCanceled))+geom_bar()

#IsRepeatedGuest
table(cityDF$IsRepeatedGuest)
ggplot(cityDF,aes(x=IsRepeatedGuest))+geom_bar()

#resortDF

#Agent
table(resortDF$Agent)

#ReservedRoomType
table(resortDF$ReservedRoomType)
ggplot(resortDF,aes(x=ReservedRoomType))+geom_bar()

#VisitorType
table(resortDF$VisitorType)
ggplot(resortDF,aes(x=VisitorType))+geom_bar()

#Season
table(resortDF$Season)
ggplot(resortDF,aes(x=Season))+geom_bar()

#CustomerType
table(resortDF$CustomerType)
ggplot(resortDF,aes(x=CustomerType))+geom_bar()

#Company
table(resortDF$Company)

#DepositType
table(resortDF$DepositType)
ggplot(resortDF,aes(x=DepositType))+geom_bar()

#AssignedRoomType
table(resortDF$AssignedRoomType)
ggplot(resortDF,aes(x=AssignedRoomType))+geom_bar()

#DistributionChannel
table(resortDF$DistributionChannel)
ggplot(resortDF,aes(x=DistributionChannel))+geom_bar()

#MarketSegment
table(resortDF$MarketSegment)
ggplot(resortDF,aes(x=MarketSegment))+geom_bar()+theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1))

#Country
resortDF$Country<-as.character(resortDF$Country)
resortDF$Country[resortDF$Country=='CN']<-'CAN'
resortDF$Country<-as.factor(resortDF$Country)
resortDF$Country<-countrycode(resortDF$Country,origin='iso3c',destination='country.name.en')
table(resortDF$Country)

#Meal
table(resortDF$Meal)
ggplot(resortDF,aes(x=Meal))+geom_bar()

#ReservationStatus
table(resortDF$ReservationStatus)
resortDF$ReservationStatus<-replace(resortDF$ReservationStatus,resortDF$ReservationStatus=='`',NA)
ggplot(subset(resortDF,!is.na(ReservationStatus)),aes(x=ReservationStatus))+geom_bar(na.rm=TRUE)

#IsCancelled
table(resortDF$IsCanceled)
ggplot(resortDF,aes(x=IsCanceled))+geom_bar()

#IsRepeatedGuest
table(resortDF$IsRepeatedGuest)
ggplot(resortDF,aes(x=IsRepeatedGuest))+geom_bar()

#2c
###Boxplots of ADR Grouped with Various Categorical Variables

#cityDF

#ADR across different Customer Types
ggplot(cityDF,aes(x=CustomerType,y=ADR))+geom_boxplot(fill='white')+coord_cartesian(ylim=c(0,300))

#ADR across different Meal Plans
ggplot(cityDF,aes(x=Meal,y=ADR))+geom_boxplot(fill='white')+coord_cartesian(ylim=c(0,300))
ggplot(cityDF,aes(x=Meal,y=ADR))+geom_bar(stat='identity')
#ADR across different Reserved Room Type
ggplot(cityDF,aes(x=ReservedRoomType,y=ADR))+geom_boxplot(fill='white')+coord_cartesian(ylim=c(0,400))

#ADR across different Deposit Types
ggplot(cityDF,aes(x=DepositType,y=ADR))+geom_boxplot(fill='white')+coord_cartesian(ylim=c(0,300))

#ADR across different Distribution Channels
ggplot(cityDF,aes(x=DistributionChannel,y=ADR))+geom_boxplot(fill='white')+coord_cartesian(ylim=c(0,300))

#ADR across Repeated Guests
ggplot(cityDF,aes(x=IsRepeatedGuest,y=ADR))+geom_boxplot(fill='white')+coord_cartesian(ylim=c(0,250))

#ADR across different Market Segments
ggplot(cityDF,aes(x=MarketSegment,y=ADR))+geom_boxplot(fill='white')+theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1))+coord_cartesian(ylim=c(0,250))

#ADR across different Seasons
ggplot(cityDF,aes(x=Season,y=ADR))+geom_boxplot(fill='white')+coord_cartesian(ylim=c(0,250))

#ADR across different Visitor Types
ggplot(cityDF,aes(x=VisitorType,y=ADR))+geom_boxplot(fill='white')+coord_cartesian(ylim=c(0,250))

#resortDF

#ADR across different Customer Types
ggplot(resortDF,aes(x=CustomerType,y=ADR))+geom_boxplot(fill='white')+coord_cartesian(ylim=c(0,300))

#ADR across different Meal Plans
ggplot(resortDF,aes(x=Meal,y=ADR))+geom_boxplot(fill='white')+coord_cartesian(ylim=c(0,300))

#ADR across different Reserved Room Type
ggplot(resortDF,aes(x=ReservedRoomType,y=ADR))+geom_boxplot(fill='white')+coord_cartesian(ylim=c(0,400))

#ADR across different Deposit Types
ggplot(resortDF,aes(x=DepositType,y=ADR))+geom_boxplot(fill='white')+coord_cartesian(ylim=c(0,200))

#ADR across different Distribution Channels
ggplot(resortDF,aes(x=DistributionChannel,y=ADR))+geom_boxplot(fill='white')+coord_cartesian(ylim=c(0,300))

#ADR across Repeated Guests
ggplot(resortDF,aes(x=IsRepeatedGuest,y=ADR))+geom_boxplot(fill='white')+coord_cartesian(ylim=c(0,250))

#ADR across different Market Segments
ggplot(resortDF,aes(x=MarketSegment,y=ADR))+geom_boxplot(fill='white')+theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1))+coord_cartesian(ylim=c(0,200))

#ADR across different Seasons
ggplot(resortDF,aes(x=Season,y=ADR))+geom_boxplot(fill='white')+coord_cartesian(ylim=c(0,250))

#ADR across different Visitor Types
ggplot(resortDF,aes(x=VisitorType,y=ADR))+geom_boxplot(fill='white')+coord_cartesian(ylim=c(0,250))

#3
###Number of Visitors from each Country Portrayed on a World Map

world_map<-map_data('world')
world_map$region[world_map$region=='USA']<-'United States'
world_map$region[world_map$region=='UK']<-'United Kingdom'
world_map$region[world_map$region=='Bosnia and Herzegovina']<-'Bosnia & Herzegovina'
world_map$region[world_map$region=='Ivory Coast']<-'Côte d’Ivoire'
world_map$region[world_map$region=='Czech Republic']<-'Czechia'
world_map$region[world_map$region=='Myanmar']<-'Myanmar (Burma)'
world_map$region[world_map$region=='Sao Tome and Principe']<-'São Tomé & Príncipe'

#cityDF
cityDF$Country<-as.character(cityDF$Country)

uniq_countries1<-count(cityDF$Country)

colnames(uniq_countries1)<-c('Country','Count')

uniq_countries1$count_group<-cut(uniq_countries1$Count, 
                                 breaks = c(-Inf, 200, 600, 1000, 2000, Inf), 
                                 labels = c("Less than 200", "600-200", "1000-600", "2000-1000", "More than 2000"))
library(ggplot2)
library(RColorBrewer)
library(forcats)
ggplot(uniq_countries1)+
  geom_map(aes(map_id=uniq_countries1$Country,fill=fct_rev(uniq_countries1$count_group)),map=world_map)+
  geom_polygon(data=world_map,aes(x=world_map$long,y=world_map$lat,group=world_map$group),color='black',fill=NA)+
  expand_limits(x=world_map$long,y=world_map$lat)+
  scale_fill_manual(name="Number of Visitors",values=rev(brewer.pal(5,name="BuPu")))+
  theme_void()+coord_fixed()

#resortDF

resortDF$Country<-as.character(resortDF$Country)

uniq_countries2<-count(resortDF$Country)

colnames(uniq_countries2)<-c('Country','Count')

uniq_countries2$count_group<-cut(uniq_countries2$Count, 
                                 breaks = c(-Inf, 200, 600, 1000, 2000, Inf), 
                                 labels = c("Less than 200", "600-200", "1000-600", "2000-1000", "More than 2000"))

ggplot(uniq_countries2)+
  geom_map(aes(map_id=uniq_countries2$Country,fill=fct_rev(uniq_countries2$count_group)),map=world_map)+
  geom_polygon(data=world_map,aes(x=world_map$long,y=world_map$lat,group=world_map$group),color='black',fill=NA)+
  expand_limits(x=world_map$long,y=world_map$lat)+
  scale_fill_manual(name="Number of Visitors",values=rev(brewer.pal(5,name="BuPu")))+
  theme_void()+coord_fixed()

#4


library(tidyverse)
library(arules)
library(dplyr)

#cityDF Apriori Rules
city_subset <- subset(cityDF,select=c(-1,-3,-4))
city_subset <-city_subset %>%
  mutate_if(is.character,funs(as.factor)) 
warnings()
str(city_subset)
city_transactions<- as(city_subset,"transactions")

rules<- apriori(city_transactions,parameter=list(supp=.1615, conf=.99, maxlen=5),appearance=list(default="lhs",rhs="ReservationStatus=Canceled"))
inspect(rules)
appearance=list(default="lhs",rhs="ReservationStatus=Canceled")
View(city_subset)

#resortDF Apriori Rules
resort_subset <- subset(resortDF,select=c(-1,-3,-4))
resort_subset <-resort_subset %>%
  mutate_if(is.character,funs(as.factor)) 
str(resort_subset)
report_transactions <- as(resort_subset,"transactions")
warnings()

rules2<- apriori(report_transactions,parameter=list(supp=.04, conf=.95, maxlen=5),appearance=list(default="lhs",rhs="ReservationStatus=Canceled"))
inspect(rules2)
appearance=list(default="lhs",rhs="ReservationStatus=Canceled")
View(resort_subset)

inspect(rules)
inspect(rules2)

#5
#cityDF Linear Models
str(shortcityDF,1)
shortcityDF$Season<-as.character(shortcityDF$Season)
shortcityDF<-as.data.frame(shortcityDF)
str(shortcityDF)
citymodel1 <- lm(formula = AverageRevenuePerStay ~ StaysInWeekendNights + StaysInWeekNights + ArrivalMonth , data = shortcityDF)
summary(citymodel1)

citymodel2 <- lm(formula = AverageRevenuePerStay ~ StaysInWeekendNights + StaysInWeekNights + Children + Adults + Babies+ ArrivalMonth, data = shortcityDF)
summary(citymodel2)

citymodel3 <- lm(formula = AverageRevenuePerStay ~ StaysInWeekendNights + StaysInWeekNights + Adults + Children + Babies + IsRepeatedGuest + BookingChanges + DaysInWaitingList + TotalOfSpecialRequests + ArrivalMonth, data = shortcityDF)
summary(citymodel3)

citymodel4 <- lm(formula = IsCanceled ~  IsRepeatedGuest + PreviousCancellations + DaysInWaitingList + ArrivalMonth + AverageRevenuePerStay, data = shortcityDF)
summary(citymodel4)

#resortDF Linear Models
str(resortDF)
resortDF$IsCanceled<-as.numeric(resortDF$IsCanceled)
resortDF$IsRepeatedGuest<-as.numeric(resortDF$IsRepeatedGuest)
resortDF$Season<-as.character(resortDF$Season)
resortmodel1 <- lm(formula = AverageRevenuePerStay ~ StaysInWeekendNights + StaysInWeekNights + Children + Adults + Babies+ ArrivalMonth, data = resortDF)
summary(resortmodel1)

resortmodel2 <- lm(formula = IsCanceled ~  IsRepeatedGuest + PreviousCancellations + DaysInWaitingList + ArrivalMonth +  AverageRevenuePerStay, data = resortDF)
summary(resortmodel2)

resortmodel3 <- lm(formula = AverageRevenuePerStay ~ StaysInWeekendNights + StaysInWeekNights + Adults + Children + Babies + IsRepeatedGuest + BookingChanges + DaysInWaitingList + TotalOfSpecialRequests + ArrivalMonth, data = resortDF)
summary(resortmodel3)

#6 SVM

#cityDF SVM

svmcityDF<-data.frame(IsCanceled=as.factor(shortcityDF$IsCanceled),
                      LeadTime=shortcityDF$LeadTime,
                      StaysInWeekendNights=shortcityDF$StaysInWeekendNights,
                      StaysinWeekNights=shortcityDF$StaysInWeekNights,
                      Adults=shortcityDF$Adults,
                      Children=shortcityDF$Children,
                      Babies=shortcityDF$Babies,
                      RepeatedGuest=shortcityDF$IsRepeatedGuest,
                      PreviousCancellations=shortcityDF$PreviousCancellations,
                      PreviousBookingsNotCanceled=shortcityDF$PreviousBookingsNotCanceled,
                      BookingChanges=shortcityDF$BookingChanges,
                      DaysInWaitingList=shortcityDF$DaysInWaitingList,
                      ADR=shortcityDF$ADR,
                      RequiredCarParkingSpaces=shortcityDF$RequiredCarParkingSpaces,
                      TotalOfSpecialRequests=shortcityDF$TotalOfSpecialRequests,
                      SumVisitors=shortcityDF$SumVisitors,
                      AverageRevenuePerStay=shortcityDF$AverageRevenuePerStay,
                      ArrivalMonth=shortcityDF$ArrivalMonth)

trainList<-createDataPartition(y=svmcityDF$IsCanceled, p=.60, list=FALSE)
str(trainList)
trainSet<-svmcityDF[trainList,]
testSet<-svmcityDF[-trainList,]
na.omit(testSet)->testSet
trainList<-as.data.frame(trainList)
svmOut<-ksvm(IsCanceled~., data=trainSet, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
svmPred<- predict(svmOut,testSet)

table(svmPred,testSet$IsCanceled)
confusionMatrix(svmPred,testSet$IsCanceled)

#RESORT
svmresortDF<-data.frame(IsCanceled=as.factor(resortDF$IsCanceled),
                        LeadTime=resortDF$LeadTime,
                        StaysInWeekendNights=resortDF$StaysInWeekendNights,
                        StaysinWeekNights=resortDF$StaysInWeekNights,
                        Adults=resortDF$Adults,
                        Children=resortDF$Children,
                        Babies=resortDF$Babies,
                        RepeatedGuest=resortDF$IsRepeatedGuest,
                        PreviousCancellations=resortDF$PreviousCancellations,
                        PreviousBookingsNotCanceled=resortDF$PreviousBookingsNotCanceled,
                        BookingChanges=resortDF$BookingChanges,
                        DaysInWaitingList=resortDF$DaysInWaitingList,
                        ADR=resortDF$ADR,
                        RequiredCarParkingSpaces=resortDF$RequiredCarParkingSpaces,
                        TotalOfSpecialRequests=resortDF$TotalOfSpecialRequests,
                        SumVisitors=resortDF$SumVisitors,
                        AverageRevenuePerStay=resortDF$AverageRevenuePerStay,
                        ArrivalMonth=resortDF$ArrivalMonth)

trainList1<-createDataPartition(y=svmresortDF$IsCanceled, p=.60, list=FALSE)
trainSet1<-svmresortDF[trainList1,]
testSet1<-svmresortDF[-trainList1,]
na.omit(testSet1)->testSet1
trainList1<-as.data.frame(trainList1)
svmOut1<-ksvm(IsCanceled~., data=trainSet1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
svmPred1<- predict(svmOut1,testSet1)

table(svmPred1,testSet1$IsCanceled)
confusionMatrix(svmPred1,testSet1$IsCanceled)

#NEURAL NET

library(neuralnet)

cityDF$IsCanceled<-as.numeric(cityDF$IsCanceled)
cityDF$IsRepeatedGuest<-as.numeric(cityDF$IsRepeatedGuest)
NNcityDF<-data.frame(IsCanceled=as.factor(cityDF$IsCanceled),
                     LeadTime=cityDF$LeadTime/100,
                     StaysInWeekendNights=cityDF$StaysInWeekendNights,
                     StaysinWeekNights=cityDF$StaysInWeekNights,
                     Adults=cityDF$Adults,
                     Children=cityDF$Children,
                     Babies=cityDF$Babies,
                     RepeatedGuest=cityDF$IsRepeatedGuest,
                     PreviousCancellations=cityDF$PreviousCancellations,
                     PreviousBookingsNotCanceled=cityDF$PreviousBookingsNotCanceled,
                     BookingChanges=cityDF$BookingChanges,
                     DaysInWaitingList=cityDF$DaysInWaitingList,
                     ADR=cityDF$ADR/100,
                     RequiredCarParkingSpaces=cityDF$RequiredCarParkingSpaces,
                     TotalOfSpecialRequests=cityDF$TotalOfSpecialRequests,
                     SumVisitors=cityDF$SumVisitors,
                     AverageRevenuePerStay=cityDF$AverageRevenuePerStay)
NNcityDF<-NNcityDF[40061:79330,]
str(NNcityDF)
CITYNN1<-neuralnet(formula=IsCanceled~., data=NNcityDF,
                   hidden=c(1,1), act.fct="logistic")
summary(CITYNN1)
predict(CITYNN1, NNcityDF)->NNprediction
NNprediction<-as.data.frame(NNprediction)
NNprediction<-round(NNprediction,0)
sum(NNprediction[,1])
sum(NNprediction[,2])
plot(CITYNN1)

#BAYESGLM
train(IsCanceled~., data=NNcityDF, method="bayesglm")->Last1
summary(Last1)
predict(Last1,NNcityDF)->Lastpred
Lastpred
count(Lastpred)

#BAYESGLM WITH SIGNIFICANCE
train(IsCanceled~PreviousCancellations+PreviousBookingsNotCanceled+
        ADR+AverageRevenuePerStay, data=NNcityDF, method="bayesglm")->Last2
summary(Last2)
predict(Last2,NNcityDF)->Lastpred2
count(Lastpred2)
sum(resortDF[,1])
