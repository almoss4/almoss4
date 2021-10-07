library(readxl)
library(kernlab)
library(caret)
library(dplyr)
library(e1071)
library(readxl)
library(xlsx)
cityDF<- read_excel("~/Downloads/H2-City (1).xlsx")
resortDF <- read_excel("~/Downloads/H1-Resort (2).xlsx")

#ADDING SEASON TO CITY DATA FRAME
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

#
#
#
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

#
##
#
#
#
#
#
#
#
##
#
#
#PART 6
#SV<
#For City
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
#CityNeural Net Model
NNcityDF<-data.frame(IsCanceled=as.factor(cityDF$IsCanceled),
                     LeadTime=cityDF$LeadTime,
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
                     ADR=cityDF$ADR,
                     RequiredCarParkingSpaces=cityDF$RequiredCarParkingSpaces,
                     TotalOfSpecialRequests=cityDF$TotalOfSpecialRequests,
                     SumVisitors=cityDF$SumVisitors,
                     AverageRevenuePerStay=cityDF$AverageRevenuePerStay)
NNcityDF<-NNcityDF[40061:79330,]
head(NNcityDF)
set.seed(9)
neuralnet(IsCanceled~., data=NNcityDF,hidden=1, act.fct="logistic")->CITYNN
predict(CITYNN,NNcityDF)->NNprediction
NNprediction<-round(NNprediction,0)
sum(NNprediction[,1])
sum(NNprediction[,2])

