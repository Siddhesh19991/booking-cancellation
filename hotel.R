hotel<- read.csv("~/Desktop/hotel_bookings.csv")


hotel$is_canceled<-as.factor(hotel$is_canceled)
hotel$reservation_status_date<- as.Date(hotel$reservation_status_date)
hotel[sapply(hotel,is.character)]<- lapply(hotel[sapply(hotel, is.character)], as.factor)
hotel$is_repeated_guest<-as.factor(hotel$is_repeated_guest)


hotel<-hotel[,-4]
hotel<-hotel[,-5]
hotel<-hotel[,-30]
hotel$company=NULL

library(dplyr)
hotel<-distinct(hotel)


hotel$children<-ifelse(is.na(hotel$children),0,hotel$children)
library(ggplot2)

hotel<-subset(hotel,lead_time<349.5)
hotel$lead_time<-sqrt(hotel$lead_time)
hotel<-subset(hotel,adr<199)
hotel<-subset(hotel,stays_in_week_nights<6)
hotel<-subset(hotel,stays_in_weekend_nights<4)
hotel<-subset(hotel,adults<5)
hotel<-subset(hotel,babies<8)
hotel<-subset(hotel,previous_cancellations<4)
hotel<-subset(hotel,previous_bookings_not_canceled<7)
hotel<-subset(hotel,booking_changes<5)
hotel<-subset(hotel,hotel$agent!="NULL")
hotel<-subset(hotel,required_car_parking_spaces<2)
hotel<-subset(hotel,total_of_special_requests<4)
hotel<-subset(hotel,days_in_waiting_list<50)
hotel<-hotel[,-28]
library(caret)
intrain<-createDataPartition(y=hotel$is_canceled,p=0.7,list = FALSE)
train<-hotel[intrain,]
test<-hotel[-intrain,]
library(ROSE)
#imbalance in data 
#a<-ovun.sample(is_canceled~.,data=train,method = "under",N=46400)
#under<-a$data
#accuracy reduced


library(party)

model1<-ctree(is_canceled~.,data=train)
check<-predict(model1,train)

pred<-predict(model1,newdata=test)
eva<-confusionMatrix(pred,test$is_canceled)#84% accuracy
