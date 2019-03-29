# This program contains data clean-up and pre-processing for the Sun Country Airlines case

data <- read.csv("/Users/yanghaoying/Desktop/Job hunting/project/sun country/SC_Data.csv")
head(data)

# using the dplyr package for clean up and pre-processing 
install.packages("dplyr")
library(dplyr)

#Only keep records where we know the birthdate
data<-filter(data, !is.na(birthdateid))

#Only retain records where we know the gender
data$GenderCode<-as.character(data$GenderCode)
data<-filter(data, GenderCode!="")
data$GenderCode<-as.factor(data$GenderCode)

#Some odd age values...  replace with the median.
data$Age[data$Age < 0] <- median(data$Age)
data$Age[data$Age > 120] <- median(data$Age)

#  If don't have a reward number assign it a 0
data$UFlyRewardsNumber[is.na(data$UFlyRewardsNumber)]<-0

# construct a reward status factor variable.
data$UflyMemberStatus<-as.character(data$UflyMemberStatus)
data$UflyMemberStatus[data$UflyMemberStatus==''] <-"non-ufly"

# Discard duplicate records
data <- group_by(data, PNRLocatorID,CouponSeqNbr,PaxName,ServiceStartCity,ServiceEndCity,ServiceStartDate)
filter(data, n() == 1)

# Replace odd one off booking channels with 'Other'
data$BookingChannel<-as.character(data$BookingChannel)
data$BookingChannel[data$BookingChannel!="Outside Booking" & 
                      data$BookingChannel!="SCA Website Booking" & 
                      data$BookingChannel!="Tour Operator Portal" & 
                      data$BookingChannel!="Reservations Booking" & 
                      data$BookingChannel!="SY Vacation"] <- "Other"
data$BookingChannel<-as.factor(data$BookingChannel)

# Only keep records that involve SunCountry airlines tickets, for which MarketingAirlineCode=="SY".
data$MarketingAirlineCode<-as.character(data$MarketingAirlineCode)
data<-filter(data,MarketingAirlineCode=="SY")
data$MarketingAirlineCode<-as.factor(data$MarketingAirlineCode)


# Delete PNRs that have odd values and indicate an error.
data <- group_by(data, PNRLocatorID)
data <- mutate(data, error=ifelse(min(CouponSeqNbr)!=1,1,0))
filter(data, error==0)

# Create a unique customer ID by concatenating name, gender and birthday
data<-mutate(data, uid=paste(PaxName, GenderCode, birthdateid, sep =" "))

# Create Age buckets for age ranges, creating a new categorical variable "age_group" 
data<-mutate(data,age_group=(ifelse(Age>=0 & Age<18,"0-17",
                                    ifelse(Age>=18 & Age<25,"18-24",
                                           ifelse(Age>=25 & Age<35,"25-34",
                                                  ifelse(Age>=35 & Age<55,"35-55",
                                                         ifelse(Age>=55,"55+",0))))) ))

# create new variable as true origin city (source of first leg)
true_origins <- data%>%
  arrange(PNRLocatorID,CouponSeqNbr)%>% 
  group_by(PNRLocatorID,PaxName)%>% 
  do(data.frame(true_origin=first(.$ServiceStartCity)))

data<-merge(data,true_origins, by.x=c("PNRLocatorID","PaxName"),by.y = c("PNRLocatorID","PaxName"))

# final destination (target of last leg), 
final_destination<-data%>%
  arrange(PNRLocatorID,CouponSeqNbr)%>%
  group_by(PNRLocatorID,PaxName)%>%
  do(data.frame(final_destination=last(.$ServiceEndCity)))

data<-merge(data,final_destination, by.x=c("PNRLocatorID","PaxName"),by.y = c("PNRLocatorID","PaxName"))

# remove the true_origins and final_destination data frames
rm(true_origins)
rm(final_destination)

# use the lubridate package for operations involving date strings

install.packages("lubridate")
library(lubridate)

# "true" destination, city in which customer spent the most time
diff1<-data%>%arrange(PNRLocatorID,CouponSeqNbr)%>% 
  group_by(PNRLocatorID,PaxName)%>%
  mutate(stay=lead(date(ServiceStartDate))-date(ServiceStartDate),default=0)%>% 
  select(PNRLocatorID,PaxName,ServiceStartCity,ServiceEndCity,ServiceStartDate,stay)

diff1$stay[is.na(diff1$stay)]<-0 
diff1$stay<-as.numeric(diff1$stay) 
true_destination<-diff1%>%
  group_by(PNRLocatorID,PaxName)%>% 
  do(data.frame(true_destination=first(.$ServiceEndCity[.$stay==max(.$stay)])))

data<-merge(data,true_destination, by.x=c("PNRLocatorID","PaxName"),by.y = c("PNRLocatorID","PaxName"))

rm(diff1)
rm(true_destination)

# round-trip or one-way

data<-data%>%
  mutate(round_trip = ifelse(as.character(true_origin)==as.character(final_destination), 1, 0))

# size of the group

data<-data%>%
  group_by(PNRLocatorID)%>% 
  mutate(group_size = length(unique(uid)))

# Create a binary indicator "group" corresponding to whether it was a group or single party traveling.

data<-data%>%
  group_by(PNRLocatorID)%>% 
  mutate(group= ifelse(group_size>1,1,0))

# calendar quarter the trip took place in.
data<-mutate(data,mon=month(ServiceStartDate))
data<-mutate(data, Seasonality=(ifelse(mon>=1 &mon<=3,'Q1',
                                            ifelse(mon>=4 &mon<=6,"Q2",
                                                   ifelse(mon>=7 &mon<=9,"Q3",
                                                          ifelse(mon>=10 &mon<=12,'Q4',0))))))
data$mon=NULL
#How many days in advance was the trip booked
data$PNRCreateDate<-as.Date(data$PNRCreateDate) 
data$ServiceStartDate<-as.Date(data$ServiceStartDate) 
data<-data%>%
  mutate(days_pre_booked=as.numeric(floor(difftime(ServiceStartDate,PNRCreateDate,units=c("days")))))

write.csv(data, "SC_data_CleanedUp.csv")
