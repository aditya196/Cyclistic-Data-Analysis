#3
library(tidyverse)
library(janitor)
library(skimr)
library(here)
library(lubridate)
library(ggrepel)
library(tidyr)
library(dplyr)
library(readODS)
library(DescTools)
library(quantreg)
library(gitcreds)
library(credentials)
raw_data2 <- read_ods('C:/Users/adity/Downloads/Coursera Certificates DA/Case study 1/Raw Data/Divvy_Trips_2018_Q4/DTQ42018editable.ods')

## Fetch data
head(raw_data2)
glimpse(raw_data2)

##Next step is to clean data. 
## Many records do not have a gender associated to them. Replace blank values with "Unknown in the excel file"

##create a new column called ridetime and add weekday column

raw_data2 <- mutate(raw_data2,ridetime = end_time-start_time)
raw_data2$weekday1 <- wday(raw_data2$start_time, label=TRUE, abbr=FALSE)
clean_data$weekday2 <- wday(clean_data$start_time, label=TRUE, abbr=FALSE)



clean_data$hourofday <- hour(clean_data$start_time)
##The weekday column is of "Ord" data type. Convert it into chr
raw_data2$weekday1 <- as.character(raw_data2$weekday1)

##Next task would be to fetch records which have same start and end station id
same_start_end <- raw_data2 %>% filter(from_station_id==to_station_id)

mean_same_start_end <- summarise(same_start_end,avg_same_station_trip = mean(ridetime))
print(mean_same_start_end)


SD_same_start_end <- summarise(same_start_end,avg_same_station_trip = sd(tripduration))
print(SD_same_start_end)

same_start_end2 <- raw_data2 %>% filter(from_station_id==to_station_id,ridetime<5)

## for a  ride starting and ending at the same station to be relevant, it would need to be of at least 5 mins. 
## This is an assumption done to clear out the noise.

clean_data <- anti_join(raw_data2, same_start_end2)

#Ensuring that ridetime is greater than 0. Previously there were 7 records with ridetime less than 0. Total records is 640673
clean_data <- subset(clean_data,ridetime>0)
#Now the cleaned data contains all the relevant and required data for analysis. 

#4
## Calculate overall average ride time 
avg_ride_time <- clean_data %>% summarise(avg_ride_time = mean(ridetime))
print(avg_ride_time)

avg_ride_times_usertype <- clean_data %>% group_by(usertype) %>% 
    summarise(avg_ride_time = mean(ridetime))

print(avg_ride_times_usertype)

avg_ride_time_by_u <- ggplot(data = avg_ride_times_usertype, aes(x = usertype, y = avg_ride_time, fill = usertype)) +
  geom_bar(stat = "identity", width = 0.5, color = "black") +
  labs(x = "User Type", y = "Average Ride Time (minutes)", title = "Average Ride Time by User Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )
print(avg_ride_time_by_u)

num_rides_by_usertype <- count(clean_data, usertype)
num_rides_by_usertype2 <- mutate(num_rides_by_usertype,n/1000)
print(num_rides_by_usertype2)

total_rides_by_u <- ggplot(data = num_rides_by_usertype2,aes(x=usertype,y=n/1000,fill=usertype) )+
  geom_bar(stat="identity",width=0.5,color="black")  +
  labs(x = "User Type",y="Number Of Rides (In Thousands)",title = "Total Rides by Usertype")+
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )
print(total_rides_by_u)
##---------------------------------------------------------------------------------------------------------
# Analyzing our results , we can see that the customers have a significantly higher average ride time than than the subscribers , 
#while having significantly less number of rides.
#this could be due to following reasons . 1. Customers who are yet to become subscribers have less access to the bike station and thus are only using it for 
# less number of times and long commutes. In this case number of bike stations need to be increased. Since most customers prefer long rides, they could be using other 
# modes of transportation to cover longer distances. 

##-------------------------------------------------
##top customer start station rankings

top_cstarts <- clean_data %>% 
  filter(usertype=="Customer") %>% 
  group_by(from_station_name) %>% 
  summarise(Total= n()) %>% 
  arrange(desc(Total)) %>% 
  slice(1:5)

print(top_cstarts)

##top customer end rankings
top_cends <- clean_data %>% 
  filter(usertype=="Customer") %>% 
  group_by(to_station_name) %>% 
  summarise(Total= n()) %>% 
  arrange(desc(Total)) %>% 
  slice(1:5)

print(top_cends)
##---------------------------------------------------------------------------------------------------------
##top subscriber start station rankings
top_substarts <- clean_data %>% 
  filter(usertype=="Subscriber") %>% 
  group_by(from_station_name) %>% 
  summarise(Total= n()) %>% 
  arrange(desc(Total)) %>% 
  slice(1:5)

print(top_substarts)

##top subscriber end station rankings
top_subends <- clean_data %>% 
  filter(usertype=="Subscriber") %>% 
  group_by(to_station_name) %>% 
  summarise(Total= n()) %>% 
  arrange(desc(Total)) %>% 
  slice(1:5)

print(top_subends)
##---------------------------------------------------------------------------------------------------------

##---------------------------------------------------------------------------------------------------------

##---------------------------------------------------------------------------------------------------------
## Average ride time On weekday basis


##---------------------------------------------------------------------------------------------------------

##Number of rides based on weekday 
num_rides_weekday_c <- clean_data %>% 
  filter(usertype=="Customer") %>% 
  summarise(rides=n(),.by=c(weekday2,usertype))
print(num_rides_weekday_c)

ggplot(data = num_rides_weekday_c,aes(x=weekday2,y=rides,fill=rides))+
  geom_bar(stat="identity",width=0.5,color="black")  +
  
  labs(x = "Weekday",y="Total Rides",title = "Customer Rides Frequency")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_gradient(low = "blue", high = "red")+
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

##################
## Subscriber

num_rides_weekday_s <- clean_data %>% 
  filter(usertype=="Subscriber") %>% 
  summarise(rides=n(),.by=c(weekday2,usertype))
print(num_rides_weekday_s)

ggplot(data = num_rides_weekday_s,aes(x=weekday2,y=rides,fill=rides))+
  geom_bar(stat="identity",width=0.5,color="black")  +
  labs(x = "Weekday",y="Total Rides",title = "Subscriber Rides Frequency")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_gradient(low = "blue", high = "red")+
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

##---------------------------------------------------------------------------------------------------------
##---------------------------------------------------------------------------------------------------------

#Find Out most popular trips to and from particular stations
most_pop_trips <- clean_data %>% 
  summarise(numrides = n(),avg_rt = mean(ridetime),.by=c(from_station_id,to_station_id)) %>% 
  arrange(desc(numrides)) %>% 
  slice(1:5)
print(most_pop_trips)
##---------------------------------------------------------------------------------------------------------
##Hourly data
hr_data_cust <- clean_data %>% 
  filter(usertype=="Customer") %>% 
  group_by(hourofday) %>% 
  summarise(Totalrides = n())
print(hr_data_cust)


ggplot(data = hr_data_cust,aes(x=hourofday,y=Totalrides,fill=Totalrides))+
  geom_col()+
  labs(x = "Hour Of Day",y="Total Rides",title = "Total Rides each hour by Customers")+
  scale_fill_gradient(low = "green", high = "red")+
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

hr_data_sub <- clean_data %>% 
  filter(usertype=="Subscriber") %>% 
  group_by(hourofday) %>% 
  summarise(Totalrides = n())
print(hr_data_cust)


hr_rides_sub <- ggplot(data = hr_data_sub,aes(x=hourofday,y=Totalrides,fill=Totalrides))+
  geom_col()+
  labs(x = "Hour Of Day",y="Total Rides",title = "Total Rides each hour by Customers")+
  scale_fill_gradient(low = "green", high = "red")+
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

##---------------------------------------------------------------------------------------------------------

##Gender Distribution 
#Total Rides
gend_rides <- clean_data %>% 
  filter(gender == "Male" | gender == "Female") %>% 
  summarise(total_rides = n(),.by=c(gender,usertype))

print(gend_rides)

##Customer Gender Classification

gend_rides_cust2 <- clean_data %>% 
  filter((gender == "Male" | gender == "Female")&usertype=="Customer") %>% 
  summarise(total_rides = n(),.by=c(gender)) %>% 
  mutate(totals = sum(total_rides)) %>% 
  group_by(gender) %>% 
  summarise(total_percent = total_rides / totals) %>%
  mutate(labels = scales::percent(total_percent))

print(gend_rides_cust2)


gend_rides_cust3 <- clean_data %>% 
  filter(gender == "Male" & usertype == "Customer") %>% 
  summarise(Total = n(),.by=c(to_station_name)) %>% 
  arrange(desc(Total)) %>% 
  slice(1:5)

print(gend_rides_cust3)

gend_rides_sub3 <- clean_data %>% 
  filter(gender == "Male" & usertype == "Subscriber") %>% 
  summarise(Total = n(),.by=c(to_station_name)) %>% 
  arrange(desc(Total)) %>% 
  slice(1:5)

print(gend_rides_sub3)

  
  
  
colors <- c('#FFB6C1', '#87CEEB')
 

ggplot(data = gend_rides_cust2,aes(x="",y=total_percent,fill=gender))+
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values=colors,labels=c("Male","Female")) +
  theme_minimal() +
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  labs(title = "Gender Distribution of Customer Rides")


##Subscriber Gender Classification
gend_rides_sub2 <- clean_data %>% 
  filter((gender == "Male" | gender == "Female")&usertype=="Subscriber") %>% 
  summarise(total_rides = n(),.by=c(gender)) %>% 
  mutate(totals = sum(total_rides)) %>% 
  group_by(gender) %>% 
  summarise(total_percent = total_rides / totals) %>%
  mutate(labels = scales::percent(total_percent))

print(gend_rides_sub2)

ggplot(data = gend_rides_sub2,aes(x="",y=total_percent,fill=gender))+
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values=colors,labels=c("Male","Female")) +
  theme_minimal() +
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  labs(title = "Gender Distribution of Subscriber Rides") 



##---------------------------------------------------------------------------------------------------------
##Age based classification 
by_class <- clean_data %>% 
  filter(birthyear >=1800) 



by_class <- mutate(by_class,age = 2018-birthyear)
glimpse(by_class)  

by_class$age_group <- cut(by_class$age, breaks = c(15, 25, 35, 45, 55,Inf), labels = c("15-25", "25-35", "35-45", "45-55","55+"), right = FALSE)


by_class_s <- by_class %>% 
  filter(usertype=="Subscriber") %>% 
  summarise(total_rides = n(),.by=c(age_group)) %>% 
  mutate(totals = sum(total_rides)) %>%
  group_by(age_group) %>% 
  summarise(total_percent = total_rides / totals) %>%
  mutate(labels = scales::percent(total_percent))
print(by_class_s)

colors2 <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")


ggplot(data = by_class_s,aes(x="",y=total_percent,fill=age_group))+
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values=colors2,labels=c("15-25","25-35","35-45","45-55","55+")) +
  theme_minimal() +
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  labs(title = "Total Subscriber Rides by Age group")


by_class_c <- by_class %>% 
  filter(usertype=="Customer") %>% 
  summarise(total_rides = n(),.by=c(age_group)) %>% 
  mutate(totals = sum(total_rides)) %>%
  group_by(age_group) %>% 
  summarise(total_percent = total_rides / totals) %>%
  mutate(labels = scales::percent(total_percent))

ggplot(data = by_class_c,aes(x="",y=total_percent,fill=age_group))+
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values=colors2,labels=c("15-25","25-35","35-45","45-55","55+")) +
  theme_minimal() +
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  labs(title = "Total Customer Rides by Age group")
  

#------------------------------------------------------------------------------------  
count(clean_data, (gender != "Male" & gender != "Female"))
count(clean_data, (birthyear >= 1900 & birthyear <=2003  ))

#------------------------------------------------------------------------------------

# avg trip duration and start time for customers
avg_dur_per_hour_c <- clean_data %>% 
  filter(usertype=="Customer") %>% 
  summarise(avg_duration = mean(ridetime),.by=c(hourofday,usertype))

View(avg_dur_per_hour_c)

avg_tripTime_hrc <- ggplot(data = avg_dur_per_hour_c,aes(x=hourofday,y=avg_duration,fill=avg_duration))+
  geom_col()+
  
  labs(x = "Hour Of Day",y="Average Ride duration",title = "Average Ride Duration vs Time Customers")+
  scale_fill_gradient(low = "red", high = "green")+
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

print(avg_tripTime_hrc)

