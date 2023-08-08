install.packages("tidyverse")
library("tidyverse")
install.packages("skimr")
library(skimr)

#uploading files
file_07_2022 <- read.csv("D:/Download/R Project 1/202207-divvy-tripdata.csv")
file_08_2022 <- read.csv("D:/Download/R Project 1/202208-divvy-tripdata.csv")
file_09_2022 <- read.csv("D:/Download/R Project 1/202209-divvy-tripdata.csv")
file_10_2022 <- read.csv("D:/Download/R Project 1/202210-divvy-tripdata.csv")
file_11_2022 <- read.csv("D:/Download/R Project 1/202211-divvy-tripdata.csv")
file_12_2022 <- read.csv("D:/Download/R Project 1/202212-divvy-tripdata.csv")
file_01_2023 <- read.csv("D:/Download/R Project 1/202301-divvy-tripdata.csv")
file_02_2023 <- read.csv("D:/Download/R Project 1/202302-divvy-tripdata.csv")
file_03_2023 <- read.csv("D:/Download/R Project 1/202303-divvy-tripdata.csv")
file_04_2023 <- read.csv("D:/Download/R Project 1/202304-divvy-tripdata.csv")
file_05_2023 <- read.csv("D:/Download/R Project 1/202305-divvy-tripdata.csv")
file_06_2023 <- read.csv("D:/Download/R Project 1/202306-divvy-tripdata.csv")

colnames(file_01_2023)
colnames(file_07_2022)
colnames(file_12_2022)

str(file_01_2023)
str(file_07_2022)

#combine all trips into one dataframe
all_trips <- bind_rows(file_07_2022, file_08_2022, file_09_2022, file_10_2022, file_11_2022, file_12_2022, file_01_2023, file_02_2023, file_03_2023, file_04_2023, file_05_2023, file_06_2023)

#remove unwanted columns
all_trips <- all_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng, ride_length))

#checking new dataframe
colnames(all_trips) #check column names
nrow(all_trips) #check no. of rows
dim(all_trips) #check dimension
head(all_trips)
str(all_trips)
summary(all_trips)

table(all_trips$member_casual)

#Change value in day_of_week column
all_trips <- all_trips %>% 
  mutate(day_of_week = recode(day_of_week,
                              "1" = "Sunday",
                              "2" = "Monday",
                              "3" = "Tuesday",
                              "4" = "Wednesday",
                              "5" = "Thursday",
                              "6" = "Friday",
                              "7" = "Saturday"))

#separate day,month,year,week for started_at

all_trips$date <- as.Date(all_trips$started_at, format = "%d-%m-%y")
all_trips$day <- day(ymd(all_trips$date))
all_trips$month <- month(ymd(all_trips$date))
all_trips$year <- year(ymd(all_trips$date))

all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)
str(all_trips)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
skim_without_charts(all_trips)

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
skim_without_charts(all_trips_v2)

#Descriptive Analysis on ride length
mean(all_trips_v2$ride_length)
median(all_trips_v2$ride_length)
min(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)

#Compare members and casual user
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

#average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

table1 <- all_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>%                  #groups by usertype and weekday
  summarise(number_of_rides = n(),							          #calculates the number of rides and average duration 
            average_duration = mean(ride_length)/3600) %>% 		# calculates the average duration
  arrange(member_casual, day_of_week)								          # sorts

ggplot(data = table1, mapping = aes(x = day_of_week, y = number_of_rides , fill = member_casual)) + 
  geom_col(position = "dodge") +
  ggtitle("Number of rides vs day of the week for Member and Casual Riders") +
  xlab("Day") +
  ylab("Number of Rides") +
  scale_fill_discrete(name = "Type of Ridership")

ggplot(data = table1, mapping = aes(x = day_of_week, y = average_duration , fill = member_casual)) + 
  geom_col(position = "dodge") +
  ggtitle("Daily Avg ride duration for Member and Casual Riders") +
  xlab("Day") +
  ylab("Average Ride Duration (hr)") +
  scale_fill_discrete(name = "Type of Ridership")

no_unique_rides <- all_trips_v2 %>% 
  group_by(member_casual) %>% 
  summarise(count_distinct = n_distinct(ride_id)) %>% 
  mutate(perc = count_distinct/ sum(count_distinct),
         labels = scales::percent(perc))

ggplot(data = no_unique_rides, mapping = aes(x = "", y = perc, fill = member_casual)) + 
  geom_col(color = "black") +
  geom_label(aes(label = labels), color = c("black", "black"),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Type of Rider")) +
  coord_polar(theta = "y") +                                    # change from geom col to pie shape
  theme_void()                                                  #remove grid
  
top_station_member_casual <- all_trips_v2 %>% 
  group_by(start_station_name,member_casual) %>%                 
  summarise(number_of_rides = n_distinct(ride_id)) %>% 						          
  arrange(desc(number_of_rides))

top_station_member_casual <- top_station_member_casual[-c(1,2),]

top_station_all <- all_trips_v2 %>% 
  group_by(start_station_name) %>%                 
  summarise(number_of_rides = n_distinct(ride_id)) %>% 						          
  arrange(desc(number_of_rides))
 
top_station_all <- top_station_all[-c(1,2),]
top5 <- head(top_station_all, 5)

ggplot(data = top5, mapping = aes(x = start_station_name, y = number_of_rides, fill = start_station_name ))+
  geom_col() +
  ggtitle("Top 5 stations") +
  xlab("Station") +
  ylab("Number of rides") +
  scale_fill_discrete(name = "Station")

write.csv(all_trips_v2, file = "D:/Download/R Project 1/2022_2023_trip.csv")

