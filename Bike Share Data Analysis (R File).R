
#installing packages for data cleaning, sorting, formatting, analysis and presentation

library(tidyverse)
library(janitor)
library(lubridate)
library(readr)
library(ggplot2)
library(repr)
library(plotrix)

#importing the CSV files to upload the Bike Share data set to R

bsd_1 <- read_csv("~/PROJECT/BIKE SHARE/202101-divvy-tripdata.csv")
bsd_2 <- read_csv("~/PROJECT/BIKE SHARE/202102-divvy-tripdata.csv")
bsd_3 <- read_csv("~/PROJECT/BIKE SHARE/202103-divvy-tripdata.csv")
bsd_4 <- read_csv("~/PROJECT/BIKE SHARE/202104-divvy-tripdata.csv")
bsd_5 <- read_csv("~/PROJECT/BIKE SHARE/202105-divvy-tripdata.csv")
bsd_6 <- read_csv("~/PROJECT/BIKE SHARE/202106-divvy-tripdata.csv")
bsd_7 <- read_csv("~/PROJECT/BIKE SHARE/202107-divvy-tripdata.csv")
bsd_8 <- read_csv("~/PROJECT/BIKE SHARE/202108-divvy-tripdata.csv")
bsd_9 <- read_csv("~/PROJECT/BIKE SHARE/202109-divvy-tripdata.csv")
bsd_10 <- read_csv("~/PROJECT/BIKE SHARE/202110-divvy-tripdata.csv")
bsd_11 <- read_csv("~/PROJECT/BIKE SHARE/202111-divvy-tripdata.csv")
bsd_12 <- read_csv("~/PROJECT/BIKE SHARE/202112-divvy-tripdata.csv")



bike_share_data <- rbind(bsd_1, bsd_2, bsd_3, bsd_4, bsd_5, bsd_6, bsd_7, bsd_8, bsd_9, bsd_10, bsd_11, bsd_12)
glimpse(bike_share_data)


clean_data  <- bike_share_data[complete.cases(bike_share_data), ] # remove Null values

table(clean_data$member_casual)      # counting the number of casual and annual member

ggplot(data = clean_data) +
  geom_bar(mapping = aes(x = member_casual))    #plotting the graph of number of casual and annual member


#ANALYZING DATA

clean_data$month <- month(clean_data$started_at)  #retrieving the date from  started_at column
clean_data$day <- day(clean_data$started_at)
clean_data$year <- year(clean_data$started_at)
clean_data$day_of_week <- weekdays(clean_data$started_at)
clean_data$month_of_year <- month.abb[clean_data$month]
clean_data$bikeuse_days <- (day(clean_data$ended_at) - day(clean_data$started_at)) #to see how many customers have used bikes for more than a day 

show <- clean_data %>% count(bikeuse_days) #I found discrepancy in data, some rows are having negative value which suggests the ending date before the starting date
discrepant_value <- clean_data[clean_data$bikeuse_days < 0,] # I got to find out that the formula that for calculating the bikeuse_days was considering only the day in part, so when the next month arrives, it created discrepant values and same thing will happen for positive values. So now I am going to correct this by putting condition
clean_data$bikeuse_days <- NULL    # removing the column with incorrect value

clean_data$start_date <- as.Date(clean_data$started_at)
clean_data$end_date <- as.Date(clean_data$ended_at)
clean_data$days_bikeuse <- ((clean_data$end_date) - (clean_data$start_date))    # corrected column for no of days of bike use


clean_data$start_time <- format(clean_data$started_at, format = "%H:%M")
clean_data$end_time <- format(clean_data$ended_at, format = "%H:%M")
clean_data$time_duration <- difftime(clean_data$ended_at, clean_data$started_at, units = "mins") #getting some negative values
wrong_duration_value <- clean_data[clean_data$time_duration < 0,] # values with negative time duration (needs to be removed)
clean_data <- subset(clean_data, clean_data$time_duration > 0)

summary(clean_data)
str(clean_data)


#DATA ANALYSIS
clean_data %>% group_by(member_casual) %>% 
  summarize(max_duration = max(time_duration),
            min_duration = min(time_duration),
            avg_duration = mean(time_duration),
            middle_duration = median(time_duration))

# Compare ride length between members and casual riders
aggregate(clean_data$time_duration ~ clean_data$member_casual, FUN = mean)
aggregate(clean_data$time_duration ~ clean_data$member_casual, FUN = median)
aggregate(clean_data$time_duration ~ clean_data$member_casual, FUN = max)
aggregate(clean_data$time_duration ~ clean_data$member_casual, FUN = min)


# See the average ride length by each day of week for members vs. casual riders
clean_data$day_of_week <- ordered(clean_data$day_of_week, 
                                  levels = c("Monday", "Tuesday", "Wednesday", 
                                             "Thursday", "Friday", "Saturday", "Sunday"))

aggregate(clean_data$time_duration ~ clean_data$member_casual + clean_data$day_of_week, FUN = mean)

# See the average ride length by month for members vs. casual riders
clean_data$month_of_year <- ordered(clean_data$month_of_year, 
                                  levels = c("Jan", "Feb", "Mar","Apr", "May", "Jun", 
                                             "Jul", "Aug", "Sep","Oct", "Nov", "Dec"))
aggregate(clean_data$time_duration ~ clean_data$member_casual + clean_data$month_of_year, FUN = mean)


# Number of rides between members and casual riders for each day of week
clean_data %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(day_of_week)

# Number of rides between members and casual riders for each month
clean_data %>% 
  group_by(member_casual, month_of_year) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(month_of_year)

# Comparing general bike type preference between members and casual riders
clean_data %>% 
  group_by(rideable_type, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop')

# Number of rides between members and casual riders for start hour
clean_data$start_hour <- format(clean_data$started_at, format = "%H")
clean_data %>% 
  group_by(member_casual, start_hour) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(start_hour)


# Get the top 10 start and end stations for casual riders
top_start_station <- clean_data %>% 
  group_by(member_casual = "Casual",start_station_name) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count)) %>% 
  slice(1:10)
View(top_start_station)


top_end_station <- clean_data %>% 
  group_by(member_casual = "Casual",end_station_name) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count)) %>% 
  slice(1:10)



#DATA VISUALIZATION

# 1.Number of casual and annual member
ggplot(data = clean_data) +
  geom_bar(mapping = aes(x = member_casual))

# 2.Number of rides casual and annual member for each month
clean_data %>%
  ggplot(aes(month_of_year, fill=member_casual)) +
  geom_bar(position = "dodge") +
  labs(x="Month", y = "No. of rides", title="Distribution by month") 

# 3.Average Duration ime of casual and annual member for each month
AvgDur_VS_MonthOfYear <- data.frame(aggregate(clean_data$time_duration ~ 
                                              clean_data$member_casual + 
                                              clean_data$month_of_year, FUN = mean))

AvgDur_VS_MonthOfYear %>%
  ggplot(aes(x = clean_data.month_of_year, 
             y = clean_data.time_duration, 
             fill = clean_data.member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Month Of Year", y = "Average Duration (min)", 
       fill = "Member/Casual",
       title = "Average Riding Duration by Month: Members vs. Casual Riders") 


# 4.Number of rides casual and annual member for each week
clean_data %>%
  ggplot(aes(day_of_week, fill=member_casual)) +
  geom_bar(position = "dodge") +
  labs(x="Day Of Week", y = "No. of rides", title="Distribution by Day of Week") 

# 5.Average Duration time of casual and annual member for each day
AvgDur_VS_DayOfWeek<- data.frame(aggregate(clean_data$time_duration ~ clean_data$member_casual + 
                                             clean_data$day_of_week, FUN = mean))

AvgDur_VS_DayOfWeek %>%
  ggplot(aes(x = clean_data.day_of_week, 
             y = clean_data.time_duration, 
             fill = clean_data.member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Day of Week", y = "Average Duration (min)", 
       fill = "Member/Casual",
       title = "Average Riding Duration by Day: Members vs. Casual Riders") 


# 6. Number of rides between members and casual riders for start hour
NumRides_VS_StartHour <- data.frame(clean_data %>% 
  group_by(member_casual, start_hour) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(start_hour))
View(NumRides_VS_StartHour)

NumRides_VS_StartHour %>%
  ggplot(aes(x = as.numeric(start_hour), 
             y = number_of_rides, 
             fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Start Hour", y = "Number Of Rides", 
       fill = "Member/Casual",
       title = "Number Of Rides by Start Hour") +
  scale_x_continuous(breaks = c(00, 01, 02, 03, 04, 05, 06, 07, 08, 
                                09, 10, 11, 12, 13, 14, 15, 16, 17, 
                                18, 19, 20, 21, 22, 23), 
                     labels = c("12 \n AM", "01 \n AM", 
                                "02 \n AM", "03 \n AM", 
                                "04 \n AM", "05 \n AM", 
                                "06 \n AM", "07 \n AM", 
                                "08 \n AM", "09 \n AM", 
                                "10 \n AM", "11 \n AM",
                                "12 \n PM", "01 \n PM", 
                                "02 \n PM", "03 \n PM",
                                "04 \n PM", "05 \n PM", 
                                "06 \n PM", "07 \n PM",
                                "08 \n PM", "09 \n PM", 
                                "10 \n PM", "11 \n PM"))


# 7.Average Duration time of casual and annual member for each day
AvgDur_VS_StartHour<- data.frame(aggregate(clean_data$time_duration ~ clean_data$member_casual + 
                                             clean_data$start_hour, FUN = mean))
View(AvgDur_VS_StartHour)

AvgDur_VS_StartHour %>%
  ggplot(aes(x = as.numeric(clean_data.start_hour), 
             y = clean_data.time_duration, 
             fill = clean_data.member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Start Hour", y = "Average Duration (min)", 
       fill = "Member/Casual",
       title = "Average Riding Duration by Start Hour: Members vs. Casual Riders")  +
  scale_x_continuous(breaks = c(00, 01, 02, 03, 04, 05, 06, 07, 08, 
                                09, 10, 11, 12, 13, 14, 15, 16, 17, 
                                18, 19, 20, 21, 22, 23), 
                     labels = c("12 \n AM", "01 \n AM", 
                                "02 \n AM", "03 \n AM", 
                                "04 \n AM", "05 \n AM", 
                                "06 \n AM", "07 \n AM", 
                                "08 \n AM", "09 \n AM", 
                                "10 \n AM", "11 \n AM",
                                "12 \n PM", "01 \n PM", 
                                "02 \n PM", "03 \n PM",
                                "04 \n PM", "05 \n PM", 
                                "06 \n PM", "07 \n PM",
                                "08 \n PM", "09 \n PM", 
                                "10 \n PM", "11 \n PM"))


#8. Number Of Rides on different type of cycles for casual and annual member
NumOfRides_VS_RideableType <- data.frame(clean_data %>% 
  group_by(rideable_type, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop'))
View(NumOfRides_VS_RideableType)

NumOfRides_VS_RideableType %>%
  ggplot(aes(x = rideable_type, 
             y = number_of_rides, 
             fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Type Of Cycle used", y = "Number of Rides", 
       fill = "Member/Casual",
       title = "Number of Rides on different cycles: Members vs. Casual Riders")
View(NumOfRides_VS_RideableType)



options(repr.plot.width = 14, repr.plot.height = 10)


# 9.Usage of different bikes by casual and annual member based on the week
clean_data %>% 
  group_by(month_of_year, member_casual, rideable_type) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  ggplot(aes(x = month_of_year, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  facet_grid(member_casual~rideable_type) +
  labs(x = "Month", y = "Number of Rides", fill = "Member/Casual",
       title = "Number of Rides by Month") +
  theme(axis.text.x = element_text(angle = 90))


# 10. Usage of different bikes by casual and annual member based on the Month
clean_data %>% 
  group_by(day_of_week, member_casual, rideable_type) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  facet_grid(member_casual~rideable_type) +
  labs(x = "Week", y = "Number of Rides", fill = "Member/Casual",
       title = "Number of Rides by Week") +
  theme(axis.text.x = element_text(angle = 90)) 



# 11. Number Of Rides on different type of cycles for casual and annual member

top_start_station %>%
  ggplot(aes(x = station_count, 
             y = start_station_name,
             fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Num of Rides", y = "Start Station Name", 
       fill = "Member/Casual",
       title = "Top 10 Start Station name for casual members")

top_end_station %>%
  ggplot(aes(x = station_count, 
             y = end_station_name,
             fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Num of Rides", y = "End Station Name", 
       fill = "Member/Casual",
       title = "Top 10 End Station name for casual members")

