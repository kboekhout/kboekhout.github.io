install.packages("lubridate")
library(tidyverse)
library(readxl)
library(lubridate)


#imported data into environment
"2021_june_bike_data" <-
  read_excel("C:/Users/kyleb/Data Analysis Course/Case Study/Bikes Excel Workbooks/2021_06_bike_ride_data.xlsx")

"2021_july_bike_data" <- 
  read_excel("C:/Users/kyleb/Data Analysis Course/Case Study/Bikes Excel Workbooks/2021_07_bike_ride_data.xlsx")

"2021_aug_bike_data" <- 
  read_excel("C:/Users/kyleb/Data Analysis Course/Case Study/Bikes Excel Workbooks/2021_08_bike_ride_data.xlsx")

"2021_sept_bike_data" <- 
  read_excel("C:/Users/kyleb/Data Analysis Course/Case Study/Bikes Excel Workbooks/2021_09_bike_ride_data.xlsx")

"2021_oct_bike_data" <- 
  read_excel("C:/Users/kyleb/Data Analysis Course/Case Study/Bikes Excel Workbooks/2021_10_bike_ride_data.xlsx")

"2021_nov_bike_data" <- 
  read_excel("C:/Users/kyleb/Data Analysis Course/Case Study/Bikes Excel Workbooks/2021_11_bike_ride_data.xlsx")

"2021_dec_bike_data" <- 
  read_excel("C:/Users/kyleb/Data Analysis Course/Case Study/Bikes Excel Workbooks/2021_12_bike_ride_data.xlsx")

"2022_jan_bike_data" <- 
  read_excel("C:/Users/kyleb/Data Analysis Course/Case Study/Bikes Excel Workbooks/2022_01_bike_ride_data.xlsx")

"2022_feb_bike_data" <- 
  read_excel("C:/Users/kyleb/Data Analysis Course/Case Study/Bikes Excel Workbooks/2022_02_bike_ride_data.xlsx")

"2022_mar_bike_data" <- 
  read_excel("C:/Users/kyleb/Data Analysis Course/Case Study/Bikes Excel Workbooks/2022_03_bike_ride_data.xlsx")

"2022_apr_bike_data" <- 
  read_excel("C:/Users/kyleb/Data Analysis Course/Case Study/Bikes Excel Workbooks/2022_04_bike_ride_data.xlsx")

"2022_may_bike_data" <- 
  read_excel("C:/Users/kyleb/Data Analysis Course/Case Study/Bikes Excel Workbooks/2022_05_bike_ride_data.xlsx")


#combining months into full year
full_year_data <- bind_rows(`2021_aug_bike_data`, `2021_dec_bike_data`, `2021_july_bike_data`, `2021_june_bike_data`, 
                            `2021_nov_bike_data`, `2021_oct_bike_data`, `2021_sept_bike_data`, `2022_apr_bike_data`,
                            `2022_feb_bike_data`, `2022_jan_bike_data`, `2022_mar_bike_data`, `2022_may_bike_data`)

#adding date, day, month, and year columns
full_year_data$date <- as.Date(full_year_data$started_at) #The default format is yyyy-mm-dd
full_year_data$month <- format(as.Date(full_year_data$date), "%m")
full_year_data$day <- format(as.Date(full_year_data$date), "%d")
full_year_data$year <- format(as.Date(full_year_data$date), "%Y")
full_year_data$day_of_week <- format(as.Date(full_year_data$date), "%A")

#creating ride_length column
full_year_data$ride_length <- difftime(full_year_data$ended_at,full_year_data$started_at)

#changing "ride_length" from factor to numeric
is.factor(full_year_data$ride_length)
full_year_data$ride_length <- as.numeric(as.character(full_year_data$ride_length))
is.numeric(full_year_data$ride_length)


#cleaning of data, removing quality check and negative ride_length
full_year_v1_5 <-full_year_data[!(full_year_data$start_station_name == "HQ QR" | full_year_data$ride_length<0),]

#removing NA values from ride_length
full_year_v2 <- drop_na(full_year_v1_5, ride_length)

  
##analyzing of data
summary(full_year_v2$ride_length)

#comparing member and casual riders
aggregate(full_year_v2$ride_length ~ full_year_v2$member_casual, FUN = mean)
aggregate(full_year_v2$ride_length ~ full_year_v2$member_casual, FUN = median)
aggregate(full_year_v2$ride_length ~ full_year_v2$member_casual, FUN = max)
aggregate(full_year_v2$ride_length ~ full_year_v2$member_casual, FUN = min)

#putting days of week in correct order
full_year_v2$day_of_week <- ordered(full_year_v2$day_of_week, levels=c(
  "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#average ride time by day for members and casuals
aggregate(full_year_v2$ride_length ~ full_year_v2$member_casual + full_year_v2$day_of_week, FUN = mean)

#looking at rider data by type and weekday. Number of rides is thousands of rides
full_year_v2 %>%  
  mutate(weekday = wday(started_at, label = TRUE))  %>%   #creates weekday field using wday()
  group_by(member_casual, weekday)  %>%   #groups by user type and weekday
  summarize(number_of_rides = n(),		#calculates the number of rides and average duration 
            average_duration = mean(ride_length)) %>%    # calculates the average duration
  arrange(member_casual, weekday)	%>% # sorts
  ggplot(aes(x = weekday, y = number_of_rides/1000, fill = member_casual)) +
  geom_col(position = "dodge") + #creating plot for total number of rides
  xlab("Day of Week") + #renaming x axis
  ylab("Thousands of Rides")+ #renaming y
  ggtitle("Number of Rides per Day by Member or Casual Riders") #adding title

  
  
full_year_v2 %>%  
  mutate(weekday = wday(started_at, label = TRUE))  %>%   #creates weekday field using wday()
  group_by(member_casual, weekday)  %>%   #groups by user type and weekday
  summarize(number_of_rides = n(),		#calculates the number of rides and average duration 
            average_duration = mean(ride_length)) %>%    # calculates the average duration
  arrange(member_casual, weekday)	%>% # sorts
  ggplot(aes(x = weekday, y = average_duration/60, fill = member_casual)) +
  geom_col(position = "dodge") + #creating visual for average duration
  xlab("Day of Week") +
  ylab("Average Ride Time in Minutes") +
  ggtitle("Average Ride Time by Member and Casual Riders")




#saving csv file for future visualization
counts <- aggregate(full_year_v2$ride_length ~ full_year_v2$member_casual + 
                     full_year_v2$day_of_week, FUN = mean)
write.csv(counts, file="C:/Users/kyleb/Data Analysis Course/average_ride_length.csv")

