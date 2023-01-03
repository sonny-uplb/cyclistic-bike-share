### Divvy Cyclistic Trip Data ###
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
library(scales)
getwd()
setwd("/Users/Sonny Maniaol/Desktop/Case Study 1/processed") # change directory path to yours

#Upload datasets
trip202112 <- read_excel("202112-tripdata.xlsx")
trip202201 <- read_excel("202201-tripdata.xlsx")
trip202202 <- read_excel("202202-tripdata.xlsx")
trip202203 <- read_excel("202203-tripdata.xlsx")
trip202204 <- read_excel("202204-tripdata.xlsx")
trip202205 <- read_excel("202205-tripdata.xlsx")
trip202206 <- read_excel("202206-tripdata.xlsx")
trip202207 <- read_excel("202207-tripdata.xlsx")
trip202208 <- read_excel("202208-tripdata.xlsx")
trip202209 <- read_excel("202209-tripdata.xlsx")
trip202210 <- read_excel("202210-tripdata.xlsx")
trip202211 <- read_excel("202211-tripdata.xlsx")


#Convert ride_id and rideable_type to characters
trip202112 <- mutate(trip202112, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
trip202201 <- mutate(trip202201, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
trip202202 <- mutate(trip202202, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
trip202203 <- mutate(trip202203, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
trip202204 <- mutate(trip202204, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
trip202205 <- mutate(trip202205, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
trip202206 <- mutate(trip202206, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
trip202207 <- mutate(trip202207, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
trip202208 <- mutate(trip202208, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
trip202209 <- mutate(trip202209, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
trip202210 <- mutate(trip202210, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
trip202211 <- mutate(trip202211, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))

# check if consistent
str(trip202112)
str(trip202201)
str(trip202202)
str(trip202203)
str(trip202204)
str(trip202205)
str(trip202206)
str(trip202207)
str(trip202208)
str(trip202209)
str(trip202210)
str(trip202211)

# Combine data
all_trips= bind_rows(trip202112, trip202201, trip202202,trip202203,trip202204,trip202205,trip202206,trip202207,trip202208,trip202209,trip202210,trip202211)

#Inspect new table
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

table(all_trips$member_casual)

# Add columns that list the date, month and day
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$day <- format(as.Date(all_trips$date),"%d")
all_trips$year <- format(as.Date(all_trips$date),"%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date),"%A")

# arrange day of week starting from Sunday
all_trips$day_of_week <- ordered(all_trips$day_of_week, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character((all_trips$ride_length)))
is.numeric(all_trips$ride_length)

# trim leading and trailing white space, if any
all_trips$start_station_name <- trimws(all_trips$start_station_name, which = c("both"))
all_trips$end_station_name <- trimws(all_trips$end_station_name, which = c("both"))

# Descriptive Analysis
# Descriptive Analysis on ride_length in seconds
mean(all_trips$ride_length)
median(all_trips$ride_length)
max(all_trips$ride_length)
min(all_trips$ride_length)

summary(all_trips$ride_length)

# Compare members vs casual users
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN=mean)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN=median)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN=max)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN=min)

# Average ride time by each day for members vs casual users
aggregate(all_trips$ride_length ~ all_trips$member_casual + all_trips$day_of_week, FUN=mean)

# analyze data by type and weekday
all_trips %>%
  mutate(weekday = wday(started_at, label=TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides=n(),average_duration=mean(ride_length)) %>%
  arrange(member_casual,weekday)


# visualize for number of rides
all_trips %>%
  mutate(weekday = wday(started_at, label=TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides=n(),average_duration=mean(ride_length)) %>%
  arrange(member_casual,weekday) %>%
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge") +
  labs(title="Total Number of Rides Per Day of Week", x="Day of Week", y="Number of Rides", fill="Type of Rider") +
  theme_bw() +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3))

ggsave(filename="total_count_perday.png", device="png")

# visualize for number of rides per month
all_trips %>%
  mutate(weekday = wday(started_at, label=TRUE)) %>%
  group_by(member_casual, weekday, year, month) %>%
  summarise(number_of_rides=n(),average_duration=mean(ride_length)) %>%
  arrange(member_casual,weekday) %>%
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge") +
  labs(title="Total Number of Rides Per Day of Week Monthly", x="Day of Week", y="Number of Rides", fill="Type of Rider") +
  facet_wrap(facets = vars(year, month)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3))

ggsave(filename="total_count_perday_monthly.png", device="png")
  

# visualize for average duration
all_trips %>%
  mutate(weekday = wday(started_at, label=TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides=n(),average_duration=mean(ride_length)) %>%
  arrange(member_casual,weekday) %>%
  ggplot(aes(x=weekday, y=average_duration, fill=member_casual)) +
  geom_col(position = "dodge") +
  labs(title="Average Duration of Rides Per Day of Week", x="Day of Week", y="Average Duration (seconds)", fill="Type of Rider") +
  theme_bw()

ggsave(filename="ave_duration_perday.png", device="png")

# visualize for average duration per month
all_trips %>%
  mutate(weekday = wday(started_at, label=TRUE)) %>%
  group_by(member_casual, weekday, year, month) %>%
  summarise(number_of_rides=n(),average_duration=mean(ride_length)) %>%
  arrange(member_casual,weekday) %>%
  ggplot(aes(x=weekday, y=average_duration, fill=member_casual)) +
  geom_col(position = "dodge") +
  labs(title="Average Duration of Rides Per Day of Week Monthly",x="Day of Week", y="Average Duration (seconds)", fill="Type of Rider") +
  facet_wrap(facets = vars(year, month)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))

ggsave(filename="ave_duration_perday_monthly.png", device="png")

# Export Summary files
counts <- aggregate(all_trips$ride_length ~ all_trips$member_casual + all_trips$day_of_week, FUN=mean)
write.csv(counts, file = '/Users/Sonny Maniaol/Desktop/Case Study 1/code/average_ride_length.csv') # change your directory and filename