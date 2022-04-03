# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization

install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales) #to correct abbreviated viz labels

getwd() 
setwd("/Users/nataliewood/Desktop/Coursera_Capstone/csv") 

#Upload previous 12 month data set: March 2021 thru Feb 2022
all_trips <- read_csv("all_trips.csv")

#Confirm data type
str(`all_trips`)

# Decided to keep all columns

# Inspect Data
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)


str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

str(all_trips)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# Found over 700,000 rows of no data. Just NA in every cell in the row. Deleting.
# I am eliminating all rows with a value of 0 since no rides were taken.
# We will create a new version of the dataframe since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
all_trips_v3 <- na.omit(all_trips_v2)
all_trips_v4 <- all_trips_v3[all_trips_v3$ride_length !=0,]

# There are some excessively long trips. 
max(all_trips_v4$ride_length) 
# Max trip is 3356649 = over 38 days. I notice there are "test" stations from the warehouse.
# If this was real, I would confirm if these numbers should be included. I'm going to assume they should not be.
trips <- all_trips_v4[!grepl("TEST", all_trips_v4$end_station_id),]

# Descriptive analysis on ride_length (all figures in seconds)
mean(trips$ride_length) #straight average (total ride length / rides)
median(trips$ride_length) #midpoint number in the ascending array of ride lengths
max(trips$ride_length) #longest ride
min(trips$ride_length) #shortest ride
summary(trips$ride_length)

# MAX ride is still really long. It looks like most docked bikes have longer ride times.
# I want to compare bike use to see if there is a correlation to user type.
plot(as.factor(trips$member_casual),as.factor(trips$rideable_type), main="Correlation between Member Type & Bike Type", xlab="Member Type", ylab="Bike Type")

# It looks like I have more variables that I thought? Checking columns.
n_distinct(trips$rideable_type,na.rm=FALSE)
print(unique(trips$rideable_type))

# There are 3 bike types (rideable_type): classic_bike, electric_bike and docked_bike.
# Only Casual members have used docked_bike.
# Correlation between member type & ride length
plot(as.factor(trips$member_casual),trips$ride_length, main="Correlation between Member Type & Ride Length", xlab="Member Type", ylab="Ride Length")


# Compare ride_length and rideable_type
aggregate(trips$ride_length ~ trips$rideable_type, FUN = mean)
aggregate(trips$ride_length ~ trips$rideable_type, FUN = median)
aggregate(trips$ride_length ~ trips$rideable_type, FUN = max)
aggregate(trips$ride_length ~ trips$rideable_type, FUN = min)

# Compare members and casual users
aggregate(trips$ride_length ~ trips$member_casual, FUN = mean)
aggregate(trips$ride_length ~ trips$member_casual, FUN = median)
aggregate(trips$ride_length ~ trips$member_casual, FUN = max)
aggregate(trips$ride_length ~ trips$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(trips$ride_length ~ trips$member_casual + trips$day_of_week, FUN = mean)
trips$day_of_week <- ordered(trips$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Run average ride time again with day of week now in order
aggregate(trips$ride_length ~ trips$member_casual + trips$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Visualize the number of rides by rider type, number_of_rides showing as 0e+00
trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Number of rides is being abbreviated. Found this and giving it a try.
# https://statisticsglobe.com/change-formatting-of-numbers-of-ggplot2-plot-axis-in-r
library(scales)

# Attempt #2 Visualize the number of rides by rider type
trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  scale_y_continuous(labels = comma) +
  geom_col(position = "dodge")

# Visualization for average duration
trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

# Casual riders spend more time on bikes. I want to look at overall frequency.
# analyze ridership data by type and weekday
trips %>% 
  group_by(member_casual) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual)  %>% 
  ggplot(aes(x = member_casual, y = number_of_rides)) +
  scale_y_continuous(labels = comma) +
  geom_col(position = "dodge")
# Member frequency is higher overall.

# Create a csv file that we will visualize in Excel, Tableau, etc
counts <- aggregate(trips$ride_length ~ trips$member_casual + trips$day_of_week, FUN = mean)
write.csv(counts, file = '~/Desktop/Coursera_Capstone/avg_ride_length.csv')

# Download copy of cleaned data
write.csv(trips,"~/Desktop/Coursera_Capstone/rides_final.csv", row.names = FALSE)