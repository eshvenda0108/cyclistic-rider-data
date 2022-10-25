  library(tidyverse)
  library(lubridate)
  library(dplyr)      #For data manipulation
  library(skimr)      #For summarizing data
  library(sqldf)      #For using SQL queries
  library(janitor)
  library(writexl)
  library(readxl)
  library(hms)        #For`` time
  library(data.table) #For exporting data frame
  
  #Load original .csv files, a years worth of data from septembere 2021 to august 2022
  september2021 <- read.csv("202109-divvy-tripdata.csv")
  october2021 <- read.csv("202110-divvy-tripdata.csv")
  november2021 <- read.csv("202111-divvy-tripdata.csv")
  december2021 <- read.csv("202112-divvy-tripdata.csv")
  january2022 <- read.csv("202201-divvy-tripdata.csv")
  febuary2022 <- read.csv("202202-divvy-tripdata.csv")
  march2022 <- read.csv("202203-divvy-tripdata.csv")
  april2022 <- read.csv("202204-divvy-tripdata.csv")
  may2022 <- read.csv("202205-divvy-tripdata.csv")
  june2022 <- read.csv("202206-divvy-tripdata.csv")
  july2022 <- read.csv("202207-divvy-tripdata.csv")
  august2022 <- read.csv("202208-divvy-tripdata.csv")
  
  #Merge all of the data frames into one year view
  all_trips <- rbind (september2021, october2021, november2021, december2021, january2022, febuary2022, march2022, april2022, may2022, june2022, july2022, august2022)
  
  #Remove individual month data frame to clear up space in environment
  remove(september2021, october2021, november2021, december2021, january2022, febuary2022, march2022, april2022, may2022, june2022, july2022, august2022)
  
  #Creating columns for:date, month, day, and day of week
  all_trips$month <- format(as.Date(all_trips$started_at), "%m")
  all_trips$day <- format(as.Date(all_trips$started_at), "%d")
  all_trips$day_of_week <- format(as.Date(all_trips$started_at), "%A")
  
  #Calculate ride length by subtracting ended_at time from started_at time and converted it to minutes
  all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
  
  #Changing the data type for the column ride_length to numeric, and making sure it is numeric
  all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
  is.numeric(all_trips$ride_length)
  
  #Checking ride length
  sum(all_trips$ride_length <= 0)
  
  #Calculating mean, median, min, and max depending on the value in the column "member_casual" and "day_of_week"
  all_trips %>% 
    group_by(member_casual) %>% 
    summarize(number_of_rides = n(),
              average_duration = mean(ride_length),
              median_duration = median(ride_length),
              min_duration = min(ride_length), 
              max_duration = max(ride_length),
              percentage_day_total = (number_of_rides/nrow(all_trips))*100)
  
  # calculating total rides and average ride time by each day for members and  casual riders
  
  all_trips%>%
    group_by(member_casual, day_of_week) %>%
    summarise(number_of_rides = n() ,.groups = "drop")
  
  all_trips%>%
    group_by(member_casual,month) %>%
    summarise(number_of_rides = n(),average_ride_length = mean(ride_length),  .groups = "drop")
  
  #create a column for the month using the full month name
  all_trips <-all_trips %>% mutate(month = 
                                               case_when(month == "01" ~ "January",
                                                         month == "02" ~ "February",
                                                         month == "03" ~ "March",
                                                         month == "04" ~ "April",
                                                         month == "05" ~ "May",
                                                         month == "06" ~ "June",
                                                         month == "07" ~ "July",
                                                         month == "08" ~ "August",
                                                         month == "09" ~ "September",
                                                         month == "10" ~ "October",
                                                         month == "11" ~ "November",
                                                         month == "12" ~ "December" 
                                                         )
  )                                 
                                   
  
  #Cleaning data
  all_trips <- all_trips[!(all_trips$ride_length <= 0),]
  sum(all_trips$ride_length <= 0)
  all_trips <- na.omit(all_trips)
  all_trips <- distinct(all_trips)
  all_trips <- all_trips %>%
    select(-c(ride_id, start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng))
  
  #creating a new data frame to use in tableau
  cyclistic_rider_data <- all_trips
  
  # downloading the new data as a .csv file
  fwrite(cyclistic_rider_data,"cyclistic_data.csv")
