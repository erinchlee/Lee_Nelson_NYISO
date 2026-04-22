#set up session
library(here)
#set working directory
setwd("/home/guest/ENV872/final_project")
#check working directory
here()
#returns "/home/guest/ENV872/final_project"
#load in packages
library(tidyverse); library(zoo); library(trend); library(lubridate)
library(rvest); library(purrr); library(readr); library(dplyr)

#modify this code skeleton to import temperature files in bulk
all_temp_files <- list.files(
  path = here("temp_data"),
  pattern = "temp.csv$",
  full.names = TRUE
)

# Read temperature files as one dataframe
list_temp_files <- lapply(all_temp_files, read.csv, header = TRUE)
all_temp <- bind_rows(list_temp_files)
#sorting dataframe into chronological order by date
all_temp <- all_temp %>% #need to manually add year for this to work correctly
  mutate(date = mdy(paste0(DATE, "-2025"))) %>%
  arrange(date)
#adding a column for daily temperature range which we will use in analyses
all_temp["Temp_Range"] <- all_temp$DLY.TMAX.NORMAL - all_temp$DLY.TMIN.NORMAL

#import load data - there is 1 file per day in 2025
#I could not figure out how to bulk upload these into R, I did it 1 by 1 which is tedious
#Erin maybe you know?
#I started with just december and we can replicate this process with other months
dec_files <- list.files(
  here("load_2025"),
  pattern = "^202512.*pal\\.csv$",
  full.names = TRUE
)

dec_list <- lapply(dec_files, read.csv, stringsAsFactors = TRUE)
dec_list_clean <- lapply(dec_list, function(df) {
  df %>%
    filter(Name == "N.Y.C.")
})
dec_all <- bind_rows(dec_list_clean)

#peak demand on an hourly basis
#first put date in datetime format
dec_all <- dec_all %>%
  mutate(
    datetime = mdy_hms(Time.Stamp)
  )
#collapse readings into each hour (they are given in 5 second intervals)
hourly_peak <- dec_all %>%
  mutate(hour = floor_date(datetime, "hour")) %>%
  group_by(hour) %>%
  #find peak demand every hour
  summarise(peak_load = max(Load, na.rm = TRUE),
            .groups = "drop")

#create a time series for time versus demand
dec.daily.ts <- ts(hourly_peak$peak_load,
                             frequency=24) 

#not sure if this is useful. might be good to show something we did in class
dec.daily.ts.decomp <- stl(dec.daily.ts, s.window = "periodic")
plot(dec.daily.ts.decomp)

#bar chart for hour of day versus peak demand
hourly_peak <- hourly_peak %>%
  mutate(hour_of_day = hour(hour))
#finding average peak load for each hour of the day during the month
hourly_summary <- hourly_peak %>%
  group_by(hour_of_day) %>%
  summarise(avg_peak = mean(peak_load, na.rm = TRUE))
ggplot(hourly_summary, aes(x = hour_of_day, y = avg_peak)) +
  geom_col() +
  labs(
    title = "Average Peak Demand by Hour of Day",
    x = "Hour of Day",
    y = "Load (MW)"
  ) +
  theme_minimal() #maybe we can build this out into a better theme
#we can make our bars different colors
#we can do this for all 12 months to see how the profile changes over months and then write about it?

#scatterplot for daily temperature range versus peak demand
#getting the date column in the same format as we have for the temperature data
hourly_peak <- hourly_peak %>%
  mutate(
    hour = as.POSIXct(hour, format = "%m/%d/%Y %H:%M:%S"),
    date = as.Date(hour)
  )
#finding the daily peak
daily_peak <- hourly_peak %>%
  group_by(date) %>%
  summarise(
    daily_peak = max(peak_load, na.rm = TRUE),
    .groups = "drop"
  )
#combine load and temperature datasets
dec_merged <- left_join(daily_peak, all_temp, by = "date")
#create scatter plot for temp range versus peak demand
dec_merged_scatter <-
  ggplot(dec_merged, aes(x = Temp_Range, y = daily_peak)) +
  geom_point() +
  labs(
    title = "Daily Temperature Range versus Peak Load",
    x = "Tenperature Range (degrees)", #need to add in temp uni
    y = "Peak Load", #add in unit
  )
print(dec_merged_scatter)

#perform linear regression test to verify sensitivity differences
#null hypothesis: there is no relationship between temperature range and daily peak demand
regression <- 
  lm(dec_merged$Temp_Range ~ dec_merged$daily_peak)
print(regression) #displays the coefficients

#show the *strength* of the relationship
cor.test(dec_merged$Temp_Range, dec_merged$daily_peak)
#prints a p value of < 0.5287, indicating that the null hypothesis is not rejected at 95% confidence interval
#cor is 0.117, weak positive relationship

#create a dual axis time series chart for time vs temperature range vs peak demand
#Erin, is this what you had in mind?
library(ggplot2)
ggplot(dec_merged, aes(x = date)) +
  # Temperature range, our primary axis
  geom_line(aes(y = Temp_Range, color = "Temperature Range")) +
  # Peak demand (scaled for aesthetic purposes)
  #500 is an arbitrary number, to show both lines on the graph
  geom_line(aes(y = daily_peak / 500, color = "Peak Demand (scaled)")) +
  scale_y_continuous(
    name = "Temperature Range (°F)",
    sec.axis = sec_axis(~ . * 500, name = "Peak Demand (MW)")
  ) +
  scale_color_manual(values = c(
    "Temperature Range" = "blue",
    "Peak Demand (scaled)" = "red"
  )) +
  labs(
    title = "Temperature Range vs Peak Demand",
    x = "Date",
    color = ""
  ) +
  
  theme_minimal()


#notes:
#a suggestion for further work is that we could analyze peak demand versus max temp or min temp
#we also probably want to repeat the december analysis with other months
#since the final output is a standalone Rmd file, we should add commentary about 
#our interpretation of the visuals as well as the regression analysis
#we can improve the aesthetics of the plot







