# plot_vm.R
# Blair Greenan
# Fisheries and Oceans Canada
# 28 Nov 2023
#
# Description: this script generates a filled contour plot with Mike Dinniman's
# model data simulated at the SeaHorse mooring location on Ross Bank. The model
# data is in the form of a row with a time stamp (in 15 min intervals) followed
# by 240 rows of depth amd salinity/temperature data. I am reformatting these into
# a tidy data format and creating a tibble.
#
# load libraries
library(tidyverse)
library(lubridate)

############ Process the salinity file
salt_data <- read_lines("salt.csv")
lines_length <- length(salt_data)

# create two counters for the loops
time_counter <- 0
ii <- 1

# create vectors of zero length
depth <- numeric(0)
sal <- numeric(0)
dt <- as.POSIXct(character(0), tz = "UTC")

# Define the start data/time for the virtual mooring time series
date_start <- ymd_hms("2012-01-21 12:00:00")  #UTC

for (line in 1:lines_length) {
  if (!time_counter){
    time_step_str <- strsplit(salt_data[[line]], ",")
    if (line == 1) {
      time_step <- as.numeric(time_step_str)
      new_date <- date_start + seconds(as.numeric(time_step_str))
      date_step <- new_date
    }
    else {
      time_step <- c(time_step, as.numeric(time_step_str))
      new_date <- date_start + seconds(as.numeric(time_step_str))
      date_step <- c(date_step, new_date)
    }
  }
  else {
    #
    xx <- strsplit(salt_data[[line]], ",")
    yy <- as.numeric(xx[[1]])
    depth[ii] <- yy[1]
    sal[ii] <- yy[2]
    dt[ii] <- new_date
    ii <- ii + 1
  }
  # reset the time counter since the new time appears every 240 rows (depths 0 to 239 m)
  time_counter <- time_counter + 1
  if (time_counter == 241) {
    time_counter <- 0
  }

}

############ Process the temperature file
temp_data <- read_lines("temperature.csv")
lines_length <- length(temp_data)

# create two counters for the loops
time_counter <- 0
ii <- 1

# create vectors of zero length
depth <- numeric(0)
temperature <- numeric(0)
dt <- as.POSIXct(character(0), tz = "UTC")

# Define the start data/time for the virtual mooring time series
date_start <- ymd_hms("2012-01-21 12:00:00")  #UTC

for (line in 1:lines_length) {
  if (!time_counter){
    time_step_str <- strsplit(temp_data[[line]], ",")
    if (line == 1) {
      time_step <- as.numeric(time_step_str)
      new_date <- date_start + seconds(as.numeric(time_step_str))
      date_step <- new_date
    }
    else {
      time_step <- c(time_step, as.numeric(time_step_str))
      new_date <- date_start + seconds(as.numeric(time_step_str))
      date_step <- c(date_step, new_date)
    }
  }
  else {
    #
    xx <- strsplit(temp_data[[line]], ",")
    yy <- as.numeric(xx[[1]])
    depth[ii] <- yy[1]
    temperature[ii] <- yy[2]
    dt[ii] <- new_date
    ii <- ii + 1
  }
  # reset the time counter since the new time appears every 240 rows (depths 0 to 239 m)
  time_counter <- time_counter + 1
  if (time_counter == 241) {
    time_counter <- 0
  }

}

data_tibble <- tibble(DateTime = dt, WaterDepth = depth, Salinity = sal, Temperature = temperature)



