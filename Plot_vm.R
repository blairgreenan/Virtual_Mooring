#

library(tidyverse)
library(lubridate)

salt_data <- read_lines("salt.csv")

lines_length <- length(salt_data)
time_counter <- 0
ii <- 1

# Define the start data/time for the virtual mooring time series
date_start <- ymd_hms("2012-01-21 12:00:00")  #UTC

for (line in 1:lines_length) {
  if (!time_counter){
    time_step_str <- strsplit(salt_data[[line]], ",")
    if (line == 1) {
      time_step <- as.numeric(time_step_str)
      date_step <- date_start + seconds(as.numeric(time_step_str))
    }
    else {
      time_step <- c(time_step, as.numeric(time_step_str))
      date_step <- c(date_step, date_start + seconds(as.numeric(time_step_str)))
    }
    #time_step_numeric <- as.numeric(time_step_str)
    #time_step <- c(time_step, time_step_numeric)
    ii <- ii + 1
  }
  time_counter <- time_counter + 1
  if (time_counter == 241) {
    time_counter <- 0
  }


}

