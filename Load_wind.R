# Load_wind.R
# Blair Greenan
# Fisheries and Oceans Canada
# 05 Dec 2023
#
# Description: this script loads the wind data from the ERA-interim product and
# the NBP1201 ship winds during the period of the SeaHorse mooring deployment.
# The script then creates a line plot of the data.
#
# load libraries
library(tidyverse)
library(lubridate)


# Define the start data/time for the virtual mooring time series
date_start <- ymd_hms("2012-01-21 00:00:00")  #UTC

# load the wind data
WS_ERA <- read_csv("Wind_speed_ERA.csv", col_names = c("Time_inc1","Wind_ERA"))
WS_NBP <- read_csv("Wind_speed_NBP.csv", col_names = c("Time_inc2","Wind_NBP"))

# Convert the time increment of days in column 1 of the csv to a lubridate time
date_ERA <- date_start + hours(WS_ERA$Time_inc1*24)
date_NBP <- date_start + hours(WS_NBP$Time_inc2*24)

colors <- c("ERA" = "red", "NBP" = "black")

pWind <- ggplot() +
  geom_line(data = WS_ERA, aes(x=date_ERA,y=Wind_ERA,color="ERA"), linewidth = 1.0) +
  geom_line(data = WS_NBP, aes(x=date_NBP,y=Wind_NBP,color="NBP"), linewidth = 1.0) +
  labs(x=NULL, y=expression(paste("Wind (m ", s^-1, ")")), color = NULL) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = c(as.POSIXct("2012-01-22", tz = "UTC"), as.POSIXct("2012-01-23", tz = "UTC"), as.POSIXct("2012-01-24", tz = "UTC"), as.POSIXct("2012-01-25", tz = "UTC"), as.POSIXct("2012-01-26", tz = "UTC")),
                     labels = c("", "", "", "", ""), limits = c(as.POSIXct("2012-01-21 04:00:00", tz = "UTC"), as.POSIXct("2012-01-26 21:00:00", tz = "UTC")))





#data_tibble <- tibble(DateTime = dt, WaterDepth = depth, Salinity = sal, Temperature = temperature, Density = dens)

#save(data_tibble, file = "Virtual_Mooring.RData")


