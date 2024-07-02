
library(tidyverse)
library(lubridate)
library(cmocean)
library(interp)
library(patchwork)
library(Cairo)

load("Virtual_Mooring.RData")
load("SeaHorse_CTD_data.RData")
load(file = "MLD_VM.Rdata")

pTmp_VM <- ggplot() +
  geom_raster(data=data_tibble, aes(DateTime, WaterDepth, fill = Temperature)) +
  scale_fill_cmocean(name = "thermal", limits=c(-1.9,0.5), breaks = c(-1.5, -1.0, -0.5, 0.0, 0.5)) +
  labs(x=NULL,y="Depth (m)") +
  guides(fill = guide_colourbar(direction = "horizontal", title.position = "top", title=expression("Temperature (\u00B0C)"))) +
  #  scale_x_continuous(labels = NULL, limits = c(as.POSIXct("2012-01-21", tz = "UTC"), as.POSIXct("2012-01-27", tz = "UTC"))) +
  scale_x_continuous(breaks = c(as.POSIXct("2012-01-22", tz = "UTC"), as.POSIXct("2012-01-23", tz = "UTC"), as.POSIXct("2012-01-24", tz = "UTC"), as.POSIXct("2012-01-25", tz = "UTC"), as.POSIXct("2012-01-26", tz = "UTC")),
                     labels = c("","","","",""), limits = c(as.POSIXct("2012-01-21 04:00:00", tz = "UTC"), as.POSIXct("2012-01-26 21:00:00", tz = "UTC"))) +
  #labels = c("Jan 22", "Jan 23", "Jan 24", "Jan 25", "Jan 26"), limits = c(as.POSIXct("2012-01-21 04:00:00", tz = "UTC"), as.POSIXct("2012-01-26 21:00:00", tz = "UTC"))) +
  scale_y_continuous(limits = c(-200, 0), breaks = c(-200, -150, -100, -50, 0), labels = c("200", "150", "100", "50", "0"))

pSal_VM <- ggplot() +
  geom_raster(data=data_tibble, aes(DateTime, WaterDepth, fill = Salinity)) +
  scale_fill_cmocean(name = "haline", limits=c(34.2,34.75), breaks = c(34.25, 34.5, 34.75)) +
  labs(x=NULL,y="Depth (m)") +
  guides(fill = guide_colourbar(direction = "horizontal", title.position = "top", title=expression("Salinity"))) +
  #  scale_x_continuous(labels = NULL, limits = c(as.POSIXct("2012-01-21", tz = "UTC"), as.POSIXct("2012-01-27", tz = "UTC"))) +
  scale_x_continuous(breaks = c(as.POSIXct("2012-01-22", tz = "UTC"), as.POSIXct("2012-01-23", tz = "UTC"), as.POSIXct("2012-01-24", tz = "UTC"), as.POSIXct("2012-01-25", tz = "UTC"), as.POSIXct("2012-01-26", tz = "UTC")),
                     labels = c("Jan 22", "Jan 23", "Jan 24", "Jan 25", "Jan 26"), limits = c(as.POSIXct("2012-01-21 04:00:00", tz = "UTC"), as.POSIXct("2012-01-26 21:00:00", tz = "UTC"))) +
  scale_y_continuous(limits = c(-200, 0), breaks = c(-200, -150, -100, -50, 0), labels = c("200", "150", "100", "50", "0")) +
  geom_smooth(data=MLD_df, aes(unique_time,MLD_VM), method="loess", se=FALSE, span=0.1, colour="white", linewidth = 0.5)
  #geom_line(data=MLD_df, aes(unique_time,MLD_VM), colour="white", linewidth = 1)


pTmp <- ggplot(tidy_SH, aes(Time, Pressure)) +
  geom_raster(aes(fill = Temperature)) +
  scale_fill_cmocean(name = "thermal", limits=c(-1.9,0.5), breaks = c(-1.5, -1.0, -0.5, 0.0, 0.5)) +
  scale_y_reverse() +
  labs(x=NULL,y="Depth (m)") +
  guides(fill = guide_colourbar(direction = "horizontal", title.position = "top", title=expression("Temperature (\u00B0C)"))) +
  scale_x_continuous(breaks = c(as.POSIXct("2012-01-22", tz = "UTC"), as.POSIXct("2012-01-23", tz = "UTC"), as.POSIXct("2012-01-24", tz = "UTC"), as.POSIXct("2012-01-25", tz = "UTC"), as.POSIXct("2012-01-26", tz = "UTC")),
                     labels = c("","","","",""), limits = c(as.POSIXct("2012-01-21 04:00:00", tz = "UTC"), as.POSIXct("2012-01-26 21:00:00", tz = "UTC")))
# labels = c("Jan 22", "Jan 23", "Jan 24", "Jan 25", "Jan 26"), limits = c(as.POSIXct("2012-01-21 04:00:00", tz = "UTC"), as.POSIXct("2012-01-26 21:00:00", tz = "UTC")))

pSal <- ggplot(tidy_SH, aes(Time, Pressure)) +
  geom_raster(aes(fill = Salinity)) +
  scale_fill_cmocean(name = "haline", limits=c(34.2,34.75), breaks = c(34.25, 34.5, 34.75)) +
  scale_y_reverse() +
  labs(x=NULL,y="Depth (m)") +
  guides(fill = guide_colourbar(direction = "horizontal", title.position = "top", title=expression("Salinity"))) +
  scale_x_continuous(breaks = c(as.POSIXct("2012-01-22", tz = "UTC"), as.POSIXct("2012-01-23", tz = "UTC"), as.POSIXct("2012-01-24", tz = "UTC"), as.POSIXct("2012-01-25", tz = "UTC"), as.POSIXct("2012-01-26", tz = "UTC")),
                     labels = c("","","","",""), limits = c(as.POSIXct("2012-01-21 04:00:00", tz = "UTC"), as.POSIXct("2012-01-26 21:00:00", tz = "UTC"))) +
  # labels = c("Jan 22", "Jan 23", "Jan 24", "Jan 25", "Jan 26"), limits = c(as.POSIXct("2012-01-21 04:00:00", tz = "UTC"), as.POSIXct("2012-01-26 21:00:00", tz = "UTC"))) +
  geom_smooth(data=tidy_MLD, aes(Time,Pressure_rho), method="loess", se=FALSE, span=0.1, colour="white", linewidth = 0.5)

################
# Run the scipt Load_wind.R before executing the patchwork command below.

# facet plot using patchwork package
# pWind/pTmp/pTmp_VM/pSal/pSal_VM

# Use the following command to create the output file
# ggsave(filename = "SH_VM_CTD.png", device = "png", scale = 1, width = 6, height = 6, units = "in", dpi = 1200)

