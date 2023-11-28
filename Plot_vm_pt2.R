
library(tidyverse)
library(lubridate)
library(cmocean)
library(interp)
library(patchwork)

load("Virtual_Mooring.RData")

pTmp <- ggplot() +
  geom_tile(data=data_tibble, aes(DateTime, WaterDepth, fill = Temperature)) +
  scale_fill_cmocean(name = "thermal", limits=c(-1.9,0.5), breaks = c(-1.5, -1.0, -0.5, 0.0, 0.5)) +
  labs(x=NULL,y="Depth (m)") +
  guides(fill = guide_colourbar(direction = "horizontal", title.position = "top", title=expression("Temperature (\u00B0C)"))) +
#  scale_x_continuous(labels = NULL) +
  scale_y_continuous(limits = c(-200, 0), breaks = c(-200, -150, -100, -50, 0), labels = c("200", "150", "100", "50", "0"))

pSal <- ggplot() +
  geom_tile(data=data_tibble, aes(DateTime, WaterDepth, fill = Salinity)) +
  scale_fill_cmocean(name = "haline", limits=c(34.2,34.75), breaks = c(34.25, 34.5, 34.75)) +
  labs(x=NULL,y="Depth (m)") +
  guides(fill = guide_colourbar(direction = "horizontal", title.position = "top", title=expression("Salinity"))) +
  #  scale_x_continuous(labels = NULL) +
  scale_y_continuous(limits = c(-200, 0), breaks = c(-200, -150, -100, -50, 0), labels = c("200", "150", "100", "50", "0"))

# facet plot using patchwork package
#pTmp/pSal
