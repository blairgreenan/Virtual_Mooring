# MLD_VM.R
# Blair Greenan
# Fisheries and Oceans Canada
# 01 Dec 2023
#
# Description: this script computes the mixed layer depth for Mike Dinniman's
# model data simulated at the SeaHorse mooring location on Ross Bank. The model
# data is in the form of a row with a time stamp (in 15 min intervals) followed
# by 240 rows of depth and salinity/temperature/density data.  Using a density
# criteria to determine the MLD
#
# load libraries
library(tidyverse)
library(lubridate)
library(interp)


# load the Dinniman model data
load("Virtual_Mooring.RData")

MLD_VM <- numeric(0)

# Create a set of unique times for each model profile of T/S/rho
unique_time <- unique(data_tibble$DateTime)

# filter out the data above 10m and below 190m depths to eliminate issues near the boundary.
# The bottom criteria does not really apply for the model like the SeaHorse data, but just keeping it for consistency
# Loop through each profile time and compute the mixed layer depth

for (ut in unique_time) {
  Press_subset <- subset(data_tibble$WaterDepth,  data_tibble$DateTime == ut & data_tibble$WaterDepth> -190 & data_tibble$WaterDepth< -10)
  Tmp_subset <- subset(data_tibble$Temperature, data_tibble$DateTime == ut & data_tibble$WaterDepth> -190 & data_tibble$WaterDepth< -10)
  SigmaT_subset <- subset(data_tibble$Density, data_tibble$DateTime == ut & data_tibble$WaterDepth> -190 & data_tibble$WaterDepth< -10)
  criterion_T <- 0.1
  criterion_rho <- 0.125
  inMLD_T <- abs(Tmp_subset[1]-Tmp_subset) < criterion_T
  MLDindex_T <- which.min(inMLD_T)
  MLDpressure_T <- Press_subset[MLDindex_T]
  inMLD_rho <- abs(SigmaT_subset[1]-SigmaT_subset) < criterion_rho
  MLDindex_rho <- which.min(inMLD_rho)
  MLDpressure_rho <- Press_subset[MLDindex_rho]
  # there seem to be cases where the density criterion is not met through the whole water column
  if (MLDpressure_rho>150){
    MLDpressure_rho <- NA
  }

MLD_VM <- c(MLD_VM, MLDpressure_rho)

}

# create a data frame with the profile time and the estimated mixed layer depth
MLD_df <- data.frame(unique_time, MLD_VM)

# save the data frame
save(file = "MLD_VM.rdata", MLD_df)


