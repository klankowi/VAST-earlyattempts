#### Workspace setup ####
# Clear workspace
rm(list=ls())

# Load libraries
library(VAST)
library(sf)
library(tidyverse)
library(rgdal)
library(here)
library(sp)

#### Fix degree sign problem ####
xlabs <- seq(-77, -66, 2)
ylabs <- seq(36, 46, 2)

#### Add spatial grid ####
# Add extrapolation grid
user_region <- readRDS(here('data/RData_Storage/user_region_newmethod.rds'))

#### Add sampling data and covars ####
surveys <- readRDS(here("data/RData_storage/agg_stn_all_OISST.rds"))
st_crs(surveys) <- "EPSG:4326"

ex <- sfheaders::sf_to_df(surveys, fill=T)
ex <- subset(ex, is.na(COD_N)==FALSE)
ex <- ex[with(ex, order(DATE)),]
row.names(ex) <- NULL

#### Make settings ####
survs <- dplyr::select(ex, x, y, YEAR, COD_N, STOCK)
names(survs) <- c('Lon', 'Lat', 'Year', 'Abundance', 'STRATA')
survs$swept <- as_units(1, unitless)
survs$Abundance <- as_units(survs$Abundance, 'counts')

strata.limits <- data.frame("STRATA" = c('EGOM', 'GBK', 'SNE', 'WGOM'))

setwd(here("VAST_Runs/Strata_6thattempt"))

# Run FishStatsUtils::make_settings
settings_out<- make_settings(n_x = 200, 
                             Region = "User", 
                             purpose = "index2",
                             bias.correct = FALSE,
                             fine_scale = FALSE,
                             strata.limits = strata.limits)

user_region$STRATA <- as.character(user_region$Region_Name)
user_region$Region_Name <- NULL

fit <-  fit_model(settings = settings_out,
                  Lat_i = survs$Lat,
                  Lon_i = survs$Lon,
                  t_i   = survs$Year,
                  b_i   = survs$Abundance,
                  a_i   = survs$swept,
                  Region = "User",
                  input_grid = user_region)

plot(fit)