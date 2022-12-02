# Adapted from code by Sarah Gaichas
# Joins existing field station SST data and OISST satellite SST data, tests
# relationship between the two, visualizes OISST inputs at stations missing
# empirical data.

rm(list=ls())

# Load libraries
library(tidyverse)
library(here)
library(DT)
library(FishStatsUtils)
library(sf)
library(raster)
library(terra)
library(nngeo)
library(data.table)

#list of SST dataframes
SSTdfs <- list.files(here("data-raw/gridded/sst_data/"), pattern = "*.rds")

# Create empty tibble to fill
stn_OISST <- tibble()

# Load statino data
stations <- read.csv(paste0(here("data/Survey_Data.csv")))

# Create datestring to merge SST and OISST
stations$DATE <- as.POSIXct(stations$DATE, 
                            format = "%m/%d/%Y")
stations$month <- substr(stations$DATE, start=6, stop=7)
stations$day <- substr(stations$DATE, start=9, stop=10)
stations <- subset(stations, is.na(stations$LAT)==FALSE)
stations <- subset(stations, is.na(stations$LON)==FALSE)
stations$yrmody <- paste0(stations$YEAR, stations$month, stations$day)
stations <- st_as_sf(stations, coords=c("LON", "LAT"),
                     na.fail=T)

# Initialize progress bar
pb <- txtProgressBar(min=0, max=length(SSTdfs), initial=0, char="=", style=3)

# Merge in year-based loops
for(df in SSTdfs){
  # Call the number of the current .rds file in the vector of .rds files
  t <- match(df, SSTdfs)
  
  # Update progress bar
  setTxtProgressBar(pb, t)
  getTxtProgressBar(pb)
  
  # Call annual .rds OISST file
  sstdf <- readRDS(paste0(here("data-raw/gridded/sst_data/", df)))
  
  # Create string of sampled years (removes cod data prior to 1982)
  stationsyr <- stations %>%
    filter(YEAR == unique(sstdf$year))
  
  # Filter out OISST days not sampled for cod, convert to sf object
  sstdf_survdays <- sstdf %>%
    dplyr::mutate(yrmody = as.numeric(paste0(year, month, day)) )%>%
    dplyr::filter(yrmody %in% unique(stationsyr$yrmody)) %>%
    dplyr::mutate(year = as.numeric(year),
                  month = as.numeric(month),
                  day = as.numeric(day),
                  declon = Lon,
                  declat = Lat) %>%
    dplyr::select(-Lon, -Lat) %>%
    sf::st_as_sf(coords=c("declon","declat"), crs=4326, remove=FALSE)  
  
  # Join by nearest neighbor and date
  yrdietOISST <- suppressMessages(do.call('rbind', 
                         lapply(split(stationsyr, 
                                      1:nrow(stationsyr)), 
                  function(x) {
                        st_join(x, 
                                sstdf_survdays[sstdf_survdays$yrmody == unique(x$yrmody),],
                                join = st_nn, k = 1, progress = TRUE)
                               }
  )))
  
  # Bind one year of the loop to the initialized tibble
  stn_OISST <- rbind(stn_OISST, yrdietOISST)
  
  # Close segment of progress bar every loop
  close(pb)
  
}

# Save output
saveRDS(stn_OISST, here("data-raw/stn_OISST2.rds"))