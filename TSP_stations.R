# "Traveling salesperson" analysis of stratified random selection sites

rm(list=ls())

# Load library
library(tidyverse)
library(sf)
library(here)
library(TSP)
library(udunits2)

# Set seed
set.seed(5423)

# Set GGplot auto theme
theme_set(theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', size=1, fill=NA),
                legend.title = element_text(size=12),
                legend.text = element_text(size=10),
                legend.background = element_blank(),
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=16, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

# Load stations
stations <- read.csv(here('possible_tsplocs.csv'))
# Create depth strata
stations$BATHY.DEPTH <- stations$BATHY_DEPT * -1
stations$dep.strat[stations$BATHY.DEPTH < 110] <- 'shallow'
stations$dep.strat[stations$BATHY.DEPTH >= 110] <- 'deep'
stations <- dplyr::select(stations, 
                          YEAR, LON, LAT, 
                          endlon, endlat,
                          SURVEY, STRATUM, 
                          DEPTH, BATHY.DEPTH, dep.strat, 
                          BOTTOM_TYP, COND
                          )

table(stations$dep.strat, stations$SURVEY)
table(stations$COND, stations$SURVEY)

stations.start <- dplyr::select(stations,
                                SURVEY, dep.strat, COND,
                                LON, LAT)
stations.start$id <- seq(1:nrow(stations.start))
stations.start$loc <- 'start'
stations.start <- st_as_sf(stations.start,
                           coords=c('LON', 'LAT'))
st_crs(stations.start) <- 'EPSG:4326'

stations.end <- dplyr::select(stations,
                              SURVEY, dep.strat, COND,
                              endlon, endlat)
stations.end$id <- seq(1:nrow(stations.end))
stations.end$loc <- 'end'
stations.end <- st_as_sf(stations.end,
                           coords=c('endlon', 'endlat'))
st_crs(stations.end) <- 'EPSG:4326'

stations <- rbind(stations.start, stations.end)
stations$loc <- factor(stations$loc,
                       levels=c('start', 'end'))
stations$loc <- as.numeric(stations$loc)

stations <- stations[with(stations,
                          order(SURVEY, id, loc)),]

pts_line <- stations %>% 
  group_by(id) %>% 
  summarize(SURVEY = first(SURVEY), 
            dep.strat = first(dep.strat), 
            COND = first(COND), do_union=FALSE) %>% 
  st_cast("LINESTRING")

# Load coast
coast <- ecodata::coast
coast <- st_transform(coast, crs='EPSG:4326')

# Load Portland dock
portland <- data.frame(
  lon = -70.255005, 
  lat = 43.651156,
  SURVEY = 'Portland',
  STRATUM = NA,
  DEPTH = NA,
  BATHY.DEPTH = NA,
  dep.strat = NA,
  BOTTOM_TYP = NA,
  COND = NA)
portland <- st_as_sf(portland, coords=c('lon', 'lat'))
st_crs(portland) <- 'EPSG:4326'

# Plot basic
ggplot() +
  geom_sf(data=coast) +
  geom_sf(data=pts_line, cex=1,
          aes(col=SURVEY)) +
  geom_sf(data=portland) +
  coord_sf(xlim=c(-71, -68.95),
           ylim=c(41.95, 43.65)) +
  ylab('Latitude') +
  xlab('Longitude')

# Split to surveys
blls <- subset(stations, SURVEY == "NEFSC BLLS" & stations$loc == 1)
bts <- subset(stations, SURVEY == 'NEFSC BTS' & stations$loc == 1)

# Check strata distribution
table(blls$dep.strat, blls$COND)
table(bts$dep.strat, bts$COND)
# Issue: BTS only samples 23 rough/shallow points. Will need to supplement.

# Random selection
selecdf <- data.frame(
  SURVEY = c(rep('NEFSC BLLS', 4), rep('NEFSC BTS', 4)),
  dep.strat = rep(c('shallow', 'deep'), 4),
  COND = rep(c('ROUGH', 'ROUGH', 'SMOOTH', 'SMOOTH'), 2)
)
selecdf.blls <- subset(selecdf, SURVEY == 'NEFSC BLLS')
selecdf.bts <- subset(selecdf, SURVEY == "NEFSC BTS")
rm(selecdf)

stations_blank <- stations[1,]
stations_blank$id <- NA

for(i in 1:nrow(selecdf.blls)){
  stratcombo <- subset(stations.start,
                       stations.start$SURVEY == selecdf.blls$SURVEY[i] &
                         stations.start$dep.strat == selecdf.blls$dep.strat[i] &
                         stations.start$COND == selecdf.blls$COND[i])
  workingwith <- nrow(stratcombo)
  message(i)
  print(workingwith)
  if(workingwith >= 25){
    rowsuse <- seq(1, workingwith)
    rowsuse <- sample(rowsuse, 25, replace=F)
    stratcombo <- stratcombo[rowsuse,]
  }
  stations_blank <- rbind(stations_blank, stratcombo)
  
}
stations_blank <- stations_blank[!is.na(stations_blank$id),]
table(stations_blank$loc)

for(i in 1:nrow(selecdf.bts)){
  stratcombo <- subset(stations.start,
                       stations.start$SURVEY == selecdf.bts$SURVEY[i] &
                         stations.start$dep.strat == selecdf.bts$dep.strat[i] &
                         stations.start$COND == selecdf.bts$COND[i])
  workingwith <- nrow(stratcombo)
  message(i)
  print(workingwith)
  if(workingwith >= 25){
    rowsuse <- seq(1, workingwith)
    rowsuse <- sample(rowsuse, 26, replace=F)
    stratcombo <- stratcombo[rowsuse,]
  }
  if(workingwith < 25){
    rowsuse <- seq(1, workingwith)
    rowsuse <- sample(rowsuse, 22, replace=F)
    stratcombo <- stratcombo[rowsuse,]
  }
  stations_blank <- rbind(stations_blank, stratcombo)
  
}
stations_blank <- stations_blank[!is.na(stations_blank$id),]

table(stations_blank$SURVEY)
table(stations_blank$SURVEY, stations_blank$dep.strat)
table(stations_blank$SURVEY, stations_blank$COND)

# Great. These are our randomly selected points.
# Combine with Portland.
portland <- dplyr::select(portland,
                          SURVEY, dep.strat, COND, geometry)
portland$id <- 'Portland'
portland$loc <- 'Portland'
stations_blank <- rbind(portland, stations_blank)

# Get distance matrix in nmi
dismat <- st_distance(stations_blank)
dismat <- ud.convert(dismat, 'm', 'nautical_mile')
dismat <- as.dist(dismat)
dismat <- TSP(dismat)

# Solve TSP 100 times
tour.list <- vector(mode='list', 5)
for(i in 1:length(tour.list)){
  tour.list[[i]] <- solve_TSP(dismat, method="farthest_insertion")
}
test <- do.call(rbind, tour.list)
leng.list <- vector(mode='list', 5)
for(i in 1:length(leng.list)){
  leng.list[[i]] <- tour_length(tour.list[[i]])
}
leng.df <- do.call(rbind, leng.list)
leng.df <- as.data.frame(leng.df)
names(leng.df) <- 'length'
leng.df$tourno <- seq(1, nrow(leng.df))
minlen <- rownames(leng.df[leng.df$length == min(leng.df$length),])
tour <- tour.list[[as.numeric(minlen)]]
tour.vec <- as.vector(tour)

stations_blank <- stations_blank[stations_blank$SURVEY !='Portland',]



path <- c(cut_tour(tour, 1, exclude_cut = FALSE), 1)

# Plot
ggplot() +
  geom_sf(data = coast) +
  geom_sf(data = stations_blank, aes(col=SURVEY)) +
  geom_sf(data = st_sfc(
    st_cast(
      do.call(c, st_geometry(stations_blank[c(path),])),
      'LINESTRING'
    ), crs = 4326
  )) +
  coord_sf(xlim=c(-71, -68.95),
           ylim=c(41.95, 44)) +
  ylab('Latitude') +
  xlab('Longitude')
