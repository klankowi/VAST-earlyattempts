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
head(stations)
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
stations$id <- seq(1, nrow(stations))

stations <- st_as_sf(stations, coords=c('LON', 'LAT'))
st_crs(stations) <- "EPSG:4326"

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
  geom_sf(data=stations, cex=1,
          aes(col=SURVEY)) +
  geom_sf(data=portland) +
  coord_sf(xlim=c(-71, -68.95),
           ylim=c(41.95, 43.65)) +
  ylab('Latitude') +
  xlab('Longitude')

# Split to surveys
blls <- subset(stations, SURVEY == "NEFSC BLLS")
bts <- subset(stations, SURVEY == 'NEFSC BTS')

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
  stratcombo <- subset(stations,
                       stations$SURVEY == selecdf.blls$SURVEY[i] &
                         stations$dep.strat == selecdf.blls$dep.strat[i] &
                         stations$COND == selecdf.blls$COND[i])
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

for(i in 1:nrow(selecdf.bts)){
  stratcombo <- subset(stations,
                       stations$SURVEY == selecdf.bts$SURVEY[i] &
                         stations$dep.strat == selecdf.bts$dep.strat[i] &
                         stations$COND == selecdf.bts$COND[i])
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

rand.points <- stations[stations$id %in% stations_blank$id,]
# Load Portland dock
portland <- data.frame(
  id = "Portland",
  YEAR = NA,
  lon = -70.255005, 
  lat = 43.651156,
  endlon = NA, 
  endlat = NA,
  SURVEY = 'Portland',
  STRATUM = NA,
  DEPTH = NA,
  BATHY.DEPTH = NA,
  dep.strat = NA,
  BOTTOM_TYP = NA,
  COND = NA)
portland <- st_as_sf(portland, coords=c('lon', 'lat'))
st_crs(portland) <- 'EPSG:4326'

rand.points <- rbind(portland, rand.points)
row.names(rand.points) <- NULL
head(rand.points)
length(unique(rand.points$id))

rand.points[1, "endlon"] <- -70.255004
rand.points[1, "endlat"] <- 43.651155

# Great. These are our randomly selected points.

# Remove intermediates
rm(blls, bts, portland, selecdf.blls, selecdf.bts,
   stations, stations_blank, stratcombo, i, rowsuse, workingwith)

# Plot basic
ggplot() +
  geom_sf(data=coast) +
  geom_sf(data=rand.points, cex=1,
          aes(col=SURVEY)) +
  #geom_sf(data=portland) +
  coord_sf(xlim=c(-71, -68.95),
           ylim=c(41.95, 43.65)) +
  ylab('Latitude') +
  xlab('Longitude')

# Traveling salesperson problem - what is the most efficient order to get these
tsp <- ((st_distance(rand.points)))
colnames(tsp) <- rand.points$id
tsp <- TSP(as.dist(tsp))
tour.list <- vector(mode='list', 100)
for(i in 1:length(tour.list)){
  tour.list[[i]] <- solve_TSP(tsp, method = 'farthest_insertion')
}
length.list <- vector(mode='list', 100)
for(i in 1:length(length.list)){
  length.list[[i]] <- tour_length(tour.list[[i]])
}
length.list <- do.call(rbind, length.list)
length.list <- as.data.frame(length.list)
length.list$run <- seq(1, nrow(length.list))
shortest.run <- length.list$run[length.list$V1 == min(length.list$V1)]

# Shortest path of 100 runs
path <- cut_tour(tour.list[[shortest.run]], "Portland", exclude_cut = FALSE)
path.lines <- st_sfc(
  st_cast(
    do.call(c, st_geometry(rand.points[c(path),])),
    'LINESTRING'
  ), crs = 4326
)
path.lines <- st_as_sf(path.lines)

path.lines <- nngeo::st_segments(path.lines, progress = TRUE)
path.lines$id <- seq(1:nrow(path.lines))

#rm(length.list, path.lines, tour.list, i, path, shortest.run, tsp)

transects.start <- sfheaders::sf_to_df(rand.points, fill=T)
transects.end <- dplyr::select(transects.start,
                               id, SURVEY, endlon, endlat)
transects.start <- dplyr::select(transects.start,
                                 id, SURVEY, x, y)

transects.start <- st_as_sf(transects.start, coords=c('x', 'y'))
transects.start$loc <- 1
transects.start$sid <- paste0('s', transects.start$id)
transects.start <- transects.start[match(names(path), transects.start$id),]
transects.start$order <- seq(1, nrow(transects.start))

transects.end <- st_as_sf(transects.end, coords=c('endlon', 'endlat'))
transects.end$loc <- 2
transects.end$sid <- paste0('e', transects.end$id)
transects.end <- transects.end[match(names(path), transects.end$id),]
transects.end$order <- seq(1, nrow(transects.end))

transects <- rbind(transects.start, transects.end)
transects <- transects[with(transects, order(order, loc)),]

points <- st_cast(st_geometry(transects), "POINT") 

# Number of total linestrings to be created
n <- length(points) - 1

# Build linestrings
linestrings <- lapply(X = 1:n, FUN = function(x) {
  
  pair <- st_combine(c(points[x], points[x + 1]))
  line <- st_cast(pair, "LINESTRING")
  return(line)
  
})

mst <- st_multilinestring(do.call("rbind", linestrings))
mst <-  nngeo::st_segments(mst)
mst <- st_sf(mst)
mst$len <- st_length(mst)
mst$use <- NA
mst$use[seq(1, 401, 2)] <- 'transect'
mst$use[seq(2, 400, 2)] <- 'steam'
mst$id[mst$use == 'transect'] <- seq(1, 201)
st_crs(mst) <- "EPSG:4326"

ggplot() +
  #geom_sf(data=coast) +
  geom_sf(data=mst, aes(col=use)) + 
  #coord_sf(xlim=c(-71, -68.95),
  #         ylim=c(41.95, 43.65)) +
  ylab('Latitude') +
  xlab('Longitude')

fliplist <- c(3, 12, 13, 23, 33, 34, 37, 45, 48, 53, 56, 57, 59, 65, 71, 76, 96,
              100, 107, 108, 110, 114, 115, 116, 117, 119, 120, 121, 132, 133,
              136, 151, 152, 153, 171, 174, 179, 180, 183, 185, 188, 198)

pleaseflip <- transects[transects$order %in% fliplist,]
pleaseflip$newloc[pleaseflip$loc == 1] <-2 
pleaseflip$newloc[pleaseflip$loc == 2] <- 1

pleaseflip$loc <- pleaseflip$newloc
pleaseflip$newloc <- NULL

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

transects <- transects[transects$order %notin% fliplist,]

transects <- rbind(transects, pleaseflip)
transects <- transects[with(transects, order(order, loc)),]

points <- st_cast(st_geometry(transects), "POINT") 

# Number of total linestrings to be created
n <- length(points) - 1

# Build linestrings
linestrings <- lapply(X = 1:n, FUN = function(x) {
  
  pair <- st_combine(c(points[x], points[x + 1]))
  line <- st_cast(pair, "LINESTRING")
  return(line)
  
})

mst <- st_multilinestring(do.call("rbind", linestrings))
mst <-  nngeo::st_segments(mst)
mst <- st_sf(mst)
mst$len <- st_length(mst)
mst$use <- NA
mst$use[seq(1, 401, 2)] <- 'transect'
mst$use[seq(2, 400, 2)] <- 'steam'
mst$id[mst$use == 'transect'] <- seq(1, 201)
st_crs(mst) <- "EPSG:4326"

ggplot() +
  geom_sf(data=coast) +
  geom_sf(data=mst[mst$use == 'transect',]) + 
  coord_sf(xlim=c(-71, -68.95),
           ylim=c(41.95, 43.65)) +
  ylab('Latitude') +
  xlab('Longitude')

# Plot against sediments
sed <- ggplot() +
  geom_sf(data=grid, col=NA, aes(fill=value)) +
  scale_fill_viridis_c(option='viridis',
                       na.value = NA,
                       direction = 1,
                       begin=0.4, end=1) +
  geom_sf(data=coast) +
  geom_sf(data=mst[mst$use == 'transect',]) + 
  coord_sf(xlim=c(-71, -68.95),
           ylim=c(41.95, 43.65)) +
  theme(axis.text.x=element_text(size=9),
        axis.text.y=element_text(size=9)) +
  facet_wrap(vars(sed.ty))
sed$labels$fill <- 'Probability'
plot(sed)
ggsave(sed,
       device='png',
       filename='Random_sediment.png')

# Plot against rugosity
rug <- ggplot() +
  geom_sf(data=rugos, col=NA, 
          aes(fill=as.factor(COND))) +
  geom_sf(data=coast) +
  geom_sf(data=mst[mst$use == 'transect',]) + 
  coord_sf(xlim=c(-71, -68.95),
           ylim=c(41.95, 43.65)) +
  theme(axis.text.x=element_text(size=9),
        axis.text.y=element_text(size=9))
plot(rug)
rug$labels$fill <- 'Rugosity condition'
ggsave(rug,
       device='png',
       filename='Random_rugosity.png')

mst <- st_transform(mst, "EPSG:4326")
coast <- st_transform(coast, "EPSG:4326")

dep <- ggplot() +
  geom_contour_filled(data = BathyData, aes(x = lon, y = lat, z = value),
                      breaks = dbreaks) + 
  scale_fill_manual(values =  dcol) +
  geom_sf(data=coast) +
  geom_sf(data=mst[mst$use == 'transect',]) + 
  coord_sf(xlim=c(-71, -68.95),
           ylim=c(41.95, 43.65)) +
  theme(legend.position = 'n') +
  xlab(" ") + ylab(" ")
plot(dep)
ggsave(dep,
       device='png',
       filename='Random_depth.png')




mst <- st_transform(mst, "EPSG:32619")
mst$len <- as.numeric(st_length(mst))
mst$len.nmi <- mst$len * 0.000539957
mst

mst <- mst[-1,]

mst$time <- NA
mst$time[1] <- mst$len.nmi[1] / 20
mst$time[seq(2, 400, 2)] <- mst$len.nmi[seq(2, 400, 2)] / 9
mst$time[seq(3, 399, 2)] <- mst$len.nmi[seq(3, 399, 2)] / 12

mst
sum(mst$time) / 24
rownames(mst) <- NULL
# Less than 4 days working time in total.
# Now need to cut to 9 hour working days, dock to dock.
# Load Portland dock
portland <- data.frame(
  lon = -70.255005, 
  lat = 43.651156,
  id='Portland',
  SURVEY = 'Portland',
  loc=NA,
  sid=NA,
  order=NA)
portland <- st_as_sf(portland, coords=c('lon', 'lat'))
st_crs(portland) <- 'EPSG:4326'

#### Day 1 ####

cut <- 38
sum(mst$time[1:cut])

day1 <- mst[1:cut,]

endpoint <- transects[transects$order == paste0(mst$id[cut]),]
endpoint <- endpoint[2,]
st_crs(endpoint) <- "EPSG:4326"
endsteam <- rbind(endpoint, portland)
points <- st_cast(st_geometry(endsteam), "POINT") 

# Number of total linestrings to be created
n <- length(points) - 1

# Build linestrings
linestrings <- lapply(X = 1:n, FUN = function(x) {
  
  pair <- st_combine(c(points[x], points[x + 1]))
  line <- st_cast(pair, "LINESTRING")
  return(line)
  
})

endsteam <- st_multilinestring(do.call("rbind", linestrings))
endsteam <-  nngeo::st_segments(endsteam)
st_crs(endsteam) <- "EPSG:4326"
endsteam <- st_sf(endsteam)
endsteam$len <- st_length(endsteam)
endsteam$use <- 'steam'
endsteam$id <- NA
st_crs(endsteam) <- "EPSG:4326"
endsteam <- st_transform(endsteam, "EPSG:32619")
endsteam$len <- as.numeric(st_length(endsteam))
endsteam$len.nmi <- endsteam$len * 0.000539957
endsteam

endsteam$time <- NA
endsteam$time[1] <- endsteam$len.nmi[1] / 20
endsteam <- endsteam %>% 
  mutate(mst = endsteam)
endsteam$endsteam <- NULL
st_geometry(endsteam) <- 'mst'

day1 <- rbind(day1, endsteam)

sum(day1$time)

coast <- st_transform(coast, "EPSG:32619")

ggplot() +
  geom_sf(data=coast) +
  geom_sf(data=day1, aes(col=use)) + 
  coord_sf(xlim=c(st_bbox(day1)[1], st_bbox(day1)[3]),
           ylim=c(st_bbox(day1)[2], st_bbox(day1)[4])) +
  ylab('Latitude') +
  xlab('Longitude')

#### Day 2 ####

old <- 40
cut <- 54
sum(mst$time[old:cut])

day2 <- mst[old:cut,]

endpoint <- transects[transects$order == paste0(mst$id[cut]),]
endpoint <- endpoint[2,]
st_crs(endpoint) <- "EPSG:4326"
endsteam <- rbind(endpoint, portland)
points <- st_cast(st_geometry(endsteam), "POINT") 

# Number of total linestrings to be created
n <- length(points) - 1

# Build linestrings
linestrings <- lapply(X = 1:n, FUN = function(x) {
  
  pair <- st_combine(c(points[x], points[x + 1]))
  line <- st_cast(pair, "LINESTRING")
  return(line)
  
})

endsteam <- st_multilinestring(do.call("rbind", linestrings))
endsteam <-  nngeo::st_segments(endsteam)
st_crs(endsteam) <- "EPSG:4326"
endsteam <- st_sf(endsteam)
endsteam$len <- st_length(endsteam)
endsteam$use <- 'steam'
endsteam$id <- NA
st_crs(endsteam) <- "EPSG:4326"
endsteam <- st_transform(endsteam, "EPSG:32619")
endsteam$len <- as.numeric(st_length(endsteam))
endsteam$len.nmi <- endsteam$len * 0.000539957
endsteam

endsteam$time <- NA
endsteam$time[1] <- endsteam$len.nmi[1] / 20
endsteam <- endsteam %>% 
  mutate(mst = endsteam)
endsteam$endsteam <- NULL
st_geometry(endsteam) <- 'mst'

# Start
startpoint <- transects[transects$order == paste0(mst$id[old]),]
startpoint <- startpoint[2,]
st_crs(startpoint) <- "EPSG:4326"
startsteam <- rbind(portland, startpoint)
points <- st_cast(st_geometry(startsteam), "POINT") 

# Number of total linestrings to be created
n <- length(points) - 1

# Build linestrings
linestrings <- lapply(X = 1:n, FUN = function(x) {
  
  pair <- st_combine(c(points[x], points[x + 1]))
  line <- st_cast(pair, "LINESTRING")
  return(line)
  
})

startsteam <- st_multilinestring(do.call("rbind", linestrings))
startsteam <-  nngeo::st_segments(startsteam)
st_crs(startsteam) <- "EPSG:4326"
startsteam <- st_sf(startsteam)
startsteam$len <- st_length(startsteam)
startsteam$use <- 'steam'
startsteam$id <- NA
st_crs(startsteam) <- "EPSG:4326"
startsteam <- st_transform(startsteam, "EPSG:32619")
startsteam$len <- as.numeric(st_length(startsteam))
startsteam$len.nmi <- startsteam$len * 0.000539957
startsteam

startsteam$time <- NA
startsteam$time[1] <- startsteam$len.nmi[1] / 20
startsteam <- startsteam %>% 
  mutate(mst = startsteam)
startsteam$startsteam <- NULL
st_geometry(startsteam) <- 'mst'

day2 <- rbind(startsteam, day2, endsteam)

sum(day2$time)

coast <- st_transform(coast, "EPSG:32619")

ggplot() +
  geom_sf(data=coast) +
  geom_sf(data=day2, aes(col=use)) + 
  coord_sf(xlim=c(st_bbox(day2)[1], st_bbox(day2)[3]),
           ylim=c(st_bbox(day2)[2], st_bbox(day2)[4])) +
  ylab('Latitude') +
  xlab('Longitude')

#### Day 3 ####

old <- 56
cut <- 68
sum(mst$time[old:cut])

day3 <- mst[old:cut,]

endpoint <- transects[transects$order == paste0(mst$id[cut]),]
endpoint <- endpoint[2,]
st_crs(endpoint) <- "EPSG:4326"
endsteam <- rbind(endpoint, portland)
points <- st_cast(st_geometry(endsteam), "POINT") 

# Number of total linestrings to be created
n <- length(points) - 1

# Build linestrings
linestrings <- lapply(X = 1:n, FUN = function(x) {
  
  pair <- st_combine(c(points[x], points[x + 1]))
  line <- st_cast(pair, "LINESTRING")
  return(line)
  
})

endsteam <- st_multilinestring(do.call("rbind", linestrings))
endsteam <-  nngeo::st_segments(endsteam)
st_crs(endsteam) <- "EPSG:4326"
endsteam <- st_sf(endsteam)
endsteam$len <- st_length(endsteam)
endsteam$use <- 'steam'
endsteam$id <- NA
st_crs(endsteam) <- "EPSG:4326"
endsteam <- st_transform(endsteam, "EPSG:32619")
endsteam$len <- as.numeric(st_length(endsteam))
endsteam$len.nmi <- endsteam$len * 0.000539957
endsteam

endsteam$time <- NA
endsteam$time[1] <- endsteam$len.nmi[1] / 20
endsteam <- endsteam %>% 
  mutate(mst = endsteam)
endsteam$endsteam <- NULL
st_geometry(endsteam) <- 'mst'

# Start
startpoint <- transects[transects$order == paste0(mst$id[old]),]
startpoint <- startpoint[2,]
st_crs(startpoint) <- "EPSG:4326"
startsteam <- rbind(portland, startpoint)
points <- st_cast(st_geometry(startsteam), "POINT") 

# Number of total linestrings to be created
n <- length(points) - 1

# Build linestrings
linestrings <- lapply(X = 1:n, FUN = function(x) {
  
  pair <- st_combine(c(points[x], points[x + 1]))
  line <- st_cast(pair, "LINESTRING")
  return(line)
  
})

startsteam <- st_multilinestring(do.call("rbind", linestrings))
startsteam <-  nngeo::st_segments(startsteam)
st_crs(startsteam) <- "EPSG:4326"
startsteam <- st_sf(startsteam)
startsteam$len <- st_length(startsteam)
startsteam$use <- 'steam'
startsteam$id <- NA
st_crs(startsteam) <- "EPSG:4326"
startsteam <- st_transform(startsteam, "EPSG:32619")
startsteam$len <- as.numeric(st_length(startsteam))
startsteam$len.nmi <- startsteam$len * 0.000539957
startsteam

startsteam$time <- NA
startsteam$time[1] <- startsteam$len.nmi[1] / 20
startsteam <- startsteam %>% 
  mutate(mst = startsteam)
startsteam$startsteam <- NULL
st_geometry(startsteam) <- 'mst'

day3 <- rbind(startsteam, day3, endsteam)

sum(day3$time)

coast <- st_transform(coast, "EPSG:32619")

ggplot() +
  geom_sf(data=coast) +
  geom_sf(data=day3, aes(col=use)) + 
  coord_sf(xlim=c(st_bbox(day3)[1], st_bbox(day3)[3]),
           ylim=c(st_bbox(day3)[2], st_bbox(day3)[4])) +
  ylab('Latitude') +
  xlab('Longitude')

#### Day 4 ####

old <- 70
cut <- 84
sum(mst$time[old:cut])

day4 <- mst[old:cut,]

endpoint <- transects[transects$order == paste0(mst$id[cut]),]
endpoint <- endpoint[2,]
st_crs(endpoint) <- "EPSG:4326"
endsteam <- rbind(endpoint, portland)
points <- st_cast(st_geometry(endsteam), "POINT") 

# Number of total linestrings to be created
n <- length(points) - 1

# Build linestrings
linestrings <- lapply(X = 1:n, FUN = function(x) {
  
  pair <- st_combine(c(points[x], points[x + 1]))
  line <- st_cast(pair, "LINESTRING")
  return(line)
  
})

endsteam <- st_multilinestring(do.call("rbind", linestrings))
endsteam <-  nngeo::st_segments(endsteam)
st_crs(endsteam) <- "EPSG:4326"
endsteam <- st_sf(endsteam)
endsteam$len <- st_length(endsteam)
endsteam$use <- 'steam'
endsteam$id <- NA
st_crs(endsteam) <- "EPSG:4326"
endsteam <- st_transform(endsteam, "EPSG:32619")
endsteam$len <- as.numeric(st_length(endsteam))
endsteam$len.nmi <- endsteam$len * 0.000539957
endsteam

endsteam$time <- NA
endsteam$time[1] <- endsteam$len.nmi[1] / 20
endsteam <- endsteam %>% 
  mutate(mst = endsteam)
endsteam$endsteam <- NULL
st_geometry(endsteam) <- 'mst'

# Start
startpoint <- transects[transects$order == paste0(mst$id[old]),]
startpoint <- startpoint[2,]
st_crs(startpoint) <- "EPSG:4326"
startsteam <- rbind(portland, startpoint)
points <- st_cast(st_geometry(startsteam), "POINT") 

# Number of total linestrings to be created
n <- length(points) - 1

# Build linestrings
linestrings <- lapply(X = 1:n, FUN = function(x) {
  
  pair <- st_combine(c(points[x], points[x + 1]))
  line <- st_cast(pair, "LINESTRING")
  return(line)
  
})

startsteam <- st_multilinestring(do.call("rbind", linestrings))
startsteam <-  nngeo::st_segments(startsteam)
st_crs(startsteam) <- "EPSG:4326"
startsteam <- st_sf(startsteam)
startsteam$len <- st_length(startsteam)
startsteam$use <- 'steam'
startsteam$id <- NA
st_crs(startsteam) <- "EPSG:4326"
startsteam <- st_transform(startsteam, "EPSG:32619")
startsteam$len <- as.numeric(st_length(startsteam))
startsteam$len.nmi <- startsteam$len * 0.000539957
startsteam

startsteam$time <- NA
startsteam$time[1] <- startsteam$len.nmi[1] / 20
startsteam <- startsteam %>% 
  mutate(mst = startsteam)
startsteam$startsteam <- NULL
st_geometry(startsteam) <- 'mst'

day4 <- rbind(startsteam, day4, endsteam)

sum(day4$time)

coast <- st_transform(coast, "EPSG:32619")

ggplot() +
  geom_sf(data=coast) +
  geom_sf(data=day4, aes(col=use)) + 
  coord_sf(xlim=c(st_bbox(day4)[1], st_bbox(day4)[3]),
           ylim=c(st_bbox(day4)[2], st_bbox(day4)[4])) +
  ylab('Latitude') +
  xlab('Longitude')

#### Day 5 ####

old <- 86
cut <- 104
sum(mst$time[old:cut])

day5 <- mst[old:cut,]

endpoint <- transects[transects$order == paste0(mst$id[cut]),]
endpoint <- endpoint[2,]
st_crs(endpoint) <- "EPSG:4326"
endsteam <- rbind(endpoint, portland)
points <- st_cast(st_geometry(endsteam), "POINT") 

# Number of total linestrings to be created
n <- length(points) - 1

# Build linestrings
linestrings <- lapply(X = 1:n, FUN = function(x) {
  
  pair <- st_combine(c(points[x], points[x + 1]))
  line <- st_cast(pair, "LINESTRING")
  return(line)
  
})

endsteam <- st_multilinestring(do.call("rbind", linestrings))
endsteam <-  nngeo::st_segments(endsteam)
st_crs(endsteam) <- "EPSG:4326"
endsteam <- st_sf(endsteam)
endsteam$len <- st_length(endsteam)
endsteam$use <- 'steam'
endsteam$id <- NA
st_crs(endsteam) <- "EPSG:4326"
endsteam <- st_transform(endsteam, "EPSG:32619")
endsteam$len <- as.numeric(st_length(endsteam))
endsteam$len.nmi <- endsteam$len * 0.000539957
endsteam

endsteam$time <- NA
endsteam$time[1] <- endsteam$len.nmi[1] / 20
endsteam <- endsteam %>% 
  mutate(mst = endsteam)
endsteam$endsteam <- NULL
st_geometry(endsteam) <- 'mst'

# Start
startpoint <- transects[transects$order == paste0(mst$id[old]),]
startpoint <- startpoint[2,]
st_crs(startpoint) <- "EPSG:4326"
startsteam <- rbind(portland, startpoint)
points <- st_cast(st_geometry(startsteam), "POINT") 

# Number of total linestrings to be created
n <- length(points) - 1

# Build linestrings
linestrings <- lapply(X = 1:n, FUN = function(x) {
  
  pair <- st_combine(c(points[x], points[x + 1]))
  line <- st_cast(pair, "LINESTRING")
  return(line)
  
})

startsteam <- st_multilinestring(do.call("rbind", linestrings))
startsteam <-  nngeo::st_segments(startsteam)
st_crs(startsteam) <- "EPSG:4326"
startsteam <- st_sf(startsteam)
startsteam$len <- st_length(startsteam)
startsteam$use <- 'steam'
startsteam$id <- NA
st_crs(startsteam) <- "EPSG:4326"
startsteam <- st_transform(startsteam, "EPSG:32619")
startsteam$len <- as.numeric(st_length(startsteam))
startsteam$len.nmi <- startsteam$len * 0.000539957
startsteam

startsteam$time <- NA
startsteam$time[1] <- startsteam$len.nmi[1] / 20
startsteam <- startsteam %>% 
  mutate(mst = startsteam)
startsteam$startsteam <- NULL
st_geometry(startsteam) <- 'mst'

day5 <- rbind(startsteam, day5, endsteam)

sum(day5$time)

coast <- st_transform(coast, "EPSG:32619")

ggplot() +
  geom_sf(data=coast) +
  geom_sf(data=day5, aes(col=use)) + 
  coord_sf(xlim=c(st_bbox(day5)[1], st_bbox(day5)[3]),
           ylim=c(st_bbox(day5)[2], st_bbox(day5)[4])) +
  ylab('Latitude') +
  xlab('Longitude')


#### Day 6 ####

old <- 106
cut <- 118
sum(mst$time[old:cut])

day6 <- mst[old:cut,]

endpoint <- transects[transects$order == paste0(mst$id[cut]),]
endpoint <- endpoint[2,]
st_crs(endpoint) <- "EPSG:4326"
endsteam <- rbind(endpoint, portland)
points <- st_cast(st_geometry(endsteam), "POINT") 

# Number of total linestrings to be created
n <- length(points) - 1

# Build linestrings
linestrings <- lapply(X = 1:n, FUN = function(x) {
  
  pair <- st_combine(c(points[x], points[x + 1]))
  line <- st_cast(pair, "LINESTRING")
  return(line)
  
})

endsteam <- st_multilinestring(do.call("rbind", linestrings))
endsteam <-  nngeo::st_segments(endsteam)
st_crs(endsteam) <- "EPSG:4326"
endsteam <- st_sf(endsteam)
endsteam$len <- st_length(endsteam)
endsteam$use <- 'steam'
endsteam$id <- NA
st_crs(endsteam) <- "EPSG:4326"
endsteam <- st_transform(endsteam, "EPSG:32619")
endsteam$len <- as.numeric(st_length(endsteam))
endsteam$len.nmi <- endsteam$len * 0.000539957
endsteam

endsteam$time <- NA
endsteam$time[1] <- endsteam$len.nmi[1] / 20
endsteam <- endsteam %>% 
  mutate(mst = endsteam)
endsteam$endsteam <- NULL
st_geometry(endsteam) <- 'mst'

# Start
startpoint <- transects[transects$order == paste0(mst$id[old]),]
startpoint <- startpoint[2,]
st_crs(startpoint) <- "EPSG:4326"
startsteam <- rbind(portland, startpoint)
points <- st_cast(st_geometry(startsteam), "POINT") 

# Number of total linestrings to be created
n <- length(points) - 1

# Build linestrings
linestrings <- lapply(X = 1:n, FUN = function(x) {
  
  pair <- st_combine(c(points[x], points[x + 1]))
  line <- st_cast(pair, "LINESTRING")
  return(line)
  
})

startsteam <- st_multilinestring(do.call("rbind", linestrings))
startsteam <-  nngeo::st_segments(startsteam)
st_crs(startsteam) <- "EPSG:4326"
startsteam <- st_sf(startsteam)
startsteam$len <- st_length(startsteam)
startsteam$use <- 'steam'
startsteam$id <- NA
st_crs(startsteam) <- "EPSG:4326"
startsteam <- st_transform(startsteam, "EPSG:32619")
startsteam$len <- as.numeric(st_length(startsteam))
startsteam$len.nmi <- startsteam$len * 0.000539957
startsteam

startsteam$time <- NA
startsteam$time[1] <- startsteam$len.nmi[1] / 20
startsteam <- startsteam %>% 
  mutate(mst = startsteam)
startsteam$startsteam <- NULL
st_geometry(startsteam) <- 'mst'

day6 <- rbind(startsteam, day6, endsteam)

sum(day6$time)

coast <- st_transform(coast, "EPSG:32619")

ggplot() +
  geom_sf(data=coast) +
  geom_sf(data=day6, aes(col=use)) + 
  coord_sf(xlim=c(st_bbox(day6)[1], st_bbox(day6)[3]),
           ylim=c(st_bbox(day6)[2], st_bbox(day6)[4])) +
  ylab('Latitude') +
  xlab('Longitude')
