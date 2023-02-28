# Set up workspace
rm(list=ls())
gc()

# Libraries
library(tidyverse)
library(sf)
library(here)
library(fastcluster)
library(fasterize)
library(VAST)
library(rgeoda)
library(geodaData)
library(sp)
library(raster)

# Set GGplot auto theme
theme_set(theme(plot.margin = unit(c(0,0,0,0), "cm"),
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

# Adjust ecodata coast projection
coast <- ecodata::coast
coast <- st_transform(coast, crs = "EPSG:32619")

# Surveys
surveys <- read.csv(here("data/Dataframes/Survey_Data.csv"))
surveys <- subset(surveys, !is.na(LAT))
surveys <- subset(surveys, YEAR > 1981)
surveys <- st_as_sf(surveys, coords=c('LON', 'LAT'))
st_crs(surveys) <- 'EPSG:4326'
surveys <- st_transform(surveys, crs = "EPSG:32619")

# Closed areas
closed.areas <- st_read(here("data/GIS/closed_areas_wgs.shp"))
closed.areas <- st_transform(closed.areas, crs="EPSG:32619")

# # VAST Output
# load(here("VAST_runs/StrataDensCats_7/strata_cats_7.RData"))
# 
# # Make VAST grid (2000 cells)
# CRS_orig = sp::CRS("+proj=longlat")
# CRS_proj = sp::CRS("+init=epsg:32619")
# loc_g <- cbind(fit$extrapolation_list$Data_Extrap[,"Lon"], 
#                fit$extrapolation_list$Data_Extrap[,"Lat"])
# 
# n_cells <- dim(loc_g)[1]
# 
# Points_orig = sp::SpatialPointsDataFrame(coords = loc_g, 
#                                          data = data.frame(Inc = rep(1, 
#                                                                 nrow(loc_g))), 
#                                          proj4string = CRS_orig)
# 
# Points_LongLat = sp::spTransform(Points_orig, sp::CRS("+proj=longlat"))
# 
# Points_proj = sp::spTransform(Points_orig, CRS_proj)
# 
# cell.size = mean(diff(Points_proj@bbox[1, ]), 
#                  diff(Points_proj@bbox[2,]))/floor(sqrt(n_cells))
# Points_sf = sf::st_as_sf(Points_proj)
# grid = sf::st_make_grid(Points_sf, cellsize = cell.size)
# grid_i = sf::st_intersects(Points_sf, grid)
# grid = sf::st_sf(grid, Inc = tapply(Points_sf$Inc, 
#                                   INDEX = factor(as.numeric(grid_i),
#                                                  levels = 1:length(grid)), 
#                                   FUN = mean, na.rm = TRUE))
# colnames(grid)
# colnames(grid) <- c('Inc', 'geometry')
# st_geometry(grid) <- 'geometry'
# 
# # Remove intermediates
# rm(CRS_orig, CRS_proj, ex3, ex4, fit, grid_i, loc_g,
#    Points_LongLat, Points_orig, Points_proj, Points_sf,
#    scaled.covars, settings, strata_use, survs, user_region, cell.size,
#    n_cells, seas.labs, year.labs)
# 
# # Add columns for mean sediment values
# grid$cobble <- NA; grid$gravel <- NA; grid$mud <- NA
# grid$sand <- NA; grid$rock <- NA
# 
# # Substrate
# seds <- st_read(here('data/GIS/Sediment_Krig_1K_Polygons.shp'))
# seds <- st_transform(seds, crs="EPSG:32619")
# 
# # Initialize progress bar
# pb <- txtProgressBar(min=0, max=nrow(grid), initial=0, char="=", style=3)
# 
# for(i in 1:nrow(grid)){
#   
#   # Update progress bar
#   setTxtProgressBar(pb, i)
#   getTxtProgressBar(pb)
#   
#   # Pull cell of the grid
#   use.cell <- grid[i,]
#   
#   # Make sure it matches CRS
#   use.cell <- st_transform(use.cell, crs=st_crs(seds))
#   
#   # Find overlapping sediment values
#   use.seds <- seds[st_intersects(seds, use.cell, sparse=F),]
#   
#   # Mean sediment value per grid cell
#   grid$cobble[i] <- mean(use.seds$cobble_P)
#   grid$gravel[i] <- mean(use.seds$gravel_P)
#   grid$mud[i]    <- mean(use.seds$mud_P)
#   grid$sand[i]   <- mean(use.seds$sand_P)
#   grid$rock[i]   <- mean(use.seds$rock_P)
#   
#   # Close segment of progress bar every loop
#   close(pb)
#   
# }
# #saveRDS(grid, file=here('data/RData_Storage/sediment_interp_grid.RDS'))
grid <- readRDS(here('data/RData_Storage/sediment_interp_grid.RDS'))

substrates <- c('cobble', 'gravel', #'Rock', 
                'sand', 'mud')

# data.list <- list(grid$cobble, 
#                   grid$gravel,
#                   #grid$rock, 
#                   grid$sand,
#                   grid$mud)
# 
# data.f <- as.data.frame(cbind(grid$cobble, 
#                               grid$gravel,
#                               #grid$rock, 
#                               grid$sand,
#                               grid$mud))
# 
# names(data.list) <- substrates

# data.cats <- data.frame(
#   Cobble = rep(NA, length(grid$cobble)),
#   Gravel = rep(NA, length(grid$cobble)),
#   #Rock = rep(NA, length(grid$cobble)),
#   Sand = rep(NA, length(grid$cobble)),
#   Mud = rep(NA, length(grid$cobble))
# )

for(i in 1:length(substrates)){
  data.use <- as.data.frame(grid[,paste0(substrates[i])])
  names(data.use) <- 'sub'
  data_noNA <- data.use[!is.na(data.use[,'sub']),]
  set.seed(123)
  c <- kmeans(data_noNA[,'sub'], centers = 2, nstart= 10, iter.max = 20)
  lo <- which(c$centers == min(c$centers))
  hi <- which(c$centers == max(c$centers))
  
  c$cluster[c$cluster== lo] <- 'Low'
  c$cluster[c$cluster== hi] <- 'High'
  table(c$cluster)
  
  grid[is.na(data.use[,'sub'])==FALSE,paste0(substrates[i], "_c")] <- c$cluster
  
  # p <- ggplot(coast) +
  #   geom_sf(data = grid , aes(fill = paste0(substrates[i], "_c"))) + 
  #   geom_sf() +
  #   coord_sf(xlim=c(-125*1000, 780*1000),
  #            ylim=c(4050*1000,5000*1000),
  #            crs="EPSG:32619")+
  #   scale_fill_viridis_d(option = "viridis", direction=1,
  #                        na.value = NA, name='Likelihood') +
  #   ggtitle(paste0(substrates[i], ' likelihood')) +
  #   xlab('Longitude') +
  #   ylab('Latitude')
  # plot(p)
  # Save
  # ggsave(p, 
  #        filename = 
  #          paste0("C:/Users/klankowicz/Desktop/",
  #                 substrates[i],
  #                 '.png'),
  #        device="png")
  rm(data.use, data_noNA, c, lo, hi)
}

grid$Cell <- seq(1, nrow(grid))
grid_noNA <- grid[!is.na(grid$cobble_c),]
grid_noNA <- sfheaders::sf_to_df(grid_noNA, fill=T)
grid_noNA <- dplyr::select(grid_noNA, cobble_c, gravel_c, sand_c, mud_c, Cell)
grid_noNA <- unique(grid_noNA)
grid_noNA$Cell <- NULL

for(i in 1:ncol(grid_noNA)){
  grid_noNA[,i] <- as.numeric(factor(grid_noNA[,i],
                                          levels=c('Low', 'High')))
}

wss <- rep(NA, 10)
for (i in 2:10){
    set.seed(123)
    temp <- kmeans(grid_noNA, centers = i, nstart=99, iter.max = 99)
    wss[i] <- temp$betweenss / temp$totss
}
wss <- wss[-1]
plot(2:10, wss, type="b", xlab="Number of groups",
     ylab="Sum of squares within a group")
wss

set.seed(123)
c <- kmeans(grid_noNA, centers = 10, nstart= 10, iter.max = 99)
grid$cluster[!is.na(grid$cobble)] <- c$cluster
cent.vals <- c$centers
cent.vals <- as.data.frame(cent.vals)
cent.vals$num <- seq(1:10)

cent.vals

hospital_names <- list(
  '1'="Sandy-Mud",
  '2'="Mixture",
  '3'="Pure sand",
  '4'="Gravel-Mud",
  '5'="Mixture wo mud",
  '6'="Indefinite",
  '7'="Gravel-Sand",
  '8'="Gravel",
  '9'="Mud-cobble",
  '10'="Mixture wo cobble"
)

hospital_labeller <- function(variable,value){
  return(hospital_names[value])
}

# Plot
ggplot(coast) +
     geom_sf(data=grid[!is.na(grid$cluster),], fill='black', col='black') +
     scale_fill_viridis_d(na.value = NA, option='viridis') +
     geom_sf() +
     coord_sf(xlim=c(-125*1000, 780*1000),
                              ylim=c(4050*1000,5000*1000),
                              crs="EPSG:32619")+
     facet_wrap(vars(cluster), labeller = hospital_labeller)

# Outcome
# Groups 5 and 2 are "harder" (high cobble)
# Groups 7,8,4,9, 10 are "mixed" (high gravel or some cobble)
# Groups 1,3,6 are smooth (no cobble and low gravel content)

grid$outcome <- NA
grid$outcome[grid$clust %in% c(5,2)] <- 'Hard'
grid$outcome[grid$clust %in% c(7,8,4,9,10)] <- 'Mix'
grid$outcome[grid$clust %in% c(1,3,6,9)] <- 'Soft'

ggplot(coast) +
  geom_sf(data=grid[!is.na(grid$outcome),], aes(fill=as.factor(outcome))) +
  scale_fill_viridis_d(na.value = NA, option='viridis', name="Bottom") +
  geom_sf() +
  coord_sf(xlim=c(-125*1000, 780*1000),
           ylim=c(4050*1000,5000*1000),
           crs="EPSG:32619")

hard <- grid[grid$outcome == "Hard" & !is.na(grid$outcome),]
in.hard <- surveys[st_intersects(surveys, hard, 
                                  sparse=F),]
in.hard <- subset(in.hard, !is.na(SURVEY))
for(i in 2:nrow(hard)){
  test <- surveys[st_intersects(surveys, hard[i,], sparse=F),]
  test <- subset(test, !is.na(SURVEY))
  in.hard <- rbind(in.hard, test)
}
table(in.hard$SURVEY)
table(in.hard$YEAR)
table(in.hard$YEAR, in.hard$SURVEY)

mix <- grid[grid$outcome == "Mix" & !is.na(grid$outcome),]
in.mix <- surveys[st_intersects(surveys, mix, 
                                 sparse=F),]
in.mix <- subset(in.mix, !is.na(SURVEY))
for(i in 2:nrow(mix)){
  test <- surveys[st_intersects(surveys, mix[i,], sparse=F),]
  test <- subset(test, !is.na(SURVEY))
  in.mix <- rbind(in.mix, test)
}
table(in.mix$SURVEY)
table(in.mix$YEAR)

soft <- grid[grid$outcome == "Soft" & !is.na(grid$outcome),]
in.soft <- surveys[st_intersects(surveys, soft, 
                                 sparse=F),]
in.soft <- subset(in.soft, !is.na(SURVEY))
for(i in 2:nrow(soft)){
  test <- surveys[st_intersects(surveys, soft[i,], sparse=F),]
  test <- subset(test, !is.na(SURVEY))
  in.soft <- rbind(in.soft, test)
}
table(in.soft$SURVEY)
table(in.soft$YEAR)

save.image(here('data/RData_Storage/sediment_grids.RData'))
