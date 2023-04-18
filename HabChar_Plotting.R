rm(list=ls())

library(tidyverse)
library(here)
library(sf)
library(raster)

# Load sediments
load(here('data/RData_Storage/sediment_grids.RData'))
rm(c, cent.vals, closed.areas, grid_noNA, hard, hospital_names,
   in.hard, in.soft, in.mix, mix, soft, temp, substrates, wss,
   hospital_labeller, i, surveys, coast)

# Reshape grid
gridold <- grid
grid <- dplyr::select(grid, cobble, gravel, mud, sand, rock, 
                      cluster, Cell, outcome, geometry)
grid <- st_transform(grid, crs="EPSG:4326")
grid <- rbind(grid, grid, grid, grid, grid)
grid$value[1:1980] <- grid$cobble[1:1980]
grid$sed.ty[1:1980] <- 'cobble'

grid$value[1981:3960] <- grid$gravel[1:1980]
grid$sed.ty[1981:3960] <- 'gravel'

grid$value[3961:5940] <- grid$mud[1:1980]
grid$sed.ty[3961:5940] <- 'mud'

grid$value[5941:7920] <- grid$sand[1:1980]
grid$sed.ty[5941:7920] <- 'sand'

grid$value[7921:9900] <- grid$rock[1:1980]
grid$sed.ty[7921:9900] <- 'rock'

# Load rugosity
rugos <- st_read(paste0("C:/Users/klankowicz/Box/Katie Lankowicz/",
                        "Data_Analysis/Cod/GIS/Rugosity_15as_F.shp"))
rugos <- st_transform(rugos, st_crs(grid))
rugos <- dplyr::select(rugos, rugosity, COND, geometry)
rugos.small <- dplyr::select(rugos, COND, geometry)
rugos.small$COND[rugos.small$COND=='ROUGH'] <- 1
rugos.small$COND[rugos.small$COND=='SMOOTH'] <- 2
rugos.small$COND <- as.numeric(rugos.small$COND)
test <- stars::st_rasterize(rugos.small)
plot(test)

# Load surveys
survs <- read.csv(here('data/Dataframes/cod_agesep_VASTdata.csv'))
blls <- subset(survs, SURVEY == 'NEFSC BLLS')
blls <- subset(blls, AGEGROUP == 'Age2-5')
rm(survs)
blls <- st_as_sf(blls, coords=c('LON', 'LAT'), crs='EPSG:4326')

# Load coast
coast <- ecodata::coast
coast <- st_transform(coast, crs=st_crs(grid))

# Plot against sediments
sed <- ggplot() +
  geom_sf(data=grid, col=NA, aes(fill=value)) +
  scale_fill_viridis_c(option='viridis',
                       na.value = NA,
                       direction = 1,
                       begin=0.2, end=1) +
  geom_sf(data=coast) +
  geom_sf(data=blls, cex=0.5) +
  coord_sf(xlim=c(-71, -67.5),
           ylim=c(42, 43.5)) +
  facet_wrap(vars(sed.ty))
ggsave(sed,
       device='png',
       filename='BLLS_sediment.png')

sedclus <- ggplot() +
  geom_sf(data=grid, col=NA, aes(fill=as.factor(cluster))) +
  scale_fill_viridis_d(option='viridis',
                       na.value = NA,
                       direction = 1,
                       begin=0.2, end=1) +
  geom_sf(data=coast) +
  geom_sf(data=blls, cex=0.5) +
  coord_sf(xlim=c(-71, -67.5),
           ylim=c(42, 43.5))

gridclus <- dplyr::select(grid, cluster)
test <- st_intersection(blls, gridclus)
head(test)
table(test$cluster)

head(gridold[!is.na(gridold$cluster ) & gridold$cluster == 1,])
# Low cobble and gravel, high sand and mud == SOFT
head(gridold[!is.na(gridold$cluster ) & gridold$cluster == 2,])
# High everything == HARD
head(gridold[!is.na(gridold$cluster ) & gridold$cluster == 4,])
head(gridold[!is.na(gridold$cluster ) & gridold$cluster == 9,])
head(gridold[!is.na(gridold$cluster ) & gridold$cluster == 10,])

# Plot against rugosity
rug <- ggplot() +
  geom_sf(data=rugos, col=NA, 
          aes(fill=as.factor(COND))) +
  geom_sf(data=coast) +
  geom_sf(data=blls, cex=0.5) +
  coord_sf(xlim=c(-71, -67.5),
           ylim=c(42, 43.5))
ggsave(rug,
       device='png',
       filename='BLLS_rugosity.png')

# Plot against depth
# If you're interested in adding a bathymetry layer
library(marmap)
library(raster)
# Pull NOAA bathymetry data
Bathy <- getNOAA.bathy(lon1 = -78, lon2 = -65,
                       lat1 = 35, lat2 = 47, 
                       resolution = 1)
# Ignore any error messages, it still produces what we want.

# Now do a bit of data wrangling so we can plot it:
# Convert data to matrix
Bathy_Final <- as.matrix(Bathy)
class(Bathy_Final) <- "matrix"

# Reshape for plotting
BathyData <- Bathy_Final %>%
  as.data.frame() %>% 
  rownames_to_column(var = "lon") %>%
  gather(lat, value, -1) %>%
  mutate_all(funs(as.numeric))

# Set depth breaks and colors
dbreaks <- c(seq(0, 20, 2),
             seq(20, 40, 4),
             seq(40, 70, 5),
             seq(70, 100, 10),
             seq(100, 200, 20),
             seq(200, 400, 50),
             seq(400, 1000, 100),
             seq(1000, 2000, 200),
             seq(2000, 4000, 500),
             seq(4000, 7000, 1000))
dbreaks <- unique(dbreaks) * -1
dcol <- colorRampPalette(c("#DEF5E5", "#40498E"))
dcol <- dcol(length(dbreaks))

dep <- ggplot() +
  geom_contour_filled(data = BathyData, aes(x = lon, y = lat, z = value),
                      breaks = dbreaks) + 
  scale_fill_manual(values =  dcol) +
  geom_sf(data=coast) +
  geom_sf(data=blls, cex=0.5) +
  coord_sf(xlim=c(-71, -67.5),
           ylim=c(42, 43.5)) +
  theme(legend.position = 'n')
ggsave(dep,
       device='png',
       filename='BLLS_rugosity.png')

head(blls)
depbreaks <- c(seq(0,100,5),
               seq(125, 500, 25))
