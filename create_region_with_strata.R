# Code adapted from Cecilia O'Leary and Cole Monnahan 
# Create a user-defined extrapolation grid for VAST 

rm(list=ls())

# Load libraries
library(sp)
library(sf)
library(rgdal)
library(here)

# Load shapefile of desired extrapolation region
shp <- readOGR(here("data/GIS/cod_region_UTM.shp"))
head(shp@data)
plot(shp)

# Transform to unprojected lat-lon
sps <- spTransform(shp, 
                   CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"))
plot(sps)

# Find UTM
lon <- sum(bbox(sps)[1,])/2
utmzone <- floor((lon + 180)/6)+1
crs_LL <- CRS('+proj=longlat +ellps=WGS84 +no_defs')
sps@proj4string <- crs_LL

# Convert the final in polygon to UTM
crs_UTM <- CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 
                      +units=m +no_defs "))
region_polygon <- spTransform(sps, crs_UTM)
region_polygon_sf <- st_as_sf(region_polygon)
ggplot(ecodata::coast) +
  geom_sf(data=region_polygon_sf, fill="lightblue") +
  geom_sf() +
  coord_sf(xlim=c(-76, -65), ylim=c(36, 46))

# Construct the extrapolation grid for VAST using sf package
# Size of grid **in meters** (since working in UTM). Controls
# the resolution of the grid.
cell_size <- 1000

# This step is slow at high resolutions
region_grid <- st_make_grid(region_polygon_sf, cellsize = cell_size,
                            what = "centers")

# Convert region_grid to Spatial Points to SpatialPointsDataFrame
region_grid <- as(region_grid, "Spatial")
region_grid_sp <- as(region_grid, "SpatialPointsDataFrame")

# combine shapefile data (region_polygon) with Spatial Points
# (region_grid_spatial) & place in SpatialPointsDataFrame data
# (this provides you with your strata identifier (here called
# Id) in your data frame))
region_grid_sp@data <- over(region_grid, region_polygon)
#region_grid_sp <- subset(region_grid_sp, is.na(region_grid_sp@data$OBJECTID)==FALSE)

# Convert back to lon/lat coordinates as that is what VAST uses
region_grid_LL <- as.data.frame(spTransform(region_grid_sp, crs_LL))
region_df <- with(region_grid_LL,
                  data.frame(Lon=coords.x1,
                             Lat=coords.x2, OBJECTID,
                             Area_km2=( (cell_size/1000)^2),
                             row=1:nrow(region_grid_LL)))

# Filter out the grid that does not overlap (outside extent)
region <- subset(region_df, is.na(OBJECTID)==FALSE)
region_sf <- st_as_sf(region, coords=c('Lon', 'Lat'))
st_crs(region_sf) <- "EPSG: 4326"
ggplot(ecodata::coast) +
  geom_sf(data=region_sf, aes(col=OBJECTID)) +
  geom_sf() +
  coord_sf(xlim=c(-75, -75.2), ylim=c(36.6, 36.8))

# Great. That's the base grid for all strata. Now overlay separate strata to grid.
# Load shapefiles of desired extrapolation region
EGOM <- readOGR(here("data/GIS/EGOM_UTM.shp"))
EGOM <- spTransform(EGOM, crs_UTM)
EGOM@data <- dplyr::select(EGOM@data, OBJECTID, Shape_Area)

GBK <- readOGR(here("data/GIS/GBK_UTM.shp"))
GBK <- spTransform(GBK, crs_UTM)
GBK@data <- dplyr::select(GBK@data, OBJECTID, Shape_Area)

SNE <- readOGR(here("data/GIS/SNE_UTM.shp"))
SNE <- spTransform(SNE, crs_UTM)
SNE@data <- dplyr::select(SNE@data, OBJECTID, Shape_Area)

WGOM <- readOGR(here("data/GIS/WGOM_UTM.shp"))
WGOM <- spTransform(WGOM, crs_UTM)
WGOM@data <- dplyr::select(WGOM@data, OBJECTID, Shape_Area)

WIND <- readOGR(here("data/GIS/BOEM_GoM_Lease.shp"))
WIND <- spTransform(WIND, crs_UTM)
WIND@data <- dplyr::select(WIND@data, OBJECTID, AREA_GEO)
colnames(WIND@data) <- c("OBJECTID", 'Shape_Area')

index_area_shapes <- rbind(EGOM, GBK, SNE, WGOM, WIND)

region$OBJECTID <- 0; region_sf$OBJECTID <- 0

totalarea <- region
totalarea_sf <- region_sf

for(i in 1:nrow(index_area_shapes)){
  reg <- index_area_shapes@data$OBJECTID[i]
  touse <- index_area_shapes[index_area_shapes@data$OBJECTID == paste0(reg),]
  touse_polygon <- spTransform(touse, crs_UTM)
  touse_polygon_sf <- st_as_sf(touse_polygon)
  touse_grid <- st_make_grid(touse_polygon_sf, cellsize = cell_size,
                              what = "centers")
  
  # Convert region_grid to Spatial Points to SpatialPointsDataFrame
  touse_grid <- as(touse_grid, "Spatial")
  touse_grid_sp <- as(touse_grid, "SpatialPointsDataFrame")
  
  touse_grid_sp@data <- over(region_grid, touse_polygon)
  touse_grid_sp@coords <- region_grid_sp@coords
  touse_grid_sp@bbox <- region_grid_sp@bbox
  
  # Convert back to lon/lat coordinates as that is what VAST uses
  touse_grid_LL <- as.data.frame(spTransform(touse_grid_sp, crs_LL))
  touse_df <- with(touse_grid_LL,
                    data.frame(Lon=coords.x1,
                               Lat=coords.x2, OBJECTID,
                               Area_km2=( (cell_size/1000)^2),
                               row=1:nrow(touse_grid_LL)))
  
  # Remove points outside polygon
  touse_refine <- subset(touse_df, is.na(OBJECTID)==FALSE)
  touse_refine_sf <- st_as_sf(touse_refine, coords=c('Lon', 'Lat'))
  st_crs(touse_refine_sf) <- "EPSG: 4326"
  
  # Join with whole area polygon
  totalarea <- rbind(totalarea, touse_refine)
  totalarea_sf <- rbind(totalarea_sf, touse_refine_sf)
  
}

# Rename for easier identification
totalarea$STRATA[totalarea$OBJECTID == 0] <- 'All'
totalarea$STRATA[totalarea$OBJECTID == 1] <- 'EGOM'
totalarea$STRATA[totalarea$OBJECTID == 2] <- 'GBK'
totalarea$STRATA[totalarea$OBJECTID == 3] <- 'SNE'
totalarea$STRATA[totalarea$OBJECTID == 4] <- 'WGOM'
totalarea$STRATA[totalarea$OBJECTID == 5] <- 'WIND'
totalarea$OBJECTID <- NULL
head(totalarea)

# Plot for posterity
ggplot() +
  geom_sf(data=totalarea_sf, aes(color=as.factor(as.numeric(OBJECTID))))


# Save it to be read in and passed to VAST later.
saveRDS(totalarea, file = here("data/RData_Storage/user_region_withwind.rds"))
