#### Workspace setup ####
# Clear workspace
rm(list=ls())

# Load libraries
# IMPORTANT NOTE: VAST must be running >=V14, will not work with V13.
library(VAST)
library(sf)
library(tidyverse)
library(rgdal)
library(here)
library(sp)

# Fix degree sign problem
xlabs <- seq(-77, -66, 2)
ylabs <- seq(36, 46, 2)

# Set GGplot auto theme
theme_set(theme(panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', size=1, fill=NA),
                legend.position = "bottom",
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

# Load functions
# Andrew's function to build extrapolation list
devtools::source_url("https://raw.github.com/aallyn/TargetsSDM/main/R/vast_functions.r")

# My function to standardize month-season relationships
source(here("utilities/true_seasons_func.R"))

#### Add spatial information ####
# Add whole region shapefile, transform to WGS84 unprojected
region_shape <- st_read(here("data/GIS/cod_region_UTM.shp"))
st_crs(region_shape)
region_shape <- st_transform(region_shape, crs="EPSG:4326")
region_shape <- st_make_valid(region_shape)
region_shape$Region <- 'All'
region_shape <- dplyr::select(region_shape, Region, geometry)

# Add singular strata shapefiles, transform to WGS84 unprojected
EGOM <- st_read(here("data/GIS/EGOM_UTM.shp"))
EGOM <- st_transform(EGOM, crs="EPSG:4326")
EGOM$Region <- "EGOM"
EGOM <- st_make_valid(EGOM)
EGOM$Region <- 'EGOM'
EGOM <- dplyr::select(EGOM, Region, geometry)

GBK <- st_read(here("data/GIS/GBK_UTM.shp"))
GBK <- st_transform(GBK, crs="EPSG:4326")
GBK$Region <- 'GBK'
GBK <- st_make_valid(GBK)
GBK$Region <- 'GBK'
GBK <- dplyr::select(GBK, Region, geometry)

SNE <- st_read(here("data/GIS/SNE_UTM.shp"))
SNE <- st_transform(SNE, crs="EPSG:4326")
SNE$Region <- 'SNE'
SNE <- st_make_valid(SNE)
SNE$Region <- 'SNE'
SNE <- dplyr::select(SNE, Region, geometry)

WGOM <- st_read(here("data/GIS/WGOM_UTM.shp"))
WGOM <- st_transform(WGOM, crs="EPSG:4326")
WGOM$Region <- 'WGOM'
WGOM <- st_make_valid(WGOM)
WGOM$Region <- 'WGOM'
WGOM <- dplyr::select(WGOM, Region, geometry)

# Bind all shapes
index_area_shapes <- rbind(region_shape, EGOM, GBK, SNE, WGOM)

# Plot for posterity
ggplot(ecodata::coast) +
  geom_sf() +
  geom_sf(data=index_area_shapes, aes(fill=Region))+
  coord_sf(xlim=c(-77, -66), ylim=c(36, 46)) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°W')) +
  scale_y_continuous(breaks = ylabs, labels = paste0(ylabs,'°N'))

# Make extrapolation grid
vast_grid <- vast_make_extrap_grid(region_shapefile = region_shape, 
                                   index_shapes = index_area_shapes, 
                                   cell_size = 1000) # in m
# Check Region output
table(vast_grid$Region)

# Transform for plotting
grid_sf<- st_as_sf(vast_grid, coords = c("Lon", "Lat"), crs = 4326)

# Plot for posterity
ggplot(ecodata::coast) +
  geom_sf() +
  geom_sf(data = grid_sf, aes(color = Region)) +
  facet_wrap(~Region) +
  coord_sf(xlim=c(-77, -66), ylim=c(36, 46)) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°W')) +
  scale_y_continuous(breaks = ylabs, labels = paste0(ylabs,'°N'))

# Make strata limits
strata_use <- data.frame("STRATA" = c("All", "EGOM", "GBK",
                                      "SNE", "WGOM"))

# Make extrapolation info using Andrew's function
vast_extrap_info<- make_extrapolation_info_aja(Region = "User", 
                                               strata.limits = strata_use, 
                                               input_grid = vast_grid, 
                                               index_shapes = index_area_shapes)

#### Add sample information and covars ####
# Load data
surveys <- readRDS(here("data/RData_storage/agg_stn_all_OISST.rds"))

# Assign season
for(i in 1:nrow(surveys)){
  surveys$SEASON[i] <- true_seasons(surveys$DATE[i])
}

# Convert to df
ex <- sfheaders::sf_to_df(surveys, fill=T)

# Clean
ex2 <- ex %>% 
  dplyr::select(YEAR, SEASON, DATE, COD_N, COD_KG, )
ex2 <- subset(ex2, is.na(COD_N)==FALSE)
ex2 <- ex2[with(ex2, order(DATE)),]
row.names(ex2) <- NULL

#### Make settings ####
setwd(here("VAST_runs/StrataDens_1"))
settings = make_settings( n_x = 75,
                          Region = "User",
                          purpose = "index2", 
                          bias.correct = FALSE,
                          knot_method = "grid",
                          strata.limits = strata_use)
#### Run model ####

#### Plot results ####
