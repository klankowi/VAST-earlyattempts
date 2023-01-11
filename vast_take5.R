# Clear workspace
rm(list=ls())

# Load libraries
library(VAST)
library(sf)
library(tidyverse)
library(rgdal)
library(here)
library(sp)

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

#### Fix degree sign problem ####
xlabs <- seq(-77, -66, 2)
ylabs <- seq(36, 46, 2)

## Add spatial grid
# First, we need our region shapefile
region_shape <- st_read(here("data/GIS/cod_region_UTM.shp"))
st_crs(region_shape)
region_shape <- st_transform(region_shape, crs="EPSG:4326")
region_shape <- st_make_valid(region_shape)

# Second, get our index area shapefile
# We could just use this same shapefile in the "index_shapes" argument, but to 
# show off the new functions we wrote, we will also want to have a sf 
# multipolygon shapefiles with areas defined within this general region
EGOM <- st_read(here("data/GIS/EGOM_UTM.shp"))
EGOM <- st_transform(EGOM, crs="EPSG:4326")
EGOM$Region <- "EGOM"
EGOM <- st_make_valid(EGOM)

GBK <- st_read(here("data/GIS/GBK_UTM.shp"))
GBK <- st_transform(GBK, crs="EPSG:4326")
GBK$Region <- 'GBK'
GBK <- st_make_valid(GBK)

SNE <- st_read(here("data/GIS/SNE_UTM.shp"))
SNE <- st_transform(SNE, crs="EPSG:4326")
SNE$Region <- 'SNE'
SNE <- st_make_valid(SNE)

WGOM <- st_read(here("data/GIS/WGOM_UTM.shp"))
WGOM <- st_transform(WGOM, crs="EPSG:4326")
WGOM$Region <- 'WGOM'
WGOM <- st_make_valid(WGOM)

index_area_shapes <- rbind(EGOM, GBK, SNE, WGOM)
index_area_shapes <- dplyr::select(index_area_shapes, Region, area_km, geometry)

# Finally, run `vast_make_extrap_grid` function after specifying the strata we 
#want to use
strata_use <- data.frame("STRATA" = c("EGOM", "GBK", "SNE", "WGOM"))
source(here("utilities/vast_functions.R"))
vast_extrap_grid <- vast_make_extrap_grid(region_shapefile = region_shape, 
                                          index_shapes = index_area_shapes, 
                                          strata.limits = strata_use, 
                                          cell_size = 1000)

# Let's just look at this quickly...
vast_grid_sf <- st_as_sf(vast_extrap_grid, 
                         coords = c("Lon", "Lat"), 
                         crs = 4326, 
                         remove = FALSE)

ggplot(ecodata::coast) +
  geom_sf() +
  geom_sf(data = vast_grid_sf, aes(color = Region)) +
  coord_sf(ylim=c(36, 46), xlim=c(-76, -65))

## Add sampling data and covars
surveys <- readRDS(here("data/RData_storage/agg_stn_all_OISST.rds"))
st_crs(surveys) <- "EPSG:4326"
index_area_shapes.trans <- st_transform(index_area_shapes, crs = "EPSG: 4326")

surveys.sp <- as(surveys, "Spatial")
index.sp <- as(index_area_shapes.trans, "Spatial")
index.sp$area_km <- NULL

surveys.sp$Region <- (sp::over(geometry(surveys.sp), index.sp))
surveys.sp$Region <- as.character(surveys.sp$Region$Region)
table(surveys.sp$Region, surveys.sp$STOCK)

surveys <- st_as_sf(surveys.sp)


# Assign season
source(here("utilities/true_seasons_func.R"))
for(i in 1:nrow(surveys)){
  surveys$SEASON[i] <- true_seasons(surveys$DATE[i])
}

ex <- sfheaders::sf_to_df(surveys, fill=T)
ex2 <- ex %>% 
  dplyr::select(-c(SALINITY, BOTTOM.TYPE, cobble_V,
                   gravel_V, rock_V, mud_V, sand_V, month, day, yrmody, declon,
                   declat, sfg_id, point_id, STRATUM, COD_KG, HAUL_ID,
                   DEPTH, SURFACE.TEMP, BOTTOM.TEMP, TRUE_SEASON, STOCK))
ex2 <- subset(ex2, is.na(COD_N)==FALSE)
ex2 <- ex2[with(ex2, order(DATE)),]
row.names(ex2) <- NULL

# Change data structure
ex2$SEASON <- as.factor(ex2$SEASON)
ex2$COND <- as.factor(ex2$COND)
ex2$SURVEY <- as.factor(ex2$SURVEY)
table(ex2$SURVEY)
ex2$STRATA <- as.factor(ex2$Region)
ex2$Region <- NULL
ex2$COD_N <- as_units(ex2$COD_N, "counts")
ex2$swept <- as_units(1, unitless)
ex2$VESSEL <- as.integer(ex2$SURVEY) - 1
table(ex2$VESSEL)
ex2$SURVEY <- NULL
head(ex2)

# Check structure
names(ex2) <- c('Year', names(ex2[2:12]), 'Lon', 'Lat', 'STRATA', 'swept', 'vessel')
str(ex2)

# rm(surveys, ex, good.seas, na.gso, na.seas, months.fall, months.spring,
#    months.summer, months.winter, i, na.rid)

# Pull covariate data
covars <- dplyr::select(ex2, 
                        Year, Lon, Lat, cobble_P, gravel_P, mud_P, rock_P,
                        sand_P)

# Remove intermediates
# rm(stox, stox.limits, fall, i , spring, summer, winter,
#    EGOM, GBK, All_areas, SNE, WGOM, strata.limits)

setwd(here("VAST_Runs/Strata_1stattempt"))

# Make settings (turning off bias.correct to save time for example)
settings = make_settings( n_x = 200, 
                          Region='User',
                          purpose = "index2", 
                          bias.correct = FALSE,
                          strata.limits = strata_use)

# Run model
fit = fit_model( settings = settings,
                 input_grid = vast_extrap_grid,
                 strata.limits = strata_use,
                 Lat_i = ex2[,'Lat'], 
                 Lon_i = ex2[,'Lon'], 
                 t_i = ex2[,'Year'], 
                 b_i = ex2[,'COD_N'], 
                 a_i = ex2[,'swept'])


# Plot results
plot( fit )

index <- read.csv(here("VAST_Runs/Gravel_Dens_Covar3/Index.csv"))
head(index)
index$Stratum[index$Stratum == "Stratum_1"] <- "All areas"
index$Stratum[index$Stratum == "Stratum_2"] <- "EGOM"
index$Stratum[index$Stratum == "Stratum_3"] <- "GBK"
index$Stratum[index$Stratum == "Stratum_4"] <- "SNE"
index$Stratum[index$Stratum == "Stratum_5"] <- "WGOM"
table(index$Stratum)

ggplot( ) +
  geom_line(data=index[index$Stratum !='All areas',], 
            aes(x=Time, y=Estimate, col=Stratum, group=Stratum),
            lwd=1.5) #+
  # geom_errorbar(data = index,
  #               aes(x = Time,
  #                   ymin=Estimate-Std..Error.for.Estimate, 
  #                   ymax=Estimate+Std..Error.for.Estimate,
  #                   col=Stratum, group=Stratum),
  #               lwd=1) 
