# Clear workspace
rm(list=ls())

# Load libraries
library(VAST)
library(sf)
library(tidyverse)
library(rgdal)
library(here)

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
user_region <- readRDS(here('data/RData_Storage/user_region_all.rds'))

## Add stock limits
stox <- readOGR(here("data/GIS/codstox.shp"))
stox.sf <- st_as_sf(stox)
stox.df <- sfheaders::sf_to_df(stox.sf, fill=T)
# Plot for posterity
ggplot(ecodata::coast) +
  geom_sf(data=stox.sf, col='black', 
          aes(fill=STOCK)) +
  geom_sf() +
  coord_sf(xlim=c(-76, -65), ylim=c(36, 46))

## Set limits by OBJECTID
Allstox <- unique(as.numeric(stox.df$OBJECTID))
EGOM <- unique(as.numeric(stox.df$OBJECTID[stox.df$STOCK == 'EGOM']))
WGOM <- unique(as.numeric(stox.df$OBJECTID[stox.df$STOCK == 'WGOM']))
GBK <- unique(as.numeric(stox.df$OBJECTID[stox.df$STOCK == 'GBK']))
SNE <- unique(as.numeric(stox.df$OBJECTID[stox.df$STOCK == 'SNE']))

stox.limits <- as.list(c("Allstox" = Allstox, 
                           "EGOM" = EGOM, 
                           "WGOM" = WGOM, 
                           "GBK" = GBK, 
                           "SNE" = SNE))
rm(stox.df, stox.sf, Allstox, EGOM, GBK, SNE, WGOM)

## Add sampling data and covars
surveys <- readRDS(here("data/RData_storage/agg_stn_all_OISST.rds"))
#wgom <- subset(surveys, STOCK=='GBK')
#wgom <- subset(wgom, SURVEY=='NEFSC_BTS')
#wgom <- subset(wgom, SEASON=='FALL')
ex <- sfheaders::sf_to_df(surveys, fill=T)
ex2 <- ex %>% 
  dplyr::select(-c(SALINITY, BOTTOM.TYPE, TRUE_SEASON, STOCK, cobble_V,
            gravel_V, rock_V, mud_V, sand_V, month, day, yrmody, declon,
            declat, sfg_id, point_id, STRATUM, COD_KG))
ex2 <- subset(ex2, is.na(COD_N)==FALSE)
ex2 <- ex2[with(ex2, order(DATE, HAUL_ID)),]
row.names(ex2) <- NULL
rm(surveys, ex)

# Add area swept
# NEFSC Bottom trawl survey - standard area swept (estimated)
bts <- subset(ex2, SURVEY=='NEFSC_BTS')
bts.swept <- rep(as_units(0.0384, "km^2"), nrow(bts))

# DFO Trawl survey- area swept calculated
dfo <- subset(ex2, SURVEY=='DFO_Trawl')
load_example( data_set="NWA_yellowtail_seasons" )
NWA_yellowtail_seasons <- subset(NWA_yellowtail_seasons, season=='DFO')
dfo.seasons <- NWA_yellowtail_seasons
names(dfo.seasons) <- c('YEAR', 'SURVEY', 'y', 'x', 'swept', 'weight', 'id')
dfo.seasons <- dfo.seasons %>% 
  dplyr::select(-c(SURVEY, weight, id))
dfoswept <- left_join(dfo, dfo.seasons, by=c('YEAR', 'x', 'y'))
dfoswept.good <- dfoswept[is.na(dfoswept$swept)==FALSE,]
dfoswept.bad  <- dfoswept[is.na(dfoswept$swept)==TRUE,]

dfoswept.good$swept <- as_units(dfoswept.good$swept, "km^2")
dfoswept.bad$swept  <- as_units(1, unitless)

dfo.good <- dfoswept.good$swept
dfo.bad <- dfoswept.bad$swept

dfoswept.good$swept <- NULL
dfoswept.bad$swept <- NULL

# All other surveys - area swept neither calculated nor estimated
allelse <- subset(ex2, SURVEY!="NEFSC_BTS" & SURVEY!="DFO_Trawl")
else.swept <- rep(as_units(1, unitless), nrow(allelse))

# Create mixed-unit vector
mix.swept <- c(bts.swept, dfo.good, dfo.bad, else.swept, allow_mixed=TRUE)

# Recombine
ex3 <- rbind(bts, dfoswept.good, dfoswept.bad, allelse)
ex3$swept <- mix.swept

# Reorder, remove unnecessary variables
ex3 <- ex3[with(ex3, order(DATE)),]
row.names(ex3) <- NULL
ex3 <- ex3 %>% 
  dplyr::select(-c(HAUL_ID))

# Change data structure
ex3$SEASON <- as.factor(ex3$SEASON)
ex3$COND <- as.factor(ex3$COND)
ex3$SURVEY <- as.factor(ex3$SURVEY)
ex3$COD_N <- as_units(ex3$COD_N, "counts")
ex3$VESSEL <- as.integer(ex3$SURVEY) - 1

# Check structure
str(ex3)

rm(allelse, dfo, dfo.seasons, dfoswept, dfoswept.bad, dfoswept.good, ex2,
   mix.swept, NWA_yellowtail_seasons, bts.swept, dfo.bad, dfo.good, else.swept,
   bts)

# Pull covariate data
covars <- dplyr::select(ex3, SEASON, DEPTH, SURFACE.TEMP,
                        BOTTOM.TEMP, cobble_P, gravel_P, mud_P, rock_P,
                        sand_P, COND, BATHY.DEPTH, oisst)
# For matrix, remove factor covariates
dfmat <- dplyr::select(covars, -SEASON, -COND)

# Check for correlation
model.matrix(~0+., data=dfmat) %>% 
  cor(use="all.obs", method="spearman") %>% 
  ggcorrplot::ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=3)
# OISST, surface temp, and bottom temp all correlated. Choose OISST as 
# surviving covar because it has no NA values.
covars <- dplyr::select(covars, -SURFACE.TEMP, -BOTTOM.TEMP)
# Bathy depth and sample depth are correlated. Choose bathydepth as surviving
# covar because it has no NA values.
covars <- dplyr::select(covars, -DEPTH)
# Cobble and rock are correlated. Need to map them to see which makes sense to
# drop. Probably rock.
map.sub <- dplyr::select(ex3, x, y, cobble_P, rock_P)
map.sub$flags <- NA
map.sub <- st_as_sf(map.sub, coords=c('x', 'y'))
st_crs(map.sub) <-st_crs(stox)
# Simple yes/no plot mechanism
for(i in 1:nrow(map.sub)){
  if(map.sub$cobble_P[i] == 0 & map.sub$rock_P[i] == 0){
    map.sub$flags[i] <- 'neither'
  }
  if(map.sub$cobble_P[i] != 0 & map.sub$rock_P[i] != 0){
    map.sub$flags[i] <- 'both'
  }
  if(map.sub$cobble_P[i] == 0 & map.sub$rock_P[i] != 0){
    map.sub$flags[i] <- 'rock'
  }
  if(map.sub$cobble_P[i] != 0 & map.sub$rock_P[i] == 0){
    map.sub$flags[i] <- 'cobble'
  }
}
# Location plot
ggplot(ecodata::coast) +
  geom_sf(data=map.sub, aes(col=flags)) +
  scale_color_manual(values=c('gray', 'red', 'transparent', 'blue')) +
  geom_sf() + 
  labs(title = "Cobble and rock locations") +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°W')) +
  scale_y_continuous(breaks = ylabs, labels = paste0(ylabs,'°N')) +
  coord_sf(xlim = c(-77, -66),  ylim = c(36, 46))
table(map.sub$flags)
# Tough call. Not going to drop either.

# Load shapefile of desired extrapolation region
shp <- readOGR(here("data/GIS/codstox.shp"))

# Subset to just Western Gulf of Maine (for now)
#shp <- subset(shp, STOCK=='WGOM')

# Set ID (if only one stock is included)
#shp$Id <- 1

# Set ID (if more than one stock is included)
shp$Id <- seq(1:4)

# Remove stock designation, not needed
table(shp$STOCK, shp$Id)
shp$STOCK <- NULL

# Transform to unprojected lat-lon
sps <- spTransform(shp, 
                   CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"))

# Find UTM
lon <- sum(bbox(sps)[1,])/2
utmzone <- floor((lon + 180)/6)+1
crs_LL <- CRS('+proj=longlat +ellps=WGS84 +no_defs')
sps@proj4string <- crs_LL

# Convert the final in polygon to UTM
crs_UTM <- CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 
                      +units=m +no_defs "))
region_polygon <- spTransform(sps, crs_UTM)

# Construct the extrapolation grid for VAST using sf package
# Size of grid **in meters** (since working in UTM). Controls
# the resolution of the grid.
cell_size <- 1000

# This step is slow at high resolutions
region_grid <- st_make_grid(region_polygon, cellsize = cell_size,
                            what = "centers")

# Create extrapolation grid
extrapin <- make_extrapolation_info(Region = "user",
                                    strata.limits=stox.limits,
                                    grid_dim_km = c(1,1),
                                    input_grid = region_grid)






# Create spatial list
spatlist <- make_spatial_info(n_x = 200,
                              Lon_i = ex3$x, 
                              Lat_i = ex3$y,
                              E)
  
## Combine to VAST list format
# attempt <- vector(mode="list", length=3)
# names(attempt) <- c('sampling_data', 'Region', 'strata.limits')
# attempt[[1]] <- ex3
# attempt[[2]] <- user_region
# attempt[[3]] <- stox.limits
attempt <- make_data(b_i = ex3$COD_N,
                     a_i = ex3$swept,
                     t_i = ex3$YEAR,
                     v_i = ex3$VESSEL,
                     covariate_data = covars,
                     X1_formula = cobble_P + gravel_P + mud_P + rock_P + 
                       sand_P + COND + BATHY.DEPTH + oisst,
                     spatial_list = user_region)

# Remove intermediates
rm(dfmat, map.sub, stox)

# Make settings (turning off bias.correct to save time for example)
settings = make_settings( n_x = 200, 
                          Region='User', 
                          purpose = "index2", 
                          bias.correct = FALSE)
#settings$FieldConfig[2,1] <- 0 # turn off spatiotemporal LP1
#settings$FieldConfig[2,2] <- 0 # turn off spatiotemporal LP2
settings$use_anisotropy <- FALSE # turn off anisotropy

# Run model
fit = fit_model( settings = settings, 
                 Lat_i = attempt$sampling_data[,'y'], 
                 Lon_i = attempt$sampling_data[,'x'], 
                 t_i = attempt$sampling_data[,'YEAR'], 
                 b_i = attempt$sampling_data[,'COD_N'], 
                 a_i = attempt$sampling_data[,'swept'], 
                 input_grid=user_region)

# Plot results
plot( fit )
