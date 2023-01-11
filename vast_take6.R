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
#source(here::here("utilities/nmfs_functions.R"))
#source(here::here("utilities/dfo_functions.R"))
#source(here::here("utilities/combo_functions.R"))
#source(here::here("utilities/covariate_functions.R"))
#source(here::here("utilities/vast_functions.R"))
#source(here::here("utilities/SDM_PredValidation_Functions.R"))

#### Fix degree sign problem ####
xlabs <- seq(-77, -66, 2)
ylabs <- seq(36, 46, 2)

#### Add spatial grid ####
# Add extrapolation grid
user_region <- readRDS(here('data/RData_Storage/user_region_newmethod.rds'))

# Assign to proper strata
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
# strata_use <- data.frame("STRATA" = c("EGOM", "GBK", "SNE", "WGOM"))
# source(here("utilities/vast_functions.R"))
# vast_extrap_grid <- vast_make_extrap_grid(region_shapefile = region_shape, 
#                                           index_shapes = index_area_shapes, 
#                                           strata.limits = strata_use, 
#                                           cell_size = 1000)

#### Add sampling data and covars ####
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

rm(index.sp, surveys.sp, index_area_shapes.trans, EGOM, GBK, SNE, WGOM,
   index_area_shapes, region_shape)

# Assign season
source(here("utilities/true_seasons_func.R"))
for(i in 1:nrow(surveys)){
  surveys$SEASON[i] <- true_seasons(surveys$DATE[i])
}

ex <- sfheaders::sf_to_df(surveys, fill=T)
ex2 <- ex %>% 
  dplyr::select(-c(SALINITY, BOTTOM.TYPE, cobble_V,
                   gravel_V, rock_V, mud_V, sand_V, month, day, yrmody, declon,
                   declat, sfg_id, point_id, STRATUM, 
                   DEPTH, SURFACE.TEMP, BOTTOM.TEMP, TRUE_SEASON, STOCK))
ex2 <- subset(ex2, is.na(COD_N)==FALSE)
ex2 <- ex2[with(ex2, order(DATE)),]
row.names(ex2) <- NULL

# # Create all_occu
# all_occu <- dplyr::select(ex2, HAUL_ID, COD_KG, COD_N, SURVEY)
# all_occu$PRESENCE <- NA
# for(i in 1:nrow(all_occu)){
#   if(all_occu$COD_N[i] != 0){
#     all_occu$PRESENCE[i] <- 1
#   }
#   if(all_occu$COD_N[i] == 0){
#     all_occu$PRESENCE[i] <- 0
#     all_occu$COD_KG[i] <- 0
#   }
# }
# head(all_occu)
# str(all_occu)
# all_occu$HAUL_ID <- as.factor(all_occu$HAUL_ID)
# all_occu$SURVEY <- as.factor(all_occu$SURVEY)
# names(all_occu) <- c('ID', 'BIOMASS', 'ABUNDANCE', 'SURVEY', 'PRESENCE')
# all_occu$SPECIES <- as.factor('COD')
# all_occu$BIOMASS <- as_units(all_occu$BIOMASS, 'kg')
# all_occu$ABUNDANCE <- as_units(all_occu$ABUNDANCE, 'counts')
# str(all_occu)
# 
# # Create all_tows
# all_tows <- dplyr::select(ex2, YEAR, x, y, HAUL_ID, DATE, SEASON, SURVEY)
# names(all_tows) <- c('EST_YEAR', 'DECDEG_BEGLAT', 'DECDEG_BEGLON', 'ID', 
#                      'DATE', 'SEASON', 'SURVEY')
# all_tows$ID <- as.factor(all_tows$ID)
# all_tows$DATE <- as.Date(all_tows$DATE)
# all_tows$SEASON <- as.factor(all_tows$SEASON)
# all_tows$SURVEY <- as.factor(all_tows$SURVEY)
# str(all_tows)
# 
# # Create all_tows_with_all_covs
# all_tows_with_all_covs <- ex2
# names(all_tows_with_all_covs) <- c('ID', 'EST_YEAR', 'SEASON', 'DATE', 
#                                    'ABUNDANCE', 'BIOMASS', 'SURVEY',
#                                    'cobble', 'gravel', 'mud', 'rock', 'sand',
#                                    'COND', 'Depth', 'oisst', 'STRATA',
#                                    'DECDEG_BEGLAT', 'DECDEG_BEGLON')
# all_tows_with_all_covs$STRATA <- NULL
# all_tows_with_all_covs$ID <- as.factor(all_tows_with_all_covs$ID)
# all_tows_with_all_covs$SEASON <- as.factor(all_tows_with_all_covs$SEASON)
# all_tows_with_all_covs$COND <- as.factor(all_tows_with_all_covs$COND)
# all_tows_with_all_covs$SURVEY <- as.factor(all_tows_with_all_covs$SURVEY)
# all_tows_with_all_covs$ABUNDANCE <- as_units(all_tows_with_all_covs$ABUNDANCE, "counts")
# all_tows_with_all_covs$BIOMASS <- as_units(all_tows_with_all_covs$BIOMASS, "kg")
# all_tows_with_all_covs$DATE <- as.Date(all_tows_with_all_covs$DATE)
# all_tows_with_all_covs$DEPTH <- all_tows_with_all_covs$DEPTH * -1
# str(all_tows_with_all_covs)
# 
# # Rescale with depth cut
# all_tows_with_all_covs_rescale <- subset(all_tows_with_all_covs, Depth <= 500)
# # This doesn't actually do anything because all the surveys are on the shelf
# 
# # Make tidy dataframe
# tidy_mod_data_run <- all_tows_with_all_covs_rescale
# 
# # Make sample data
# vast_sample_data <- data.frame("Year" = tidy_mod_data_run$EST_YEAR, 
#                                "Lat" = tidy_mod_data_run$DECDEG_BEGLAT, 
#                                "Lon" = tidy_mod_data_run$DECDEG_BEGLON, 
#                                "Biomass" = tidy_mod_data_run$BIOMASS, 
#                                "Swept" = rep(as_units(1, unitless), nrow(tidy_mod_data_run)),
#                                "Pred_TF" = rep(0, nrow(tidy_mod_data_run)))


#### Make settings ####
survs <- dplyr::select(ex2, x, y, YEAR, Region, COD_N)
names(survs) <- c('Lon', 'Lat', 'Year', 'Region', 'Abundance')
survs$swept <- as_units(1, unitless)

strata_use <- data.frame("STRATA" = c("EGOM", "GBK", "SNE", "WGOM"))

setwd(here("VAST_Runs/Strata_4thattempt"))

# Run FishStatsUtils::make_settings
settings_out<- make_settings(n_x = 200, 
                             Region = "User", 
                             purpose = "index2",
                             bias.correct = FALSE,
                             strata.limits = strata_use)

fit <-  fit_model(settings = settings_out,
                  Lat_i = survs$Lat,
                  Lon_i = survs$Lon,
                  t_i   = survs$Year,
                  b_i   = survs$Abundance,
                  a_i   = survs$swept,
                  Region = "User",
                  input_grid = user_region)

#### Fit model ####
vast0 <- vast_build_sdm(settings = vast_settings, 
                       extrap_grid = vast_extrap_grid, 
                       sample_data = vast_sample_data, 
                       covariate_data = NULL, 
                       X1_formula = NULL, 
                       X2_formula = NULL, 
                       Q1_formula = NULL, 
                       Q2_formula = NULL, 
                       Xconfig_list = vast_coveff, 
                       X_contrasts = NULL, 
                       index_shapes = index_area_shapes, 
                       spatial_info_dir = here::here(""))

vast_fitted <- vast_fit_sdm(vast_build_adjust = vast0, 
                           nice_category_names = "Atlantic_cod", 
                           index_shapes = index_area_shapes, 
                           spatial_info_dir = here::here(""), 
                           out_dir = here("VAST_runs/Strata_2ndattempt/mod_fits"))

# Extracting stratified biomass indices and plotting as a time series
biomass_indices<- get_vast_index_timeseries(vast_fit = vast_fitted, 
                                            all_times = unique(vast_sample_data$Year), 
                                            nice_category_names = "Atlantic_cod", 
                                            index_scale = c("raw"), 
                                            out_dir = here("VAST_runs/Strata_2ndattempt/tables"))

plot_vast_index_timeseries(index_res_df = biomass_indices, 
                           index_scale = "raw", 
                           nice_category_names = "Atlantic_cod", 
                           nice_xlab = "Year", 
                           nice_ylab= "Biomass index (kg)", 
                           paneling = "none", 
                           color_pal = NULL, 
                           out_dir = here("VAST_runs/Strata_2ndattempt/plots_maps"))

head(vast_extrap_grid)
