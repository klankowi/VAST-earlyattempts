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
library(ggcorrplot)

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
# devtools::source_url(
#   "https://raw.github.com/aallyn/TargetsSDM/R/vast_functions.r"
#   )
# source(here("utilities/vast_make_extrap_grid.R"))
# source(here("utilities/make_extrapolation_info_aja.R"))
# source(here("utilities/Prepare_User_Extrapolation_Data_Fn_aja.R"))
# source(here("utilities/match_strata_fn_aja.R"))

# My function to standardize month-season relationships
source(here("utilities/true_seasons_func.R"))

#### Add spatial information ####
# Add whole region shapefile, transform to WGS84 unprojected
# region_shape <- st_read(here("data/GIS/cod_region_UTM.shp"))
# st_crs(region_shape)
# region_shape <- st_transform(region_shape, crs="EPSG:4326")
# region_shape <- st_make_valid(region_shape)
# region_shape$Region <- 'All'
# region_shape <- dplyr::select(region_shape, Region, geometry)
# 
# # Add singular strata shapefiles, transform to WGS84 unprojected
# EGOM <- st_read(here("data/GIS/EGOM_UTM.shp"))
# EGOM <- st_transform(EGOM, crs="EPSG:4326")
# EGOM$Region <- "EGOM"
# EGOM <- st_make_valid(EGOM)
# EGOM$Region <- 'EGOM'
# EGOM <- dplyr::select(EGOM, Region, geometry)
# 
# GBK <- st_read(here("data/GIS/GBK_UTM.shp"))
# GBK <- st_transform(GBK, crs="EPSG:4326")
# GBK$Region <- 'GBK'
# GBK <- st_make_valid(GBK)
# GBK$Region <- 'GBK'
# GBK <- dplyr::select(GBK, Region, geometry)
# 
# SNE <- st_read(here("data/GIS/SNE_UTM.shp"))
# SNE <- st_transform(SNE, crs="EPSG:4326")
# SNE$Region <- 'SNE'
# SNE <- st_make_valid(SNE)
# SNE$Region <- 'SNE'
# SNE <- dplyr::select(SNE, Region, geometry)
# 
# WGOM <- st_read(here("data/GIS/WGOM_UTM.shp"))
# WGOM <- st_transform(WGOM, crs="EPSG:4326")
# WGOM$Region <- 'WGOM'
# WGOM <- st_make_valid(WGOM)
# WGOM$Region <- 'WGOM'
# WGOM <- dplyr::select(WGOM, Region, geometry)
# 
# # Bind all shapes
# index_area_shapes <- rbind(region_shape, EGOM, GBK, SNE, WGOM)
# 
# # Plot for posterity
# ggplot(ecodata::coast) +
#   geom_sf() +
#   geom_sf(data=index_area_shapes, aes(fill=Region))+
#   coord_sf(xlim=c(-77, -66), ylim=c(36, 46)) +
#   xlab("Longitude") +
#   ylab("Latitude") +
#   scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°W')) +
#   scale_y_continuous(breaks = ylabs, labels = paste0(ylabs,'°N'))
# 
# # Make extrapolation grid
# vast_grid <- vast_make_extrap_grid(region_shapefile = region_shape, 
#                                    index_shapes = index_area_shapes, 
#                                    cell_size = 1000) # in m
# # Check Region output
# table(vast_grid$Region)
# 
# # Transform for plotting
# grid_sf<- st_as_sf(vast_grid, coords = c("Lon", "Lat"), crs = 4326)
# 
# # Plot for posterity
# ggplot(ecodata::coast) +
#   geom_sf() +
#   geom_sf(data = grid_sf, aes(color = Region)) +
#   facet_wrap(~Region) +
#   coord_sf(xlim=c(-77, -66), ylim=c(36, 46)) +
#   xlab("Longitude") +
#   ylab("Latitude") +
#   scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°W')) +
#   scale_y_continuous(breaks = ylabs, labels = paste0(ylabs,'°N'))
# 
# # Make strata limits
# strata_use <- data.frame("STRATA" = c("All", "EGOM", "GBK",
#                                       "SNE", "WGOM"))
# 
# # Make extrapolation info using Andrew's function
# vast_extrap_info<- make_extrapolation_info_aja(Region = "User", 
#                                                strata.limits = strata_use, 
#                                                input_grid = vast_grid, 
#                                                index_shapes = index_area_shapes)
# 
# # Remove intermediates
# rm(EGOM, GBK, grid_sf, index_area_shapes, region_shape, SNE, vast_grid, WGOM)

#### Add sample information and covars ####
# Load data
surveys <- readRDS(here("data/RData_storage/agg_stn_all_OISST.rds"))
surveys.age <- read.csv(here("data/Survey_Data_AgeSep.csv"))
surveys.age <- dplyr::select(surveys.age, HAUL_ID, COD_N, AGE)
surveys.age$NEW_COD_N <- surveys.age$COD_N
surveys.age$COD_N <- NULL
#surveys$COD_N <- NULL

test <- left_join(surveys, surveys.age)
broken.dfo <- subset(test, SURVEY=='DFO_Trawl' & is.na(test$AGE) == TRUE &
                       test$COD_N != 0)
fix.dfo <- subset(test, SURVEY=='DFO_Trawl' & is.na(test$AGE) == TRUE &
                    test$COD_N == 0)
fix.dfo.a0 <- fix.dfo
fix.dfo.a0$AGE <- 'A0-2'
fix.dfo.a1 <- fix.dfo
fix.dfo.a1$AGE <- 'A2-5'
fix.dfo.a2 <- fix.dfo
fix.dfo.a2$AGE <- 'A5PLUS'
fix.dfo <- rbind(fix.dfo.a0, fix.dfo.a1, fix.dfo.a2)
fix.dfo$NEW_COD_N <- 0

test <- subset(test, test$SURVEY != "GSO_Trawl" & test$SURVEY !="Shrimp_Trawl")
'%!in%' <- function(x,y)!('%in%'(x,y))
test.fixing <- subset(test, is.na(test$AGE)==FALSE)

test <- rbind(test.fixing, fix.dfo)
test <- test[with(test, order(SURVEY, SEASON, YEAR, HAUL_ID, AGE)),]
summary(test)

test$COD_N <- test$NEW_COD_N

# Convert to df
ex <- sfheaders::sf_to_df(test, fill=T)

# Remove junk that will not be needed
ex <- ex %>% 
  dplyr::select(HAUL_ID, YEAR, DATE, COD_N, AGE, SURVEY, cobble_P,
                gravel_P, mud_P, rock_P, sand_P, COND, BATHY.DEPTH, oisst,
                x, y)

# Assign season
for(i in 1:nrow(ex)){
  ex$SEASON[i] <- true_seasons(ex$DATE[i])
}

# Remove points with NA cod abundance (there should be none)
ex2 <- subset(ex, is.na(COD_N)==FALSE)

# Rearrange by date
ex2 <- ex2[with(ex2, order(DATE)),]
row.names(ex2) <- NULL

#### Finalize sampling data inputs ####
# Save sampling data
survs <- dplyr::select(ex2,
                       x, y, YEAR, SEASON, SURVEY, COD_N, AGE)
survs$COD_N <- as_units(survs$COD_N, 'counts')
survs$swept <- as_units(1, unitless)
survs$vessel <- as.numeric(as.factor(survs$SURVEY)) - 1
# SURVEY            vessel
# Coop_GoM_BLLS     0
# DFO_Trawl         1
# EGOM_Sentinel     2
# MADMF_Industry    3
# MADMF_Trawl       4
# MENH_EGOM         5
# MENH_WGOM         6
# NEFSC_BTS         7
# RIDEM_Trawl       8
# Video_Trawl_SMAST 9

survs$LBIN <- as.numeric(as.factor(survs$AGE)) -1
# AGE       LBIN
# A0-2      0
# A2-5      1
# A5PLUS    2

survs <- dplyr::select(survs, x, y, YEAR, SEASON, COD_N, LBIN, vessel, swept)
names(survs) <- c('Lon', 'Lat', 'Year', names(survs)[4:8])
str(survs)
# 'data.frame':	44277 obs. of  8 variables:
# $ Lon   : num  -71.4 -71.4 -71.4 -71.4 -71.4 ...
# $ Lat   : num  41.6 41.6 41.6 41.4 41.6 ...
# $ YEAR  : int  1982 1982 1982 1982 1982 1982 1982 1982 1982 1982 ...
# $ SEASON: chr  "WINTER" "WINTER" "WINTER" "WINTER" ...
# $ COD_N : Units: [counts] num  0 0 0 0 0 0 0 0 0 0 ...
# $ COD_KG: Units: [kg] num  0 0 0 0 0 0 0 0 0 0 ...
# $ vessel: num  3 3 3 3 3 3 3 3 3 3 ...
# $ swept : Units: [1] num  1 1 1 1 1 1 1 1 1 1 ...

# Save covariates
covars <- dplyr::select(ex2,
                        x, y, YEAR, SEASON, cobble_P, gravel_P,
                        mud_P, rock_P, sand_P, COND, BATHY.DEPTH, oisst)
names(covars) <- c('Lon', 'Lat', 'Year', names(covars)[4:12])
covars$BATHY.DEPTH <- covars$BATHY.DEPTH * -1
str(covars)
# 'data.frame':	44277 obs. of  12 variables:
# $ Lon        : num  -71.4 -71.4 -71.4 -71.4 -71.4 ...
# $ Lat        : num  41.6 41.6 41.6 41.4 41.6 ...
# $ YEAR       : int  1982 1982 1982 1982 1982 1982 1982 1982 1982 1982 ...
# $ SEASON     : chr  "WINTER" "WINTER" "WINTER" "WINTER" ...
# $ cobble_P   : num  0 0 0 0 0 0 0 0 0 0 ...
# $ gravel_P   : num  0.169 0.169 0.169 0.147 0.169 ...
# $ mud_P      : num  0.661 0.661 0.661 0.283 0.661 ...
# $ rock_P     : num  0 0 0 0 0 0 0 0 0 0 ...
# $ sand_P     : num  0.806 0.806 0.806 0.36 0.806 ...
# $ COND       : chr  "SMOOTH" "SMOOTH" "SMOOTH" "ROUGH" ...
# $ BATHY.DEPTH: num  5 5 5 16 5 16 5 16 5 16 ...
# $ oisst      : num  5.37 3.17 3.67 3.67 3.74 ...

# Remove intermediates
rm(ex, ex2, surveys)
rm(broken.dfo, fix.dfo, fix.dfo.a0, fix.dfo.a1, fix.dfo.a2,
   surveys.age, test, test.fixing)

# Test correlation
# Create correlation matrix
# df_cormat <- dplyr::select(covars, BATHY.DEPTH, COND, sand_P, rock_P, mud_P,
#                            gravel_P, cobble_P, oisst)
# model.matrix(~0+., data=df_cormat) %>% 
#   cor(use="all.obs", method="spearman") %>% 
#   ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=3)

# Rescale covariates to have mean 0 and SD 1 (author rec)
scaled.covars <- covars[,4:ncol(covars)] %>% 
  mutate(across(where(is.numeric), scale))
scaled.covars <- cbind(covars[,1:3], scaled.covars)
summary(scaled.covars)

#### Year subset ####
#survs <- subset(survs, Year > 2001)
#scaled.covars <- subset(scaled.covars, Year > 2001) 
#survs <- subset(survs, LBIN ==1 | LBIN ==2)
#survs$LBIN <- survs$LBIN - 1

#### Make settings ####
user_region <- readRDS(here('data/RData_Storage/user_region_all.rds'))

setwd(here("VAST_runs/StrataDensCats_3"))
settings = make_settings( n_x = 200,
                          Region = "User",
                          purpose = "index2", 
                          bias.correct = FALSE,
                          knot_method = "grid"#,
                          #strata.limits = strata_use
                          )



#### Run model ####
fit = fit_model( 
  # Call settings
    settings = settings, 
    
  # Call survey data info
    Lat_i = survs[,'Lat'], 
    Lon_i = survs[,'Lon'], 
    t_i = survs[,'Year'], 
    b_i = survs[,'COD_N'], 
    a_i = survs[,'swept'], 
    v_i = survs[,'vessel'],
    c_iz = survs[,"LBIN"],
  
  # Call covariate info
    X1_formula = ~ gravel_P + #cobble_P + mud_P + rock_P + 
                   sand_P + 
                   BATHY.DEPTH + 
                   oisst,
    covariate_data = scaled.covars,
  
  # Call spatial 
    input_grid=user_region,
    #extrapolation_list = vast_extrap_info,
  
  # Tell model to run
    run_model = TRUE)
#beep(8)

#### Plot results ####

save.image('strata_cats_3.RData')

plot( fit )


