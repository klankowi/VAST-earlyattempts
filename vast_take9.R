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
devtools::source_url(
  "https://raw.github.com/aallyn/TargetsSDM/main/R/vast_functions.r"
  )

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
EGOM.pt <- st_centroid(EGOM)

GBK <- st_read(here("data/GIS/GBK_UTM.shp"))
GBK <- st_transform(GBK, crs="EPSG:4326")
GBK$Region <- 'GBK'
GBK <- st_make_valid(GBK)
GBK$Region <- 'GBK'
GBK <- dplyr::select(GBK, Region, geometry)
GBK.pt <- st_centroid(GBK)

SNE <- st_read(here("data/GIS/SNE_UTM.shp"))
SNE <- st_transform(SNE, crs="EPSG:4326")
SNE$Region <- 'SNE'
SNE <- st_make_valid(SNE)
SNE$Region <- 'SNE'
SNE <- dplyr::select(SNE, Region, geometry)
SNE.pt <- st_centroid(SNE)

WGOM <- st_read(here("data/GIS/WGOM_UTM.shp"))
WGOM <- st_transform(WGOM, crs="EPSG:4326")
WGOM$Region <- 'WGOM'
WGOM <- st_make_valid(WGOM)
WGOM$Region <- 'WGOM'
WGOM <- dplyr::select(WGOM, Region, geometry)
WGOM.pt <- st_centroid(WGOM)

# Bind all shapes
index_area_shapes <- rbind(region_shape, EGOM, GBK, SNE, WGOM)

# Plot for posterity
ggplot(ecodata::coast) +
  geom_sf() +
  geom_sf(data=index_area_shapes, aes(fill=Region)) +
  geom_sf(data = EGOM.pt) + geom_sf(data = WGOM.pt) + 
  geom_sf(data = GBK.pt)  + geom_sf(data = SNE.pt) + 
  coord_sf(xlim=c(-77, -66), ylim=c(36, 46)) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°W')) +
  scale_y_continuous(breaks = ylabs, labels = paste0(ylabs,'°N'))


# Make extrapolation grid
vast_grid <- vast_make_extrap_grid(region_shapefile = region_shape, 
                                   index_shapes = index_area_shapes, 
                                   cell_size = 1000) # in m
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

# Make strata limits
strata_use <- data.frame("STRATA" = c("All", "EGOM", "GBK",
                                      "SNE", "WGOM"))

# Make extrapolation info using Andrew's function
vast_extrap_info<- make_extrapolation_info_aja(Region = "User", 
                                               strata.limits = strata_use, 
                                               input_grid = vast_grid, 
                                               index_shapes = index_area_shapes)

# Remove intermediates
rm(EGOM, GBK, grid_sf, index_area_shapes, region_shape, SNE, vast_grid, WGOM)

#### Add sample information and covars ####
# Load data
surveys <- readRDS(here("data/RData_storage/agg_stn_all_OISST.rds"))
surveys$BATHY.DEPTH <- surveys$BATHY.DEPTH * -1

# Convert to df
ex <- sfheaders::sf_to_df(surveys, fill=T)

# Remove junk that will not be needed
ex <- ex %>% 
  dplyr::select(HAUL_ID, YEAR, DATE, COD_N, COD_KG, SURVEY, cobble_P,
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

# Check weight vs number
cod0_n <- subset(ex2, COD_N == 0 & COD_KG !=0 & is.na(COD_KG)==F)
summary(cod0_n$COD_KG)
table(cod0_n$SURVEY)
# When COD_N = 0, COD_KG should = 0. Set COD_KG to 0.
for(i in 1:nrow(ex2)){
  if(ex2$COD_N[i] == 0 & is.na(ex2$COD_KG[i]) == F &
     ex2$COD_KG[i] != 0){
    ex2$COD_KG[i] <- 0
  }
}

# Check number vs weight
cod0_kg <- subset(ex2, COD_KG == 0 & COD_N != 0)
summary(cod0_kg$COD_N)
table(cod0_kg$SURVEY)
# When COD_KG = 0, COD_N should = 0. Set COD_KG to NA.
for(i in 1:nrow(ex2)){
  if(ex2$COD_KG[i] == 0 & ex2$COD_N[i] != 0){
    ex2$COD_KG[i] <- NA
  }
}

# Add weight 0 to instance where cod_n=0 but cod_kg=NA
for(i in 1:nrow(ex2)){
  if(ex2$COD_N[i] == 0 & is.na(ex2$COD_KG[i])==TRUE){
    ex2$COD_KG[i] <- 0
  }
}

# Check work.
cod0_n <- subset(ex2, COD_N == 0 & COD_KG !=0); nrow(cod0_n)
cod0_kg <- subset(ex2, COD_KG == 0 & COD_N != 0); nrow(cod0_kg)

#### Finalize sampling data inputs ####
# Save sampling data
all_dat <- dplyr::select(ex2,
                       x, y, YEAR, SEASON, SURVEY, COD_N, COD_KG,
                       cobble_P, gravel_P,
                       mud_P, rock_P, sand_P, COND, BATHY.DEPTH, oisst)
all_dat$COD_N <- as_units(all_dat$COD_N, 'counts')
all_dat$COD_KG <- as_units(all_dat$COD_KG, 'kg')
all_dat$swept <- as_units(1, unitless)
all_dat$vessel <- as.numeric(as.factor(all_dat$SURVEY)) - 1
all_dat$SURVEY <- NULL
str(all_dat)
names(all_dat) <- c("Lon", "Lat", "Year", "Season", names(all_dat)[5:ncol(all_dat)])

# Remove intermediates
rm(cod0_kg, cod0_n, ex, ex2)

# Test correlation
# Create correlation matrix
df_cormat <- dplyr::select(all_dat, BATHY.DEPTH, COND, sand_P, rock_P, mud_P,
                           gravel_P, cobble_P, oisst)
model.matrix(~0+., data=df_cormat) %>% 
  cor(use="all.obs", method="spearman") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=3)

# Rescale covariates to have mean 0 and SD 1 (author rec)
scaled.covars <- all_dat[,c('cobble_P', 'gravel_P', 'mud_P', 'rock_P',
                            'sand_P', 'BATHY.DEPTH', 'oisst')] %>% 
  mutate(across(where(is.numeric), scale))
all.scaled <- cbind(all_dat[,c('Year', 'Season', 'Lon', 'Lat', 'vessel',
                               'swept', 'COD_N', 'COD_KG', 'COND')],
                    scaled.covars)
str(all.scaled)

# Remove intermediates
rm(df_cormat, all_dat, scaled.covars)

#### Add seasonal information ####
# Set of years and seasons
year_set = sort(unique(all.scaled$Year))
season_set = c('WINTER', 'SPRING', 'SUMMER', 'FALL')

# Create a grid with all unique combinations of seasons and years and then combine these into one "year_season" variable
yearseason_grid = expand.grid("Season" = season_set, "Year" = year_set)
yearseason_levels = apply(yearseason_grid[,2:1], MARGIN = 1, FUN = paste, collapse = "_")
yearseason_labels = round(yearseason_grid[,'Year'] + 
                         (as.numeric(factor(yearseason_grid[,'Season'], 
                                            levels = season_set))-1)/
                          length(season_set), digits=1)

# Similar process, but for the observations
yearseason_i = apply(all.scaled[,c("Year","Season")], MARGIN = 1, FUN = paste, collapse = "_")
yearseason_i = factor(yearseason_i, levels = yearseason_levels)

# Add the year_season factor column to our sampling_data data set
all.scaled$year_season = yearseason_i
all.scaled$Season = factor(all.scaled$Season, levels = season_set)

# Save mean lat-lon
EGOM.vals <- sfheaders::sf_to_df(EGOM.pt, fill=F)
WGOM.vals <- sfheaders::sf_to_df(WGOM.pt, fill=T)
GBK.vals  <- sfheaders::sf_to_df(GBK.pt, fill=T)
SNE.vals  <- sfheaders::sf_to_df(SNE.pt, fill=T)

# Make dummy observation for each season-year-strata combination
dummy_data.EGOM = data.frame(
  Year = yearseason_grid[,'Year'],
  Season = yearseason_grid[,'Season'],
  Lon = rep(EGOM.vals$x, nrow(yearseason_grid)),
  Lat = rep(EGOM.vals$y, nrow(yearseason_grid)),
  vessel = rep(8, nrow(yearseason_grid)),
  swept = rep(as_units(1, unitless), nrow(yearseason_grid)),
  COD_N = rep(as_units(0, 'counts'), nrow(yearseason_grid)),
  COD_KG = rep(as_units(0, 'kg'), nrow(yearseason_grid)),
  COND = rep('SMOOTh', nrow(yearseason_grid)),
  cobble_P = rep(mean(surveys$cobble_P[surveys$STOCK == 'EGOM']),
                 nrow(yearseason_grid)),
  gravel_P = rep(mean(surveys$gravel_P[surveys$STOCK == 'EGOM']),
                 nrow(yearseason_grid)),
  mud_P = rep(mean(surveys$mud_P[surveys$STOCK == 'EGOM']),
                 nrow(yearseason_grid)),
  rock_P = rep(mean(surveys$rock_P[surveys$STOCK == 'EGOM']),
                 nrow(yearseason_grid)),
  sand_P = rep(mean(surveys$sand_P[surveys$STOCK == 'EGOM']),
                 nrow(yearseason_grid)),
  BATHY.DEPTH = rep(mean(surveys$BATHY.DEPTH[surveys$STOCK == 'EGOM']),
                 nrow(yearseason_grid)),
  oisst = rep(mean(surveys$oisst[surveys$STOCK == 'EGOM']),
                 nrow(yearseason_grid)),
  year_season = yearseason_levels,
  dummy = TRUE)
dummy_data.GBK = data.frame(
  Year = yearseason_grid[,'Year'],
  Season = yearseason_grid[,'Season'],
  Lon = rep(GBK.vals$x, nrow(yearseason_grid)),
  Lat = rep(GBK.vals$y, nrow(yearseason_grid)),
  vessel = rep(8, nrow(yearseason_grid)),
  swept = rep(as_units(1, unitless), nrow(yearseason_grid)),
  COD_N = rep(as_units(0, 'counts'), nrow(yearseason_grid)),
  COD_KG = rep(as_units(0, 'kg'), nrow(yearseason_grid)),
  COND = rep('SMOOTh', nrow(yearseason_grid)),
  cobble_P = rep(mean(surveys$cobble_P[surveys$STOCK == 'GBK']),
                 nrow(yearseason_grid)),
  gravel_P = rep(mean(surveys$gravel_P[surveys$STOCK == 'GBK']),
                 nrow(yearseason_grid)),
  mud_P = rep(mean(surveys$mud_P[surveys$STOCK == 'GBK']),
              nrow(yearseason_grid)),
  rock_P = rep(mean(surveys$rock_P[surveys$STOCK == 'GBK']),
               nrow(yearseason_grid)),
  sand_P = rep(mean(surveys$sand_P[surveys$STOCK == 'GBK']),
               nrow(yearseason_grid)),
  BATHY.DEPTH = rep(mean(surveys$BATHY.DEPTH[surveys$STOCK == 'GBK']),
                    nrow(yearseason_grid)),
  oisst = rep(mean(surveys$oisst[surveys$STOCK == 'GBK']),
              nrow(yearseason_grid)),
  year_season = yearseason_levels,
  dummy = TRUE)
dummy_data.SNE = data.frame(
  Year = yearseason_grid[,'Year'],
  Season = yearseason_grid[,'Season'],
  Lon = rep(SNE.vals$x, nrow(yearseason_grid)),
  Lat = rep(SNE.vals$y, nrow(yearseason_grid)),
  vessel = rep(8, nrow(yearseason_grid)),
  swept = rep(as_units(1, unitless), nrow(yearseason_grid)),
  COD_N = rep(as_units(0, 'counts'), nrow(yearseason_grid)),
  COD_KG = rep(as_units(0, 'kg'), nrow(yearseason_grid)),
  COND = rep('SMOOTh', nrow(yearseason_grid)),
  cobble_P = rep(mean(surveys$cobble_P[surveys$STOCK == 'SNE']),
                 nrow(yearseason_grid)),
  gravel_P = rep(mean(surveys$gravel_P[surveys$STOCK == 'SNE']),
                 nrow(yearseason_grid)),
  mud_P = rep(mean(surveys$mud_P[surveys$STOCK == 'SNE']),
              nrow(yearseason_grid)),
  rock_P = rep(mean(surveys$rock_P[surveys$STOCK == 'SNE']),
               nrow(yearseason_grid)),
  sand_P = rep(mean(surveys$sand_P[surveys$STOCK == 'SNE']),
               nrow(yearseason_grid)),
  BATHY.DEPTH = rep(mean(surveys$BATHY.DEPTH[surveys$STOCK == 'SNE']),
                    nrow(yearseason_grid)),
  oisst = rep(mean(surveys$oisst[surveys$STOCK == 'SNE']),
              nrow(yearseason_grid)),
  year_season = yearseason_levels,
  dummy = TRUE)
dummy_data.WGOM = data.frame(
  Year = yearseason_grid[,'Year'],
  Season = yearseason_grid[,'Season'],
  Lon = rep(WGOM.vals$x, nrow(yearseason_grid)),
  Lat = rep(WGOM.vals$y, nrow(yearseason_grid)),
  vessel = rep(8, nrow(yearseason_grid)),
  swept = rep(as_units(1, unitless), nrow(yearseason_grid)),
  COD_N = rep(as_units(0, 'counts'), nrow(yearseason_grid)),
  COD_KG = rep(as_units(0, 'kg'), nrow(yearseason_grid)),
  COND = rep('SMOOTh', nrow(yearseason_grid)),
  cobble_P = rep(mean(surveys$cobble_P[surveys$STOCK == 'WGOM']),
                 nrow(yearseason_grid)),
  gravel_P = rep(mean(surveys$gravel_P[surveys$STOCK == 'WGOM']),
                 nrow(yearseason_grid)),
  mud_P = rep(mean(surveys$mud_P[surveys$STOCK == 'WGOM']),
              nrow(yearseason_grid)),
  rock_P = rep(mean(surveys$rock_P[surveys$STOCK == 'WGOM']),
               nrow(yearseason_grid)),
  sand_P = rep(mean(surveys$sand_P[surveys$STOCK == 'WGOM']),
               nrow(yearseason_grid)),
  BATHY.DEPTH = rep(mean(surveys$BATHY.DEPTH[surveys$STOCK == 'WGOM']),
                    nrow(yearseason_grid)),
  oisst = rep(mean(surveys$oisst[surveys$STOCK == 'WGOM']),
              nrow(yearseason_grid)),
  year_season = yearseason_levels,
  dummy = TRUE)

dummy_data <- rbind(dummy_data.EGOM, dummy_data.WGOM,
                    dummy_data.GBK , dummy_data.SNE)

# Combine with sampling data
full_data = rbind(cbind(all.scaled, dummy = FALSE), dummy_data)

# Create sample data
samp_dat = data.frame(
  "year_season" = as.numeric(full_data$year_season)-1,
  "Lat" = full_data$Lat,
  "Lon" = full_data$Lon,
  "COD_N" = full_data$COD_N,
  "COD_KG" = full_data$COD_KG,
  "swept" = full_data$swept,
  "vessel" = full_data$vessel,
  "Dummy" = full_data$dummy )

# Covariate data. Note here, case sensitive!
cov_dat = data.frame(
  "Year" = as.numeric(full_data$year_season)-1,
  "Year_Cov" = factor(full_data$Year, levels = year_set),
  "Season" = full_data$Season,
  "Lat" = full_data$Lat,
  "Lon" = full_data$Lon,
  "gravel_P" = full_data$cobble_P,
  "oisst" = full_data$oisst)

#### Make settings ####
rm(list=setdiff(ls(), c("cov_dat", "strata_use", "samp_dat", "vast_extrap_info")))

setwd(here("VAST_runs/Season_1"))
settings = make_settings( n_x = 200,
                          Region = "User",
                          purpose = "index2", 
                          bias.correct = FALSE,
                          knot_method = "grid",
                          strata.limits = strata_use,
                          FieldConfig = c("Omega1" = 1, "Epsilon1" = 1, "Omega2" = 1, "Epsilon2" = 1),
                          RhoConfig = c("Beta1" = 3, "Beta2" = 3, "Epsilon1" = 4, "Epsilon2" = 4),
                          ObsModel = c(1, 1))

# Creating model formula
formula_use = ~ Season + Year_Cov + gravel_P

# Implement corner constraint for linear effect but not spatially varying effect:
# * one level for each term is 2 (just spatially varying)
# * all other levels for each term is 3 (spatialy varying plus linear effect)
X1config_cp_use = matrix( c(2, rep(3,nlevels(cov_dat$Season)-1), 2, rep(3,nlevels(cov_dat$Year_Cov)-1) ), nrow=1 )
X2config_cp_use = matrix( c(2, rep(3,nlevels(cov_dat$Season)-1), 2, rep(3,nlevels(cov_dat$Year_Cov)-1) ), nrow=1 )


#### Run model ####

fit_orig = fit_model(
  # Call settings
  "settings" = settings,
                     
  # Call survey data info                   
  "Lat_i" = samp_dat[, 'Lat'],
  "Lon_i" = samp_dat[, 'Lon'],
  "t_i" = samp_dat[, 'year_season'],
  "b_i" = samp_dat[, 'COD_N'],
  "a_i" = samp_dat[, 'swept'],
  #"v_i" = samp_dat[, 'vessel'],
  
  # Call covariate info                   
  "X1config_cp" = X1config_cp_use,
  "X2config_cp" = X2config_cp_use,
  "covariate_data" = cov_dat,
  "X1_formula" = formula_use,
  "X2_formula" = formula_use,
  "X_contrasts" = list(Season = contrasts(cov_dat$Season, contrasts = FALSE), 
                       Year_Cov = contrasts(cov_dat$Year_Cov, contrasts = FALSE)),
  
  # Call spatial info
  extrapolation_list = vast_extrap_info,
  
  # Identify predictions
  "PredTF_i" = samp_dat[, 'Dummy'],
  
  # Tell model to run                   
  "run_model" = FALSE)

# Adjust mapping for log_sigmaXi and fitting final model -- pool variance for all seasons and then set year's to NA
Map_adjust = fit_orig$tmb_list$Map

# Pool variances for each term to a single value
Map_adjust$log_sigmaXi1_cp = factor(c(rep(as.numeric(Map_adjust$log_sigmaXi1_cp[1]), nlevels(cov_dat$Season)),
                                      rep(as.numeric(Map_adjust$log_sigmaXi1_cp[nlevels(cov_dat$Season)+1]), nlevels(cov_dat$Year_Cov))))
Map_adjust$log_sigmaXi2_cp = factor(c(rep(as.numeric(Map_adjust$log_sigmaXi2_cp[1]), nlevels(cov_dat$Season)),
                                      rep(as.numeric(Map_adjust$log_sigmaXi2_cp[nlevels(cov_dat$Season)+1]), nlevels(cov_dat$Year_Cov))))

# Fit final model with new mapping
fit  = fit_model(
  # Call settings
  "settings" = settings,
  
  # Call survey data info                   
  "Lat_i" = survs[, 'Lat'],
  "Lon_i" = survs[, 'Lon'],
  "t_i" = survs[, 'year_season'],
  "b_i" = survs[, 'COD_N'],
  "a_i" = survs[, 'swept'],
  
  # Call covariate info                   
  "X1config_cp" = X1config_cp_use,
  "X2config_cp" = X2config_cp_use,
  "covariate_data" = scaled.covars,
  "X1_formula" = formula_use,
  "X2_formula" = formula_use,
  "X_contrasts" = list(Season = contrasts(cov_dat$Season, contrasts = FALSE), 
                       Year_Cov = contrasts(cov_dat$Year_Cov, contrasts = FALSE)),
  "newtonsteps" = 1,
                 "PredTF_i" = samp_dat[, 'Dummy'],
                 "Map" = Map_adjust )

# fit = fit_model( 
#   # Call settings
#     settings = settings, 
#     
#   # Call survey data info
#     Lat_i = survs[,'Lat'], 
#     Lon_i = survs[,'Lon'], 
#     t_i = survs[,'Year'], 
#     b_i = survs[,'COD_N'], 
#     a_i = survs[,'swept'], 
#     v_i = survs[,'vessel'],
#   
#   # Call covariate info
#     X1_formula = ~ gravel_P + #cobble_P + mud_P + rock_P + 
#                    sand_P + 
#                    #BATHY.DEPTH + 
#                    oisst,
#     covariate_data = scaled.covars,
#   
#   # Call spatial info
#     extrapolation_list = vast_extrap_info,
#   
#   # Tell model to run
#     run_model = TRUE)

#### Plot results ####

save.image('strata_covs_1.RData')

plot( fit )
