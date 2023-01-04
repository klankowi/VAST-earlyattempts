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
user_region <- readRDS(here('data/RData_Storage/user_region_all_2.rds'))

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
EGOM <- unique(as.numeric(stox.df$OBJECTID[stox.df$STOCK == 'EGOM']))
WGOM <- unique(as.numeric(stox.df$OBJECTID[stox.df$STOCK == 'WGOM']))
GBK <- unique(as.numeric(stox.df$OBJECTID[stox.df$STOCK == 'GBK']))
SNE <- unique(as.numeric(stox.df$OBJECTID[stox.df$STOCK == 'SNE']))

stox.limits <- as.list(c("Allstox" = NA, 
                         "EGOM" = EGOM, 
                         "WGOM" = WGOM, 
                         "GBK" = GBK, 
                         "SNE" = SNE))
stox.limits$Allstox <- seq(1:4)
#rm(stox.df, stox.sf, EGOM, GBK, SNE, WGOM)

## Add sampling data and covars
surveys <- readRDS(here("data/RData_storage/agg_stn_all_OISST.rds"))

# Assign season
source(here("utilities/true_seasons_func.R"))
for(i in 1:nrow(surveys)){
  surveys$SEASON[i] <- true_seasons(surveys$DATE[i])
}
table(surveys$month, surveys$SEASON)

ex <- sfheaders::sf_to_df(surveys, fill=T)
ex2 <- ex %>% 
  dplyr::select(-c(SALINITY, BOTTOM.TYPE, cobble_V,
                   gravel_V, rock_V, mud_V, sand_V, month, day, yrmody, declon,
                   declat, sfg_id, point_id, STRATUM, COD_KG, HAUL_ID,
                   DEPTH, SURFACE.TEMP, BOTTOM.TEMP, TRUE_SEASON))
ex2 <- subset(ex2, is.na(COD_N)==FALSE)
ex2 <- ex2[with(ex2, order(DATE)),]
row.names(ex2) <- NULL

# Change data structure
ex2$SEASON <- as.factor(ex2$SEASON)
ex2$COND <- as.factor(ex2$COND)
ex2$SURVEY <- as.factor(ex2$SURVEY)
ex2$STOCK <- as.factor(ex2$STOCK)
ex2$COD_N <- as_units(ex2$COD_N, "counts")
ex2$swept <- as_units(1, unitless)
ex2$VESSEL <- as.integer(ex2$SURVEY) - 1
ex2$SURVEY <- NULL
head(ex2)

# Check structure
names(ex2) <- c('Year', names(ex2[2:13]), 'Lon', 'Lat', 'swept', 'vessel')
str(ex2)

rm(surveys, ex, good.seas, na.gso, na.seas, months.fall, months.spring,
   months.summer, months.winter, i, na.rid)

# Pull covariate data
covars <- dplyr::select(ex2, 
                        Year, Lon, Lat, cobble_P, gravel_P, mud_P, rock_P,
                        sand_P)


## Combine to VAST list format
attempt <- vector(mode="list", length=3)
names(attempt) <- c('sampling_data', 'Region', 'strata.limits')
attempt[[1]] <- ex2
attempt[[2]] <- user_region
attempt[[3]] <- strata.limits

# Rename stocks
attempt$sampling_data$STRATA <- as.numeric(attempt$sampling_data$STOCK)
attempt$sampling_data$STOCK <- NULL

# Remove intermediates
rm(stox, stox.limits, fall, i , spring, summer, winter)

setwd(here("VAST_Runs/Gravel_Dens_Covar2"))

# Make settings (turning off bias.correct to save time for example)
settings = make_settings( n_x = 200, 
                          Region='User', 
                          purpose = "index2", 
                          bias.correct = FALSE,
                          strata.limits = attempt$strata.limits)

# Run model
fit = fit_model( settings = settings, 
                 Lat_i = attempt$sampling_data[,'Lat'], 
                 Lon_i = attempt$sampling_data[,'Lon'], 
                 t_i = attempt$sampling_data[,'Year'], 
                 b_i = attempt$sampling_data[,'COD_N'], 
                 a_i = attempt$sampling_data[,'swept'], 
                 input_grid=user_region,
                 X1_formula = ~gravel_P + cobble_P + rock_P + sand_P + mud_P,
                 covariate_data=covars)


# Plot results
plot( fit )
