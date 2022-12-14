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
# user_region <- readRDS(here('data/RData_Storage/user_region_all_2.rds'))

## Add stock limits
# stox <- readOGR(here("data/GIS/codstox.shp"))
# stox.sf <- st_as_sf(stox)
# stox.df <- sfheaders::sf_to_df(stox.sf, fill=T)
##  Plot for posterity
# ggplot(ecodata::coast) +
#  geom_sf(data=stox.sf, col='black',
#         aes(fill=STOCK)) +
#  geom_sf() +
#  coord_sf(xlim=c(-76, -65), ylim=c(36, 46))
#
# ## Set limits by OBJECTID
# EGOM <- unique(as.numeric(stox.df$OBJECTID[stox.df$STOCK == 'EGOM']))
# WGOM <- unique(as.numeric(stox.df$OBJECTID[stox.df$STOCK == 'WGOM']))
# GBK <- unique(as.numeric(stox.df$OBJECTID[stox.df$STOCK == 'GBK']))
# SNE <- unique(as.numeric(stox.df$OBJECTID[stox.df$STOCK == 'SNE']))
#
# stox.limits <- as.list(c("Allstox" = NA,
#                          "EGOM" = EGOM,
#                          "WGOM" = WGOM,
#                          "GBK" = GBK,
#                          "SNE" = SNE))
# stox.limits$Allstox <- seq(1:4)
# rm(stox.df, stox.sf, EGOM, GBK, SNE, WGOM)

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

# Set strata.limits
All_areas = as.data.frame(seq(1:1e+05))
colnames(All_areas) <- 'stratum'
EGOM = as.data.frame(c(1351,
                       1360,
                       1380,
                       1390,
                       3800,
                       3810,
                       3820,
                       3830,
                       3840,
                       3850,
                       3860,
                       3870,
                       3880,
                       3890,
                       3900
))
colnames(EGOM) <- 'stratum'
GBK = as.data.frame(c(1130,
                    1140,
                    1150,
                    1160,
                    1170,
                    1180,
                    1190,
                    1200,
                    1210,
                    1220,
                    1290,
                    1300
))
colnames(GBK) <- 'stratum'
SNE = as.data.frame(c(1,
                      1010,
                      1020,
                      1030,
                      1050,
                      1060,
                      1070,
                      1090,
                      1650,
                      1690,
                      1700,
                      1730,
                      1740,
                      1750,
                      1760,
                      3010,
                      3020,
                      3040,
                      3050,
                      3070,
                      3080,
                      3100,
                      3110,
                      3130,
                      3140,
                      3160,
                      3170,
                      3190,
                      3200,
                      3220,
                      3230,
                      3250,
                      3260,
                      3280,
                      3290,
                      3310,
                      3320,
                      3340,
                      3350,
                      3370,
                      3380,
                      3450,
                      3460,
                      3470,
                      3480,
                      3500,
                      3510,
                      3530,
                      3540,
                      3920
))
colnames(SNE) <- 'stratum'
WGOM = as.data.frame(c(1100,
                       1110,
                       1120,
                       1230,
                       1240,
                       1250,
                       1260,
                       1270,
                       1280,
                       1370,
                       1400,
                       3520,
                       3550,
                       3560,
                       3570,
                       3580,
                       3590,
                       3600,
                       3610,
                       3620,
                       3630,
                       3640,
                       3650,
                       3660,
                       3670,
                       3680,
                       3690,
                       3700,
                       3710,
                       3720,
                       3730,
                       3740,
                       3750,
                       3760,
                       3770,
                       3780,
                       3790,
                       3908
))
colnames(WGOM) <- 'stratum'
strata.limits <- as.list(c("All_areas" = All_areas, 
                           "EGOM" = EGOM,
                           "GBK" = GBK,
                           "SNE" = SNE,
                           "WGOM" = WGOM))
strata.limits

## Combine to VAST list format
attempt <- vector(mode="list", length=3)
names(attempt) <- c('sampling_data', 'Region', 'strata.limits')
attempt[[1]] <- ex2
attempt[[2]] <- "northwest_atlantic"
attempt[[3]] <- strata.limits

# Remove intermediates
rm(stox, stox.limits, fall, i , spring, summer, winter,
   EGOM, GBK, All_areas, SNE, WGOM, strata.limits)

setwd(here("VAST_Runs/Gravel_Dens_Covar3"))

# Make settings (turning off bias.correct to save time for example)
settings = make_settings( n_x = 200, 
                          Region='northwest_atlantic',
                          purpose = "index2", 
                          bias.correct = FALSE,
                          strata.limits = attempt$strata.limits)

# Run model
#Extrapolation_List <- readRDS(here("data/RData_Storage/CustomExtrapolationList.RDS"))
fit = fit_model( settings = settings,
                 strata.limits = attempt$strata.limits,
                 Lat_i = attempt$sampling_data[,'Lat'], 
                 Lon_i = attempt$sampling_data[,'Lon'], 
                 t_i = attempt$sampling_data[,'Year'], 
                 b_i = attempt$sampling_data[,'COD_N'], 
                 a_i = attempt$sampling_data[,'swept'], 
                 X1_formula = ~gravel_P + cobble_P + rock_P + sand_P + mud_P,
                 covariate_data=covars)


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
