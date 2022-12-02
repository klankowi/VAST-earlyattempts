rm(list=ls())

library(VAST)
library(sf)
library(tidyverse)
library(rgdal)
library(here)

## Add spatial grid
user_region <- readRDS(here('data/user_region_wgom.rds'))

## Add strata
strata <- readOGR(here("data/strata_stox_orig.shp"))
strata.2 <- st_as_sf(strata)
strata.tab <- table(strata.2$STRATA_1, strata.2$STOCK)

## Set limits
AllStrata <- as.numeric(strata.2$STRATA_1)
EGOM <- strata.2 %>% 
  filter(STRATA_1 %in% names(strata.tab[,"EGOM"])) %>% 
  select(STRATA_1)
EGOM <- as.numeric(EGOM$STRATA_1)
WGOM <- strata.2 %>% 
  filter(STRATA_1 %in% names(strata.tab[,"WGOM"])) %>% 
  select(STRATA_1)
WGOM <- as.numeric(WGOM$STRATA_1)
GBK <- strata.2 %>% 
  filter(STRATA_1 %in% names(strata.tab[,"GBK"])) %>% 
  select(STRATA_1)
GBK <- as.numeric(GBK$STRATA_1)
SNE <- strata.2 %>% 
  filter(STRATA_1 %in% names(strata.tab[,"SNE"])) %>% 
  select(STRATA_1)
SNE <- as.numeric(SNE$STRATA_1)
strata.limits <- as.list(c("AllStrata" = AllStrata, 
                           "EGOM" = EGOM, 
                           "WGOM" = WGOM, 
                           "GBK" = GBK, 
                           "SNE" = SNE))

## Plot for posterity
ggplot(data = ecodata::coast) + 
  #geom_sf() + 
  geom_sf(data=strata.2, aes(color=STOCK, fill=STOCK),
          alpha=0.5) + 
  geom_sf() +
  coord_sf(xlim=c(-78, -65.5), ylim=c(36, 45)) + 
  theme_bw()

## Add sampling data
surveys <- read.csv(here("data/Survey_Data.csv"))
wgom <- subset(surveys, STOCK=='WGOM')
wgom <- subset(wgom, SURVEY=='NEFSC BTS')
wgom <- subset(wgom, SEASON=='FALL')
ex2 <- dplyr::select(wgom, COD_N, YEAR, LAT, LON)
ex2 <- subset(ex2, YEAR > 1981)
ex2 <- subset(ex2, YEAR < 2020)
ex2 <- subset(ex2, is.na(COD_N)==FALSE)
ex2 <- ex2[with(ex2, order(YEAR)),]
row.names(ex2) <- NULL

## Add covariates
# 

## Combine to VAST list format
attempt <- vector(mode="list", length=4)
names(attempt) <- c('sampling_data', 'Region', 'strata.limits', 'covariate_data')
attempt[[1]] <- ex2
attempt[[2]] <- user_region
attempt[[3]] <- strata.limits
attempt[[4]] <- example[[4]]
names(attempt) <- names(example)

# Make settings (turning off bias.correct to save time for example)
settings = make_settings( n_x = 50, 
                          Region='User', 
                          purpose = "index2", 
                          bias.correct = FALSE )

# Run model
fit = fit_model( settings = settings, 
                 Lat_i = attempt$sampling_data[,'LAT'], 
                 Lon_i = attempt$sampling_data[,'LON'], 
                 t_i = attempt$sampling_data[,'YEAR'], 
                 b_i = attempt$sampling_data[,'COD_N'], 
                 a_i = a_i = rep(1, nrow(ex2)), 
                 input_grid=user_region)

# Plot results
plot( fit )