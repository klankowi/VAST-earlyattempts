rm(list=ls())

library(VAST)



# Dumbed down example data (surveys, wgom, spring)
surveys <- read.csv(paste0("C:/Users/klankowicz/Box/Katie Lankowicz/",
                           "Groundfish_Survey_Data_Mods/Collated_Data/",
                           "Survey_Data.csv"))

wgom <- subset(surveys, STOCK=='WGOM')

wgom <- wgom[is.na(wgom$LAT)==FALSE,]
wgom <- wgom[is.na(wgom$LON)==FALSE,]
#wgom <- subset(wgom, SURVEY=="NEFSC BTS")
wgom <- subset(wgom, SEASON=='SPRING')
ex2 <- dplyr::select(wgom, COD_N, YEAR, LAT, LON)
ex2 <- subset(ex2, YEAR > 1981)
ex2 <- subset(ex2, YEAR < 2020)
ex2 <- subset(ex2, is.na(COD_N)==FALSE)
ex2 <- ex2[with(ex2, order(YEAR)),]
row.names(ex2) <- NULL

#ex2$AREA.SWEPT <- 0.01
head(ex2)
str(ex2)

setwd("C:/Users/klankowicz/Desktop/VAST_examples/Dummy")
user_region <- readRDS('user_region_wgom.rds')

attempt <- vector(mode="list", length=4)
attempt[[1]] <- ex2
attempt[[2]] <- user_region
attempt[[3]] <- example[[3]]
attempt[[4]] <- example[[4]]
names(attempt) <- names(example)

example <- attempt

# Make settings (turning off bias.correct to save time for example)
settings = make_settings( n_x = 50, 
                          Region='User', 
                          purpose = "index2", 
                          bias.correct = FALSE )

# Run model
fit = fit_model( settings = settings, 
                 Lat_i = example$sampling_data[,'LAT'], 
                 Lon_i = example$sampling_data[,'LON'], 
                 t_i = example$sampling_data[,'YEAR'], 
                 b_i = example$sampling_data[,'COD_N'], 
                 a_i = example$sampling_data[,'AREA.SWEPT'],
                 input_grid=user_region)

# Plot results
plot( fit )
