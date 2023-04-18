rm(list=ls())

library(tidyverse)
library(here)

# Load data
blls <- read.csv(here('lls_spp_station_length_1421.csv'))
blls <- dplyr::select(blls, 
                      year, s1, season, station, station_id, stratum_use,
                      decdeg_beglat_set, decdeg_beglon_set,
                      decdeg_endlat_set, decdeg_endlon_set,
                      cruise)
blls <- unique(blls)
blls$HAUL_ID <- paste0(blls$cruise, "-", blls$stratum_use, "-",
                       blls$station)

oldbts <- read.csv(here('ending_latlon_bts_oldformat.csv'))
colnames(oldbts) <- c('HAUL_ID', 'endlon', 'endlat')
newbts <- read.csv(here('ending_latlon_bts_newformat.csv'))
newbts <- dplyr::select(newbts,
                        HAUL_ID, endlon, endlat)
btspts <- rbind(oldbts, newbts)

# Load surveys
survs <- read.csv(here('data/Dataframes/sciencecenter_wrug.csv'))
survs <- dplyr::select(survs, -FID_Rugosi, -STRATA_1, -pct70, -rugosity,
                       -OID, -FID_scienc)
head(survs)

# Strip to just one age category
survs <- subset(survs, AGEGROUP == 'Age0-2')

# Split to surveys
survs <- subset(survs, SURVEY == 'NEFSC BTS' |
                  SURVEY =='NEFSC BLLS')

# Stip to just years BLLS took place
table(survs$YEAR[survs$SURVEY == "NEFSC BLLS"])
survs <- subset(survs, YEAR %in% seq(2014, 2022))

# Strip to just WGOM
survs <- subset(survs, STOCK == 'WGOM')

# Strip to 9-box footprint
survs <- subset(survs, LON > -71 & LON < -69 &
                  LAT > 42 & LAT < 43.5)

# Strip by survey
survs.bts <- subset(survs, SURVEY == "NEFSC BTS")
survs.blls <- subset(survs, SURVEY == "NEFSC BLLS")

# Merge bts
survs.bts.m <- merge(survs.bts, btspts, by=c("HAUL_ID"))

# Merge blls
blls <- dplyr::select(blls, HAUL_ID, decdeg_endlon_set, decdeg_endlat_set)
survs.blls.m <- merge(survs.blls, blls, by=c("HAUL_ID"))
colnames(survs.blls.m) <- c(colnames(survs.blls.m)[1:34],
                            'endlon', 'endlat')

rm(blls, btspts, newbts, oldbts, survs, survs.blls, survs.bts)

survs <- rbind(survs.blls.m, survs.bts.m)

rm(survs.blls.m, survs.bts.m)

write.csv(survs, 'possible_tsplocs.csv', row.names = F)
