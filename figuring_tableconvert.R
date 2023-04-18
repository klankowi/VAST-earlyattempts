# Strip beginning and ending lat-lon from NOAA cruise reports
rm(list=ls())

# Load libraries
library(pdftools)
library(staplr)
library(tidyverse)
library(here)
# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

# Set working dir
setwd("C:/Users/klankowicz/Box/Katie Lankowicz/Data_Analysis/Cod/BTS Reports")

# Name files
file.names <- list.files(getwd())
file.names <- file.names[-1]

# Table locations
tabpag <- read.csv(paste0(getwd(), '/', file.names[18]))
file.names <- file.names[-18]

# Tables with old data format
old.format.files <- file.names[1:8]

# Tables with new data format
new.format.files <- file.names[9:17]

# Make blank df to write to later for all pages
blankup2 <- data.frame(
  station = NA,
  date = NA,
  time = NA,
  lat=NA,
  lon=NA,
  loran.x=NA,
  loran.y=NA,
  course=NA,
  depth=NA,
  sst.f=NA,
  flag=NA,
  rownum=NA,
  pagenum=NA,
  cruise=NA
)
# Extract the table
for(i in 1:
    #1 #testing
    length(old.format.files)
    ){
  # Set cruise name
  cruise <- old.format.files[i]
  # Pages with tables
  pagnums <- seq(tabpag[i, 'bpage'], tabpag[i, 'epage'])
  # Number of pages in pdf
  pdfnums <- seq(1,
                 pdf_info(paste0(getwd(), '/', cruise))$pages)
  # Pages without tables on them
  remnums <- pdfnums[pdfnums %notin% pagnums]
  # Remove pages without tables on them, save
  goodpag <- remove_pages(remnums,
                          input_filepath = paste0(getwd(), '/', cruise),
                          output_filepath = paste0(getwd(), '/clean/', cruise),
                          overwrite=T)
  # Pull pages with tables on them
  usepdf <- pdf_text(paste0(getwd(), '/clean/', cruise))
  # Convert to PDF
  usepdf <- as.data.frame(usepdf)
  # Make blank df to write to later for all pages
  blankup1 <- data.frame(
    station = NA,
    date = NA,
    time = NA,
    lat=NA,
    lon=NA,
    loran.x=NA,
    loran.y=NA,
    course=NA,
    depth=NA,
    sst.f=NA,
    flag=NA,
    rownum=NA,
    pagenum=NA,
    cruise=NA
  )
  # Loop through number of tables in cruise report
  for(j in 1:
      #1 #testing
      nrow(usepdf)
      ){
    # Pull a table
    temp <- usepdf[j,]
    # split at linebreaks
    temp <- strsplit(temp, '\n')
    temp <- temp[[1]]
    # Remove whitespace
    temp <- trimws(temp)
    # Find row that begins the data
    headstart <- temp[grep("---", temp)]
    # Set row that begins the data
    rowstart <- which(temp == paste0(headstart)) +1
    # Remove extraneous header info
    temp <- temp[rowstart: length(temp)]
    # Create blank df to write to for all rows
    blank <- data.frame(
      station = NA,
      date = NA,
      time = NA,
      lat=NA,
      lon=NA,
      loran.x=NA,
      loran.y=NA,
      course=NA,
      depth=NA,
      sst.f=NA,
      flag=NA,
      rownum=NA,
      pagenum=NA,
      cruise=NA
    )
    # Loop through rows of table
    for(k in 1:
        #1 #testing
        length(temp)
        ){
      # Split at whitespace
      hold <- strsplit(temp[[k]], " +")
      # Convert to array
      hold <- do.call(rbind, hold)
      # If a value is missing (eg, temperature)
      if(dim(hold)[2] < 10){
        # Find out how many items in vector
        baddim <- dim(hold)[2]
        # Calculate how many need to be added
        toadd <- 10 - baddim
        # Create vector of NAs to fill
        adna <- rep(NA, toadd)
        # Fill at end of data in row
        hold <- c(hold, adna)
        # Convert to df
        hold <- as.data.frame(t(hold))
        # Flag row as missing data
        hold$V11 <- 'bad'
        # Set k (row number)
        hold$V12 <- k
        # Set j (page number)
        hold$V13 <- j
        # Add cruise
        hold$cruise <- strsplit(cruise, '.pdf')[[1]]
        # Set column names
        colnames(hold) <- colnames(blank)
        # Bind to dataframe that will hold all rows of table
        blank <- rbind(blank, hold)
        # Remove extraneous
        rm(baddim, toadd, adna)
      } # end if loop -- missing values
      # If no missing values
      if(dim(hold)[2] == 10){
        # Convert to dataframe
        hold <- as.data.frame(hold)
        # Indicate no problems with row
        hold$V11 <- 'fine'
        # SEt k (row number)
        hold$V12 <- k
        # Set j (page number)
        hold$V13 <- j
        # Add cruise
        hold$cruise <- strsplit(cruise, '.pdf')[[1]]
        # Set column names
        colnames(hold) <- colnames(blank)
        # Bind to dataframe that will hold all rows of table
        blank <- rbind(blank, hold)
      } # end if loop -- no missing values
      # Set j (page number)
      blank$pagenum <- j
      # Remove rows with no station (these are blanks used in initialization)
      blank <- blank[!is.na(blank$station),]
    } # end k loop (row numbers)
    # Bind to dataframe that holds all pages of cruise report tables
    blankup1 <- rbind(blankup1, blank)
    # Remove rows with no station (these are blanks used in initialization)
    blankup1 <- blankup1[!is.na(blankup1$station),]
    # Remove duplicates
    blankup1 <- unique(blankup1)
    # remove extraneous
    rm(blank, hold)
  } # end j loop (pages)
  # remove extraneous
  rm(temp, headstart, rowstart)
  # Bind to dataframe that holds all pages of cruise report tables
  blankup2 <- rbind(blankup2, blankup1)
  # Remove rows with no station (these are blanks used in initialization)
  blankup2 <- blankup2[!is.na(blankup2$station),]
  # Remove duplicates
  blankup2 <- unique(blankup2)
  # remove extraneous
  rm(blankup1)
} # end i loop (cruises)
# Remove extraneous
rm(cruise, pagnums, pdfnums, remnums, goodpag, usepdf)
head(blankup2)

# Alter result to separate year and season
blankup2 <- separate(blankup2, cruise, into = c("noaa", "year", "season"), sep = "_")
blankup2$noaa <- NULL

# Check numbers by season
table(blankup2$year, blankup2$season)

# Create date formatted by posixct
blankup2$realdate <- as.POSIXct(paste0(blankup2$date, "-", blankup2$year),
                                format="%b-%d-%Y")

# Set month
blankup2$month <- month(blankup2$realdate)

# Check numbers by month
table(blankup2$year, blankup2$month)

# Save results -- alter in excel
write.csv(blankup2, row.names = F, 'btstows_oldformat.csv')

# Start fresh
rm(list=ls())

# Load altered csv
btsold <- read.csv('btstows_oldformat.csv')

# Load key to match new haul id names to old haul id names
btsidmatch <- read.csv('bts_idmatch.csv')
head(btsold)
head(btsidmatch)

# Match by haul id key
btsold$HAUL_ID2 <- paste0(btsold$year, "_", 
                          toupper(btsold$season), "_",
                          str_pad(btsold$station, 3, "left", '0'))

btsold <- merge(btsold, btsidmatch, by=c('HAUL_ID2'))

# Load survey info
survs <- read.csv(here('data/Dataframes/sciencecenter_wrug.csv'))
survs <- dplyr::select(survs, -FID_Rugosi, -STRATA_1, -pct70, -rugosity,
                       -OID, -FID_scienc)
head(survs)

# Strip to just one age category
survs <- subset(survs, AGEGROUP == 'Age0-2')

# Split to bottom trawl survey
survs <- subset(survs, SURVEY == 'NEFSC BTS')

# Stip to just years BLLS took place, old format
table(btsold$year)
survs <- subset(survs, YEAR %in% seq(2014, 2017))

# Strip to just WGOM
survs <- subset(survs, STOCK == 'WGOM')

# Strip to 9-box footprint
survs <- subset(survs, LON > -71 & LON < -69 &
                  LAT > 42 & LAT < 43.5)

# Merge possible (WGOM) locations and cruise report results
btsold <- dplyr::select(btsold,
                        HAUL_ID, course)
test <- merge(survs, btsold, by=c('HAUL_ID'))

# Strip unneeded variables
findpath <- dplyr::select(test, 
                          HAUL_ID, LAT, LON, course)
library(units)

# Set distance traveled (aim is 1 nmi)
findpath$dist_nmi <- 1

# Convert nmi to km
findpath$distance_km <- ud.convert(findpath$dist_nmi, "nautical_mile", "km")

# Remove observations missing course
findpath <- findpath[!is.na(findpath$course),]
head(findpath)

# Convert to sf
pts <- findpath %>%
  sf::st_as_sf(coords = c("LON","LAT")) %>%
  sf::st_set_crs(4326)

# Re project
pts <- st_transform(pts, 32619)

# Get eastings and northings (in meters)
pts$utm_n <- st_coordinates(pts)[,1]
pts$utm_e <- st_coordinates(pts)[,2]

# Create buffer of distance traveled
buf <- st_buffer(pts, dist = pts$distance_km*1000)

# Convert buffer to line
circ <- st_cast(buf, "LINESTRING")

# Functions to convert degree to radian
rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}

# Find new easting and northing along buffer circumfrence given course
pts$newp_e <- pts$utm_e + (pts$distance_km*1000* cos(deg2rad(pts$course)))
pts$newp_n <- pts$utm_n + (pts$distance_km*1000* sin(deg2rad(pts$course)))

# Conver to data table
dt <- data.table(pts)

# Convert to sf
pts2 <- dt %>%
  sf::st_as_sf(coords = c("newp_n", "newp_e")) %>%
  sf::st_set_crs(32619)

# Transform to unprojected lat-lon
pts2 <- st_transform(pts2, 'EPSG:4326')

# Convert to df
pts2 <- sfheaders::sf_to_df(pts2, fill=T)

# Strip unneeded variables
pts2 <- dplyr::select(pts2, HAUL_ID, x, y)

# View results
head(pts2)

# Save.
write.csv(pts2, row.names = F,
          'ending_latlon_bts_oldformat.csv')

# New format
# Set working dir
rm(list=ls())
# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))
setwd("C:/Users/klankowicz/Box/Katie Lankowicz/Data_Analysis/Cod/BTS Reports")

# Name files
file.names <- list.files(getwd())
file.names <- file.names[c(-1, -2)]

# Table locations
tabpag <- read.csv(paste0(getwd(), '/', file.names[18]))
file.names <- file.names[-18]

# Tables with new data format
new.format.files <- file.names[9:17]

# Make blank df to write to later for all pages
blankup2 <- data.frame(
  station = NA,
  date = NA,
  time = NA,
  setlat=NA,
  setlon=NA,
  endlat=NA,
  endlon=NA,
  towdur=NA,
  towdist=NA,
  depthmean=NA,
  flag=NA,
  rownum=NA,
  pagenum=NA,
  cruise=NA
)
# Extract the table
for(i in 1:
    #1 #testing
    length(new.format.files)
){
  # Set cruise name
  cruise <- new.format.files[i]
  # Pages with tables
  pagnums <- seq(tabpag[(i+8), 'bpage'], 
                 tabpag[(i+8), 'epage'])
  # Number of pages in pdf
  pdfnums <- seq(1,
                 pdf_info(paste0(getwd(), '/', cruise))$pages)
  # Pages without tables on them
  remnums <- pdfnums[pdfnums %notin% pagnums]
  # Remove pages without tables on them, save
  goodpag <- remove_pages(remnums,
                          input_filepath = paste0(getwd(), '/', cruise),
                          output_filepath = paste0(getwd(), '/clean/', cruise),
                          overwrite=T)
  # Pull pages with tables on them
  usepdf <- pdf_text(paste0(getwd(), '/clean/', cruise))
  # Convert to PDF
  usepdf <- as.data.frame(usepdf)
  # Make blank df to write to later for all pages
  blankup1 <-data.frame(
    station = NA,
    date = NA,
    time = NA,
    setlat=NA,
    setlon=NA,
    endlat=NA,
    endlon=NA,
    towdur=NA,
    towdist=NA,
    depthmean=NA,
    flag=NA,
    rownum=NA,
    pagenum=NA,
    cruise=NA
  )
  # Loop through number of tables in cruise report
  for(j in 1:
      #1 #testing
      nrow(usepdf)
  ){
    # Pull a table
    temp <- usepdf[j,]
    # split at linebreaks
    temp <- strsplit(temp, '\n')
    temp <- temp[[1]]
    # Remove whitespace
    temp <- trimws(temp)
    # Find row that begins the data
    headstart <- temp[grep("STATION", temp)]
    if(length(headstart != 0)){
      # Set row that begins the data
      rowstart <- which(temp == paste0(headstart)) +1
    }
    if(length(headstart)== 0){
      headstart <- temp[grep("START TRAWL", temp)]
      # Set row that begins the data
      rowstart <- which(temp == paste0(headstart)) +1
    }
    if(length(headstart)== 0){
      headstart <- temp[grep("(nm)", temp)]
      # Set row that begins the data
      rowstart <- which(temp == paste0(headstart)) +2
    }

    # Remove extraneous header info
    temp <- temp[rowstart: length(temp)]
    # Create blank df to write to for all rows
    blank <- data.frame(
      station = NA,
      date = NA,
      time = NA,
      setlat=NA,
      setlon=NA,
      endlat=NA,
      endlon=NA,
      towdur=NA,
      towdist=NA,
      depthmean=NA,
      flag=NA,
      rownum=NA,
      pagenum=NA,
      cruise=NA
    )
    # Loop through rows of table
    for(k in 1:
        #1 #testing
        length(temp)
    ){
      # Split at whitespace
      hold <- strsplit(temp[[k]], " +")
      # Convert to array
      hold <- do.call(rbind, hold)
      # If a value is missing (eg, temperature)
      if(dim(hold)[2] < 10){
        # Find out how many items in vector
        baddim <- dim(hold)[2]
        # Calculate how many need to be added
        toadd <- 10 - baddim
        # Create vector of NAs to fill
        adna <- rep(NA, toadd)
        # Fill at end of data in row
        hold <- c(hold, adna)
        # Convert to df
        hold <- as.data.frame(t(hold))
        # Flag row as missing data
        hold$V11 <- 'bad'
        # Set k (row number)
        hold$V12 <- k
        # Set j (page number)
        hold$V13 <- j
        # Add cruise
        hold$cruise <- strsplit(cruise, '.pdf')[[1]]
        # Set column names
        colnames(hold) <- colnames(blank)
        # Bind to dataframe that will hold all rows of table
        blank <- rbind(blank, hold)
        # Remove extraneous
        rm(baddim, toadd, adna)
      } # end if loop -- missing values
      # If no missing values
      if(dim(hold)[2] == 10){
        # Convert to dataframe
        hold <- as.data.frame(hold)
        # Indicate no problems with row
        hold$V11 <- 'fine'
        # SEt k (row number)
        hold$V12 <- k
        # Set j (page number)
        hold$V13 <- j
        # Add cruise
        hold$cruise <- strsplit(cruise, '.pdf')[[1]]
        # Set column names
        colnames(hold) <- colnames(blank)
        # Bind to dataframe that will hold all rows of table
        blank <- rbind(blank, hold)
      } # end if loop -- no missing values
      # Set j (page number)
      blank$pagenum <- j
      # Remove rows with no station (these are blanks used in initialization)
      blank <- blank[!is.na(blank$station),]
    } # end k loop (row numbers)
    # Bind to dataframe that holds all pages of cruise report tables
    blankup1 <- rbind(blankup1, blank)
    # Remove rows with no station (these are blanks used in initialization)
    blankup1 <- blankup1[!is.na(blankup1$station),]
    # Remove duplicates
    blankup1 <- unique(blankup1)
    # remove extraneous
    rm(blank, hold)
  } # end j loop (pages)
  # remove extraneous
  rm(temp, headstart, rowstart)
  # Bind to dataframe that holds all pages of cruise report tables
  blankup2 <- rbind(blankup2, blankup1)
  # Remove rows with no station (these are blanks used in initialization)
  blankup2 <- blankup2[!is.na(blankup2$station),]
  # Remove duplicates
  blankup2 <- unique(blankup2)
  # remove extraneous
  rm(blankup1)
} # end i loop (cruises)
# Remove extraneous
rm(cruise, pagnums, pdfnums, remnums, goodpag, usepdf)
head(blankup2)

# Alter result to separate year and season
blankup2 <- separate(blankup2, cruise, into = c("noaa", "year", "season"), sep = "_")
blankup2$noaa <- NULL

# Check numbers by season
table(blankup2$year, blankup2$season)

# Create date formatted by posixct
blankup2$realdate <- as.POSIXct(paste0(blankup2$date, "-", blankup2$year),
                                format="%m/%d/%Y")
for(i in 1:nrow(blankup2)){
  if(is.na(blankup2$realdate[i])){
    blankup2$realdate[i] <- as.POSIXct(blankup2$date[i],
                                       format="%d-%b-%Y")
  }
}

# Set month
blankup2$month <- month(blankup2$realdate)

# Check numbers by month
table(blankup2$year, blankup2$month)

# Save results -- alter in excel
write.csv(blankup2, row.names = F, 'btstows_newformat.csv')

# Start fresh
rm(list=ls())
btsnew <- read.csv('btstows_newformat.csv')
head(btsnew)
btsnew$station <- str_pad(btsnew$station, 3, 'left', '0')
btsnew$HAUL_ID2 <- paste0(btsnew$year, "_",
                          toupper(btsnew$season), "_",
                          btsnew$station)
btsnew <- dplyr::select(btsnew, 
                        HAUL_ID2, setlat, setlon, endlat, endlon)
head(btsnew)
bts <- btsnew

btsnew$setlat <- gsub("‐", "-", bts$setlat)
btsnew$setlon <- gsub("‐", "-", bts$setlon)
btsnew$endlat <- gsub("‐", "-", bts$endlat)
btsnew$endlon <- gsub("‐", "-", bts$endlon)

btsnew$setlat <- as.numeric(btsnew$setlat)
btsnew$endlat <- as.numeric(btsnew$endlat)
btsnew$setlon <- as.numeric(btsnew$setlon)
btsnew$endlon <- as.numeric(btsnew$endlon)

bts <- btsnew[complete.cases(btsnew),]

summary(bts)

for(i in 1:nrow(bts)){
  # Set lat
   if(bts$setlat[i] < 0){
     bts$real.setlat[i] <- bts$setlon[i]
   } 
  if(bts$setlat[i] > 0){
    bts$real.setlat[i] <- bts$setlat[i]
  }
  # End lat
  if(bts$endlat[i] < 0){
    bts$real.endlat[i] <- bts$endlon[i]
  } 
  if(bts$endlat[i] > 0){
    bts$real.endlat[i] <- bts$endlat[i]
  }
  # Set lon
  if(bts$setlon[i] > 0){
    bts$real.setlon[i] <- bts$setlat[i]
  } 
  if(bts$setlon[i] < 0){
    bts$real.setlon[i] <- bts$setlon[i]
  }
  # End lon
  if(bts$endlon[i] > 0){
    bts$real.endlon[i] <- bts$endlat[i]
  } 
  if(bts$endlon[i] < 0){
    bts$real.endlon[i] <- bts$endlon[i]
  }
  
}

summary(bts)

bts <- dplyr::select(bts,
                     HAUL_ID2,
                     real.setlat, real.endlat,
                     real.setlon, real.endlon)
colnames(bts) <- c('HAUL_ID2', 'setlat', 'endlat', 'setlon', 'endlon')
rm(btsnew)

btsold <- bts
rm(bts)
# Load key to match new haul id names to old haul id names
btsidmatch <- read.csv('C:/Users/klankowicz/Documents/GitHub/VAST-earlyattempts/bts_idmatch.csv')
head(btsold)
head(btsidmatch)

# Match by haul id key
btsold <- merge(btsold, btsidmatch, by=c('HAUL_ID2'))

# Load survey info
survs <- read.csv(here('data/Dataframes/sciencecenter_wrug.csv'))
survs <- dplyr::select(survs, -FID_Rugosi, -STRATA_1, -pct70, -rugosity,
                       -OID, -FID_scienc)
head(survs)

# Strip to just one age category
survs <- subset(survs, AGEGROUP == 'Age0-2')

# Split to bottom trawl survey
survs <- subset(survs, SURVEY == 'NEFSC BTS')

# Stip to just years BLLS took place, old format
table(btsold$year)
survs <- subset(survs, YEAR %in% seq(2018, 2022))

# Strip to just WGOM
survs <- subset(survs, STOCK == 'WGOM')

# Strip to 9-box footprint
survs <- subset(survs, LON > -71 & LON < -69 &
                  LAT > 42 & LAT < 43.5)

# Merge possible (WGOM) locations and cruise report results
test <- merge(survs, btsold, by=c('HAUL_ID'))

# Save.
write.csv(test, row.names = F,
          'ending_latlon_bts_newformat.csv')
