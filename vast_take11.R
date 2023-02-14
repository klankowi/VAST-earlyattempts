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
library(beepr)

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

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

#### Add sample information and covars ####
# Load data
surveys <- readRDS(here("data/RData_storage/agg_stn_all_OISST_agesep2.rds"))
surveys.list <- split(surveys, f=surveys$AGEGROUP)

for(i in 1:length(surveys.list)){
  if(names(surveys.list)[i] == "Unknown"){
    blank <- subset(surveys, AGEGROUP == 'Age0-2')
    blank$AGE_N <- 0; blank$AGE_KG <- 0
    blank$Data_type <- 'Biomass_KG'
    blank$AGEGROUP <- 'Unknown'
    surveys.list[[i]] <- rbind(surveys.list[[i]], blank)
    
    rm(blank)
  }
  if(names(surveys.list)[i] !="Unknown"){
    blank <- subset(surveys, AGEGROUP == 'Unknown')
    blank$AGE_N <- 0; blank$AGE_KG <- 0
    blank$Data_type <- 'Biomass_KG'
    blank$AGEGROUP <- surveys.list[[i]]$AGEGROUP[1]
    surveys.list[[i]] <- rbind(surveys.list[[i]], blank)
    
    rm(blank)
  }
}

surveys <- do.call(rbind, surveys.list)
surveys <- surveys[with(surveys, order(SURVEY, DATE, HAUL_ID, AGEGROUP)),]
row.names(surveys) <- NULL
table(surveys$AGEGROUP)
head(surveys)

surveys <- unique(surveys)

surveys$month <- as.numeric(surveys$month)

surveys$TRUE_SEASON[surveys$month %in% c(3,  4,  5,  6, 7, 8)] <- 'A.SPRING'
surveys$TRUE_SEASON[surveys$month %in% c(9, 10, 11, 12, 1, 2)] <- 'B.FALL'

surveys$TRUE_SEASON <- factor(surveys$TRUE_SEASON, 
                              levels = c('A.SPRING', 
                                         'B.FALL'))

surveys$t <- paste0(surveys$YEAR, "_", surveys$TRUE_SEASON)
table(surveys$t); length(table(surveys$t))

# Convert to df
ex <- sfheaders::sf_to_df(surveys, fill=T)

ex$RESPONSE <- ex$AGE_KG
nrow(ex[is.na(ex$RESPONSE)==TRUE,])
for(i in 1:nrow(ex)){
  if(is.na(ex$RESPONSE[i])){
    ex$RESPONSE[i] <- ex$AGE_N[i]
  }
}
nrow(ex[is.na(ex$RESPONSE)==TRUE,])

# Make sure there aren't duplicates
ex$sfg_id <- NULL; ex$point_id <- NULL
ex2 <- unique(ex)

# # Remove junk that will not be needed
# ex <- ex %>% 
#   dplyr::select(HAUL_ID, YEAR, DATE, COD_N, AGE, SURVEY, cobble_P,
#                 gravel_P, mud_P, rock_P, sand_P, COND, BATHY.DEPTH, oisst,
#                 x, y)
# 
# # Assign season
# for(i in 1:nrow(ex)){
#   ex$SEASON[i] <- true_seasons(ex$DATE[i])
# }

# # Remove points with NA cod abundance (there should be none)
# ex2 <- subset(ex, is.na(COD_N)==FALSE)

# Rearrange by date
ex2 <- ex2[with(ex2, order(DATE, HAUL_ID, AGEGROUP)),]
row.names(ex2) <- NULL

# Remove missing rugosity values (as density covar, cannot have NAs)
ex3 <- ex2[is.na(ex2$rugos)==FALSE,]

# Convert season-year to time
ex3$t <- as.numeric(as.factor(ex3$t))
table(ex3$t)

#### Finalize sampling data inputs ####
# Save sampling data
survs <- dplyr::select(ex3,
                       x, y, YEAR, TRUE_SEASON, t, SURVEY, RESPONSE, 
                       AGEGROUP, Data_type)
#survs$COD_N <- as_units(survs$COD_N, 'counts')
survs$swept <- as_units(1, unitless)
survs$vessel <- as.numeric(as.factor(survs$SURVEY)) - 1
# vessel    survey
# 0         ASMFC Shrimp Trawl  
# 1         DFO Trawl  
# 2         GSO Trawl  
# 3         MADMF Industry  
# 4         MADMF Inshore Trawl  
# 5         ME-NH Inshore Trawl  
# 6         NEFSC BLLS   
# 7         NEFSC BTS 
# 8         RIDEM Trawl  
# 9         Sentinel   
# 10        SMAST Video Trawl   

survs$Data_type <- factor(survs$Data_type, levels=c("Count", "Biomass_KG"))

# survs$L_S_BIN <- paste0(survs$AGE, "_", survs$SEASON)
# survs$L_S_BIN <- factor(survs$L_S_BIN,
#                         levels = c("A0-2_WINTER", "A0-2_SPRING", "A0-2_SUMMER", "A0-2_FALL",
#                                    "A2-5_WINTER", "A2-5_SPRING", "A2-5_SUMMER", "A2-5_FALL",
#                                    "A5PLUS_WINTER", "A5PLUS_SPRING", "A5PLUS_SUMMER", "A5PLUS_FALL"),
#                         labels = c(0, 1, 2, 3,
#                                    4, 5, 6, 7,
#                                    8, 9, 10, 11))
# survs$L_S_BIN <- as.numeric(survs$L_S_BIN) -1
# AGE     SEASONS                       LEVELS
# A0-2    WINTER, SPRING, SUMMER, FALL  0, 1, 2, 30
# A2-5    WINTER, SPRING, SUMMER, FALL  4, 5, 6, 7
# A5PLUS  WINTER, SPRING, SUMMER, FALL  8, 9, 10, 11

survs$AGE <- as.numeric(as.factor(survs$AGEGROUP)) -1
# Age 0 - 2: 0
# Age 2 - 5: 1
# Age 5+   : 2
# Unknown  : 3

survs <- dplyr::select(survs, x, y, t, RESPONSE, AGE, vessel, swept, Data_type)
names(survs) <- c('Lon', 'Lat', 'Year', 'Response_variable', 
                  'Age', 'vessel', 'swept', "Data_type")
str(survs)
# 'data.frame':	173880 obs. of  8 variables:
#   $ Lon              : num  -71.4 -71.4 -71.4 -71.4 -71.4 ...
# $ Lat              : num  41.6 41.6 41.6 41.6 41.6 ...
# $ Year             : num  0 0 0 0 0 0 0 0 0 0 ...
# $ Response_variable: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Age              : num  0 1 2 3 0 1 2 3 0 1 ...
# $ vessel           : num  2 2 2 2 2 2 2 2 2 2 ...
# $ swept            : Units: [1] num  1 1 1 1 1 1 1 1 1 1 ...
# $ Data_type        : num  1 1 1 1 1 1 1 1 1 1 ...

# Save covariates
covars <- dplyr::select(ex3,
                        x, y, t, cobble_P, gravel_P,
                        mud_P, rock_P, sand_P, rugos, BATHY.DEPTH, oisst)
names(covars) <- c('Lon', 'Lat', 'Year', names(covars)[4:11])
covars$BATHY.DEPTH <- covars$BATHY.DEPTH * -1
table(covars$Year)
str(covars)
# 'data.frame':	173880 obs. of  11 variables:
#   $ Lon        : num  -71.4 -71.4 -71.4 -71.4 -71.4 ...
# $ Lat        : num  41.6 41.6 41.6 41.6 41.6 ...
# $ Year       : num  0 0 0 0 0 0 0 0 0 0 ...
# $ cobble_P   : num  0 0 0 0 0 0 0 0 0 0 ...
# $ gravel_P   : num  0.169 0.169 0.169 0.169 0.169 ...
# $ mud_P      : num  0.661 0.661 0.661 0.661 0.661 ...
# $ rock_P     : num  0 0 0 0 0 0 0 0 0 0 ...
# $ sand_P     : num  0.806 0.806 0.806 0.806 0.806 ...
# $ rugos      : num  0.393 0.393 0.393 0.393 0.393 ...
# $ BATHY.DEPTH: num  7.02 7.02 7.02 7.02 7.02 ...
# $ oisst      : num  5.37 5.37 5.37 5.37 3.17 ...

# Test correlation
# Create correlation matrix
# df_cormat <- dplyr::select(covars, BATHY.DEPTH, rugos, sand_P, rock_P, mud_P,
#                            gravel_P, cobble_P, oisst)
# model.matrix(~0+., data=df_cormat) %>%
#   cor(use="all.obs", method="spearman") %>%
#   ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=3)

# Rescale covariates to have mean 0 and SD 1 (author rec)
scaled.covars <- covars[,4:ncol(covars)] %>% 
  mutate(across(where(is.numeric), scale))
scaled.covars <- cbind(covars[,1:3], scaled.covars)
summary(scaled.covars)

#### Year subset for testing ####
#survs <- subset(survs, Year > 2001)
#scaled.covars <- subset(scaled.covars, Year > 2001) 
#survs <- subset(survs, LBIN ==1 | LBIN ==2)
#survs$LBIN <- survs$LBIN - 1

#### Make settings ####
user_region <- readRDS(here('data/RData_Storage/user_region_all.rds'))
user_region$STRATA <- 'All'
user_region$Id <- NULL; user_region$row <- NULL
user_region <- user_region[with(user_region, order(Lon, Lat)),]
row.names(user_region) <- NULL
head(user_region)
#strata_use <- data.frame('STRATA' = c("All", "EGOM", 'GBK', 'SNE', 'WGOM'))

# Remove intermediates
rm(covars, ex, ex2, ex3, surveys, surveys.list, i, `%notin%`, df_cormat)
gc()

setwd(here("VAST_runs/StrataDensCats_5"))
settings = make_settings( n_x = 200,
                          Region = "User",
                          purpose = "index2", 
                          bias.correct = FALSE,
                          knot_method = "grid"
                          #strata.limits = strata_use
                          )
settings$ObsModel = cbind( c(14,2), 1 )

#### Run model ####
fit = fit_model( 
  # Call settings
    settings = settings, 
    
  # Call survey data info
    Lat_i = survs[,'Lat'], 
    Lon_i = survs[,'Lon'], 
    t_i = survs[,'Year'], 
    b_i = survs[,'Response_variable'], 
    a_i = survs[,'swept'], 
    v_i = survs[,'vessel'],
    c_iz = survs[,'Age'],
    e_i = as.numeric(survs[,'Data_type'])-1,
  
  # Call catchability info
    Q1_formula = ~ factor(Data_type),
    catchability_data = survs[,'Data_type',drop=FALSE],
  
  # Call covariate info
    X1_formula = ~ gravel_P + cobble_P + mud_P + rock_P + sand_P + 
                   BATHY.DEPTH + 
                   oisst +
                   rugos,
    covariate_data = scaled.covars,
  
  # Call spatial 
    input_grid=user_region,
  
  # Tell model to run
    run_model = TRUE)
beep(8)

#### Plot results ####

save.image('strata_cats_5.RData')

plot( fit )
beep(8)

