#### Workspace setup ####
# Clear workspace
rm(list=ls())

# Load libraries
# IMPORTANT NOTE: VAST must be running >=V14, will not work with V13.
library(TMB)
library(units)
library(VAST)
library(here)
library(tidyverse)
library(beepr)
library(sf)
library(rgdal)
library(sp)
library(ggcorrplot)

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
surveys <- read.csv(here("data/Dataframes/Bio_Data_Agesep4.csv"))

# Remove data from 2022 (incomplete)
ex <- subset(surveys, YEAR < 2022)
ex$RESPONSE <- ceiling(ex$AGE_N)
#ex <- subset(ex, AGEGROUP == 'Age2-5' | AGEGROUP == 'Age5+')

# Check that there are no missing responses
nrow(ex[is.na(ex$RESPONSE)==TRUE,])
#nrow(ex[is.na(ex$Data_type)==TRUE,])

# Add environmental data
env <- readRDS(here("data/RData_Storage/agg_stn_all_OISST.RDS"))
env <- sfheaders::sf_to_df(env, fill=T)
env <- dplyr::select(env,
                     HAUL_ID, cobble_P, gravel_P, rock_P, mud_P, sand_P,
                     BATHY.DEPTH, oisst)
ex2 <- left_join(ex, env, by="HAUL_ID")

# Add annoying rugosity data
rugos <- readRDS(here("data/RData_Storage/agg_stn_all_OISST_agesep2.RDS"))
rugos <- sfheaders::sf_to_df(rugos, fill=T)
rugos <- dplyr::select(rugos, HAUL_ID, rugos)
rugos <- unique(rugos)

ex3 <- left_join(ex2, rugos, by="HAUL_ID")

ex4 <- subset(ex3, !is.na(ex3$rugos))
ex4$AREA_SWEPT[is.na(ex4$AREA_SWEPT)] <- 1
#ex4 <- subset(ex4, AGEGROUP == "Age0-2" | AGEGROUP == "Age2-5")
table(ex4$AGEGROUP)

head(ex4)

ex4$RESPONSE <- NA
ex4$RESPONSE[ex4$Data_type == "Count"] <- ex4$AGE_N[ex4$Data_type == "Count"]
ex4$RESPONSE[ex4$Data_type == "Biomass_KG"] <- ex4$AGE_KG[ex4$Data_type == "Biomass_KG"]

# Combine Ages 2-5 and Ages 5+
ex4.a25 <- subset(ex4, AGEGROUP == "Age2-5")
ex4.a5p <- subset(ex4, AGEGROUP == 'Age5+')
ex4.a5p <- dplyr::select(ex4.a5p,
                         HAUL_ID, AGE_N, AGE_KG, Data_type)
colnames(ex4.a5p) <- c('HAUL_ID', 'AGE_N_5P', 'AGE_KG_5P', 'Data_type')

ex4.older <- merge(ex4.a25, ex4.a5p, by=c('HAUL_ID', 'Data_type'))

ex4.older$RESPONSE[ex4.older$Data_type == 'Biomass_KG'] <- 
  ex4.older$AGE_KG[ex4.older$Data_type == 'Biomass_KG'] +
  ex4.older$AGE_KG_5P[ex4.older$Data_type == 'Biomass_KG']

ex4.older$RESPONSE[ex4.older$Data_type == 'Count'] <- 
  ex4.older$AGE_N[ex4.older$Data_type == 'Count'] +
  ex4.older$AGE_N_5P[ex4.older$Data_type == 'Count']
ex4.older$AGE_KG_5P <- NULL; ex4.older$AGE_N_5P <- NULL
ex4.older$AGE_N <- NA; ex4.older$AGE_KG <- NA
ex4.older$AGEGROUP <- 'Age2+'

ex4.younger <- subset(ex4, AGEGROUP == 'Age0-2' | AGEGROUP == 'Unknown')
ex4.younger$AGE_N <- NA; ex4.younger$AGE_KG <- NA

ex5 <- rbind(ex4.younger, ex4.older)

ex5 <- ex5[with(ex5, order(DATE, HAUL_ID, AGEGROUP)),]
row.names(ex5) <- NULL
head(ex5)

#### Finalize sampling data inputs ####
# Save sampling data
survs <- dplyr::select(ex5,
                       LON, LAT, AREA_SWEPT,
                       TIME, SURVEY, RESPONSE, 
                       AGEGROUP, Data_type)
#survs$COD_N <- as_units(survs$COD_N, 'counts')
survs$swept <- as_units(survs$AREA_SWEPT, 'km^2')
#survs$swept <- survs$AREA_SWEPT
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

survs$AGE <- as.numeric(factor(survs$AGEGROUP, levels=c(
  'Age0-2', 
  'Age2+', 
  #'Age5+',
  'Unknown'
  )
  )) - 1
# Age 2 - 5: 0
# Age 5+   : 1
table(survs$AGE)

survs <- dplyr::select(survs, LON, LAT, TIME, RESPONSE, 
                       AGE, 
                       vessel, swept, Data_type)
names(survs) <- c('Lon', 'Lat', 'Year', 'Response_variable', 
                  'Age', 
                  'vessel', 'swept', 'Data_type')
#survs$Response_variable <- as_units(survs$Response_variable, 'counts')
#table(survs$Year)
str(survs)

# Save covariates
covars <- dplyr::select(ex5,
                        LON, LAT, TIME, cobble_P, gravel_P,
                        mud_P, rock_P, sand_P, rugos, BATHY.DEPTH, oisst)
covars$BATHY.DEPTH[covars$BATHY.DEPTH < 0] <- 
  covars$BATHY.DEPTH[covars$BATHY.DEPTH < 0] * -1
names(covars) <- c('Lon', 'Lat', 'Year', names(covars)[4:11])
table(covars$Year)

# Test correlation
# Create correlation matrix
df_cormat <- dplyr::select(covars, BATHY.DEPTH, rugos, sand_P, rock_P, mud_P,
                           gravel_P, cobble_P, oisst)
model.matrix(~0+., data=df_cormat) %>%
  cor(use="all.obs", method="spearman") %>%
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=3)

# Rescale covariates to have mean 0 and SD 1 (author rec)
scaled.covars <- covars[,4:ncol(covars)] %>% 
  mutate(across(where(is.numeric), scale))
scaled.covars <- cbind(covars[,1:3], scaled.covars)
summary(scaled.covars)
scaled.covars <- data.frame(
  Lon         = as.numeric(scaled.covars$Lon),
  Lat         = as.numeric(scaled.covars$Lat),
  Year        = as.numeric(scaled.covars$Year),
  cobble_P    = as.numeric(scaled.covars$cobble_P),
  gravel_P    = as.numeric(scaled.covars$gravel_P),
  mud_P       = as.numeric(scaled.covars$mud_P),
  rock_P      = as.numeric(scaled.covars$rock_P),
  sand_P      = as.numeric(scaled.covars$sand_P),
  rugos       = as.numeric(scaled.covars$rugos),
  BATHY.DEPTH = as.numeric(scaled.covars$BATHY.DEPTH),
  oisst       = as.numeric(scaled.covars$oisst)
)
str(scaled.covars)

#### Make settings ####
user_region <- readRDS(here('data/RData_Storage/user_region_all.rds'))
user_region$STRATA <- 'All'
user_region$Id <- NULL; user_region$row <- NULL
user_region <- user_region[with(user_region, order(Lon, Lat)),]
row.names(user_region) <- NULL
head(user_region)
strata_use <- data.frame('STRATA' = c("All"))

# Remove intermediates
rm(covars, ex, ex2, surveys, surveys2, sus, badtab,
   surveys.list, i, `%notin%`, df_cormat, temp, temp.list, f, sacrifices,
   alllocs, badtab2, ex.f, ex5, shit, bads, checktab, j, reponses, responses)
rm(df, env, rugos)
gc()

setwd(here('data/RData_Storage'))
#save.image('shortcut_VAST_data2.RData')

#### Start from here ####
#m(list=ls())
gc()
library(VAST)
library(here)
library(tidyverse)
library(beepr)
setwd(here('data/RData_Storage'))
#save.image('shortcut_VAST_data.RData')
#load(here('data/RData_Storage/shortcut_VAST_data2.RData'))

setwd(here("VAST_runs/data_type_combine"))

#### Run model ####
# Subsample
#survs <- survs[survs$Year < 1993,]
#scaled.covars <- scaled.covars[scaled.covars$Year < 1993,]

#### Year subset for testing ####
#survs <- subset(survs, Year > 70)
#survs$Year <- as.numeric(as.factor(survs$Year)) -1

#scaled.covars <- subset(scaled.covars, Year > 70) 
#scaled.covars$Year <- as.integer(scaled.covars$Year)
#scaled.covars$Year <- as.integer(as.factor(scaled.covars$Year)) -1


#year.labs <-  c(seq(2017, 2021, 1), seq(2017, 2021, 1))
#year.labs <- year.labs[order(year.labs)]
#seas.labs <- rep(c('Spring', 'Fall'), 5)
#year.labs <- paste0(year.labs, " ", seas.labs)

# Set year labels
# year.labs <- c(seq(1982, 2021, 1), seq(1982, 2021, 1))
# year.labs <- year.labs[order(year.labs)]
# seas.labs <- rep(c('Spring', 'Fall'), 40)
# year.labs <- paste0(year.labs, " ", seas.labs)
# cat.labs <- c('Small', 'Medium', 'Large', 'Unknown')

# Make settings
settings <- make_settings(
  n_x = 200,
  purpose = "index2",
  Region = "User",
  fine_scale = TRUE,
  bias.correct = FALSE,
  knot_method = "grid"#,
  #max_cells = 5000
)

# Change `ObsModel` to indicate type of data for level of `e_i`
settings$ObsModel = cbind( c(14,14,14,
                             2,  2, 2), 1 )
catchability_data = survs[,'Data_type',drop=FALSE]
Q1_formula = ~ factor(Data_type)

survs$e_i <- paste0(survs$Age, "_", survs$Data_type)
survs$e_i <- factor(survs$e_i,
                    levels = c('0_Count', '1_Count', '2_Count',
                               '0_Biomass_KG', '1_Biomass_KG', '2_Biomass_KG'))

fit = fit_model( 
  # Adjust newton steps
    #newtonsteps = 0,

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
    e_i = as.numeric(survs[,'e_i'])-1,
  
  # Call catchability info
   Q1_formula = Q1_formula,
   catchability_data  = catchability_data,
  
  # Call covariate info
    X1_formula = ~ gravel_P, #+ cobble_P + mud_P + sand_P + # rock_P +
                   #BATHY.DEPTH + oisst + rugos,
    X2_formula = ~ gravel_P, #+ cobble_P + mud_P + sand_P + # rock_P +
                   #BATHY.DEPTH + oisst + rugos,
    covariate_data = scaled.covars,
  
  # Call spatial 
    input_grid=user_region,
  
  # Set naming conventions
    #category_names = cat.labs,
    #year_labels = year.labs,
  
  # Tell model to run
    run_model = TRUE,
    build_model = TRUE)

  
beep(8)

#### Plot results ####

save.image('strata_cats_7.RData')

plot( fit )
beep(8)

map_list = make_map_info(Region = fit$settings$Region, 
                         spatial_list = fit$spatial_list, 
                         Extrapolation_List = fit$extrapolation_list)

plot_clusters(fit=fit,
              var_name = "D_gct",
              transform_var = log,
              k=4,
              method='ward',
              year_labels = fit$year_labels,
              category_names = fit$category_names,
              map_list = map_list,
              working_dir = "C:/Users/klankowicz/Documents/GitHub/VAST-earlyattempts/",
              file_name = "Class-D_gct",
              file_name2 = "Class-D_gct-averages",
              replace_Inf_with_NA = TRUE,
              size_threshold = 1e+10,
              col = viridisLite::viridis,
              yaxis_log = TRUE
              
)
