# Download release number 3.6.0; its useful for reproducibility to use a specific release number
#devtools::install_github("James-Thorson-NOAA/VAST", ref="3.6.0")

rm(list=ls())

# Load packages
library(VAST)
library(tidyverse)

# load data set
# see `?load_example` for list of stocks with example data
# that are installed automatically with `FishStatsUtils`.
example = load_example( data_set="multimodal_red_snapper" )
example$sampling_data <- subset(example$sampling_data,
                                Data_type != "Encounter")
example$sampling_data <- droplevels(example$sampling_data)
temp <- example$sampling_data
temp.1 <- temp %>% 
  mutate(Age = 0)
temp.2 <- temp %>% 
  mutate(Age = 1)
# temp.3 <- temp %>% 
#   mutate(Age = 2)
# temp.4 <- temp %>% 
#   mutate(Age = 3)

temp.2$Response_variable <- sample(temp.2$Response_variable,
                                    length(temp.2$Response_variable),
                                    replace = F)
# temp.3$Response_variable <- sample(temp.3$Response_variable,
#                                    length(temp.3$Response_variable),
#                                    replace = F)
# temp.4$Response_variable <- sample(temp.4$Response_variable,
#                                    length(temp.4$Response_variable),
#                                    replace = F)

example$sampling_data <- rbind(temp.1, temp.2)#, temp.3)#, temp.4)

# Make settings
settings = make_settings( n_x = 500,
                          Region = example$Region,
                          purpose = "index2",
                          strata.limits = example$strata.limits )

# Change `ObsModel` to indicate type of data for level of `e_i`
settings$ObsModel = cbind( c(14,2), 1 )

# Add a design matrix representing differences in catchability relative to a reference (biomass-sampling) gear
catchability_data = example$sampling_data[,'Data_type',drop=FALSE]
Q1_formula = ~ factor(Data_type)

# Add density covariate
# Pull out just one set of observations (Age 0 and Age 1 duplicate space-time)
covars <-  example$sampling_data[example$sampling_data$Age == 0,]
covars <- dplyr::select(covars, Lon, Lat, Year, Response_variable)
covars$fake1 <- covars$Response_variable * abs(rnorm(n=nrow(covars)))
covars$fake2 <- covars$Response_variable * abs(rnorm(n=nrow(covars)))
covars$fake3 <- covars$Response_variable * abs(rnorm(n=nrow(covars)))
covars$fake4 <- covars$Response_variable * abs(rnorm(n=nrow(covars)))
covars$fake5 <- covars$Response_variable * abs(rnorm(n=nrow(covars)))
covars$fake6 <- covars$Response_variable * abs(rnorm(n=nrow(covars)))
covars$fake7 <- covars$Response_variable * abs(rnorm(n=nrow(covars)))
covars$fake8 <- covars$Response_variable * abs(rnorm(n=nrow(covars)))

scaled.covars <- scale(covars[,5:ncol(covars)])
scaled.covars <- as.data.frame(cbind(covars[,1:3], scaled.covars))

setwd("C:/Users/klankowicz/Desktop/VAST_examples/catchability_combine")

rm(covars, multimodal_red_snapper_example, temp, temp.1, temp.2)
gc()


# Run model
fit = fit_model( 
  # Call settings
    settings = settings,
    
  # Call survey data info
    Lat_i = example$sampling_data[,'Lat'],
    Lon_i = example$sampling_data[,'Lon'],
    t_i = example$sampling_data[,'Year'],
    b_i = example$sampling_data[,'Response_variable'],
    a_i = example$sampling_data[,'AreaSwept_km2'],
    #v_i = example$sampling_data[,'vessel'],
    c_i = example$sampling_data[,'Age'],
    e_i = as.numeric(example$sampling_data[,'Data_type'])-1,
  
  # Call catchability info
    Q1_formula = Q1_formula,
    catchability_data = catchability_data,
  
  # Call covariate info
    X1_formula = ~fake1 + fake2 + fake3 + fake4 + fake5 + fake6 + fake7 + fake8,
    covariate_data = scaled.covars
                 )

# Plot results
plot( fit )