# There are several ways to describe fish: age, weight, and length. If/ which
# descriptors are provided varies by survey. There are also, predictably, a 
# few observations that have missing data due to data collector error or other
# at-sea reasons. We want to preserve  all data for inclusion in VAST, which can
# integrate encounter, count, and biomass data into a single index of abundance.
# This is possible, but requires some reshaping of the data. We will be able to
# separate and label data that include biomass, data that only include 
# abundance, and data that only reflect presence/ absence.

# We also want to split these data by age groupings, (Ages 0-2, Ages 2-5, 
# Ages 5+). We suspect these age groupings use different habitats, etc. 

# The following script reshapes the survey and bio data to create these branched
# groupings: Biomass data (Ages 0-2, Ages 2-5, Ages 5+), Abundance data (same
# age groupings), and Encounter data (same age groupings).

#### Set up workspace ####
rm(list=ls())

# Libraries and operators
library(tidyverse)
library(FSA)
library(car)
library(here)

'%notin%' <- function(x,y)!('%in%'(x,y))
source(here("utilities/true_seasons_func.R"))
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

#### Read in data ####
# Survey data
surv <- read.csv(here("data/Dataframes/Survey_Data.csv"))
surv <- subset(surv, YEAR >=1982)

# Bio data
bioda <- read.csv(here("data/Dataframes/Bio_Data.csv"))
bioda <- subset(bioda, YEAR>=1982)

# Append haul_id and date to bio data
surv.sub <- dplyr::select(surv, INDEX_NAME, HAUL_ID, DATE)
bioda <- left_join(bioda, surv.sub, by=c('HAUL_ID'))

# Assign season
bioda$DATE <- as.POSIXct(bioda$DATE, format="%m/%d/%Y %H:%M")
for(i in 1:nrow(bioda)){
  bioda$TRUE_SEASON[i] <- true_seasons(bioda$DATE[i])
}
bioda$SEASON <- NULL
# Combine NA and UKNOWN sex
bioda$SEX[bioda$SEX == "UNKNOWN"] <- NA
bioda$SEX <- droplevels(as.factor(bioda$SEX))

# Remove intermediates
rm(surv.sub, fall, i, spring, summer, winter)

#### von Bertalanffy growth curve ####
# 
# # All fish with recorded biodata have length. Weight and age are more variable. 
# # Step one of analysis is to fit a von Bert growth curve to determine age based
# # on length.
# 
# # Split out data with age and length
# bioage <- subset(bioda, is.na(bioda$AGE)==FALSE)
# summary(bioage$AGE); summary(bioage$LENGTH_CM)
# 
# # Call important variables
# wf15T <- bioage
# wf15T <- dplyr::select(wf15T, HAUL_ID, STOCK, YEAR, LENGTH_CM, WEIGHT, SEX,
#                        MATURITY_STAGE, AGE)
# names(wf15T) <- c("setID", "loc", "year", "tl", "w", "sex", "mat", "age")
# 
# # Determine age range
# agesum <- wf15T %>%
#   summarize(minage=min(age),maxage=max(age))
# 
# # Call function
# vb <- vbFuns(param="Typical")
# 
# # Determine length at age-0
# f.starts <- vbStarts(tl~age,data=wf15T)
# 
# # Fit initial growth curve
# f.fit <- nls(tl~vb(age,Linf,K,t0),data=wf15T,start=f.starts)
# 
# # Create function to fit length at age
# predict2 <- function(x) predict(x,data.frame(age=ages))
# 
# # Set age range (extend past min and max by a few years)
# ages <- seq(-1,20,by=0.2)
# 
# # Bootstrap confidence intervals
# f.boot2 <- Boot(f.fit, f=predict2)
# 
# # Predict lengths at ages for age interval 0.2 years
# preds1 <- data.frame(ages,
#                      predict(f.fit,data.frame(age=ages)),
#                      confint(f.boot2))
# names(preds1) <- c("age","fit","LCI","UCI")
# 
# # Filter to ages with observations
# preds2 <- filter(preds1,age>=agesum$minage,age<=agesum$maxage)
# nums <- seq(0, 17, 1)
# ints <- subset(preds2, preds2$age %in% nums)
# 
# # Plot
# ggplot() + 
#   geom_ribbon(data=preds2,aes(x=age,ymin=LCI,ymax=UCI),fill="gray90") +
#   geom_point(data=wf15T,aes(y=tl,x=age, col=sex),size=2,alpha=0.1) +
#   geom_line(data=preds2,aes(y=fit,x=age),size=1)
# 
# # Save length-based age cuts
# # These are all-encompassing: sex, location, decade, all of it.
# cuts2 <- round(ints$fit[ints$age ==2],2)
# cuts5 <- round(ints$fit[ints$age ==5],2)
cuts2 <- 39.17
cuts5 <- 70.16

# Remove intermediates
# rm(ints, nums, preds2, preds1, f.boot2, ages, predict2, f.fit, f.starts, vb,
#    agesum, wf15T, bioage, true_seasons)

#### Length at weight ####
age.bio <- subset(bioda, !is.na(bioda$LENGTH_CM) & 
                    !is.na(bioda$WEIGHT) & bioda$WEIGHT != 0)

# Plot for posterity
ggplot(data=age.bio, aes(x=LENGTH_CM, y=WEIGHT)) +
  geom_point()
# Some of these are biologically implausible and need to be removed.

# Create flag for implausible values
age.bio$FLAG <- 0

# Set flag for implausible values
age.bio$FLAG[age.bio$WEIGHT == 9.999] <- 1
age.bio$FLAG[age.bio$WEIGHT == 3.000 & age.bio$LENGTH_CM == 32] <- 1
age.bio$FLAG[age.bio$WEIGHT == 4.936 & age.bio$LENGTH_CM == 46] <- 1
age.bio$FLAG[age.bio$WEIGHT == 5.440 & age.bio$LENGTH_CM == 53] <- 1
age.bio$FLAG[age.bio$WEIGHT == 7.898 & age.bio$LENGTH_CM == 57] <- 1
age.bio$FLAG[age.bio$WEIGHT == 0.020 & age.bio$LENGTH_CM == 53] <- 1
age.bio$FLAG[age.bio$WEIGHT == 0.200 & age.bio$LENGTH_CM == 57] <- 1
age.bio$FLAG[age.bio$WEIGHT == 0.345 & age.bio$LENGTH_CM == 67] <- 1
age.bio$FLAG[age.bio$WEIGHT == 0.385 & age.bio$LENGTH_CM == 73] <- 1
age.bio$FLAG[age.bio$WEIGHT == 1.838 & age.bio$LENGTH_CM == 95] <- 1
age.bio$FLAG[age.bio$WEIGHT == 0.270 & age.bio$LENGTH_CM == 119] <- 1
age.bio$FLAG[age.bio$WEIGHT == 12.38 & age.bio$LENGTH_CM == 142] <- 1
age.bio$FLAG[age.bio$WEIGHT == 22.04 & age.bio$LENGTH_CM == 161] <- 1

# Identify bad biomass values
bad.biomass <- subset(age.bio, FLAG ==1)

# Remove bad biomass values
age.bio <- subset(age.bio, FLAG ==0)

# Replot
ggplot(data=age.bio, aes(x=LENGTH_CM, y=WEIGHT)) +
  geom_point()
# Remaining values look to be reasonable

# Linear regression of length at weight
age.bio$logL <- log(age.bio$LENGTH_CM)
age.bio$logW <- log(age.bio$WEIGHT)
lm1 <- lm(logW ~ logL, data=age.bio)

# Find correction factor
syx <- summary(lm1)$sigma
cf <- exp((syx^2)/2)

# Predict weight at length == age 2
pred2.log <- predict(lm1,data.frame(logL=log(cuts2)),interval="c")
bias2.pred.orig <- exp(pred2.log)
pred2.orig <- cf*bias2.pred.orig 
wts2 <- round(pred2.orig[1],2)

# Predict weight at length == age5
pred5.log <- predict(lm1,data.frame(logL=log(cuts5)),interval="c")
bias5.pred.orig <- exp(pred5.log)
pred5.orig <- cf*bias5.pred.orig 
wts5 <- round(pred5.orig[1],2)

rm(pred5.orig, bias5.pred.orig, pred5.log, pred2.orig, bias2.pred.orig,
   pred2.log, lm1, age.bio, bad.biomass, cf, syx)

#### Assign age by length ####
# Create new age grouping column
bioda$LENAGE <- NA

# Loop through rows
for(i in 1:nrow(bioda)){
  
  # Age 0-2 cutoff
  if(bioda$LENGTH_CM[i] <= cuts2){
    bioda$LENAGE[i] <- 0
  }
  # Ages 2-5
  if(bioda$LENGTH_CM[i] > cuts2 & bioda$LENGTH_CM[i] <=cuts5){
    bioda$LENAGE[i] <- 3
  }
  # Age 5 cutoff
  if(bioda$LENGTH_CM[i] > cuts5){
    bioda$LENAGE[i] <- 6
  }
}

# View results
table(bioda$LENAGE)
nrow(bioda[is.na(bioda$LENAGE)==TRUE,])

# Pull out data that has provided age
bioage <- subset(bioda, is.na(bioda$AGE)==FALSE)
# Reassign age for those data
bioage$LENAGE <- bioage$AGE
# Pull out data that does not have provided age
bioage.noage <- subset(bioda, is.na(bioda$AGE)==TRUE)
# Recombine to make full dataset
bioda <- rbind(bioage, bioage.noage)

# View results
table(bioda$LENAGE)

# Create new age grouping column
bioda$AGEGROUP <- NA

# Loop through rows
for(i in 1:nrow(bioda)){
  # Age 0-2 cutoff
  if(bioda$LENAGE[i] <= 2){
    bioda$AGEGROUP[i] <- 'Age0-2'
  }
  # Age 2-5
  if(bioda$LENAGE[i] > 2 & bioda$LENAGE[i] <=5){
    bioda$AGEGROUP[i] <- 'Age2-5'
  }
  # Age 5 cutoff
  if(bioda$LENAGE[i] > 5){
    bioda$AGEGROUP[i] <- 'Age5+'
  }
}
# View results
table(bioda$AGEGROUP)
nrow(bioda[is.na(bioda$AGEGROUP)==TRUE,])
head(bioda)

# Remove intermediates
rm(bioage, bioage.noage)

# Split by haul
bioda.list <- split(bioda, f=bioda$HAUL_ID)

# Loop through hauls
for(i in
    1:
    #8000:
    length(bioda.list)
    #10000
    ){
  # Pull out biodata associated with haul
  biotemp <- bioda.list[[i]]
  head(biotemp)
  
  # Pull out survey data associated with haul
  survtemp <- surv[surv$HAUL_ID == biotemp$HAUL_ID[1],]
  head(survtemp)
  
  # Create row for each eage group
  survtemp.all <- rbind(survtemp, survtemp, survtemp)
  survtemp.all$AGEGROUP <- c('Age0-2', 'Age2-5', 'Age5+')
  head(survtemp.all)
  
  # Assign cod_n
  survtemp.all$AGE_N[survtemp.all$AGEGROUP == "Age0-2"] <- 
    nrow(biotemp[biotemp$AGEGROUP == "Age0-2",])
  survtemp.all$AGE_N[survtemp.all$AGEGROUP == "Age2-5"] <- 
    nrow(biotemp[biotemp$AGEGROUP == "Age2-5",])
  survtemp.all$AGE_N[survtemp.all$AGEGROUP == "Age5+"] <- 
    nrow(biotemp[biotemp$AGEGROUP == "Age5+",])
  
  # Assign cod_kg
  survtemp.all$AGE_KG[survtemp.all$AGEGROUP == "Age0-2"] <- 
    sum(biotemp$WEIGHT[biotemp$AGEGROUP == "Age0-2"], na.rm=T)
  survtemp.all$AGE_KG[survtemp.all$AGEGROUP == "Age2-5"] <- 
    sum(biotemp$WEIGHT[biotemp$AGEGROUP == "Age2-5"], na.rm=T)
  survtemp.all$AGE_KG[survtemp.all$AGEGROUP == "Age5+"] <- 
    sum(biotemp$WEIGHT[biotemp$AGEGROUP == "Age5+"], na.rm=T)
  
  head(survtemp.all)
  
  # when only one age group has cod and AGE_KG==NA and COD_KG !=NA, assign 
  # AGE_KG as COD_KG.
  group.nums <- as.vector(survtemp.all$AGE_N)
  length.zeros <- length(group.nums[group.nums == 0])
  if(length.zeros == 2 & is.na(survtemp.all$COD_KG[1])==FALSE){
    survtemp.all$AGE_KG[survtemp.all$AGE_N != 0] <- survtemp.all$COD_KG[1]
  }  
  
  # Flag when COD_N != sum(AGE_N) plus/minus 1
  survtemp.all$FLAG_N <- 0
  approp.range <- c(survtemp.all$COD_N[1] - 1, 
                    survtemp.all$COD_N[1],
                    survtemp.all$COD_N[1] + 1)
  if(sum(survtemp.all$AGE_N) %notin% approp.range){
    survtemp.all$FLAG_N <- 1
  }
  
  # N differences
  survtemp.all$HAUL_DIF_N <- survtemp.all$COD_N[1] - sum(survtemp.all$AGE_N)
  
  # Flag when COD_KG != sum(AGE_KG)
  survtemp.all$FLAG_KG <- 0
  if(sum(survtemp.all$AGE_KG) != survtemp.all$COD_KG[1] &
     is.na(survtemp.all$COD_KG[1])==FALSE){
    survtemp.all$FLAG_KG <- 1
  }
  
  # KG differences
  survtemp.all$HAUL_DIF_KG <- NA
  if(is.na(survtemp.all$COD_KG[1])== FALSE){
    survtemp.all$HAUL_DIF_KG <- survtemp.all$COD_KG[1] - sum(survtemp.all$AGE_KG)
  }

  # Assign data type
  if(nrow(biotemp[is.na(biotemp$WEIGHT)==TRUE,]) == nrow(biotemp)){
    survtemp.all$Data_type <- 'Count'
  }
  if(nrow(biotemp[is.na(biotemp$WEIGHT)==TRUE,]) != nrow(biotemp)){
    survtemp.all$Data_type <- 'Biomass_KG'
  }
  if(is.na(survtemp.all$COD_KG[1])==FALSE & 
     sum(survtemp.all$AGE_KG) == survtemp.all$COD_KG[1]){
    survtemp.all$Data_type <- 'Biomass_KG'
  }
  
  bioda.list[[i]] <- survtemp.all
  
  rm(biotemp, survtemp, survtemp.all, group.nums, length.zeros, approp.range)
    
}

test <- do.call(rbind, bioda.list)
row.names(test) <- NULL
head(test)

#### Data with no cod ####
# Pull out surveys that do not have any fish reflected in bio data
enc <- surv[surv$HAUL_ID %notin% test$HAUL_ID,]
table(enc$SURVEY)

# Pull out surveys with no recorded fish
enc.0 <- subset(enc, enc$COD_N == 0)

# Clone encounter data for each of the three age groups
enc.0.age0 <- enc.0; enc.0.age0$AGEGROUP <- 'Age0-2'
enc.0.age1 <- enc.0; enc.0.age1$AGEGROUP <- 'Age2-5'
enc.0.age2 <- enc.0; enc.0.age2$AGEGROUP <- 'Age5+'

# Rebind
enc.0 <- rbind(enc.0.age0, enc.0.age1, enc.0.age2)

# Create columns in test but not enc
enc.0$AGE_N <- 0; enc.0$AGE_KG <- 0; enc.0$FLAG_N <- 0; enc.0$HAUL_DIF_N <- 0
enc.0$FLAG_KG <- 0; enc.0$HAUL_DIF_KG <- 0; enc.0$Data_type <- "Biomass_KG"

# Bind cod==0 data into final dataframe
test <- rbind(test, enc.0)

# Remove intermediates
rm(enc.0, enc.0.age0, enc.0.age1, enc.0.age2)

#### Assign age by weight (survey data, cod==1)####
enc <- enc[enc$HAUL_ID %notin% test$HAUL_ID,]
enc.n1 <- subset(enc, enc$COD_N == 1 & !is.na(enc$COD_KG))

# Split by haul
enc.n1.list <- split(enc.n1, f=enc.n1$HAUL_ID)

# Loop through hauls
for(i in 1:length(enc.n1.list)){
  
  # Pull out biodata associated with haul
  biotemp <- enc.n1.list[[i]]
  head(biotemp)
  
  # Pull out survey data associated with haul
  survtemp <- surv[surv$HAUL_ID == biotemp$HAUL_ID[1],]
  head(survtemp)
  
  # Create row for each eage group
  survtemp.all <- rbind(survtemp, survtemp, survtemp)
  survtemp.all$AGEGROUP <- c('Age0-2', 'Age2-5', 'Age5+')
  head(survtemp.all)
  
  # Assign cod_n and cod_kg
  if(biotemp$COD_KG <=wts2){
    survtemp.all$AGE_N[survtemp.all$AGEGROUP == "Age0-2"] <- 1
    survtemp.all$AGE_KG[survtemp.all$AGEGROUP == "Age0-2"] <- biotemp$COD_KG
    survtemp.all$AGE_N[survtemp.all$AGEGROUP == "Age2-5"] <- 0
    survtemp.all$AGE_KG[survtemp.all$AGEGROUP == "Age2-5"] <- 0
    survtemp.all$AGE_N[survtemp.all$AGEGROUP == "Age5+"] <- 0
    survtemp.all$AGE_KG[survtemp.all$AGEGROUP == "Age5+"] <- 0
  }
  if(biotemp$COD_KG > wts2 & biotemp$COD_KG <=wts5){
    survtemp.all$AGE_N[survtemp.all$AGEGROUP == "Age2-5"] <- 1
    survtemp.all$AGE_KG[survtemp.all$AGEGROUP == "Age2-5"] <- biotemp$COD_KG
    survtemp.all$AGE_N[survtemp.all$AGEGROUP == "Age0-2"] <- 0
    survtemp.all$AGE_KG[survtemp.all$AGEGROUP == "Age0-2"] <- 0
    survtemp.all$AGE_N[survtemp.all$AGEGROUP == "Age5+"] <- 0
    survtemp.all$AGE_KG[survtemp.all$AGEGROUP == "Age5+"] <- 0
  }
  if(biotemp$COD_KG  > wts5){
    survtemp.all$AGE_N[survtemp.all$AGEGROUP == "Age5+"] <- 1
    survtemp.all$AGE_KG[survtemp.all$AGEGROUP == "Age5+"] <- biotemp$COD_KG
    survtemp.all$AGE_N[survtemp.all$AGEGROUP == "Age2-5"] <- 0
    survtemp.all$AGE_KG[survtemp.all$AGEGROUP == "Age2-5"] <- 0
    survtemp.all$AGE_N[survtemp.all$AGEGROUP == "Age0-2"] <- 0
    survtemp.all$AGE_KG[survtemp.all$AGEGROUP == "Age0-2"] <- 0
  }
  
  head(survtemp.all)
  
  # Flag when COD_N != sum(AGE_N) plus/minus 1
  survtemp.all$FLAG_N <- 0
  approp.range <- c(survtemp.all$COD_N[1] - 1, 
                    survtemp.all$COD_N[1],
                    survtemp.all$COD_N[1] + 1)
  if(sum(survtemp.all$AGE_N) %notin% approp.range){
    survtemp.all$FLAG_N <- 1
  }
  
  # N differences
  survtemp.all$HAUL_DIF_N <- survtemp.all$COD_N[1] - sum(survtemp.all$AGE_N)
  
  # Flag when COD_KG != sum(AGE_KG)
  survtemp.all$FLAG_KG <- 0
  if(sum(survtemp.all$AGE_KG) != survtemp.all$COD_KG[1] & is.na(survtemp.all$COD_KG[1])==FALSE){
    survtemp.all$FLAG_KG <- 1
  }
  
  # KG differences
  survtemp.all$HAUL_DIF_KG <- NA
  if(is.na(survtemp.all$COD_KG[1])== FALSE){
    survtemp.all$HAUL_DIF_KG <- survtemp.all$COD_KG[1] - sum(survtemp.all$AGE_KG)
  }
  
  # Assign data type
  if(nrow(biotemp[is.na(biotemp$WEIGHT)==TRUE,]) == nrow(biotemp)){
    survtemp.all$Data_type <- 'Count'
  }
  if(nrow(biotemp[is.na(biotemp$WEIGHT)==TRUE,]) != nrow(biotemp)){
    survtemp.all$Data_type <- 'Biomass_KG'
  }
  if(is.na(survtemp.all$COD_KG[1])==FALSE & sum(survtemp.all$AGE_KG) == survtemp.all$COD_KG[1]){
    survtemp.all$Data_type <- 'Biomass_KG'
  }
  
  enc.n1.list[[i]] <- survtemp.all
  
  rm(biotemp, survtemp, survtemp.all,  approp.range)
  
}

# Bind results into dataframe
test2 <- do.call(rbind, enc.n1.list)
rownames(test2) <- NULL

# Bind results into final dataframe
test <- rbind(test, test2)

# Remove intermediates
rm(enc.n1, enc.n1.list, test2)

#### Leftovers ####
# Surveys that did not record total biomass
enc <- enc[enc$HAUL_ID %notin% test$HAUL_ID,]

# Surveys that have recorded biomass
enc.unk <- enc[!is.na(enc$COD_KG) & enc$COD_N !=0 & enc$COD_N !=1,]

# Biomass of all caught fish is less than fitted weight at age==2
enc.small <- subset(enc.unk, enc.unk$COD_KG <= wts2)

# Split by haul
enc.small.list <- split(enc.small, f=enc.small$HAUL_ID)

# Loop through haul
for(i in 1:length(enc.small.list)){
  # Pull out biodata associated with haul
  biotemp <- enc.small.list[[i]]
  head(biotemp)
  
  # Pull out survey data associated with haul
  survtemp <- surv[surv$HAUL_ID == biotemp$HAUL_ID[1],]
  head(survtemp)
  
  # Create row for each eage group
  survtemp.all <- rbind(survtemp, survtemp, survtemp)
  survtemp.all$AGEGROUP <- c('Age0-2', 'Age2-5', 'Age5+')
  head(survtemp.all)
  
  # Assign cod_n and cod_kg
  if(biotemp$COD_KG <=wts2){
    survtemp.all$AGE_N[survtemp.all$AGEGROUP == "Age0-2"] <- survtemp$COD_N
    survtemp.all$AGE_KG[survtemp.all$AGEGROUP == "Age0-2"] <- biotemp$COD_KG
    survtemp.all$AGE_N[survtemp.all$AGEGROUP == "Age2-5"] <- 0
    survtemp.all$AGE_KG[survtemp.all$AGEGROUP == "Age2-5"] <- 0
    survtemp.all$AGE_N[survtemp.all$AGEGROUP == "Age5+"] <- 0
    survtemp.all$AGE_KG[survtemp.all$AGEGROUP == "Age5+"] <- 0
  }
  
  head(survtemp.all)
  
  # Flag when COD_N != sum(AGE_N) plus/minus 1
  survtemp.all$FLAG_N <- 0
  approp.range <- c(survtemp.all$COD_N[1] - 1, 
                    survtemp.all$COD_N[1],
                    survtemp.all$COD_N[1] + 1)
  if(sum(survtemp.all$AGE_N) %notin% approp.range){
    survtemp.all$FLAG_N <- 1
  }
  
  # N differences
  survtemp.all$HAUL_DIF_N <- survtemp.all$COD_N[1] - sum(survtemp.all$AGE_N)
  
  # Flag when COD_KG != sum(AGE_KG)
  survtemp.all$FLAG_KG <- 0
  if(sum(survtemp.all$AGE_KG) != survtemp.all$COD_KG[1] &
     is.na(survtemp.all$COD_KG[1])==FALSE){
    survtemp.all$FLAG_KG <- 1
  }
  
  # KG differences
  survtemp.all$HAUL_DIF_KG <- NA
  if(is.na(survtemp.all$COD_KG[1])== FALSE){
    survtemp.all$HAUL_DIF_KG <- survtemp.all$COD_KG[1] - sum(survtemp.all$AGE_KG)
  }
  
  # Assign data type
  if(nrow(biotemp[is.na(biotemp$WEIGHT)==TRUE,]) == nrow(biotemp)){
    survtemp.all$Data_type <- 'Count'
  }
  if(nrow(biotemp[is.na(biotemp$WEIGHT)==TRUE,]) != nrow(biotemp)){
    survtemp.all$Data_type <- 'Biomass_KG'
  }
  if(is.na(survtemp.all$COD_KG[1])==FALSE & 
     sum(survtemp.all$AGE_KG) == survtemp.all$COD_KG[1]){
    survtemp.all$Data_type <- 'Biomass_KG'
  }
  
  enc.small.list[[i]] <- survtemp.all
  
  rm(biotemp, survtemp, survtemp.all,  approp.range)
}
# Rebind to dataframe
test2 <- do.call(rbind, enc.small.list)
row.names(test2) <- NULL

# Bind into final dataframe
test <- rbind(test, test2)

# Remove intermediates
rm(enc.small, enc.small.list, enc.unk, test2)

# What is left?
enc <- enc[enc$HAUL_ID %notin% test$HAUL_ID,]
enc.big <- subset(enc, COD_KG > wts2 & COD_N >1 & !is.na(COD_KG))
enc.na <- subset(enc, is.na(COD_KG))

leftovers <- rbind(enc.na, enc.big)
nrow(leftovers)
sum(leftovers$COD_N)
# There are 2198 tows, representing 214165 fish, that cannot be assigned 
# age by any method.

# Remove intermediates
rm(bioda.list, enc, enc.big, enc.na, enc.small, enc.small.list,
   enc.unk, sum.year, test2)

# REassign final dataframe columns
final.df <- test
head(final.df)
final.df <- dplyr::select(final.df, INDEX_NAME, SURVEY, STOCK, AREA, STRATUM,
                          SEASON, TRUE_SEASON, YEAR, DATE, HAUL_ID, LAT, LON,
                          DEPTH, SURFACE.TEMP, BOTTOM.TEMP, SALINITY, 
                          BOTTOM.TYPE, AGEGROUP, AGE_N, AGE_KG, Data_type)

# REassign leftover dataframe columns
head(leftovers)
leftovers$AGEGROUP <- 'Unknown'
leftovers$AGE_N <- leftovers$COD_N
leftovers$AGE_KG <- leftovers$COD_KG
leftovers <- dplyr::select(leftovers, INDEX_NAME, SURVEY, STOCK, AREA, STRATUM,
                           SEASON, TRUE_SEASON, YEAR, DATE, HAUL_ID, LAT, LON,
                           DEPTH, SURFACE.TEMP, BOTTOM.TEMP, SALINITY,
                           BOTTOM.TYPE, AGEGROUP, AGE_N, AGE_KG)

# Assign data type
leftovers$Data_type <- NA
leftovers$Data_type[is.na(leftovers$AGE_KG)] <- 'Count'
leftovers$Data_type[!is.na(leftovers$AGE_KG)] <- 'Biomass_KG'
table(leftovers$Data_type)

# Bind to ending dataframe
final.final <- rbind(final.df, leftovers)

# ORder by survey and date
final.final <- final.final[with(final.final, 
                                order(SURVEY, DATE)),]
row.names(final.final) <- NULL

# Check that all data are included
nrow(surv) == length(unique(final.final$HAUL_ID))

# Save.
write.csv(final.final,
          here("data/Dataframes/Bio_Data_Agesep.csv"),
          row.names=F)
