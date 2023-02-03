rm(list=ls())

library(tidyverse)

# Read in data
surv <- read.csv(paste0("C:/Users/klankowicz/Box/Katie Lankowicz/",
                        "Groundfish_Survey_Data_Mods/Collated_Data2/",
                        "Survey_Data.csv"))
surv <- subset(surv, INDEX_NAME != "DFO Trawl_5Z1-2_SUMMER")
surv <- subset(surv, YEAR >=1982)
surv <- subset(surv, SURVEY !='GSO Trawl')
surv.sub <- dplyr::select(surv, INDEX_NAME, HAUL_ID)

bioda <- read.csv(paste0("C:/Users/klankowicz/Box/Katie Lankowicz/",
                         "Groundfish_Survey_Data_Mods/Collated_Data2/",
                         "Bio_Data.csv"))
bioda <- subset(bioda, YEAR>=1982)
bioda <- subset(bioda, SURVEY !='GSO Trawl')

bioda <- left_join(bioda, surv.sub, by=c('HAUL_ID'))

rm(surv.sub)

all.surveys <- unique(bioda$SURVEY)

surv <- subset(surv, SURVEY %in% all.surveys)

surv.list <- split(surv, f=surv$SURVEY)

for(f in 1:length(unique(bioda$SURVEY))){
  surv.use <- all.surveys[f]
  bioda.use <- subset(bioda, SURVEY == surv.use)
  surv.use.df <- subset(surv, SURVEY == surv.use)
  
  # Age 0-2 cod
  surv.use.df.a0 <- surv.use.df
  surv.use.df.a0$AGE <- 'A0-2'
  for(s in 1:nrow(surv.use.df.a0)){
    surv.use.df.a0$COD_N[s] <- nrow(bioda.use[bioda.use$HAUL_ID == 
                                                surv.use.df$HAUL_ID[s] &
                                                bioda.use$LENGTH_CM <=25,])
  }
  
  # Age 2-5 cod
  surv.use.df.a1 <- surv.use.df
  surv.use.df.a1$AGE <- 'A2-5'
  for(s in 1:nrow(surv.use.df.a1)){
    surv.use.df.a1$COD_N[s] <- nrow(bioda.use[bioda.use$HAUL_ID == 
                                            surv.use.df$HAUL_ID[s] &
                                            bioda.use$LENGTH_CM > 25 &
                                            bioda.use$LENGTH_CM <=63,])
  }
  
  # Age 5+ cod
  surv.use.df.a2 <- surv.use.df
  surv.use.df.a2$AGE <- 'A5PLUS'
  for(s in 1:nrow(surv.use.df.a2)){
    surv.use.df.a2$COD_N[s] <- nrow(bioda.use[bioda.use$HAUL_ID == 
                                                surv.use.df$HAUL_ID[s] &
                                                bioda.use$LENGTH_CM >63,])
  }
  
  # Combine
  surv.age.df <- rbind(surv.use.df.a0, surv.use.df.a1, surv.use.df.a2)
  
  surv.age.df <- surv.age.df[with(surv.age.df, order(YEAR, HAUL_ID, AGE)),]
  row.names(surv.age.df) <- NULL
  
  surv.list[[f]] <- surv.age.df
}

surv.final <- do.call(rbind, surv.list)
surv.final <- surv.final[with(surv.final, order(SURVEY, HAUL_ID, AGE)),]
row.names(surv.final) <- NULL
write.csv(surv.final, 'Survey_Data_AgeSep.csv', row.names=F)
