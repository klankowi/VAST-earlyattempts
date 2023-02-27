rm(list=ls())

# Load libraries
library(VAST)
library(effects)
library(tidyverse)
library(here)

# Set GGplot auto theme
theme_set(theme(plot.margin = unit(c(0,0,0,0), "cm"),
                panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', size=1, fill=NA),
                legend.title = element_text(size=12),
                legend.text = element_text(size=10),
                legend.background = element_blank(),
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=16, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

# VAST Output
load(here("VAST_runs/StrataDensCats_7/strata_cats_7.RData"))

# Must add data-frames to global environment (hope to fix in future)
covariate_data_full = fit$effects$covariate_data_full
catchability_data_full = fit$effects$catchability_data_full

# Plot 1st linear predictor, but could use `transformation` to apply link function
pred = Effect.fit_model( mod=fit,
                         focal.predictors = c("gravel_P", 'cobble_P', 'mud_P', 'sand_P', 'BATHY.DEPTH', 'oisst', 'rugos'),
                         which_formula = "X1",
                         xlevels = 100,
                         transformation = list(link=identity, inverse=identity) )
plot(pred)