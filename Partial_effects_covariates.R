rm(list=ls())

# Load libraries
library(VAST)
library(effects)
library(tidyverse)
library(here)
library(splines)
library(marginaleffects)


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

# Plot 1st linear predictor, but could use `transformation` to apply link function
quant = function(x) seq(min(x),max(x),length=21)
newdata = datagrid( newdata=data.frame(gravel_P    = fit$covariate_data[,'gravel_P',drop=FALSE],
                                       cobble_P    = fit$covariate_data[,'cobble_P', drop=FALSE],
                                       mud_P       = fit$covariate_data[,'mud_P', drop=FALSE],
                                       sand_P      = fit$covariate_data[,'sand_P',drop=FALSE],
                                       BATHY.DEPTH = fit$covariate_data[,'BATHY.DEPTH', drop=FALSE],
                                       oisst       = fit$covariate_data[,'oisst', drop=FALSE],
                                       rugos       = fit$covariate_data[,'rugos', drop=FALSE]),
                    gravel_P=quant,
                    cobble_P = quant,
                    mud_P = quant,
                    sand_P = quant,
                    BATHY.DEPTH = quant,
                    oisst = quant,
                    rugos = quant)
pred = predictions( fit, newdata=newdata, covariate="X1" )

library(ggplot2)
library(gridExtra)
ggplot( pred, aes(CPE, predicted)) +
  geom_line( aes(y=predicted), color="blue", size=1 ) +
  geom_ribbon( aes( x=CPE, ymin=conf.low, ymax=conf.high), fill=rgb(0,0,1,0.2) ) +
  facet_wrap(vars(category), scales="free", ncol=2) +
  labs(y="Predicted response")