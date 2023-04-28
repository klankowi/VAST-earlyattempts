# Personalized plotting using ggplot for various VAST default plots

# Prepare workspace
rm(list=ls())

# Load libraries
library(VAST)
library(tidyverse)
library(sf)
library(here)
library(ggpubr)

# Load functions
# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

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

# Load data
load(here('VAST_runs/refine_effort/refine_effort.RData'))
rm(list=setdiff(ls(), c("fit", "%notin%")))

# Create objects needed to plot
Sdreport = fit$parameter_estimates$SD
SD = TMB::summary.sdreport(Sdreport)
TmbData = fit$data_list

# Name where data are stored in report
CogName = "mean_Z_ctm"
EffectiveName = "effective_area_ctl"

# Set labels
category_names = c('Small', 'Medium', 'Large', 'Unknown') 

###############################################################################
####                       Plot center of gravity                          ####
###############################################################################

SD_mean_Z_ctm = array(NA, dim = c(unlist(TmbData[c("n_c", "n_t", "n_m")]), 2), 
                      dimnames = list(NULL, NULL, NULL,
                                      c("Estimate", "Std. Error")))

SD_mean_Z_ctm[] = SD[which(rownames(SD) == CogName), 
                           c("Estimate", "Std. Error")]
      
names(dim(SD_mean_Z_ctm)) <- c('Category', 'Time', 'Dimension', 'Est.Err')
      
SD_mean_Z_ctm_spring <- SD_mean_Z_ctm[,seq(1,79,2),,]
SD_mean_Z_ctm_fall <- SD_mean_Z_ctm[,seq(2,80,2),,]

season.list.cog <- list(SD_mean_Z_ctm_spring, SD_mean_Z_ctm_fall)
names(season.list.cog) <- c('Spring', 'Fall')      

# for(i in 1:length(season.list.cog)){
#   for(j in 1:dim(season.list.cog[[i]])[1]){ # Number of categories
#     SD_plotting <- season.list.cog[[i]][j,,,]
#     dim(SD_plotting)
#     SD_plotting <- as.data.frame(SD_plotting[,,])
#     colnames(SD_plotting) <- c('easting', 'northing', 'e.sd', 'n.sd')
#     SD_plotting$Year <- seq(1982, 2021)
#     
#     northing <- ggplot(SD_plotting) +
#       geom_line(aes(x=Year, y=northing), col='red', lwd=1) +
#       geom_ribbon(aes(ymin=northing-n.sd,
#                       ymax=northing+n.sd,
#                       x=Year),
#                   fill=alpha('red', 0.2)) +
#       ylim(c(4600,4825)) +
#       ylab("Northing (km)")
#     
#     easting <- ggplot(SD_plotting) +
#       geom_line(aes(x=Year, y=easting), col='red', lwd=1) +
#       geom_ribbon(aes(ymin=easting-e.sd,
#                       ymax=easting+e.sd,
#                       x=Year),
#                   fill=alpha('red', 0.2)) +
#       ylim(c(300, 700)) +
#       ylab("Easting (km)")
#     
#     plotar <- ggarrange(northing, easting, nrow=2)
#     plot(annotate_figure(plotar, top = text_grob(paste0(category_names[j],
#                                                    " size class, ",
#                                                    names(season.list.cog[i])),
#                                             color = "black", 
#                                             face = "bold", size = 14)))
#   }
# }

###############################################################################
####                     Plot effective area occupied                      ####
###############################################################################

SD_effective_area_ctl = 
  SD_log_effective_area_ctl = array(NA, 
                                    dim = c(unlist(
                                      TmbData[c("n_c", 
                                                "n_t", 
                                                "n_l")]),2), 
                                    dimnames = list(NULL, 
                                                    NULL, 
                                                    NULL, 
                                                    c("Estimate", 
                                                      "Std. Error")))

SD_effective_area_ctl[] = SD[which(rownames(SD) == 
                                           EffectiveName), 
                                   c("Estimate", "Std. Error")]

names(dim(SD_effective_area_ctl)) <- c('Category', 
                                       'Time', 
                                       'Dimension', 
                                       'Est.Err')

SD_effective_area_ctl_spring <- SD_effective_area_ctl[,seq(1,79,2),,]
SD_effective_area_ctl_fall <- SD_effective_area_ctl[,seq(2,80,2),,]

season.list.eao <- list(SD_effective_area_ctl_spring, SD_effective_area_ctl_fall)
names(season.list.eao) <- c('Spring', 'Fall')      

# for(i in 1:length(season.list.eao)){
#   for(j in 1:dim(season.list.eao[[i]])[1]){ # Number of categories
#     SD_plotting <- season.list.eao[[i]][j,,]
#     dim(SD_plotting)
#     SD_plotting <- as.data.frame(SD_plotting[,])
#     colnames(SD_plotting) <- c('area.occ', 'sd.err')
#     SD_plotting$Year <- seq(1982, 2021)
#     
#     arr.occ <- ggplot(SD_plotting) +
#       geom_line(aes(x=Year, y=area.occ), col='red', lwd=1) +
#       geom_ribbon(aes(ymin=area.occ-sd.err,
#                       ymax=area.occ+sd.err,
#                       x=Year),
#                   fill=alpha('red', 0.2)) +
#       ylab("Area occupied (sq. km.)")
#     
#     plot(annotate_figure(arr.occ, top = text_grob(paste0(category_names[j],
#                                                         " size class, ",
#                                                         names(season.list.eao[i])),
#                                                  color = "black", 
#                                                  face = "bold", size = 14)))
#   }
# }

###############################################################################
####                         EAO and COG together                          ####
###############################################################################

for(i in 1:length(season.list.cog)){
  for(j in 1:dim(season.list.cog[[i]])[1]){ # Number of categories
    # COG
    SD_plotting.cog <- season.list.cog[[i]][j,,,]
    SD_plotting.cog <- as.data.frame(SD_plotting.cog[,,])
    colnames(SD_plotting.cog) <- c('easting', 'northing', 'e.sd', 'n.sd')
    SD_plotting.cog$Year <- seq(1982, 2021)
    
    # EAO
    SD_plotting.eao <- season.list.eao[[i]][j,,]
    SD_plotting.eao <- as.data.frame(SD_plotting.eao[,])
    colnames(SD_plotting.eao) <- c('area.occ', 'sd.err')
    SD_plotting.eao$Year <- seq(1982, 2021)
    
    # Both
    SD_plotting <- merge(SD_plotting.cog, SD_plotting.eao, by=c("Year"))
    
    
    northing <- ggplot(SD_plotting) +
      geom_line(aes(x=Year, y=northing), col='red', lwd=1) +
      geom_ribbon(aes(ymin=northing-n.sd,
                      ymax=northing+n.sd,
                      x=Year),
                  fill=alpha('red', 0.2)) +
      ylim(c(4600,4825)) +
      ylab("Northing (km)") +
      xlab("")
    
    easting <- ggplot(SD_plotting) +
      geom_line(aes(x=Year, y=easting), col='red', lwd=1) +
      geom_ribbon(aes(ymin=easting-e.sd,
                      ymax=easting+e.sd,
                      x=Year),
                  fill=alpha('red', 0.2)) +
      ylim(c(300, 700)) +
      ylab("Easting (km)")+
      xlab("")
    
    arr.occ <- ggplot(SD_plotting) +
      geom_line(aes(x=Year, y=area.occ), col='red', lwd=1) +
      geom_ribbon(aes(ymin=area.occ-sd.err,
                      ymax=area.occ+sd.err,
                      x=Year),
                  fill=alpha('red', 0.2)) +
      ylab("Area occupied (sq. km.)")
    
    
    
    plotar <- ggarrange(northing, easting, arr.occ, nrow=3)
    plot(annotate_figure(plotar, top = text_grob(paste0(category_names[j],
                                                        " size class, ",
                                                        names(season.list.cog[i])),
                                                 color = "black", 
                                                 face = "bold", size = 14)))
  }
}

###############################################################################
####                            Visualize COG                              ####
###############################################################################
coast <- ecodata::coast
coast <- st_transform(coast, "EPSG:32619")

stocks <- st_read(here('Data/GIS/codstox.shp'))
stocks <- st_transform(stocks, "EPSG:32619")
new_bb <- st_bbox(stocks)


# Check base
ggplot() +
  geom_sf(data=coast, fill='gray') +
  geom_sf(data=stocks, aes(col=STOCK), fill=NA, lwd=0.25) +
  coord_sf(xlim=c(new_bb[1], new_bb[3]),
           ylim=c(new_bb[2], new_bb[4])) +
  xlab("Longitude") + ylab("Latitude")

for(i in 1:length(season.list.cog)){
  for(j in 1:dim(season.list.cog[[i]])[1]){ # Number of categories
    # COG
    SD_plotting.cog <- season.list.cog[[i]][j,,,]
    SD_plotting.cog <- as.data.frame(SD_plotting.cog[,,])
    colnames(SD_plotting.cog) <- c('easting', 'northing', 'e.sd', 'n.sd')
    SD_plotting.cog$Year <- seq(1982, 2021)
    SD_plotting.cog$easting <- SD_plotting.cog$easting * 1000
    SD_plotting.cog$northing <- SD_plotting.cog$northing * 1000
    
    SD_plotting <- st_as_sf(SD_plotting.cog, coords=c("easting", "northing"))
    st_crs(SD_plotting) <- "EPSG:32619"
    
    points <- st_cast(st_geometry(SD_plotting), "POINT") 
    
    # Number of total linestrings to be created
    n <- length(points) - 1
    
    # Build linestrings
    linestrings <- lapply(X = 1:n, FUN = function(x) {
      
      pair <- st_combine(c(points[x], points[x + 1]))
      line <- st_cast(pair, "LINESTRING")
      return(line)
      
    })
    mst <- st_multilinestring(do.call("rbind", linestrings))
    mst <-  nngeo::st_segments(mst)
    mst <- st_sf(mst)
    mst$Year <- seq(1982, 2020, 1)
    st_crs(mst) <- "EPSG:32619"
    
    plotar <- ggplot() +
      geom_sf(data=coast, fill='gray') +
      #geom_sf(data=stocks, aes(col=STOCK), fill=NA, lwd=0.25) +
      geom_sf(data=SD_plotting, aes(col=(Year)), pch=19, cex=0.5) +
      geom_sf(data=mst, aes(col=Year)) +
      coord_sf(xlim=c(new_bb[1], new_bb[3]),
               ylim=c(new_bb[2], new_bb[4])) +
      xlab("Longitude") + ylab("Latitude")
  
    plot(annotate_figure(plotar, top = text_grob(paste0(category_names[j],
                                                        " size class, ",
                                                        names(season.list.cog[i])),
                                                 color = "black", 
                                                 face = "bold", size = 14)))
  }
}
