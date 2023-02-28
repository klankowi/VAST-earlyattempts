### A quick demonstration of how to extract map quantities and
### plot them externally. Cole Monnahan | May 2021
rm(list=ls())

# Load libraries
library(VAST)                           # 3.8.0
library(ggplot2)                        # 2.10.0
library(dplyr)
library(tidyr)
library(here)
library(sf)
library(ggpattern)

# Load sediment grids
load(here('data/RData_Storage/sediment_grids.RData'))
sedgrid <- grid
rm(grid)

# Load coast
coast <- ecodata::coast
coast <- st_transform(coast, crs=st_crs(sedgrid))

# Load closed areas
closed.areas <- st_read(here("data/GIS/closed_areas_wgs.shp"))

# Set GGplot auto theme
theme_set(theme(plot.margin = unit(c(0,0,0,0), "cm"),
                panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', size=1, fill=NA),
                legend.title = element_text(size=12),
                legend.text = element_text(size=10),
                legend.background = element_blank(),
                legend.position = 'right', 
                axis.text.x=element_text(size=10),
                axis.text.y=element_text(size=10),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=13, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12),
                strip.text = element_text(size=10)))

sedgrid[,'Bottom Type'] <- as.factor(sedgrid$outcome)

closed.areas[,'Closed Areas'] <- as.factor(" ")

# Plot
submap <- ggplot(ecodata::coast)+
  geom_sf(data=sedgrid[!is.na(sedgrid$outcome),], 
          aes(fill=`Bottom Type`), 
          col=NA, alpha=0.8) +
  geom_sf(data=closed.areas, fill=NA, aes(col=`Closed Areas`))+
  scale_fill_manual(na.value = 'transparent',
                    values = c('#F8766D', '#00BA38', '#619CFF')) +
  scale_color_manual(na.value='transparent',
                     values='black')+
  geom_sf(fill='gray')+
  coord_sf(xlim=c(-116000 , 771000),
           ylim=c(4070000 ,4970000),
           crs="EPSG:32619")+
  theme(legend.position = 'right',
        plot.margin = margin(t=0.25, b=0.25, l=0, r=0, 'cm'))
submap

ggsave(submap,
       filename='C:/Users/klankowicz/Desktop/Substrate_Map.png',
       device='png')

