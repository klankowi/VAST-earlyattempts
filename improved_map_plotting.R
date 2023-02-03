### A quick demonstration of how to extract map quantities and
### plot them externally. Cole Monnahan | May 2021
rm(list=ls())

# Load libraries
library(VAST)                           # 3.8.0
library(ggplot2)                        # 2.10.0
library(dplyr)
library(tidyr)
library(here)

# Load data
load(here("VAST_runs/StrataDensCats_3/strata_cats_3.Rdata"))

# Set GGplot auto theme
theme_set(theme(panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', size=1, fill=NA),
                legend.title = element_text(size=16),
                legend.text = element_text(size=12),
                legend.background = element_blank(),
                axis.text.x=element_text(size=16),
                axis.text.y=element_text(size=16),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=18, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

# Pull vector of years
years <- unique(survs$Year)

# Remake map list locally for recreating plots
mdl <- make_map_info(Region = settings$Region,
                     spatial_list = fit$spatial_list,
                     Extrapolation_List = fit$extrapolation_list)


## Get the model estimate of density for each category and year;
# link it spatially to a lat/lon extrapolation point.

# Category 1
D_gt.1 <- fit$Report$D_gct[,1,] # drop the category
dimnames(D_gt.1) <- list(cell=1:nrow(D_gt.1), year=years)
D_gt.1 <- D_gt.1 %>% as.data.frame() %>%
  tibble::rownames_to_column(var = "cell") %>%
  pivot_longer(-cell, names_to = "Year", values_to='D')
D.1 <- merge(D_gt.1, mdl$PlotDF, by.x='cell', by.y='x2i')
D.1$Cat <- 1

# Category 2
D_gt.2 <- fit$Report$D_gct[,2,] # drop the category
dimnames(D_gt.2) <- list(cell=1:nrow(D_gt.2), year=years)
D_gt.2 <- D_gt.2 %>% as.data.frame() %>%
  tibble::rownames_to_column(var = "cell") %>%
  pivot_longer(-cell, names_to = "Year", values_to='D')
D.2 <- merge(D_gt.2, mdl$PlotDF, by.x='cell', by.y='x2i')
D.2$Cat <- 2

# Category 3
D_gt.3 <- fit$Report$D_gct[,3,] # drop the category
dimnames(D_gt.3) <- list(cell=1:nrow(D_gt.3), year=years)
D_gt.3 <- D_gt.3 %>% as.data.frame() %>%
  tibble::rownames_to_column(var = "cell") %>%
  pivot_longer(-cell, names_to = "Year", values_to='D')
D.3 <- merge(D_gt.3, mdl$PlotDF, by.x='cell', by.y='x2i')
D.3$Cat <- 3

# Rebind to new shape
D <- rbind(D.1, D.2, D.3)

# Adjust data to log abundance, stip units
D$D <- strip_units(D$D)
D$logD <- log(D$D)

# Check for outliers, remove
outliers <- boxplot.stats(D$logD)$out
D$D[D$logD %in% outliers] <- NA

#Rebind to list
D.list <- split(D, f=D$Cat)
names(D.list) <- c("Ages 0-2", "Ages 2-5", "Ages 5+")

# Set CRS 
projargs <- fit$extrapolation_list$projargs
CRS_orig = sp::CRS("+proj=longlat")
CRS_proj = sp::CRS(projargs)

# Outer loop: Categories
for(i in 1:length(D.list)){
  Cat.sub <- D.list[[i]]
  
  # Set min-max of Zlim for plotting
  min.D <- floor(round(min(Cat.sub$D, na.rm=T),1))
  max.D <- ceiling(round(max(Cat.sub$D, na.rm=T),1))
  
  Year.list <- split(Cat.sub, f=Cat.sub$Year)
  
  # Inner loop: Years
  for(j in 1:length(Year.list)
      #10 # for testing
      ){
    Year.sub <- Year.list[[j]]
    
    Year <- Year.sub$Year[1]
    
    loc_g <- cbind(Year.sub$Lon, Year.sub$Lat)
    
    n_cells <- dim(loc_g)[1]
    
    Points_orig = sp::SpatialPointsDataFrame(coords = loc_g, 
                                             data = data.frame(y = Year.sub$D), 
                                             proj4string = CRS_orig)
    
    Points_LongLat = sp::spTransform(Points_orig, sp::CRS("+proj=longlat"))
    
    Points_proj = sp::spTransform(Points_orig, CRS_proj)
    
    cell.size = mean(diff(Points_proj@bbox[1, ]), 
                     diff(Points_proj@bbox[2,]))/floor(sqrt(n_cells))
    Points_sf = sf::st_as_sf(Points_proj)
    grid = sf::st_make_grid(Points_sf, cellsize = cell.size)
    grid_i = sf::st_intersects(Points_sf, grid)
    grid = sf::st_sf(grid, y = tapply(Points_sf$y, 
                                      INDEX = factor(as.numeric(grid_i),
                                                     levels = 1:length(grid)), 
                                      FUN = mean, na.rm = TRUE))
    
    # Plot
    p <- ggplot(ecodata::coast)+
      geom_sf(data=grid, aes(fill=y, col=y)) +
      scale_color_viridis_c(limits=c(min.D, max.D),
                            na.value = 'transparent',
                            option=('rocket'),
                            direction = -1,
                            alpha = 0.8) +

      scale_fill_viridis_c(limits=c(min.D, max.D),
                            na.value = 'transparent',
                            option=('rocket'), 
                           direction = -1, 
                           alpha = 0.8) +
      
      geom_sf(fill='gray')+
      coord_sf(xlim=c(-76, -65),
               ylim=c(36,46),
               crs="EPSG:4326")+
      labs(title=paste0(names(D.list)[i], " distribution ", Year.sub$Year[1])) +
      theme(legend.position = c(0.83, 0.13))
    p$labels$fill <- "Abundance"
    p$labels$colour <- "Abundance"
    
    # Save
    ggsave(p, 
           filename = 
             paste0("C:/Users/klankowicz/Desktop/VAST_examples/Mapping/abund/",
                    names(D.list)[i], "/",
                    names(D.list)[i], " distribution ", Year, '.png'),
           device="png")
    
    
    
  }
  
}

# List files in given directory
files  <- list.files(paste0("C:/Users/klankowicz/Desktop/VAST_examples/",
                            "Mapping/logabund/Ages 5+"))
setwd("C:/Users/klankowicz/Desktop/VAST_examples/Mapping/logabund/")
files <- paste0(getwd(), "/Ages 5+/", files)

# Set GIF save location
setwd("C:/Users/klankowicz/Desktop/VAST_examples/Mapping/logabund/GIFs/")

# Convert PNGs to GIF
library(gifski)
gifski(files, "Ages5+_logabund.gif", loop = FALSE, delay = 0.25)
