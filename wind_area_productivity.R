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
                plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12),
                strip.text = element_text(size=10)))

# Load data
load(here("VAST_runs/refine_effort/Kmeans_extrapolation-10000.Rdata"))
load(here("VAST_runs/refine_effort/refine_effort.Rdata"))
rm(ex3, ex4, survs, scaled.covars, settings, strata_use,
   cat.labs, seas.labs, year.labs)

grid.centers <- as.data.frame(fit$extrapolation_list$Data_Extrap)
grid.allocat <- as.data.frame(Kmeans$cluster)
grid.sizes <- as.data.frame(Kmeans$size)

grid.info <- cbind(grid.centers, grid.sizes)
grid.info$Cell <- seq(1, nrow(grid.info))
names(grid.info) <- c('Lon', 'Lat', 'Include', 'E_km', 'N_km', 'size', 'cell')
head(grid.info)

grid.allocat$point <- seq(1, nrow(grid.allocat))
names(grid.allocat) <- c('cell', 'point')

grid.total <- left_join(grid.allocat, grid.info, by=c('cell'))
grid.total <- grid.total[with(grid.total, order(point, cell)),]
row.names(grid.total) <- NULL
head(grid.total)

user_region <- subset(user_region, STRATA == 'All')
user_region$point <- seq(1, nrow(user_region))
region_points <- left_join(user_region, grid.allocat, by=c('point'))

# Set CRS 
projargs <- fit$extrapolation_list$projargs
CRS_orig = sp::CRS("+proj=longlat")
CRS_proj = sp::CRS(projargs)

# Adjust ecodata coast projection
coast <- ecodata::coast
coast <- st_transform(coast, crs = projargs)

# Add wind area
wind.area <- st_read(here('data/GIS/BOEM_GoM_Lease.shp'))
wind.area <- dplyr::select(wind.area, geometry)
wind.area$Region <- 'wind'
wind.area <- st_transform(wind.area, crs= projargs)

grid.total <- st_as_sf(grid.info, coords=c('Lon', 'Lat'), crs="EPSG:4326")
grid.total <- st_transform(grid.total, crs= st_crs(wind.area))
wind.over <- st_intersection(grid.total, wind.area)

region_points <- st_as_sf(region_points, coords=c('Lon', 'Lat'), crs="EPSG:4326")
region_points <- st_transform(region_points, crs=st_crs(wind.area))
region.wind.points <- st_intersection(region_points, wind.area)

# Plot VAST grid
loc_g <- cbind(grid.info$Lon, grid.info$Lat)

n_cells <- dim(loc_g)[1]

Points_orig = sp::SpatialPointsDataFrame(coords = loc_g, 
                                         data = data.frame(cell = grid.info$cell), 
                                         proj4string = CRS_orig)

Points_LongLat = sp::spTransform(Points_orig, sp::CRS("+proj=longlat"))

Points_proj = sp::spTransform(Points_orig, CRS_proj)

cell.size = mean(diff(Points_proj@bbox[1, ]), 
                 diff(Points_proj@bbox[2,]))/floor(sqrt(n_cells))
Points_sf = sf::st_as_sf(Points_proj)
abgrid = sf::st_make_grid(Points_sf, cellsize = cell.size)
abgrid_i = sf::st_intersects(Points_sf, abgrid)
abgrid <- abgrid[c(as.numeric(abgrid_i))]
abgrid = sf::st_sf(abgrid, cell = Points_sf$cell)

poly.extract <- st_intersection(wind.area, abgrid)
for(i in 1:nrow(poly.extract)){
  poly.extract$area[i] <- st_area(poly.extract[i,])
}

poly.extract$cell <- factor(poly.extract$cell,
                            levels=c(2398, 4246, 9399,        #1
                                     353, 5851, 9185, 9757,   #2
                                     2798, 3779, 8282, 9171,  #3
                                     27, 4863, 7046,          #4
                                     1037, 3717, 4528,        #5
                                     3120, 5273, 5942, 7913,  #6
                                     4092, 4166, 6753,        #7
                                     2096, 3988, 7546,        #8
                                     5717, 6464, 6672         #9
                                     ))
ggplot()+
  geom_sf(data=poly.extract, aes(fill=cell)) +
  facet_wrap(vars(poly.extract$cell))

keeps <- c(2398, 353, 2798, 27, 1037, 3120, 4092, 2096, 5717)
poly.keeps <- poly.extract[poly.extract$cell %in% keeps,]
row.names(poly.keeps) <- NULL
poly.keeps$totarea <- st_area(abgrid[abgrid$cell %in% keeps,])
poly.keeps$areaprop <- poly.keeps$area / strip_units(poly.keeps$totarea)

ggplot() +
  geom_sf(data=abgrid[abgrid$cell %in% keeps,]) +
  geom_sf(data=poly.keeps, aes(fill=cell))

# Remake map list locally for recreating plots
mdl <- make_map_info(Region = fit$settings$Region,
                     spatial_list = fit$spatial_list,
                     Extrapolation_List = fit$extrapolation_list)
# Category 1
D_gt.1 <- fit$Report$D_gct[,1,] # drop the category
dimnames(D_gt.1) <- list(cell=1:nrow(D_gt.1), year=fit$year_labels)
D_gt.1 <- D_gt.1 %>% as.data.frame() %>%
  tibble::rownames_to_column(var = "cell") %>%
  pivot_longer(-cell, names_to = "Year", values_to='D')
D.1 <- merge(D_gt.1, mdl$PlotDF, by.x='cell', by.y='x2i')
#D.1 <- separate(D.1, Year, into = c("Year", "Season"), sep = " (?=[^ ]+$)")
D.1$Cat <- 'Small'
D.1$cell <- as.numeric(D.1$cell)

# Category 2
D_gt.2 <- fit$Report$D_gct[,2,] # drop the category
dimnames(D_gt.2) <- list(cell=1:nrow(D_gt.2), year=fit$year_labels)
D_gt.2 <- D_gt.2 %>% as.data.frame() %>%
  tibble::rownames_to_column(var = "cell") %>%
  pivot_longer(-cell, names_to = "Year", values_to='D')
D.2 <- merge(D_gt.2, mdl$PlotDF, by.x='cell', by.y='x2i')
#D.2 <- separate(D.2, Year, into = c("Year", "Season"), sep = " (?=[^ ]+$)")
D.2$Cat <- 'Medium'
D.2$cell <- as.numeric(D.2$cell)

# Category 3
D_gt.3 <- fit$Report$D_gct[,3,] # drop the category
dimnames(D_gt.3) <- list(cell=1:nrow(D_gt.3), year=fit$year_labels)
D_gt.3 <- D_gt.3 %>% as.data.frame() %>%
  tibble::rownames_to_column(var = "cell") %>%
  pivot_longer(-cell, names_to = "Year", values_to='D')
D.3 <- merge(D_gt.3, mdl$PlotDF, by.x='cell', by.y='x2i')
#D.3 <- separate(D.3, Year, into = c("Year", "Season"), sep = " (?=[^ ]+$)")
D.3$Cat <- 'Large'
D.3$cell <- as.numeric(D.3$cell)

# Category 4
D_gt.4 <- fit$Report$D_gct[,4,] # drop the category
dimnames(D_gt.4) <- list(cell=1:nrow(D_gt.4), year=fit$year_labels)
D_gt.4 <- D_gt.4 %>% as.data.frame() %>%
  tibble::rownames_to_column(var = "cell") %>%
  pivot_longer(-cell, names_to = "Year", values_to='D')
D.4 <- merge(D_gt.4, mdl$PlotDF, by.x='cell', by.y='x2i')
#D.4 <- separate(D.4, Year, into = c("Year", "Season"), sep = " (?=[^ ]+$)")
D.4$Cat <- 'Unknown'
D.4$cell <- as.numeric(D.4$cell)

a_el <- fit$extrapolation_list$a_el
a_el <- as.data.frame(a_el)
a_el$cell <- seq(1, nrow(a_el))
colnames(a_el) <- c('area', 'cell')
a_el$area <- strip_units(a_el$area)

D <- rbind(D.1, D.2, D.3, D.4)
D$D <- strip_units(D$D)

D <- merge(D, a_el, by=c('cell'))
D$cod <- D$D * D$area

poly.prop <- dplyr::select(poly.keeps, cell, areaprop)
poly.prop <- as.data.frame(poly.prop)

inner.df <- data.frame(
  cell=seq(1, 10000),
  cod = rep(NA, 10000)
)
outer.df <- list(inner.df, inner.df, inner.df, inner.df)

D.list <- split(D, f=D$Cat)
names(outer.df) <- names(D.list)

for(i in 1:length(D.list)){
  D.list[[i]] <- split(D.list[[i]], f=D.list[[i]]$cell)
}

head(D.list[[1]][[1]])

for(i in 1:length(D.list)){
  for(j in 1:length(D.list[[i]])){
    
    outer.df[[i]]$cod[j] <- sum(D.list[[i]][[j]]$cod)
    outer.df[[i]]$size <- names(D.list)[i]
  }
}
test <- do.call(rbind, outer.df)

test <- split(test, f=test$size)

for(i in 1:length(test)){
  use.grid <- merge(test[[i]], abgrid, by=c('cell'))
  st_geometry(use.grid) <- use.grid$abgrid
  
  # Plot
  p <- ggplot(ecodata::coast)+
    geom_sf(data=use.grid, aes(fill=cod, col=cod)) +
    geom_sf(data=wind.area, fill=NA, col='black')+
    scale_color_viridis_c(#limits=c(0.01, max.D),
                          na.value = 'transparent',
                          option=('rocket'),
                          direction = -1,
                          alpha = 0.8) +
    
    scale_fill_viridis_c(#limits=c(0.01, max.D),
                         na.value = 'transparent',
                         option=('rocket'), 
                         direction = -1, 
                         alpha = 0.8) +
    
    geom_sf(fill='gray')+
    coord_sf(xlim=c(-76, -65),
             ylim=c(36,46),
             crs="EPSG:4326")+
    labs(title=paste0('Total abundance ',
                      names(test)[i],
                      ' cod, 1982-2021')) +
    theme(legend.position = c(0.90, 0.20))
  p$labels$fill <- "Abund."
  p$labels$colour <- "Abund."
  plot(p)
  
  # Save
  ggsave(p, 
         filename = 
           paste0("C:/Users/klankowicz/Desktop/VAST_examples/Mapping3/abund_wind/",
                  names(test)[i],
                  'totalabundance.png'),
         device="png")
}


D.test <- merge(D, poly.prop, by=c('cell'))
D.1 <- as.data.frame(D.1)
D.1$cod <- strip_units(D.1$D) * strip_units(D.1$a_el$a_el)

sum.1 <- data.frame(
  Time = fit$year_labels,
  Cod = rep(NA, length(fit$year_labels))
)

D.list <- split(D.1, f=D.1$Year)
for(i in 1:length(D.list)){
  sum.1$Cod[i] <- sum(D.list[[i]]$cod)
}

years <- seq(1982, 2021)
years <- rep(years, 2)
years <- years[order(years)]

seas <- rep(c('Spring', 'Fall'), 40)

sum.1$Year <- years
sum.1$Season <- seas

plot(sum.1$Year[sum.1$Season == 'Fall'], sum.1$Cod[sum.1$Season == 'Fall'], type='l')

ggplot(coast) + 
  geom_sf(data = region.wind.points) +
  geom_sf(data=vast_grid_sf, col='blue', pch='x', cex=5) +
  geom_sf(data=abgrid, col='blue', fill=NA) +
  geom_sf(fill='gray') +
  geom_sf(data=wind.area, col='red', fill=NA, lwd=2)  +
  coord_sf(xlim=c(460, 478),
           ylim=c(4790, 4810))
