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

# Load data
load(here("VAST_runs/effort_adjustment/effort_adjustment.Rdata"))

# Load sediment grids
load(here('data/RData_Storage/sediment_grids.RData'))
sedgrid <- grid
rm(grid)

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

# Pull vector of years
years <- year.labs

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
#D.1 <- separate(D.1, Year, into = c("Year", "Season"), sep = " (?=[^ ]+$)")
D.1$Cat <- 1

# Category 2
D_gt.2 <- fit$Report$D_gct[,2,] # drop the category
dimnames(D_gt.2) <- list(cell=1:nrow(D_gt.2), year=years)
D_gt.2 <- D_gt.2 %>% as.data.frame() %>%
  tibble::rownames_to_column(var = "cell") %>%
  pivot_longer(-cell, names_to = "Year", values_to='D')
D.2 <- merge(D_gt.2, mdl$PlotDF, by.x='cell', by.y='x2i')
#D.2 <- separate(D.2, Year, into = c("Year", "Season"), sep = " (?=[^ ]+$)")
D.2$Cat <- 2

# Category 3
D_gt.3 <- fit$Report$D_gct[,3,] # drop the category
dimnames(D_gt.3) <- list(cell=1:nrow(D_gt.3), year=years)
D_gt.3 <- D_gt.3 %>% as.data.frame() %>%
  tibble::rownames_to_column(var = "cell") %>%
  pivot_longer(-cell, names_to = "Year", values_to='D')
D.3 <- merge(D_gt.3, mdl$PlotDF, by.x='cell', by.y='x2i')
#D.3 <- separate(D.3, Year, into = c("Year", "Season"), sep = " (?=[^ ]+$)")
D.3$Cat <- 3

# Category 4
D_gt.4 <- fit$Report$D_gct[,4,] # drop the category
dimnames(D_gt.4) <- list(cell=1:nrow(D_gt.4), year=years)
D_gt.4 <- D_gt.4 %>% as.data.frame() %>%
  tibble::rownames_to_column(var = "cell") %>%
  pivot_longer(-cell, names_to = "Year", values_to='D')
D.4 <- merge(D_gt.4, mdl$PlotDF, by.x='cell', by.y='x2i')
#D.4 <- separate(D.4, Year, into = c("Year", "Season"), sep = " (?=[^ ]+$)")
D.4$Cat <- 4

# Rebind to new shape
D <- rbind(D.1, D.2, D.3, D.4)

# Adjust data to log abundance, strip units
D$D <- strip_units(D$D)
D$logD <- log(D$D)

#Rebind to list
D.list <- split(D, f=D$Cat)
names(D.list) <- c("Small", "Medium", "Large", "Unknown sizes")

# Set CRS 
projargs <- fit$extrapolation_list$projargs
CRS_orig = sp::CRS("+proj=longlat")
CRS_proj = sp::CRS(projargs)

closed.areas <- st_read(here("data/GIS/closed_areas_wgs.shp"))

sedgrid$Cell <- seq(1:nrow(sedgrid))

hard.agg <- st_union(hard)
mix.agg <- st_union(mix)
soft.agg <- st_union(soft)
allbutsoft <- st_union(hard, mix)
allbutsoft <- st_union(allbutsoft)
wholearea <- rbind(hard, mix, soft)
wholearea <- st_union(wholearea)

# Area proportions 
areahard <- st_area(hard.agg)
areasoft <- st_area(soft.agg)
areamix  <- st_area(mix.agg)
areatot <- st_area(wholearea)
round((st_area(hard.agg) / st_area(wholearea) * 100), 1)
round((st_area(mix.agg) / st_area(wholearea) * 100), 1)
round((st_area(soft.agg) / st_area(wholearea) * 100), 1)

fishsum.df <- data.frame(
  Small = rep(NA, length(year.labs)),
  Medium = rep(NA, length(year.labs)),
  Large = rep(NA, length(year.labs)),
  Unkknown = rep(NA, length(year.labs))
)
row.names(fishsum.df) <- year.labs

fishsum.list <- list(fishsum.df, fishsum.df, fishsum.df, fishsum.df)
names(fishsum.list) <- c('Hard', 'Mix', 'Soft', 'Total')

# Outer loop: Categories
for(i in 1:4){
  Cat.sub <- D.list[[i]]
  
  # Check for outliers, remove
  outliers <- boxplot.stats(Cat.sub$logD)$out
  Cat.sub$D[Cat.sub$logD %in% outliers] <- NA
  
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
    abgrid = sf::st_make_grid(Points_sf, cellsize = cell.size)
    abgrid_i = sf::st_intersects(Points_sf, abgrid)
    abgrid = sf::st_sf(abgrid, y = tapply(Points_sf$y, 
                                      INDEX = factor(as.numeric(abgrid_i),
                                                     levels = 1:length(abgrid)), 
                                      FUN = mean, na.rm = TRUE))
    abgrid$Cell <- seq(1:nrow(abgrid))
    
    hardsum <- sum(abgrid$y[abgrid$Cell %in% unique(sedgrid$Cell[sedgrid$outcome == 'Hard' &
                                                    !is.na(sedgrid$outcome)])],
        na.rm=T)
    mixsum <- sum(abgrid$y[abgrid$Cell %in% unique(sedgrid$Cell[sedgrid$outcome == 'Mix' &
                                                        !is.na(sedgrid$outcome)])],
        na.rm=T)
    softsum <- sum(abgrid$y[abgrid$Cell %in% unique(sedgrid$Cell[sedgrid$outcome == 'Soft' &
                                                        !is.na(sedgrid$outcome)])],
        na.rm=T)
    totsum <- sum(abgrid$y, na.rm=T)
    
    fishsum.list[["Hard"]][j,i] <- hardsum
    fishsum.list[["Mix"]][j,i] <- mixsum
    fishsum.list[["Soft"]][j,i] <- softsum
    fishsum.list[["Total"]][j,i] <- totsum
    
    # Plot
    # p <- ggplot(ecodata::coast)+
    #   geom_sf(data=grid, aes(fill=y, col=y)) +
    #   geom_sf(data=closed.areas, fill=NA, col='black')+
    #   geom_sf(data=allbutsoft, col='blue', fill=NA) +
    #   scale_color_viridis_c(limits=c(0.01, max.D),
    #                         na.value = 'transparent',
    #                         option=('rocket'),
    #                         direction = -1,
    #                         alpha = 0.8) +
    # 
    #   scale_fill_viridis_c(limits=c(0.01, max.D),
    #                         na.value = 'transparent',
    #                         option=('rocket'), 
    #                        direction = -1, 
    #                        alpha = 0.8) +
    #   
    #   geom_sf(fill='gray')+
    #   coord_sf(xlim=c(-76, -65),
    #            ylim=c(36,46),
    #            crs="EPSG:4326")+
    #   labs(title=paste0(names(D.list)[i], " distribution ", Year.sub$Year[1])) +
    #   theme(legend.position = c(0.90, 0.20))
    # p$labels$fill <- "Abund."
    # p$labels$colour <- "Abund."
    
    # # Save
    # ggsave(p, 
    #        filename = 
    #          paste0("C:/Users/klankowicz/Desktop/VAST_examples/Mapping2/abund_closed/",
    #                 names(D.list)[i], "/",
    #                 names(D.list)[i], " distribution ", Year, '.png'),
    #        device="png")
    
  }
  
}
fishsum.list[["Hard"]]$Sub <- "Hard"
fishsum.list[["Hard"]]$Effort <-  areahard

fishsum.list[["Soft"]]$Sub <- "Soft"
fishsum.list[["Soft"]]$Effort <- areasoft

fishsum.list[["Mix"]]$Sub <- "Mix"
fishsum.list[["Mix"]]$Effort <- areamix

fishsum.list[["Total"]]$Sub <- NA
fishsum.list[["Total"]]$Effort <- areatot

fishsum.all <- do.call(rbind, fishsum.list)
fishsum.all$Year <- rep(year.labs, 4)
fishsum.all$Time <- as.numeric(as.factor(fishsum.all$Year))
fishsum.all$Small.eff <- fishsum.all$Small / fishsum.all$Effort
fishsum.all$Medium.eff <- fishsum.all$Medium / fishsum.all$Effort
fishsum.all$Large.eff <- fishsum.all$Large / fishsum.all$Effort

fishsum <- subset(fishsum.all, !is.na(Sub))
row.names(fishsum) <- NULL

newfish <- rbind(data.frame( Abund = fishsum$Small.eff,
                             Year = fishsum$Year,
                             Group = rep('Under 40cm', nrow(fishsum)),
                             Sub = fishsum$Sub),
                 data.frame( Abund = fishsum$Medium.eff,
                             Year = fishsum$Year,
                             Group = rep('40-70 cm', nrow(fishsum)),
                             Sub = fishsum$Sub),
                 data.frame( Abund = fishsum$Large.eff,
                             Year = fishsum$Year,
                             Group = rep('Over 70 cm', nrow(fishsum)),
                             Sub = fishsum$Sub))

newfish$True_Year <- rep(seq(1982, 2021.5, 0.5), 3*3)
newfish$True_Year <- floor(newfish$True_Year)

newfish$Season <- rep(c('Spring', 'Fall'), 360)

newfish$Sub <- as.factor(newfish$Sub)
names(newfish) <- c('Abund', 'OldYear', 'Group', 'Substrate', 'Year', 'Season')
newfish$Abund <- strip_units(newfish$Abund)

newfish$Group <- factor(newfish$Group, levels=c('Under 40cm',
                                                '40-70 cm',
                                                'Over 70 cm'))
# Spring index
s <- ggplot() +
  geom_line(data=newfish[newfish$Season == "Spring",], 
             aes(x=Year, y=Abund, col=Substrate)) +
  facet_wrap(vars(Group)) +
  xlab('Year') + ylab(bquote('Abundance  '(m^-2))) +
  theme(plot.margin = margin(t=0.25, b=0.25, l=0.5, r=0.25, 'cm')) +
  ggtitle('Spring index')
s

# Fall index
f <- ggplot() +
  geom_line(data=newfish[newfish$Season == "Fall",], 
            aes(x=Year, y=Abund, col=Substrate)) +
  facet_wrap(vars(Group)) +
  xlab('Year') + ylab(bquote('Abundance  '(m^-2))) +
  theme(plot.margin = margin(t=0.25, b=0.25, l=0.5, r=0.25, 'cm')) +
  ggtitle('Fall index')
f

# Save
ggsave(s,
      filename =
        paste0("C:/Users/klankowicz/Desktop/spring_index.png"),
      device="png")

ggsave(f,
       filename =
         paste0("C:/Users/klankowicz/Desktop/fall_index.png"),
       device="png")
