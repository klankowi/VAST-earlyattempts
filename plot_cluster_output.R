# Set workspace
rm(list=ls())

# Load libraries
library(VAST)
library(tidyverse)
library(here)
library(sf)
library(factoextra)

# Create function for finding the mode of a vector
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Set GGplot auto theme
theme_set(theme(plot.margin = margin(t=0.25, b=0.25, l=0.5, r=0.25, 'cm'),
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

################################################################################
####                   Hierarchical clustering analysis                     ####
################################################################################

# Load data
load(here("VAST_runs/effort_adjustment/effort_adjustment.Rdata"))

# Remove intermediates
rm(ex3, ex4, non, scaled.covars, survs, strata_use, user_region, settings,
   seas.labs)

# Set variable to be clustered (density per cell)
var_name = "D_gct"

# Set transformation (natural log)
transform_var = log
yaxis_log = TRUE

# Set number of clusters (4)
k =  5

# Set clustering method (ward)
method = "ward"

# Set map list
map_list = make_map_info(Region = fit$settings$Region, 
                         spatial_list = fit$spatial_list, 
                         Extrapolation_List = fit$extrapolation_list)

# Rename report output          
fit$Report = amend_output(fit, 
                          year_labels = year.labs, 
                          category_names = cat.labs)

# Transform to natural log and strip units from density per cell
Y_gct = transform_var(strip_units(fit$Report[[var_name]]))

# Repace infinite values with NA  
Y_gct = ifelse(abs(Y_gct) == Inf, NA, Y_gct)

# Current output is a 2000 x 4 x 80 array. Essentially a Site x Category df
# for each time slice.
# We actually want to get rid of the unknown category
# Reshape  to create 2000 x 80 x 4 array, where there is a Site X time df for 
# each category type
Y_gct2 <- aperm(Y_gct, c(1,3,2))

# Remove unknown category -- keep this one
Y_gct3 <- Y_gct2[1:10000,1:80,1:3]

# Separate med and large groups, backtransform, add, transform
Y_small <- Y_gct3[1:10000, 1:80,1]
Y_med <- Y_gct3[1:10000, 1:80,2]
Y_med <- exp(Y_med)
Y_large <- Y_gct3[1:10000, 1:80,3]
Y_large <- exp(Y_large)
Y_both <- Y_med + Y_large
Y_both <- log(Y_both)

Y_gct4 <- abind::abind(Y_small, Y_both, along=3)
names(dimnames(Y_gct4)) <- c('Site', 'Time', 'Category')
dimnames(Y_gct4)[[3]] <- c("Small", "Med-Large")

# Reshape back to 2000 x 3 x 80 array
Y_gct <- aperm(Y_gct4, c(1,3,2))

# Remove intermediates
rm(Y_both, Y_large, Y_small, Y_med, Y_gct2, Y_gct4)

# Reshape output to be a 16000 x 4 array: Time_Site x Category df.
Y_z = reshape2:::melt.array(data = Y_gct, varnames = names(dimnames(Y_gct)))
Y_zc = reshape2::acast(Y_z, formula = Time + Site ~ Category)

# Identify and remove any NA values (cannot be run through hclust analysis)
which_NA = which(apply(Y_zc, MARGIN = 1, FUN = function(vec) {
    any(is.na(vec))
}))
which_notNA = setdiff(1:nrow(Y_zc), which_NA)
Yprime_zc = Y_zc[which_notNA, , drop = FALSE]

# Run hierarchical agglomerative cluster analysis using `k` cluster centers and
# `method` method WITH UNKNOWNS
# Hclust = fastcluster::hclust.vector(Yprime_zc, method = method)
# saveRDS(Hclust, here('data/RData_Storage/effort_adjust_hclust.RDS'))
# Hclust <- readRDS(here('data/RData_Storage/effort_adjust_hclust.RDS'))

# Run hierarchical agglomerative cluster analysis using `k` cluster centers and
# `method` method WITHOUT UNKNOWNS
Hclust2 = fastcluster::hclust.vector(Yprime_zc, method = method)
saveRDS(Hclust2, here('data/RData_Storage/refine_effort_hclust2.RDS'))
#Hclust2 <- readRDS(here('data/RData_Storage/refine_effort_hclust2.RDS'))

# Cut clustering tree into groups by specified cluster
Classprime_z = cutree(Hclust2, k = k)
  
# Create vector of cluster numbers per Time_site
Class_z = rep(NA, nrow(Y_zc))
Class_z[which_notNA] = Classprime_z

# Create Site x Time array where each entry is the specified cell's cluster #
Class_gt = array(Class_z, dim = dim(Y_gct)[c(1, 3)], 
                 dimnames = dimnames(Y_gct)[c(1, 3)])

# Create plot of cluster density per group (entire time series)
Ybar_kc = apply(Y_zc, MARGIN = 2, FUN = function(y_z, class_z) {
  tapply(y_z, INDEX = class_z, FUN = mean)
}, class_z = Class_z)
Ybar_t <- as.data.frame(exp(Ybar_kc))
Ybar <- data.frame(
  Density = c(Ybar_t$Small, Ybar_t$`Med-Large`),
  Category = c(rep('Small', 5), rep('Med-Large', 5)),
  Cluster = as.factor(c(seq(1, 5), seq(1, 5)))
)
Ybar$Category <- factor(Ybar$Category,
                        levels=c('Small', 'Med-Large'))
Ybar %>%
  ggplot(aes(x=Category,y=Density)) +
  geom_point(aes(color=Cluster), pch=19, cex=2) +
  geom_line(aes(group = Cluster, col=Cluster), lwd=2) +
  scale_color_viridis_d(option='viridis', direction = -1)

# Remove intermediates
rm(Hclust2, Y_z, Yprime_zc, Class_z,
   Classprime_z, k, method, var_name, which_NA, which_notNA, transform_var)

################################################################################  
#####                              Plot                                    #####
################################################################################
# Set zlim
Zlim=range(Class_gt, na.rm = T)

# Pull out cell centers
Y_gt = Class_gt[map_list$PlotDF[which(map_list$PlotDF[, "Include"] > 
                                    0), "x2i"], , drop = FALSE]

# Call number of cells
n_cells = nrow(Y_gt)

# Matrix of lat x lon for cell centers
loc_g = map_list$PlotDF[which(map_list$PlotDF[, "Include"] > 
                                0), c("Lon", "Lat")]

# Set original CRS to unprojected WGS84 (EPSG 4326)
CRS_orig = sp::CRS("+proj=longlat")

# Set projected CRS to WGS84 UTM 19N (EPSG 32619)
projargs = fit$extrapolation_list$projargs
CRS_proj = sp::CRS(projargs)

# Create empty dataframe to dump in calculated area of each cluster type
area.mat <- data.frame(
  High.large = rep(NA, 80),
  High.all   = rep(NA, 80),
  High.small = rep(NA, 80),
  Low.all    = rep(NA, 80)
)
row.names(area.mat) <- year.labs

# Load coast shapefile
coast <- ecodata::coast
coast <- st_transform(coast, crs = projargs)

# Loop through time steps
for(i in 1:ncol(Y_gt)){
  # Convert lat-lon points to spatial points data frame
  Points_orig = sp::SpatialPointsDataFrame(coords = loc_g, 
                                           data = data.frame(y = Y_gt[, i]),
                                           proj4string = CRS_orig)
  
  # Confirm they are reported in unprojected WGS84 lat-lon
  Points_LongLat = sp::spTransform(Points_orig, sp::CRS("+proj=longlat"))
  
  # Reproject to WGS84 UTM 19N
  Points_proj = sp::spTransform(Points_orig, CRS_proj)
  
  # Call cell size (if domain is not a square, find mean side length. Divide by
  # the number of cells that can be along each side length)
  # Shakes out to about 20 sq km per cell.
  cell.size = mean(diff(Points_proj@bbox[1, ]), 
                   diff(Points_proj@bbox[2, ]))/ floor(sqrt(n_cells))
  
  # Convert to Sf object
  Points_sf = sf::st_as_sf(Points_proj)
  
  # 
  Points_sf$Cluster[Points_sf$y == 4] <- 'Low density of all cod'
  Points_sf$Cluster[Points_sf$y == 1] <- 'Low density of all cod'
  Points_sf$Cluster[Points_sf$y == 3] <- 'High density of small cod'
  Points_sf$Cluster[Points_sf$y == 2] <- 'High density of med/large cod'
  Points_sf$Cluster[Points_sf$y == 5] <- 'High density of all cod'
  Points_sf$Cluster <- factor(Points_sf$Cluster,
                              levels=c('High density of med/large cod',
                                       'High density of all cod',
                                       'High density of small cod',
                                       'Low density of all cod'))
  
  sypt <- ggplot(coast) +
    geom_sf(data=Points_sf, aes(col=Cluster)) +
    geom_sf() +
    scale_color_viridis_d(option='viridis') +
    coord_sf(xlim=c(-116, 771),
             ylim=c(4060, 4970)) +
    ggtitle(paste0(year.labs[i]))
  
  ggsave(plot = sypt,
         path = 'C:/Users/klankowicz/Desktop/Yearly_Clustering/Points/',
         filename = paste0(year.labs[i], '.png'),
         device='png')
  
    
    grid = sf::st_make_grid(Points_sf, cellsize = cell.size)
    grid_i = sf::st_intersects(Points_sf, grid)
    grid = sf::st_sf(grid, y = tapply(Points_sf$y, 
                                     INDEX = factor(as.numeric(grid_i),
                                                    levels = 1:length(grid)), 
                                     FUN = getmode))
     
    grid$Cluster[grid$y == 4] <- 'Low density of all cod'
    grid$Cluster[grid$y == 1] <- 'Low density of all cod'
    grid$Cluster[grid$y == 3] <- 'High density of small cod'
    grid$Cluster[grid$y == 2] <- 'High density of med/large cod'
    grid$Cluster[grid$y == 5] <- 'High density of all cod'
    grid$Cluster <- factor(grid$Cluster,
                                levels=c('High density of med/large cod',
                                         'High density of all cod',
                                         'High density of small cod',
                                         'Low density of all cod'))
    grid[,'Cluster description'] <- grid$Cluster
    
    totar <- st_area(st_union(grid[!is.na(grid$y),]))
    
    area.mat[i,"High.large"] <- 100 * (st_area(st_union(grid[grid$Cluster == 'High density of med/large cod',])) / totar)
    area.mat[i,"High.all"] <- 100 * (st_area(st_union(grid[grid$Cluster == "High density of all cod",])) / totar)
    area.mat[i,"High.small"] <- 100 * (st_area(st_union(grid[grid$Cluster == 'High density of small cod',])) / totar)
    area.mat[i,"Low.all"] <- 100 * (st_area(st_union(grid[grid$Cluster == 'Low density of all cod',])) / totar)
    
    sygct <- ggplot(coast) +
     geom_sf(data=grid[!is.na(grid$Cluster),],
             aes(fill = Cluster),
             col=NA) +
     geom_sf() +
     coord_sf(xlim=c(-116, 771),
              ylim=c(4060, 4970)) +
    scale_fill_viridis_d(option="viridis") +
    ggtitle(paste0(year.labs[i]))
     
    ggsave(plot = sygct,
         path = 'C:/Users/klankowicz/Desktop/Yearly_Clustering/Grid',
         filename = paste0(year.labs[i], '.png'),
         device='png')
    rm(Points_orig, Points_LongLat, Points_proj, cell.size, Points_sf,
      grid, grid_i, sygct, sypt)
}

area.df <- data.frame(
  "Pct value" = rep(NA, 4*nrow(area.mat)),
  "Cluster" = c(rep('High density of med/large cod', nrow(area.mat)),
                rep('High density of all cod', nrow(area.mat)),
                rep('High density of small cod', nrow(area.mat)),
                rep('Low density of all cod', nrow(area.mat))),
  "Year" = rep(area.mat$Year, 4),
  "Season" = rep(area.mat$Season, 4)
)
area.df$Pct.value <- c(area.mat$High.large, 
                       area.mat$High.all,
                       area.mat$High.small,
                       area.mat$Low.all)

ggplot() +
  geom_point(data=area.df[area.df$Season == "Spring",],
            aes(x=Year, y=Pct.value, col=Cluster),
            pch=19) +
  geom_smooth(data=area.df[area.df$Season == "Spring",],
              aes(x=Year, y=Pct.value, col=Cluster, fill=Cluster)) + 
  scale_color_viridis_d(option="viridis") +
  scale_fill_viridis_d(option='viridis') +
  ylab('% of area')






springs <- seq(1,79,2)
falls   <- seq(2,80,2)

rownames(Class_gt) <- NULL

springs <- Class_gt[,springs]
springnames <- colnames(springs)
falls <- Class_gt[,falls]
fallnames <- colnames(falls)

test <- data.frame(
  High.All = rep(NA, 2000),
  High.Large = rep(NA, 2000),
  High.Small = rep(NA, 2000),
  Low.All = rep(NA, 2000)
)

# 4 <- 'Low density of all cod'
# 2 <- 'High density of all cod'
# 3  <- 'High density of small cod'
# 1 <- 'High density of med/large cod'

for(i in 1:nrow(springs)){
  test$High.All[i] <- length(springs[i,][springs[i,] == 2])
  test$High.Large[i] <- length(springs[i,][springs[i,] == 1])
  test$High.Small[i] <- length(springs[i,][springs[i,] == 3])
  test$Low.All[i] <- length(springs[i,][springs[i,] == 4])
}

