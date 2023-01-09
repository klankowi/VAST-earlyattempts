## Northeast:

# set bounding boxes
neus.xmin=-77
neus.xmax=-65
neus.ymin=35
neus.ymax=45


# Set strata.limits
All_areas = as.data.frame(seq(1:4))
colnames(All_areas) <- 'stratum'
EGOM = as.data.frame(1)
colnames(EGOM) <- 'stratum'
GBK = as.data.frame(2)
colnames(GBK) <- 'stratum'
SNE = as.data.frame(3)
colnames(SNE) <- 'stratum'
WGOM = as.data.frame(4)
colnames(WGOM) <- 'stratum'
strata.limits <- as.list(c("All_areas" = All_areas, 
                           "EGOM" = EGOM,
                           "GBK" = GBK,
                           "SNE" = SNE,
                           "WGOM" = WGOM))
strata.limits

# high resolution coastline
usamap <- rnaturalearth::ne_countries(scale = "large", 
                                      country = "united states of america", 
                                      returnclass = "sf")[1] %>% 
  sf::st_cast("MULTILINESTRING") # get basic map of the country 

neus.bbox1 <- sf::st_set_crs(sf::st_as_sf(as(raster::extent(neus.xmin, 
                                                            neus.xmax, 
                                                            neus.ymin, 
                                                            neus.ymax), 
                                             "SpatialPolygons")), 
                             sf::st_crs(usamap))
neus.bbox2 <- sf::st_set_crs(sf::st_as_sf(as(raster::extent(-78, -74, 42, 45), 
                                             "SpatialPolygons")), 
                             sf::st_crs(usamap))

# just the NEUS coastline high res

neuscoast <- usamap %>% 
  sf::st_intersection(neus.bbox1) %>%  
  sf::st_difference(neus.bbox2) # gets rid of extra non coastal line 
ggplot() +
  geom_sf(data=neuscoast)

# Add cod stocks
stox <- readOGR(here("data/GIS/codstox.shp"))
stox.sf <- st_as_sf(stox)
stox.sf <- st_transform(stox.sf, crs=st_crs(neuscoast))
st_crs(stox.sf)== st_crs(neuscoast)

ggplot() +
  geom_sf(data=neuscoast) +
  geom_sf(data=stox.sf, fill=alpha("blue", 0.5))

stox.df <- sfheaders::sf_to_df(stox.sf, fill=T)

Prepare_NWA_Extrapolation_Data_Fn_skg <- function (strata.limits = NULL,
                                                   projargs = NA, 
                                                   zone = NA, 
                                                   flip_around_dateline = FALSE, 
                                                   ...) 
{
  if (is.null(strata.limits)) {
    strata.limits = list(All_areas = 1:4)
  }
  message("Using strata ", strata.limits)
  utils::data(northwest_atlantic_grid, package = "FishStatsUtils")
  Data_Extrap <- stox.df
  Tmp = cbind(BEST_DEPTH_M = 0, BEST_LAT_DD = Data_Extrap[,"y"], 
              BEST_LON_DD = Data_Extrap[, "x"])
    a_el = as.data.frame(matrix(NA, nrow = nrow(Data_Extrap), 
                                ncol = length(strata.limits), 
                                dimnames = list(NULL, names(strata.limits))))
    Area_km2_x = Data_Extrap[, "Shape_Area"]
    for (l in 1:ncol(a_el)) {
      a_el[, l] = ifelse(Data_Extrap[, "OBJECTID"] %in% 
                           strata.limits[[l]], Area_km2_x, 0)
    }
  tmpUTM = project_coordinates(X = Data_Extrap[, "x"], 
                               Y = Data_Extrap[,"y"], 
                               projargs = projargs, 
                               zone = zone, 
                               flip_around_dateline = flip_around_dateline)
  Data_Extrap = cbind(Data_Extrap, Include = 1)
  Data_Extrap[, c("E_km", "N_km")] = tmpUTM[, c("X", "Y")]
  Return = list(a_el = a_el, 
                Data_Extrap = Data_Extrap, 
                zone = attr(tmpUTM, "zone"), 
                projargs = attr(tmpUTM, "projargs"), 
                flip_around_dateline = flip_around_dateline, 
                Area_km2_x = Area_km2_x)
  return(Return)
}

Extrapolation_List  <-  
  Prepare_NWA_Extrapolation_Data_Fn_skg(strata.limits=strata.limits)

saveRDS(Extrapolation_List, file = here("data/RData_Storage/CustomExtrapolationList.rds"))
