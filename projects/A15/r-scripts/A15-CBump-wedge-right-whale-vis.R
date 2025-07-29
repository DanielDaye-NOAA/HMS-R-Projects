# A15 Historic Effort in the Charleston Bump "Wedge"

#' Additional analysis for PRD regarding PLL effort in Charleston Bump and
#' potential for overlap with Right whale habitat
library(ggplot2)
library(raster)
library(readxl)
library(scales)
library(sf)
library(sp)
library(terra)
library(tidyverse)
library(tidyterra)
library(viridis)

# Load longline data
data <- read_excel("./data/UDP_PLL_2013_2022.xlsx", sheet = "PLL_2013_2022")

# QAQC 
sum(duplicated(data))
table(data$PLL)
sum(is.na(c(data$LONDEC, data$LATDEC)))

# Grabbing the data columns that we need (or might want)
setDat <- data %>%
  transmute(ID, YEAR = SET_YEAR, STATE = STATE_NAME,
            LON = LONDEC, LAT = LATDEC, HOOKS,
            LON2 = LON, LAT2 = LAT, MMYY = SET_MMYY)

# Load CBump Monitoring and Restricted
CB_R <- vect("C:/Users/daniel.daye/Documents/ddaye/local_GIS_shapefiles/CBump_f_restricted.shp") %>% 
  st_as_sf()
CB_M <- vect("C:/Users/daniel.daye/Documents/ddaye/local_GIS_shapefiles/CBump_f_monitoring.shp") %>% 
  st_as_sf()
# Bathymetry
bath <- vect("C:/Users/daniel.daye/Documents/ddaye/local_GIS_shapefiles/bathy100_200_2000_Project.shp") %>%
  st_as_sf()

# PLL & Area Plots
lims <- data.frame(x = c(-82, -74),
                   y = c( 28,  34))
states <- map_data("state")

# Plot of CBump R + M, with 100/200/2000 bathymetry for reference
ggplot(data = states) + 
  geom_polygon(aes(long, lat, group = group), color = "black", fill = "gray50") +
  geom_sf(data = CB_R, col = "red", fill = "red", alpha = 0.1, lwd = .5) +
  geom_sf(data = CB_M, col = "yellow", fill = "yellow", alpha = 0.1, lwd = .5) +
  geom_sf(data = bath, col = "gray50", fill = NA) +
  geom_point(data = setDat, aes(LON, LAT), pch = 16, alpha = 0.1) + 
  coord_sf(xlim = lims$x, ylim = lims$y) + theme_bw()

# Create points along wedge to determine where to set the lower bound
border = data.frame(lon = seq(-76.981, -80.445, length.out = 1000),
                    lat = seq(34, 31, length.out = 1000))

# Closer-up of CBump, border index chosen via trial and error for 
# point close to the set along the R/M boundary
ggplot(data = states) + 
  geom_polygon(aes(long, lat, group = group), color = "black", fill = "gray50") +
  geom_sf(data = CB_R, col = "red", fill = "red", alpha = 0.1, lwd = .5) +
  geom_sf(data = CB_M, col = "yellow", fill = "yellow", alpha = 0.1, lwd = .5) +
  geom_point(data = setDat, aes(LON, LAT), pch = 16, alpha = 0.1) + 
  geom_point(data = border[390,], aes(lon, lat), pch = 16, color = "purple", size = 1) +
  coord_sf(xlim = c(-81, -76), ylim = c(31, 34)) + theme_bw()
border[390,]

# CRS ref for consistency
crdref <- "+proj=longlat +datum=WGS84"

# Create wedge polygon for analysis
lon = c(border$lon[390], -76.981, -76.25, -77.15)
lat = c(border$lat[390], 34, 34, 33.25)
lonlat = cbind(id = 1, part=1, lon, lat) 
wedge = vect(lonlat, type = "polygons", crs = crdref) %>%
  st_as_sf()

# CBump plot with wedge visualization, aligns with the 200ftm contour
ggplot(data = states) + 
  geom_point(data = setDat, aes(LON, LAT), pch = 16, alpha = 0.1) + 
  geom_sf(data = bath, col = "gray50", alpha = .5, fill = NA) +
  geom_sf(data = CB_R, col = "red", fill = "red", alpha = 0.1, lwd = .5) +
  geom_sf(data = CB_M, col = "yellow", fill = "yellow", alpha = 0.1, lwd = .5) +
  geom_sf(data = wedge, col = "purple", fill = "purple", alpha = 0.1, lwd = .5) +
  geom_polygon(aes(long, lat, group = group), color = "black", fill = "gray50") +
  coord_sf(xlim = c(-81, -76), ylim = c(31, 34)) + theme_bw()

# Filtering to points near CBump to improve calculation speed
setDat2 <- setDat %>%
  filter(LON > -82) %>%
  filter(LON < -75) %>%
  filter(LAT > 31.5) %>%
  filter(LAT < 34.5)
plot(LAT ~ LON, data = setDat2)

# Convert point data to SpatVector
vectDat <- vect(setDat2, geom = c("LON", "LAT"), crs = crdref)

# Checking that objects are playing nicely
ggplot(vectDat)+
  geom_sf() +
  geom_sf(data = CB_M, col = "yellow", fill = "yellow", alpha = 0.1, lwd = .5) +
  geom_sf(data = wedge, col = "purple", fill = "purple", alpha = 0.1, lwd = .5) +
  coord_sf(xlim = c(-82, -75), ylim = c(31.5, 34.5)) + theme_bw()

# Formatting for PLL, wedge, and monitoring areas so they are consistent.
vectDat <- project(vectDat, crdref)
wedge <- project(wedge %>% vect(), crdref)
mnitr <- project(CB_M %>% vect(), crdref)

# Proportion of sets in Wedge:Monitoring_Area
expanse(wedge, unit = "km")
expanse(mnitr, unit = "km")

# Subset PLL to points ONLY in monitoring area and wedge
pts_wedge <- terra::intersect(vectDat, wedge) %>% terra::as.data.frame(xy = TRUE)
pts_mnitr <- terra::intersect(vectDat, mnitr) %>% as.data.frame(xy = TRUE)

# Percent Sets wedge:monitoring_area
round(nrow(pts_wedge)*100/nrow(pts_mnitr), digits = 2)

# Percent Area wedge:monitoring_area
round(expanse(wedge)*100/expanse(mnitr), digits = 2)

# Mean sets/yr
pts_wedge %>%
  group_by(YEAR) %>%
  summarize(count = n())
sets_year <- pts_mnitr %>%
  group_by(YEAR) %>%
  summarize(count = n())

# Mean sets in the wedge
nrow(pts_wedge)/10 # 10 years

# Mean sets based on overall monitoring area
mean(sets_year$count) * round(nrow(pts_wedge)/nrow(pts_mnitr), digits = 2)

# Load Right whale density raster
# raster("C:/Users/daniel.daye/Documents/ddaye/A15/EC_NARW/Rasters/2010-2019/NARW_v12.1_2010_2019_density_month02.img")
rast <- raster("C:/Users/daniel.daye/Documents/ddaye/A15/EC_NARW/Rasters/2010-2019/NARW_v12.1_2010_2019_density_month02.img")

# Project to LONGLAT CRS
rast <- projectRaster(rast, crs = crdref)

# QAQC 
plot(rast)
plot(wedge)

# Convert RasterLayer to SpatRaster
spatRast <- rast(rast)

# Cropping Right whale raster to CBump extent
spatRast <- crop(spatRast, ext(-82, -75, 30, 35))

# Plotting Right whale density relative to CBump & wedge areas
ggplot(data = states) +
  geom_spatraster(data = spatRast, aes(fill = Layer_1)) +
  geom_polygon(aes(long, lat, group = group), color = "black", fill = "gray50") +
  geom_sf(data = CB_R, col = "red", fill = NA, alpha = 0.1, lwd = .5) +
  geom_sf(data = CB_M, col = "yellow", fill = NA, alpha = 0.1, lwd = .5) +
  geom_sf(data = wedge, col = "purple", fill = NA, alpha = 0.1, lwd = .5) +
  scale_fill_viridis(na.value = "transparent", labels = comma, option = "H") +
  labs(fill = "Density:") +
  coord_sf(xlim = c(-81, -76), ylim = c(31, 34)) + theme_bw() +
  theme(legend.key.height = unit(3, "cm"))

# Masking to ONLY the Monitoring area
wedgepts <- mask(vectDat, wedge)
mask <- mask(spatRast, mnitr)

# Plotting density w/ scale only relevent to points in CBump monitoring area
ggplot(data = states) +
  geom_spatraster(data = mask, aes(fill = Layer_1)) +
  geom_polygon(aes(long, lat, group = group), color = "black", fill = "gray50") +
  geom_sf(data = CB_M, col = "yellow", fill = NA, alpha = 0.1, lwd = .5) +
  geom_sf(data = wedge, col = "purple", fill = NA, alpha = 0.1, lwd = .5) +
  geom_sf(data = wedgepts, pch = 21, col = "white") +
  scale_fill_viridis(na.value = "transparent", labels = comma, option = "H") +
  labs(fill = "Density:") +
  coord_sf(xlim = c(-81, -76), ylim = c(31, 34)) + theme_bw() +
  theme(legend.key.height = unit(3, "cm"))