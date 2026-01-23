#' Exploration of bandwidths calculated for x and y components of spatial data across all EFH species
#' and observations included in A17

# Setup ----
library(readxl)
library(sf)
library(terra)
library(tidyverse)

# Load all EFH data used in KDE
data <- readRDS("./data/consolidated-env-association-normalized.RDS")

# Sort
data_fmt <- data %>%
  arrange(COM_NAME, DATE)

# Pull out species names
species <- unique(data_fmt$COM_NAME)
species

# Convert LON and LAT to x/y coordinates in m
dat_sf <- data_fmt %>%
  mutate(Lon=LON, Lat=LAT) %>%
  vect(geom = c("LON", "LAT"), crs = "+proj=longlat +datum=WGS84") %>%
  st_as_sf() %>%
  st_transform(crs = "+proj=aea +lat_0=37.5 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs") %>%
  select(SPECIES_CODE, SRC, TID_SRC, COM_NAME, SCI_NAME, LIFESTAGE, Lon, Lat, geometry)

dat_sf <- dat_sf %>%
  mutate(X = unlist(map(dat_sf$geometry,1)),
         Y = unlist(map(dat_sf$geometry,2)))

dat_sf

bandwidths = data.frame(Species = species, 
                        Silv.X = NA, Silv.Y = NA, 
                        SJ.X = NA, SJ.Y = NA, 
                        UCV.X = NA, UCV.Y = NA)

# Calculate bandwidths for each species
for (spec in species) {
  
  message(spec)
  
  # Subset to species data
  subdat <- dat_sf %>% filter(COM_NAME == spec)
  head(data.frame(subdat) %>% select(-c(SPECIES_CODE,SRC,SCI_NAME)))
  
  if (nrow(subdat) < 100) {
    cat("Insufficient data for",spec,"\n")
  } else {
    # Silverman's Rule of Thumb
    bandwidths$Silv.X[match(spec, species)] = bw.nrd0(subdat$X)
    bandwidths$Silv.Y[match(spec, species)] = bw.nrd0(subdat$Y)
    # Sheather & Jones
    bandwidths$SJ.X[match(spec, species)]   = bw.SJ(subdat$X)
    bandwidths$SJ.Y[match(spec, species)]   = bw.SJ(subdat$Y)
    # Unbiased Cross Validation
    bandwidths$UCV.X[match(spec, species)]  = bw.ucv(subdat$X)
    bandwidths$UCV.Y[match(spec, species)]  = bw.ucv(subdat$Y)
  }
  
}

# Plot ----
ggplot(bandwidths) +
  geom_abline(slope = 1, intercept = 0, inherit.aes = FALSE, col = "gray90") +
  geom_point(mapping = aes(Silv.X, Silv.Y, col = "Silverman")) +
  geom_point(mapping = aes(SJ.X, SJ.Y, col = "SJones")) +
  geom_point(mapping = aes(UCV.X, UCV.Y, col = "CV")) +
  geom_point(data=data.frame(x=35935,y=31834), mapping = aes(x,y), shape = 21, fill = "green", size = 2) +
  geom_point(data=data.frame(x=37000,y=37000), mapping = aes(x,y), shape = 21, fill = "red", size = 2) +
  xlab("X Bandwidths") + ylab("Y Bandwidths") +
  theme_bw()

mean(c(bandwidths$Silv.X, bandwidths$SJ.X, bandwidths$UCV.X), na.rm = T)
mean(c(bandwidths$Silv.Y, bandwidths$SJ.Y, bandwidths$UCV.Y), na.rm = T)