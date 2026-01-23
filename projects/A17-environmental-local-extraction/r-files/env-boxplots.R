#' Script to generate box and whisker plots in /figures/

library(tidyverse)

# Data Visualization
data <- readRDS("consolidated-env-association-normalized.RDS") %>%
  mutate(COM_NAME = ifelse(COM_NAME == "CARCHARHINUS_POROSUS", "SMALLTAIL_SHARK", COM_NAME))

group_order <- c("ALBACORE","BIGEYE_TUNA","BLUEFIN_TUNA","SKIPJACK_TUNA","YELLOWFIN_TUNA",
                 "SWORDFISH",
                 "BLUE_MARLIN","WHITE_MARLIN","ROUNDSCALE_SPEARFISH","SAILFISH","LONGBILL_SPEARFISH",
                 "BLACKTIP_SHARK","BULL_SHARK","GREAT_HAMMERHEAD","LEMON_SHARK","NURSE_SHARK","SANDBAR_SHARK",
                 "SCALLOPED_HAMMERHEAD","SILKY_SHARK","SMOOTH_HAMMERHEAD","CAROLINA_HAMMERHEAD","SPINNER_SHARK",
                 "TIGER_SHARK",
                 "BLACKNOSE_SHARK","BONNETHEAD_SHARK","FINETOOTH_SHARK","ATLANTIC_SHARPNOSE",
                 "BLUE_SHARK","OCEANIC_WHITETIP_SHARK","PORBEAGLE","SHORTFIN_MAKO_SHARK","COMMON_THRESHER_SHARK",
                 "SMOOTH_DOGFISH","SMOOTHHOUND","FLORIDA_SMOOTHHOUND","GULF_SMOOTHHOUND",
                 "ATLANTIC_ANGEL","BASKING_SHARK","BIGEYE_SIXGILL","BIGEYE_THRESHER_SHARK","BIGNOSE_SHARK",
                 "CARIBBEAN_REEF_SHARK","CARIBBEAN_SHARPNOSE","DUSKY_SHARK","GALAPAGOS_SHARK","LONGFIN_MAKO_SHARK",
                 "NARROWTOOTH_SHARK","NIGHT_SHARK","SAND_TIGER_SHARK","SHARPNOSE_SEVENGILL","BLUNTNOSE_SIXGILL",
                 "SMALLTAIL_SHARK","WHALE_SHARK","WHITE_SHARK")


groups = c(rep("Tunas", 5), "Swordfish", rep("Billfishes", 5), rep("LCS", 12), rep("SCS", 4),
           rep("Pelagics", 5), rep("Smoothhounds", 4), rep("Prohibited", 18))
grpfil = c(rep("lightblue", 5), "gray", rep("orange", 5), rep("violet", 12), rep("cyan", 4),
           rep("lightblue3", 5), rep("gray", 4), rep("pink2", 18))

# Species boxplots ----

## Depth ----
data %>%
  mutate(DEPTH = abs(BATHYMETRY),
         COM_NAME = factor(COM_NAME, levels = rev(group_order))) %>%
  ggplot(aes(COM_NAME, DEPTH)) +
  xlab("") + ylab("Depth (m)") +
  geom_boxplot(col = "gray25", fill = rev(grpfil), outliers = FALSE) + 
  scale_y_log10() + coord_flip() +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

## Rugosity ----
data %>%
  mutate(COM_NAME = factor(COM_NAME, levels = rev(group_order))) %>%
  ggplot(aes(COM_NAME, RUGOSITY)) +
  xlab("") + ylab("Rugosity") +
  geom_boxplot(col = "gray25", fill = rev(grpfil), outliers = FALSE) + 
  coord_flip() +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

## SST ----
data %>%
  mutate(COM_NAME = factor(COM_NAME, levels = rev(group_order))) %>%
  ggplot(aes(COM_NAME, THETAO)) +
  xlab("") + ylab("SST (C)") +
  geom_boxplot(col = "gray25", fill = rev(grpfil), outliers = FALSE) + 
  coord_flip() +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

## Bottom T ----
data %>%
  mutate(COM_NAME = factor(COM_NAME, levels = rev(group_order))) %>%
  ggplot(aes(COM_NAME, BT)) +
  xlab("") + ylab("Bottom Temp (C)") +
  geom_boxplot(col = "gray25", fill = rev(grpfil), outliers = FALSE) + 
  coord_flip() +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

## SSS ----
data %>%
  mutate(COM_NAME = factor(COM_NAME, levels = rev(group_order))) %>%
  ggplot(aes(COM_NAME, SSS)) +
  xlab("") + ylab("Sea Surface Salinity (psu)") +
  geom_boxplot(col = "gray25", fill = rev(grpfil), outliers = FALSE) + 
  coord_flip() +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

## Bottom salinity ----
data %>%
  mutate(COM_NAME = factor(COM_NAME, levels = rev(group_order))) %>%
  ggplot(aes(COM_NAME, BS)) +
  xlab("") + ylab("Bottom Salinity (psu)") +
  geom_boxplot(col = "gray25", fill = rev(grpfil), outliers = FALSE) + 
  coord_flip() +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

## MLD ----
data %>%
  mutate(COM_NAME = factor(COM_NAME, levels = rev(group_order))) %>%
  ggplot(aes(COM_NAME, MLD)) +
  xlab("") + ylab("Mixed Layer Depth (m)") +
  geom_boxplot(col = "gray25", fill = rev(grpfil), outliers = FALSE) + 
  coord_flip() +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

## CHLa ----
data %>%
  mutate(COM_NAME = factor(COM_NAME, levels = rev(group_order))) %>%
  ggplot(aes(COM_NAME, CHLA)) +
  xlab("") + ylab("Chlorophyll-a (mg/m^3)") +
  geom_boxplot(col = "gray25", fill = rev(grpfil), outliers = FALSE) + 
  coord_flip() +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

## Secchi depth ----
data %>%
  mutate(COM_NAME = factor(COM_NAME, levels = rev(group_order))) %>%
  ggplot(aes(COM_NAME, TURB)) +
  xlab("") + ylab("Secchi Disk Depth") +
  geom_boxplot(col = "gray25", fill = rev(grpfil), outliers = FALSE) + 
  coord_flip() +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

## SSH ----
data %>%
  mutate(COM_NAME = factor(COM_NAME, levels = rev(group_order))) %>%
  ggplot(aes(COM_NAME, ZOS)) +
  xlab("") + ylab("Sea Surface Height") +
  geom_boxplot(col = "gray25", fill = rev(grpfil), outliers = FALSE) + 
  coord_flip() +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

# Raster Plots ----
library(ncdf4)
library(raster)
library(terra)
library(tidyterra)

# Depth ----
depth <- ncdf4::nc_open("./envdata/etopo1_bedrock.nc")
depth.lonlat <- expand.grid(lon = ncvar_get(depth, "lon"), lat = ncvar_get(depth, "lat"))
depth.ext <- extent(c(min(ncvar_get(depth, "lon")), max(ncvar_get(depth, "lon")),
                      min(ncvar_get(depth, "lat")), max(ncvar_get(depth, "lat"))))
depth.rast <- raster(depth.ext, nrow = 2791, ncol = 3661, 
                       crs = "+proj=lonlat +datum=WGS84 +no_defs")
depth.raster <- rasterize(depth.lonlat, depth.rast, ncvar_get(depth, "Band1"), fun = mean)
plot(depth.raster)

ggplot() + 
  geom_spatraster(data = rast(depth.raster), maxcell = 1e6) +
  coord_sf(xlim = c(-98, -37), ylim = c(5, 51.5), expand = FALSE) +
  scale_fill_viridis_c(limits = c(-8500, 0)) + labs(fill = "Depth (m)") +
  annotation_map(map_data("world"), col = "gray60", fill = "gray75") +
  theme_bw()

# Rugosity
rugos.raster <- focal(depth.raster, w = matrix(1, nrow = 15, ncol = 15),
                      fun = sd, na.rm = TRUE, pad = TRUE)
plot(rugos.raster)
ggplot() + 
  geom_spatraster(data = rast(rugos.raster), maxcell = 1e6) +
  coord_sf(xlim = c(-98, -37), ylim = c(5, 51.5), expand = FALSE) +
  scale_fill_viridis_c() + labs(fill = "Rugosity") +
  annotation_map(map_data("world"), col = "gray60", fill = "gray75") +
  theme_bw()










