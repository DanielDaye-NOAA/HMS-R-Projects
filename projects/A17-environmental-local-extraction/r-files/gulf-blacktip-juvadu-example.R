#' Script to generate the figures included in the "EFH Methodology for Blacktip Shark" section in 
#' the A17 methods appendix

# Setup ----
library(ggplot2)
library(terra)
library(tidyterra)
library(tidyverse)

map.xlim = c(-98, -80)
map.ylim = c( 22,  32)

data <- readRDS("./data/consolidated-env-association-normalized.RDS")

sort(unique(data$COM_NAME))
sort(unique(data$REGION))

blacktip <- data %>%
  filter(COM_NAME == "BLACKTIP_SHARK")

# Plot data by region (ATL, GULF)
ggplot(blacktip, aes(LON, LAT, col = REGION)) +
  geom_point() +
  coord_cartesian(xlim = c(-100, -65), ylim = c(20, 45)) +
  theme_bw()

sort(unique(data$LIFESTAGE))

gulf_juvadu <- blacktip %>%
  filter(REGION == "GOM",
         LIFESTAGE %in% c("ADU","JUV","JUV-ADU"))

# Gulf JUVADU point data
gulf_juvadu %>%
  ggplot(aes(LON, LAT)) +
  geom_point(alpha = 0.1, size = 0.5) +
  annotation_map(map_data("world"), col = "gray80", fill = "gray90") +
  coord_sf(xlim = map.xlim, ylim = map.ylim) +
  xlab("Longitude") + ylab("Latitude") +
  theme_bw() +
  theme(panel.grid = element_blank())

cellsize = 120  # minutes

rast <- rast(xmin = map.xlim[1], xmax = map.xlim[2],
             ymin = map.ylim[1], ymax = map.ylim[2],
             ncol = (diff(map.xlim) * 60/cellsize),
             nrow = (diff(map.ylim) * 60/cellsize))

# Vectorizing data
vect <- vect(gulf_juvadu %>% mutate(Z = 1), geom = c("LON","LAT"))

# print(vect)
# plot(vect)

# Rasterizing
rast_data <- rasterize(vect, rast, field = "Z", fun = sum, na.rm = TRUE)
plot(rast_data)

unique(vect$SRC)

rast_uniq <- rasterize(vect, rast, field = "SRC", fun = function(x) length(unique(x)))
plot(rast_uniq)

# Applying rule of 3 so data from >= 3 sources is plotted
rast_data_anon <- rast_data
rast_data_anon[rast_uniq < 3] = NA

plot(rast_data)
plot(rast_uniq)
plot(rast_data_anon)

# Gulf Blacktip Effort, Rule of 3 ----
ggplot() +
  geom_spatraster(data = rast_data_anon) +
  annotation_map(map_data("world"), col = "gray50", fill = "gray90", lwd = .25) +
  coord_sf(xlim = map.xlim, ylim = map.ylim) +
  xlab("Longitude") + ylab("Latitude") +
  theme_bw() +
  labs(fill = "N. Obs.") +
  scale_fill_viridis_c(option = "mako", na.value = "transparent") +
  theme(panel.grid = element_blank(),
        plot.margin = margin(5,50,5,5))

# Gulf KDE + PVC Output ----
pvc50 <- vect("./data/gulf-blacktip-contours/app-output/A17_EFH_CCL_GOM_JUV-ADU_50_PVC_US_EEZ.shp")
pvc75 <- vect("./data/gulf-blacktip-contours/app-output/A17_EFH_CCL_GOM_JUV-ADU_75_PVC_US_EEZ.shp")
pvc95 <- vect("./data/gulf-blacktip-contours/app-output/A17_EFH_CCL_GOM_JUV-ADU_95_PVC_US_EEZ.shp")

# Plot of effort with 50%/75%/95% contours overlaid
ggplot() +
  geom_spatraster(data = rast_data_anon) +
  geom_spatvector(data = pvc95, aes(col = "95%"), fill = "yellow2", col = "yellow3", lwd = 0.1, inherit.aes = FALSE) +
  geom_spatvector(data = pvc75, aes(col = "75%"), fill = "orange2", col = "orange3", lwd = 0.1, inherit.aes = FALSE) +
  geom_spatvector(data = pvc50, aes(col = "50%"), fill = "red2", col = "red3", lwd = 0.1, inherit.aes = FALSE) +
  annotation_map(map_data("world"), col = "gray50", fill = "gray90", lwd = .25) +
  coord_sf(xlim = map.xlim, ylim = map.ylim) +
  xlab("Longitude") + ylab("Latitude") +
  theme_bw() + labs(col = "PVC") +
  geom_rect(aes(xmin=0,xmax=0,ymin=0,ymax=0,col = "95%"),fill="yellow2") +
  geom_rect(aes(xmin=0,xmax=0,ymin=0,ymax=0,col = "75%"),fill="orange2") +
  geom_rect(aes(xmin=0,xmax=0,ymin=0,ymax=0,col = "50%"),fill="red2") +
  guides(fill = "none", col = guide_legend(override.aes = list(col = c("red3","orange3","yellow3")))) +
  scale_fill_viridis_c(option = "mako", na.value = "transparent") +
  theme(panel.grid = element_blank(),
        plot.margin = margin(5,50,5,5))

# Finalized EFH Boundary ----
temp_EFH_boundary <- vect("./data/gulf-blacktip-contours/Blacktip-GOM-JUVADU-DM.shp")

# Checking everything, point data overlay
ggplot() +
  geom_spatraster(data = rast_data_anon) +
  geom_spatvector(data = pvc95, aes(col = "95%"), fill = "yellow2", lwd = 0.1, inherit.aes = FALSE) +
  geom_spatvector(data = pvc75, aes(col = "75%"), fill = "orange2", lwd = 0.1, inherit.aes = FALSE) +
  geom_spatvector(data = pvc50, aes(col = "50%"), fill = "red2", lwd = 0.1, inherit.aes = FALSE) +
  geom_spatvector(data = temp_EFH_boundary) +
  geom_point(data = gulf_juvadu, aes(LON,LAT), alpha = 0.1, size = 0.5) +
  annotation_map(map_data("world"), col = "gray50", fill = "gray90", lwd = .25) +
  coord_sf(xlim = map.xlim, ylim = map.ylim) +
  xlab("Longitude") + ylab("Latitude") +
  theme_bw() + labs(col = "PVC") +
  guides(fill = "none", col = guide_legend(override.aes = list(col = c("red3","orange3","yellow3")))) +
  scale_fill_viridis_c(option = "mako", na.value = "transparent") +
  theme(panel.grid = element_blank(),
        plot.margin = margin(5,50,5,5))

ggplot() +
  geom_spatvector(data = pvc75, mapping = aes(col = "75%"), fill = "orange2", lwd = 0.1, inherit.aes = FALSE) +
  geom_spatvector(data = temp_EFH_boundary, mapping = aes(col = "EFH"), fill = "blue2", lwd = 0.1, alpha = 0.3) +
  geom_blank(aes(color = "75%")) +
  annotation_map(map_data("world"), col = "gray50", fill = "gray90", lwd = .25) +
  coord_sf(xlim = map.xlim, ylim = map.ylim) +
  xlab("Longitude") + ylab("Latitude") +
  theme_bw() + labs(col = "PVC") +
  guides() +
  scale_color_manual(values = c("75%" = "orange3", "EFH" = "blue3")) +
  theme(panel.grid = element_blank(),
        plot.margin = margin(5,50,5,5))

range(gulf_juvadu$DATE)

# Environmental Data Plots ----
gulf_juvadu %>%
  mutate(`Depth (m)` = BATHYMETRY,
         `Mixed Layer Depth (m)` = MLD,
         `Sea Surface Temperature` = THETAO,
         `Bottom Temperature` = BT,
         `Sea Surface Salinity` = SSS,
         `Bottom Salinity` = BT) %>%
  pivot_longer(`Depth (m)`:`Bottom Salinity`, names_to = "VAR", values_to = "VAL") %>%
  mutate(VAR = factor(VAR, levels = c("Sea Surface Temperature","Sea Surface Salinity","Depth (m)",
                                      "Bottom Temperature","Bottom Salinity","Mixed Layer Depth (m)"))) %>%
  filter(!(VAR == "Depth (m)" & VAL < -150),
         !(VAR == "Mixed Layer Depth (m)" & VAL > 200)) %>%
  filter(!is.na(VAL)) %>%
  # GGPLOT
  ggplot(aes(VAL)) +
  geom_histogram(bins = 15) +
  facet_wrap(~VAR, scales = "free_x") +
  xlab("Value") + ylab("Count") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray80"),
        margins = margin(10,10,5,5),
        strip.text = element_text(size = 12))