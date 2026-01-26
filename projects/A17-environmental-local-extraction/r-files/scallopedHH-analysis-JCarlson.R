#' Follow up exploration of scalloped hammerhead depth data for J. Carlson

# Setup ----

library(mapproj)
library(marmap)
library(tidyverse)

data <- readRDS("./data/consolidated-env-association.RDS")

sort(unique(data$COM_NAME))

# Scalloped HH

shh <- data %>%
  filter(COM_NAME == "SCALLOPED_HAMMERHEAD",
         LIFESTAGE %in% c("JUV","ADU","UNK"))

# Plotting in GOM
shh %>%
  filter(REGION == "GOM") %>%
  ggplot(aes(LON, LAT)) +
  geom_point(cex = 0.5) +
  annotation_map(map_data("world"), col = "gray50", fill = "gray90") +
  coord_cartesian(xlim = c(-98, -78), ylim = c(15, 32), expand = FALSE) +
  theme_bw()

# Depth Histogram
shh %>%
  filter(REGION == "GOM", BATHYMETRY < 0) %>%
  mutate(DEPTH = BATHYMETRY * -1) %>%
  ggplot(aes(DEPTH)) +
  geom_histogram(binwidth = 50, center = 25) +
  xlim(0, 1500) +
  labs(title = "Scalloped Hammerhead (Gulf)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

# Smaller binwidth and better scaled to data
shh %>%
  filter(REGION == "GOM", BATHYMETRY < 0) %>%
  mutate(DEPTH = BATHYMETRY * -1) %>%
  ggplot(aes(DEPTH)) +
  geom_histogram(binwidth = 18, center = 9) +
  xlim(0, 500) +
  labs(title = "Scalloped Hammerhead (Gulf)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

# Distribution of depths (<100)
shh %>%
  filter(REGION == "GOM", BATHYMETRY < 0) %>%
  mutate(DEPTH = BATHYMETRY * -1) %>%
  ggplot(aes(DEPTH)) +
  geom_histogram(binwidth = 4, center = 2) +
  xlim(0, 100) +
  labs(title = "Scalloped Hammerhead (Gulf)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

# Separating out GULF data
shh_gulf <- data %>%
  filter(COM_NAME == "SCALLOPED_HAMMERHEAD", REGION == "GOM", BATHYMETRY < 0,
         LIFESTAGE %in% c("JUV","ADU","UNK")) %>%
  mutate(DEPTH = BATHYMETRY * -1)

table(shh_gulf$SRC)

quantile(shh_gulf$DEPTH, c(0.50, 0.75, 0.95))

shh_gulf %>%
  ggplot(aes(DEPTH)) +
  geom_histogram(binwidth = 10, center = 5) +
  xlim(0, 650) +
  labs(title = "Scalloped Hammerhead (Gulf)") +
  theme_bw()

nrowshh <- nrow(shh_gulf)

#' Cumulative percentage of observations within a certain DEPTH
#' 50 / 75 / 95 quantiles which show most of the observations are in < 650 m
shh_gulf %>%
  arrange(DEPTH) %>%
  mutate(VAL = 1) %>%
  mutate(CUMPCT = cumsum(VAL)/nrowshh) %>%
  select(DEPTH, VAL, CUMPCT) %>%
  ggplot(aes(DEPTH, CUMPCT)) +
  # Throws a warning but that's okay
  geom_segment(aes(x =  62, y = 0, xend =  62, yend=0.50), linewidth = 0.25, col = "gray80")+
  geom_segment(aes(x = 151, y = 0, xend = 151, yend=0.75), linewidth = 0.25, col = "gray80")+
  geom_segment(aes(x = 654, y = 0, xend = 654, yend=0.95), linewidth = 0.25, col = "gray80")+
  geom_line() +
  coord_cartesian(xlim = c(0, 1000), ylim = c(0,1), expand = FALSE) +
  labs(title = "Scalloped Hammerhead - JUV/ADU + UNK") +
  ylab("Cumulative Percentage") +
  theme_bw() +
  theme(panel.grid = element_blank())

# Getting bathy data
b <- getNOAA.bathy(lon1 = -100, lon2 = -75, lat1 = 15, lat2 = 35, resolution = 1)
bf <- fortify.bathy(b)

# Plots with bathymetry contours ----
## 50% (62m)
ggplot() +
  coord_cartesian(xlim = c(-98,-78), ylim = c(24, 32)) +
  geom_point(data = shh_gulf, mapping = aes(LON, LAT), size = 0.5, inherit.aes = FALSE, alpha = 0.5) +
  geom_contour(data = bf, aes(x,y,z=z), breaks = c(-62), col = "red") +
  annotation_map(map_data("world"), col = "gray65", fill = "gray85") +
  labs(title = "50% Quantile (62 m)") +
  theme_bw() + theme(panel.grid = element_blank())

## 75% (151m)
ggplot() +
  coord_cartesian(xlim = c(-98,-78), ylim = c(24, 32)) +
  geom_point(data = shh_gulf, mapping = aes(LON, LAT), size = 0.5, inherit.aes = FALSE, alpha = 0.5) +
  geom_contour(data = bf, aes(x,y,z=z), breaks = c(-151), col = "red") +
  annotation_map(map_data("world"), col = "gray65", fill = "gray85") +
  labs(title = "75% Quantile (151 m)") +
  theme_bw() + theme(panel.grid = element_blank())

## 95% (654m)
ggplot() +
  coord_cartesian(xlim = c(-98,-78), ylim = c(24, 32)) +
  geom_point(data = shh_gulf, mapping = aes(LON, LAT), size = 0.5, inherit.aes = FALSE, alpha = 0.5) +
  geom_contour(data = bf, aes(x,y,z=z), breaks = c(-654), col = "red") +
  annotation_map(map_data("world"), col = "gray65", fill = "gray85") +
  labs(title = "95% Quantile (654 m)") +
  theme_bw() + theme(panel.grid = element_blank())

## 700m
ggplot() +
  coord_cartesian(xlim = c(-98,-78), ylim = c(24, 32)) +
  geom_point(data = shh_gulf, mapping = aes(LON, LAT), size = 0.5, inherit.aes = FALSE, alpha = 0.5) +
  geom_contour(data = bf, aes(x,y,z=z), breaks = c(-700), col = "red") +
  annotation_map(map_data("world"), col = "gray65", fill = "gray85") +
  labs(title = "700m") +
  theme_bw() + theme(panel.grid = element_blank())

## 1000m
ggplot() +
  coord_cartesian(xlim = c(-98,-78), ylim = c(24, 32)) +
  geom_point(data = shh_gulf, mapping = aes(LON, LAT), size = 0.5, inherit.aes = FALSE, alpha = 0.5) +
  geom_contour(data = bf, aes(x,y,z=z), breaks = c(-1000), col = "red") +
  annotation_map(map_data("world"), col = "gray65", fill = "gray85") +
  labs(title = "1000m") +
  theme_bw() + theme(panel.grid = element_blank())

# 1300m
ggplot() +
  coord_cartesian(xlim = c(-98,-78), ylim = c(24, 32)) +
  geom_point(data = shh_gulf, mapping = aes(LON, LAT), size = 0.5, inherit.aes = FALSE, alpha = 0.5) +
  geom_contour(data = bf, aes(x,y,z=z), breaks = c(-1300), col = "red") +
  annotation_map(map_data("world"), col = "gray65", fill = "gray85") +
  labs(title = "1300m") +
  theme_bw() + theme(panel.grid = element_blank())