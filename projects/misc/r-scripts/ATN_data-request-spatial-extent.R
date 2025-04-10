# Map to visualize the spatial extent of ATN data request

# Setup
library(ggplot2)
library(colorspace)

# Creating polygon borders for plotting
req_poly <- data.frame(lon = c(-100, -25, -25, -56, -100),
                       lat = c(  55,  55,  -5,  -5,   19))

# Plot
ggplot(req_poly , aes(lon, lat)) +
  geom_polygon(fill = alpha("red", 0.1), col = "red") +
  annotation_map(map_data("world"), aes(long, lat), col = "gray40", fill = "gray80") +
  geom_polygon(fill = NA, col = "red") +
  xlim(-100, -15) + ylim(-10, 60) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(color = darken("lightblue", .2)),
        panel.grid.minor = element_blank())