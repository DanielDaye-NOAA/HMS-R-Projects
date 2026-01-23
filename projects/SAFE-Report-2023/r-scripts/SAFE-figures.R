# SAFE Report Figure Updates

library(maps)

# Fig. 6.3 ----

lims <- data.frame(x = c(-100, -20),
                   y = c(   0,  55))
basemap <- map_data("world")

{par(oma = c(1, 1, 1, 1), mar = c(1, 1, 1, 1))
  plot(NULL, xlim = lims$x, ylim = lims$y, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n")
  map("world", fill = FALSE, add = TRUE, interior = FALSE, lwd = 2,
      xlim = lims$x, ylim = lims$y, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n")
  axis(side = 1, at = seq(-95, -20, by = 5), 
       lwd = 2)
  axis(side = 2, at = seq(5, 55, by = 5), 
       las = 1, lwd = 2)
  axis(side = 3, at = c(-82, -78, -71, -65, -60), 
       labels = c(-82, -78, -71, -65, -60), 
       lwd = 2)
  axis(side = 4, at = c(5, 13, 35), las = 1, lwd = 2)
  text(-99, 53.5-(0:11*1.9), adj = 0,
       labels = c("Caribbean (CAR)",
                  "Gulf of Mexico (GOM)",
                  "Florida East Coast (FEC)",
                  "South Atlantic Bight (SAB)",
                  "Mid Atlantic Bight (MAB)",
                  "Northeast Coastal (NEC)",
                  "Northeast Distant (NED)",
                  "Sargasso (SAR)",
                  "North Central Atlantic (NCA)",
                  "Tuna North (TUN)",
                  "Tuna South (TUS)"),
       cex = 1)
  text(c(73, 65.5, 40,   75.5, 90, 75.5, 65.5, 40, 75, 40, 40)*-1,
       c(38,   40, 40,   32.5, 25,   28,   28, 16, 15,  7,  2),
       adj = 0.5,
       labels = c("MAB\n(92)", "NEC\n(92)", "NED\n(94)",
                  "SAB\n(92)",
                  "GOM\n(91)", "FEC\n(92)", "SAR\n(93)", "NCA\n(93)",
                  "CAR (93)", "TUN (93)", "TUS (96)"))
  segments(x0 = c(65, 71, 78, 82, 82, 87, 60, 52)*-1,
           x1 = c(60, 65, 71, 20, 71, 60, 20, 20)*-1,
           y0 = c(50, 45, 43, 35, 30, 22, 13, 5),
           y1 = c(50, 45, 43, 35, 30, 22, 13, 5),
           lwd = 2)
  segments(x0 = c(82, 78, 71, 65, 60)*-1,
           x1 = c(82, 78, 71, 65, 60)*-1,
           y0 = c(35, 43, 45, 50, 55),
           y1 = c(22, 35, 22, 45, 8),
           lwd = 2)
  box(which = "plot", lwd = 2)}