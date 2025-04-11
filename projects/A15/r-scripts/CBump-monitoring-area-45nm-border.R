# Setup ----
library(rerddap)
library(rerddapXtracto)

border = data.frame(x = seq(-76.981, -80.445, length.out = 100),
                    y = seq( 34.000,  31.000, length.out = 100))

plot(y ~ x, border)

info("dist2coast_1deg")

dist <- rxtracto(info("dist2coast_1deg"), 
                 parameter = "dist", 
                 xcoord = border$x, 
                 ycoord = border$y)

dist$`mean dist`/1.852

points(y ~ x, data = border[dist$`mean dist`/1.852 < 45,], col = "red")

min(dist$`mean dist`/1.852)  # 28.61771