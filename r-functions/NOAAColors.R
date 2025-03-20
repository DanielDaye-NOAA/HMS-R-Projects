# Functions to Create and Display NOAA-branded Color Palette

NOAAPalette <- function () {
  palette <- data.frame(name = c("Process Blue", "Reflex Blue", "Navy Blue",
                                 "White", "Neutral Gray", "NA",
                                 "ocean1", "ocean2", "ocean3",
                                 "waves1", "waves2", "waves3",
                                 "seagrass1", "seagrass2", "seagrass3",
                                 "urchin1", "urchin2", "urchin3",
                                 "crustacean1", "crustacean2", "crustacean3",
                                 "coral1", "coral2", "coral3"),
                        hex = c("#0093D0","#0055A4","#00467F",
                                "#FFFFFF","#646464", NA,
                                "#00467F","#0055A4","#0093D0",
                                "#007078","#008998","#1ECAD3",
                                "#007934","#4C9C2E","#93D500",
                                "#575195","#625BC4","#7F7FFF",
                                "#BC4700","#D65F00","#FF8300",
                                "#B2292E","#D02C2F","#FF4438"))
  return(palette)
}

NOAAColor <- function (colname) {
  colorpalette <- NOAAPalette()
  if (!(colname %in% colorpalette$name)) {stop("Invalid NOAA Color")}
  return(colorpalette$hex[which(colorpalette$name == colname)])
}

DisplayNOAAPalette <- function () {
  palette <- NOAAPalette()
  grid <- data.frame(x1 = rep(1:3, nrow(palette)/3),
                     x2 = rep(2:4, nrow(palette)/3),
                     y1 = -1*sort(rep(1:(nrow(palette)/3), 3)),
                     y2 = -1*sort(rep(2:((nrow(palette)/3)+1), 3)))
  ggplot2::ggplot(grid, aes(x1, y1)) +
    geom_tile(width = 1, fill = palette$hex) +
    geom_text(mapping = aes(x = x1, y = y1, label = palette$name), 
              inherit.aes = FALSE, 
              col = c(rep("white", 3), "black", "white", "black", rep("white", 18))) +
    coord_cartesian(expand = FALSE) +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank())
}