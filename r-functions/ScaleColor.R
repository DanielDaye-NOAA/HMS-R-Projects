# Custom Scale Color for Discrete and Continuous Categories

scale_color <- function(rev = FALSE, discrete = TRUE) {
  
  # create pallete (replace this with whichever pallete is coming from nmfspalette)
  newpal <- c("#FF4438","#FF8300","#93D500","#1ECAD3","#0093D0","#7F7FFF")
  
  # reverse if TRUE
  if (rev) {newpal = rev(newpal)}
  
  # create colorRampPalette
  crp <- colorRampPalette(newpal)
  
  #  use scale_color_manual for discrete, scale_color_gradientn for interpolated 
  if (discrete) {
    scale_color_manual(values = newpal)
  } else {
    scale_color_gradientn(colors = crp(256))
  }
}