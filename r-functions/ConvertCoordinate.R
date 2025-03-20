ConvertCoordinate <- function (degrees=NULL, min=NULL, sec=NULL, decimal=NULL, verbose=FALSE) {
  
  if (is.null(degrees) & is.null(decimal)) {
    stop("Provide a coordinate in either degrees or decimal degrees.")
  } else if (!is.null(degrees) & !is.null(decimal)) {
    stop("Provide a coordinate only in deg-min-sec or decimal format.")
  }
  
  init_value <- ifelse(!is.null(degrees), paste0(degrees,"\u00B0",min,"'",sec,"\""), decimal)
  
  if (!is.null(degrees)) {
    
    # DEG-MIN-SEC to DECIMAL
    if (verbose) {cat("Converting coordinate from deg-min-sec to decimal degree... \n")}
    final.degrees <- degrees
    
    # Adds minute and second components, if provided
    if (!is.null(min)) {
      final.decimal <- min/60
      if (!is.null(sec)) {
        final.decimal <- final.decimal + sec/3600
      }
    }
    
    # Combining components
    final_value = ifelse(!is.null(min), final.degrees+final.decimal, final.degrees)
    
  } else if (!is.null(decimal)){
    
    # DECIMAL to DEG-MIN-SEC
    if (verbose) {cat("Converting coordinate from decimal degree to deg-min-sec... \n")}
    
    # Calculations
    dec.deg <- floor(decimal)
    dec.min <- floor((decimal %% 1) * 60)
    dec.sec <- round((((decimal %% 1)* 60) %% 1) * 60, 0)
    
    # Combine into final coordinate
    final_value = paste0(dec.deg, "\u00B0", dec.min, "'", dec.sec, "\"", sep = "")
  }
  
  # Print and return
  cat("Converted", init_value, "to", final_value, "\n")
  return(final_value)
}

#' ConvertCoordinate(degrees = 25, min = 23, sec = 15)
#' [1] 25.3875
#' 
#' ConvertCoordinate(decimal = 25.3875)
#' [1] 25°23'15"