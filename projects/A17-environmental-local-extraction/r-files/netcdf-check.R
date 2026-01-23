#' NetCDF file exploration for A17 environmental association data

library(ncdf4)

f1 <- "D:/A17-EFH-environmental-data/CMEMS/GlobColour-chlorophyll/"
f2 <- "D:/A17-EFH-environmental-data/CMEMS/GlobColour-turbidity/"
f3 <- "D:/A17-EFH-environmental-data/CMEMS/GLORYS12V1-surface/"
f4 <- "D:/A17-EFH-environmental-data/HYCOM/"

CheckFiles <- function(path) {
  list.files(path)
  files <- list.files(path)
  for (file in files) {
    print(file)
    nc_open(paste0(path,file))
  }
}

CheckFiles(f1)
CheckFiles(f2)
CheckFiles(f3)
CheckFiles(f4)
