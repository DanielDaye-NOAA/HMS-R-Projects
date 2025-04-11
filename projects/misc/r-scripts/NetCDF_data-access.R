# Extracting NetCDF Data
library(ncdf4)
library(maps)
library(raster)
library(RColorBrewer)

# Open nc file
nc_data <- ncdf4::nc_open("data/HYCOM-test/expt_93.0_202001.nc")
print(nc_data)

# Extract coordinate data and attribute data
lon <- ncvar_get(nc_data, "lon")
range(lon)
lon <- lon - 360
ncatt_get(nc_data, "lon", "units")
lat <- ncvar_get(nc_data, "lat")
ncatt_get(nc_data, "lat", "units")
t <- ncvar_get(nc_data, "time")
ncatt_get(nc_data, "time", "units")

# Water Temp Data
array <- ncvar_get(nc_data, "water_temp")
ncatt_get(nc_data, "water_temp", "units")
dim(array)
ncatt_get(nc_data, "water_temp", "_FillValue")

nc_close(nc_data)

slice <- array[,,1,1]
plot(slice)
image(lon, lat, slice, col = rev(brewer.pal(10, "RdBu")))
map(database = "world", add = T, fill = T, col = "gray80")
map.axes()    