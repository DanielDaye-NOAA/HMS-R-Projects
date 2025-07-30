# Batch Environmental Association Script
# Daniel Daye
# Finalized EFH version (2025-JUN)

#library(doParallel)
#library(doSNOW)
#library(ncdf4)
#library(raster)
#library(sp)
#library(tidyverse)


# TESTING
# dir_SF <- "G:/SF1/EFH/2024-2026_A17 EFH Analysis/R_project/Environmental_Association_Data/"
# data <- read_csv(paste0(dir_SF,"A17_BFT_2010-2024-AUG.csv"), guess_max = 1e6)
# dir_BATHY <- "D:/A17-EFH-environmental-data/"
# dir_CMEMS <- "D:/A17-EFH-environmental-data/CMEMS/"
# dir_HYCOM <- "D:/A17-EFH-environmental-data/HYCOM/"
# dir_outfiles <- "D:/A17-EFH-environmental-data/outfiles/"

BatchEnvAssign <- function (data, verbose = TRUE, dir_BATHY, dir_CMEMS, dir_HYCOM, dir_outfiles) {
  print(getwd())
  if (verbose) {print("verbose: TRUE")}
  sysTime.in <- Sys.time()
  
  data <- data %>%
    mutate(DAY = substr(DATE, 9,10),
           # DAY = ifelse(nchar(DAY) == 1, paste0("0",DAY), DAY),
           DATE = as.POSIXct(paste0(YEAR,"-",MONTH,"-",DAY),
                             format = "%Y-%m-%d", tz = "UTC")) %>%
    dplyr::arrange(DATE)
  
  table(data$YEAR, data$MONTH)
  
  # Make into spdf
  spdf <- subset(data, select = c(LON, LAT))
  colnames(spdf) <- c("lon", "lat")
  coordinates(spdf) <- ~lon + lat
  proj4string(spdf) <- CRS("+proj=lonlat +datum=WGS84 +no_defs")
  
  
  # Pre-Loop Processing ----
  ## BATHYMETRY ----
  if (verbose) {print("Opening bathymetry .nc file")}
  nc_depth <- nc_open(paste0(dir_BATHY,"etopo1_bedrock.nc"))
  depth.lon <- ncvar_get(nc_depth, "lon")
  depth.lat <- ncvar_get(nc_depth, "lat")
  depth.bat <- ncvar_get(nc_depth, "Band1")
  
  nc_close(nc_depth)
  
  d.lonlat <- expand.grid(lon = depth.lon, lat = depth.lat)
  coordinates(d.lonlat) <- ~lon + lat
  proj4string(d.lonlat) <- CRS("+proj=lonlat +datum=WGS84 +no_defs")
  d.ext <- extent(c(min(depth.lon), max(depth.lon), min(depth.lat), max(depth.lat)))
  d.rast <- raster(d.ext, nrow = 2791, ncol = 3661, # length(depth.lat), length(depth.lon)
                   crs = "+proj=lonlat +datum=WGS84 +no_defs")
  
  if (verbose) {print("rasterizing bathymetry .nc file")}
  depth.raster <- rasterize(d.lonlat, d.rast, depth.bat, fun = mean)
  
  message("Extracting associated depth data...")
  depth.val <- raster::extract(depth.raster, spdf)
  data$BATHYMETRY <- depth.val
  
  if (verbose) {print("Bathymetry data extracted, unloading files")}
  rm(nc_depth, depth.lon, depth.lat, depth.bat, 
     d.lonlat, d.ext, d.rast, depth.val)
  gc()
  
  
  ## RUGOSITY ----
  if (verbose) {print("Calculating rugosity")}
  rugos.raster <- focal(depth.raster, w = matrix(1, nrow = 15, ncol = 15),
                        fun = sd, na.rm = TRUE, pad = TRUE)
  message("Extracting associated rugosity data...")
  rugos.val <- raster::extract(rugos.raster, spdf)
  data$RUGOSITY <- rugos.val
  
  if (verbose) {print("Rugosity data extracted, unloading files")}
  rm(depth.raster, rugos.raster, rugos.val)
  gc()
  
  
  ## HYCOM ----
  #' Reference raster is generated using the HYCOM_surface (SST & SSS) files,
  #' but the extent and resolution are the same for HYCOM_bottom_ssh (SSH, BT, BS)
  if (verbose) {print("Initializing Batch Loop Reference for: SST, SSS, SSH, BT, BS")}
  data <- data %>%
    mutate(SST = NA, SSS = NA,
           SSH = NA, BT = NA, BS = NA)
  
  if (verbose) {print("Opening HYCOM surface reference .nc file")}
  # ref.HYCOM <- nc_open("environmentalData/HYCOM_surface/expt_53.X_200601_surface.nc")
  # ref.HYCOM <- nc_open("D:/Crear/Environmental_Data/Hycom/surface_1994_2020/expt_53.X_199401_surface.nc")
  ref.HYCOM <- nc_open(paste0(dir_HYCOM, "expt_53.X_2010-01-01_vertical.nc"))
  ref.hlon <- ncvar_get(ref.HYCOM, "lon")
  ref.hlat <- ncvar_get(ref.HYCOM, "lat")
  nc_close(ref.HYCOM)
  
  ref.hlon_lat <- expand.grid(lon = ref.hlon, lat = ref.hlat)
  coordinates(ref.hlon_lat) <- ~lon + lat
  proj4string(ref.hlon_lat) <- CRS("+proj=lonlat +datum=WGS84 +no_defs")
  ref.h.ext <- extent(c(min(ref.hlon), max(ref.hlon), min(ref.hlat), max(ref.hlat))) + 12
  ref.h.rast <- raster(ref.h.ext, nrow = 1001, ncol = 1001,
                       crs = "+proj=longlat +datum=WGS84 +no_defs")
  
  if (verbose) {print("HYCOM reference info extracted; unloading files")}
  # Keep ref.hlon_lat & ref.h.rast
  rm(ref.HYCOM, ref.h.ext)
  
  
  ## CMEMS (MLD) ----
  if (verbose) {print("Initializing Batch Loop Reference for: MLD")}
  data <- data %>%
    mutate(MLD = NA, ZOS = NA, THETAO = NA)
  
  if (verbose) {print("Opening CMEMS GLORYS (MLD) reference .nc file")}
  # ref.CMEMS <- nc_open("environmentalData/CMEMS_GLORYS/CMEMS_GLORYS12_zos-uo-vo-mlotst-dailymean_monthstack_boundingbox1_2006-01.nc")## ADD
  # ref.CMEMS <- nc_open("D:/Crear/Environmental_Data/CopernicusMarineData/GLORYS12_1993_2019/CMEMS_GLORYS12_zos-uo-vo-mlotst-dailymean_monthstack_boundingbox1_2006-01.nc")
  ref.CMEMS <- nc_open(paste0(dir_CMEMS,"GLORYS12V1-surface/CMEMS-GLORYS12V1-surface_zos-uo-vo-mlotst-thetao_2010-01.nc"))
  ref.clon <- ncvar_get(ref.CMEMS, "longitude")
  ref.clat <- ncvar_get(ref.CMEMS, "latitude")
  nc_close(ref.CMEMS)
  
  ref.clon_lat <- expand.grid(lon = ref.hlon, lat = ref.hlat)
  coordinates(ref.clon_lat) <- ~lon + lat
  proj4string(ref.clon_lat) <- CRS("+proj=lonlat +datum=WGS84 +no_defs")
  ref.c.ext <- extent(c(min(ref.clon), max(ref.clon), min(ref.clat), max(ref.clat)))
  ref.c.rast <- raster(ref.c.ext, nrow = 721, ncol = 961,
                       crs = "+proj=longlat +datum=WGS84 +no_defs")
  
  if (verbose) {print("CMEMS GLORYS reference info extracted; unloading files")}
  # Keep ref.clon_lat & ref.c.rast
  rm(ref.CMEMS, ref.c.ext)
  
  
  ## CMEMS (TURB) ----
  if (verbose) {print("Initializing Batch Loop Reference for: TURB")}
  data <- data %>%
    mutate(TURB = NA)
  
  if (verbose) {print("Opening CMEMS TURB reference .nc file")}
  # ref.CTURB <- nc_open("environmentalData/CMEMS_TURB/CMEMS_TURBID_zsb-dailymean_monthstack_boundingbox1_2006-01.nc")
  # ref.CTURB <- nc_open("D:/Crear/Environmental_Data/CopernicusMarineData/Turbidity_1997_2019/CMEMS_TURBID_zsb-dailymean_monthstack_boundingbox1_1997-10.nc")
  ref.CTURB <- nc_open(paste0(dir_CMEMS,"GlobColour-turbidity/CMEMS-GlobColour-turbid_ZSD-ZSD_uncertainty_2010-01.nc"))
  ref.tlon <- ncvar_get(ref.CTURB, "longitude")
  ref.tlat <- ncvar_get(ref.CTURB, "latitude")
  nc_close(ref.CTURB)
  
  ref.tlon_lat <- expand.grid(lon = ref.tlon, lat = ref.tlat)
  coordinates(ref.tlon_lat) <- ~lon + lat
  proj4string(ref.tlon_lat) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  ref.t.ext <- extent(c(min(ref.tlon), max(ref.tlon), min(ref.tlat), max(ref.tlat)))
  ref.t.rast <- raster(ref.t.ext, nrow = 1440, ncol = 1920, 
                       crs = "+proj=longlat +datum=WGS84 +no_defs")
  
  if (verbose) {print("CMEMS TURB reference info extracted; unloading files")}
  # Keep ref.tlon_lat & ref.t.rast
  rm(ref.CTURB, ref.t.ext)
  
  
  ## CMEMS (CHLA) ----
  if (verbose) {print("Initializing Batch Loop Reference for: CHLA")}
  data <- data %>%
    mutate(CHLA = NA)
  
  if (verbose) {print("Opening CMEMS CHLA reference .nc file")}
  ref.CHLA <- nc_open(paste0(dir_CMEMS,"GlobColour-chlorophyll/CMEMS-GlobColour-chloro_CHL-CHL_uncertainty_2010-01.nc"))
  ref.chllon <- ncvar_get(ref.CHLA, "longitude")
  ref.chllat <- ncvar_get(ref.CHLA, "latitude")
  nc_close(ref.CHLA)
  
  ref.chllon_lat <- expand.grid(lon = ref.chllon, lat = ref.chllat)
  coordinates(ref.chllon_lat) <- ~lon + lat
  proj4string(ref.chllon_lat) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  ref.chl.ext <- extent(c(min(ref.chllon), max(ref.chllon), min(ref.chllat), max(ref.chllat)))
  ref.chl.rast <- raster(ref.chl.ext, nrow = 1440, ncol = 1920,
                         crs = "+proj=longlat +datum=WGS84 +no_defs")
  
  if (verbose) {print("CMEMS CHLA reference info extracted; unloading files")}
  # Keep ref.chllon_lat & ref.chl.rast
  rm(ref.CHLA, ref.chl.ext)
  
  
  ## CHLA (OLD) ----
  #if (verbose) {print("Initializing Batch Loop Reference for: CHLA")}
  #data <- data %>%
  #  mutate(CHLA = NA)
  #
  #if (verbose) {print("Stacking CHLA grid...")}
  #chla_stack <- stack("environmentalData/CHLA/chlastack.grd")
  #chla_layer_names <- names(chla_stack)
  #chla_names_split <- unlist(strsplit(chla_layer_names, "chla."))
  #chla_names <- chla_names_split[seq(2, length(chla_names_split), by = 2)] %>%
  #  as.POSIXct(format = "%Y.%m.%d", tz = "UTC")
  
  
  # Setting up YEAR_MO batches for foreach
  data$YEAR_MO <- paste0(data$YEAR,"_",data$MONTH)
  data <- data %>% mutate(YEAR_MO = ifelse(as.character(DATE) == "2021-06-30", "2021_07", YEAR_MO)) # %>% filter(as.character(DATE)=="2021-08-01") %>% View()
  yearMo_batches <- unique(data$YEAR_MO)
  table(data$YEAR_MO)
  if (verbose) {
    print(paste(length(yearMo_batches), "unique YEAR_MO batches to be processed"))
  }
  
  print("BEGINNING FOREACH LOOPS ------")
  
  
  # Month Loop ----
  # i = 1
  filled_batches <- foreach (i = 1:length(yearMo_batches), .packages=c("ncdf4","raster","tidyverse")) %dopar% {
    capture.output({
      ind_batch <- which(data$YEAR_MO == yearMo_batches[i])
      b.id <- paste0("[B.",i,"] ")
      
      print(paste("Batch Loop", i, "of", length(yearMo_batches)))
      print(paste(length(ind_batch), "entries for", yearMo_batches[i]))
      
      sel.yr <- strsplit(yearMo_batches[i], split = "_")[[1]][1]
      sel.mn <- strsplit(yearMo_batches[i], split = "_")[[1]][2]
      if (nchar(sel.mn) == 1) {sel.mn = paste0("0",sel.mn)}
      
      
      # [M] HYCOM Surface ----
      files.HYCOM.surf <- dir(path = dir_HYCOM, pattern = ".nc")
      fileP.HYCOM.surf <- files.HYCOM.surf[grepl(sprintf(paste(sel.yr, sel.mn, "01_vertical", sep="-")),
                                                 files.HYCOM.surf)]
      
      # Load 
      if (verbose) {print(paste0(dir_HYCOM, fileP.HYCOM.surf))}
      nc_HYCOM_surf <- nc_open(paste0(dir_HYCOM, fileP.HYCOM.surf))
      nc_SST <- ncvar_get(nc_HYCOM_surf, "water_temp")
      nc_SSS <- ncvar_get(nc_HYCOM_surf, "salinity")
      nc_HyS_time <- (ncvar_get(nc_HYCOM_surf, "time")*3600) %>%
        as.POSIXct(origin="2000-01-01 00:00:00", tz="UTC") %>%
        strftime(., format = "%Y-%m-%d", tz = "UTC") %>%
        as.POSIXct(tz = "UTC")
      
      
      # [M] HYCOM Bottom ----
      files.HYCOM.bott <- dir(path = dir_HYCOM, pattern = ".nc")
      fileP.HYCOM.bott <- files.HYCOM.bott[grepl(sprintf(paste(sel.yr, sel.mn, "01_non-vert", sep="-")),
                                                 files.HYCOM.bott)]
      
      # Load
      if (verbose) {print(paste0(dir_HYCOM, fileP.HYCOM.bott))}
      nc_HYCOM_bott <- nc_open(paste0(dir_HYCOM, fileP.HYCOM.bott))
      nc_SSH <- ncvar_get(nc_HYCOM_bott, "surf_el")
      nc_BT  <- ncvar_get(nc_HYCOM_bott, "water_temp_bottom")
      nc_BS  <- ncvar_get(nc_HYCOM_bott, "salinity_bottom")
      nc_HyB_time <- (ncvar_get(nc_HYCOM_bott, "time")*3600) %>%
        as.POSIXct(origin="2000-01-01 00:00:00", tz="UTC") %>%
        strftime(., format = "%Y-%m-%d", tz = "UTC") %>%
        as.POSIXct(tz = "UTC")
      
      
      # [M] CMEMS MLD ----
      path.CMEMS <- paste0(dir_CMEMS,"GLORYS12V1-surface/")
      files.CMEMS <- dir(path = path.CMEMS, pattern = ".nc")
      fileP.CMEMS <- files.CMEMS[grepl(sprintf(paste(sel.yr, sel.mn, sep = "-")),
                                       files.CMEMS)]
      
      # Load
      if (verbose) {print(paste0(path.CMEMS, fileP.CMEMS))}
      nc_CMEMS <- nc_open(paste0(path.CMEMS, fileP.CMEMS))
      nc_MLD <- ncvar_get(nc_CMEMS, "mlotst")
      nc_ZOS <- ncvar_get(nc_CMEMS, "zos")
      nc_THETAO <- ncvar_get(nc_CMEMS, "thetao")
      nc_CMS_time <- (ncvar_get(nc_CMEMS, "time")*3600) %>%
        as.POSIXct(origin="1950-01-01 00:00:00", tz="UTC") %>%
        strftime(., format = "%Y-%m-%d", tz = "UTC") %>%
        as.POSIXct(tz = "UTC")
      
      
      # [M] CMEMS TURB ----
      path.CTURB <- paste0(dir_CMEMS,"GlobColour-turbidity/")
      files.CTURB <- dir(path = path.CTURB, pattern = ".nc")
      fileP.CTURB <- files.CTURB[grepl(sprintf(paste(sel.yr, sel.mn, sep = "-")),
                                       files.CTURB)]
      
      # Load
      if (verbose) {print(paste0(path.CTURB, fileP.CTURB))}
      nc_CTURB <- nc_open(paste0(path.CTURB, fileP.CTURB))
      nc_TUR <- ncvar_get(nc_CTURB, "ZSD")
      nc_TUR_time <- (ncvar_get(nc_CTURB, "time") * 86400) %>%
        as.POSIXct(origin="1900-01-01 00:00:00", tz="UTC") %>%
        strftime(., format = "%Y-%m-%d", tz = "UTC") %>%
        as.POSIXct(tz = "UTC")
      
      
      # [M] CMEMS CHLA ----
      path.CCHLA <- paste0(dir_CMEMS,"GlobColour-chlorophyll/")
      files.CCHLA <- dir(path = path.CCHLA, pattern = ".nc")
      fileP.CCHLA <- files.CCHLA[grepl(sprintf(paste(sel.yr, sel.mn, sep = "-")),
                                       files.CCHLA)]
      
      # Load
      if (verbose) {print(paste0(path.CCHLA, fileP.CCHLA))}
      nc_CCHLA <- nc_open(paste0(path.CCHLA, fileP.CCHLA))
      nc_CHL <- ncvar_get(nc_CCHLA, "CHL")
      nc_CHL_time <- (ncvar_get(nc_CCHLA, "time") * 86400) %>%
        as.POSIXct(origin="1900-01-01 00:00:00", tz="UTC") %>%
        strftime(., format = "%Y-%m-%d", tz = "UTC") %>%
        as.POSIXct(tz = "UTC")
      
      # Day Loop ----    
      dayBatch <- unique(data$DAY[ind_batch])
      for (j in 1:length(dayBatch)) {
        sel.dy <- which(data$DAY[ind_batch] == dayBatch[j])
        caredate = unique(data$DATE[ind_batch][sel.dy])
        
        print(paste0(b.id, j, " of ", length(dayBatch), " (",
                     length(sel.dy)," entries)"))
        print(paste0(b.id, "caredate: ", caredate, " UTC"))
        
        # [D] HYCOM Surface ----
        print("HYCOM surface")
        timeloc.HyS <- which(nc_HyS_time == caredate)
        raster.HyS.SST <- rasterize(ref.hlon_lat, ref.h.rast, nc_SST[,,timeloc.HyS], fun = mean)
        raster.HyS.SSS <- rasterize(ref.hlon_lat, ref.h.rast, nc_SSS[,,timeloc.HyS], fun = mean)
        
        extVal_SST <- raster::extract(raster.HyS.SST, spdf[ind_batch][sel.dy])
        extVal_SSS <- raster::extract(raster.HyS.SSS, spdf[ind_batch][sel.dy])
        
        data$SST[ind_batch][sel.dy] <- extVal_SST
        data$SSS[ind_batch][sel.dy] <- extVal_SSS
        
        rm(raster.HyS.SST, extVal_SST,
           raster.HyS.SSS, extVal_SSS)
        gc()
        
        # [D] HYCOM Bottom ----
        print("HYCOM bottom")
        timeloc.HyB <- which(nc_HyB_time == caredate)
        raster.HyB.SSH <- rasterize(ref.hlon_lat, ref.h.rast, nc_SSH[,,timeloc.HyB], fun = mean)
        raster.HyB.BT  <- rasterize(ref.hlon_lat, ref.h.rast, nc_BT[,,timeloc.HyB],  fun = mean)
        raster.HyB.BS  <- rasterize(ref.hlon_lat, ref.h.rast, nc_BS[,,timeloc.HyB],  fun = mean)
        
        extVal_SSH <- raster::extract(raster.HyB.SSH, spdf[ind_batch][sel.dy])
        extVal_BT  <- raster::extract(raster.HyB.BT,  spdf[ind_batch][sel.dy])
        extVal_BS  <- raster::extract(raster.HyB.BS,  spdf[ind_batch][sel.dy])
        
        data$SSH[ind_batch][sel.dy] <- extVal_SSH
        data$BT[ind_batch][sel.dy]  <- extVal_BT
        data$BS[ind_batch][sel.dy]  <- extVal_BS
        
        rm(raster.HyB.SSH, extVal_SSH,
           raster.HyB.BT,  extVal_BT,
           raster.HyB.BS,  extVal_BS)
        
        # [D] CMEMS GLORYS ----
        print("CMEMS MLD, ZOS, THETAO")
        cmld.t.1 <- as.POSIXct("1993-01-01",format="%Y-%m-%d",tz="UTC")
        if (caredate >= cmld.t.1) {       
          timeloc.CMS <- which(nc_CMS_time == caredate)
          raster.CMS.MLD <- rasterize(ref.clon_lat, ref.c.rast, nc_MLD[,,timeloc.CMS], fun = mean)
          raster.CMS.ZOS <- rasterize(ref.clon_lat, ref.c.rast, nc_ZOS[,,timeloc.CMS], fun = mean)
          raster.CMS.TTO <- rasterize(ref.clon_lat, ref.c.rast, nc_THETAO[,,timeloc.CMS], fun = mean)
          extVal_MLD <- raster::extract(raster.CMS.MLD, spdf[ind_batch][sel.dy])
          extVal_ZOS <- raster::extract(raster.CMS.ZOS, spdf[ind_batch][sel.dy])
          extVal_TTO <- raster::extract(raster.CMS.TTO, spdf[ind_batch][sel.dy])
          data$MLD[ind_batch][sel.dy] <- extVal_MLD
          data$ZOS[ind_batch][sel.dy] <- extVal_ZOS
          data$THETAO[ind_batch][sel.dy] <- extVal_TTO
          rm(raster.CMS.MLD, raster.CMS.ZOS, raster.CMS.TTO, extVal_MLD, extVal_ZOS, extVal_TTO)
        } else {
          print("Extraction date outside range for MLD")
          data$MLD[ind_batch][sel.dy] <- NA
        }
        
        # [D] CMEMS TURB ----
        print("CMEMS TURB")
        turb.t.1 <- as.POSIXct("1998-01-01",format="%Y-%m-%d",tz="UTC")
        if (caredate >= turb.t.1) {
          timeloc.CMT <- which(nc_TUR_time == caredate)
          raster.CMS.TUR <- rasterize(ref.tlon_lat, ref.t.rast, nc_TUR[,,timeloc.CMT], fun = mean)
          extVal_TUR <- raster::extract(raster.CMS.TUR, spdf[ind_batch][sel.dy])
          data$TURB[ind_batch][sel.dy] <- extVal_TUR
          rm(raster.CMS.TUR, extVal_TUR)
        } else {
          print("Extraction date outside range for TURB")
          data$TURB[ind_batch][sel.dy] <- NA
        }
        
        # [D] CMEMS CHLA ----
        print("CMEMS CHLA")
        chla.t.1 <- as.POSIXct("2010-01-01",format="%Y-%m-%d",tz="UTC")
        if (caredate >= chla.t.1) {
          timeloc.CMC <- which(nc_CHL_time == caredate)
          raster.CMS.CHL <- rasterize(ref.chllon_lat, ref.chl.rast, nc_CHL[,,timeloc.CMC], fun = mean)
          extVal_CHL <- raster::extract(raster.CMS.CHL, spdf[ind_batch][sel.dy])
          data$CHLA[ind_batch][sel.dy] <- extVal_CHL
          rm(raster.CMS.CHL, extVal_CHL)
        } else {
          print("Extraction date not in CMEMS CHLA data range")
          data$CHLA[ind_batch][sel.dy] <- NA
        }
        
        # [D] CHLA ----
        #print("CHLA")
        #chla.t.1 <- as.POSIXct("1997-09-04",format="%Y-%m-%d",tz="UTC")
        #chla.t.2 <- as.POSIXct("2019-12-27",format="%Y-%m-%d",tz="UTC")
        # chla_layer_names # (Used as a reference for first/last, maybe?)
        #if (caredate >= chla.t.1 & caredate <= chla.t.2) {
        # Finds the left-hand (earlier) side of the time interval
        #  chla.tloc <- findInterval(caredate, chla_names)
        # Extract 
        #  extVal_CHLA <- raster::extract(chla_stack[[chla.tloc]], spdf[ind_batch][sel.dy])
        # Associate with data
        #  data$CHLA[ind_batch][sel.dy] <- extVal_CHLA
        #} else {
        #  print("Extraction date outside range for CHLA")
        #  data$CHLA[ind_batch][sel.dy] <- NA
        #}
        
        gc()
      }
      
      batchList <- list(data = data[ind_batch,])
      names(batchList) <- yearMo_batches[i]
      return(batchList)
    },
    file = paste0(dir_outfiles, "Batch_loop_", ifelse(nchar(i) == 1, paste0(0,i),i), ".txt"))
  }
  
  finalData <- filled_batches
  
  sysTime.out <- Sys.time()
  print(difftime(sysTime.out, sysTime.in))
  
  return(finalData)
}

# setwd(dir_outfiles)
# cl <- makeCluster(4, outfile = "")
# registerDoSNOW(cl)
# environmental_data <- BatchEnvAssign(data, verbose = TRUE, dir_SF, dir_BATHY, dir_CMEMS, dir_HYCOM, dir_outfiles)