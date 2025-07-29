# Consolidating MRIP LPIS Data Files

# Setup ----
library(ddaye)
library(readxl)
library(tidyverse)
library(writexl)

# LPS file directories
files_catch <- list.files("./data/LPS_Data/catch/")
files_main  <- list.files("./data/LPS_Data/main/")
files_size  <- list.files("./data/LPS_Data/size/")

#' MRIP files don't always have the same column names, so I needed to subset each dataframe to a 
#' selection of columns that are present across all years/months. These column names are present in 
#' all years of MRIP data for each data type
#' 
col_catch <- c("tracker", "year", "month", "stcode", "control", "docno", "id",
               "species", "othlp", "kept", "observe", "alive", 
               "dead", "sell", "weighed", "SRC")

col_main  <- c("tracker", "year", "month", "stcode", "control", "docno", "id",
               "day", "location", "sitetype", "latddmm", "londdmm", 
               "intcode", "intdate", "inttime", "county", "siteno", "site_no", "cluster",
               "prim_op", "todayt", "category", "perm_ver", "ppstate", "ppstfips", "returnt", "prim1", 
               "prim2", "tourn", "tcode", "hook", "hook_oth", "lines", "fhours", "bt_live", "bt_art", 
               "bt_dead", "miles", "depth", "sst", "contact", "proxy", "catch", "measur", "SRC" )

col_size  <- c("tracker", "year", "month", "stcode", "control", "docno", "id",
               "species", "length", 
               "curve", "upprbill", "lowerjaw", "gender", "prep")

# Functions ----
ConsolidateFiles <- function (filenames, path, cols, option = "list") {
  
  # Initializing empty objects
  flist <- list()
  longdata <- data.frame()
  
  for (i in 1:length(filenames)) {
    
    #' This section loops through all the files in the directory and loads them into a list. If saving
    #' as a dataframe, it will bind all the data files together
    print(i)
    
    if (option == "list") {
      flist[[i]] <- read_csv(paste0(path,filenames[i]), guess_max = 1e6, locale = readr::locale(encoding = "UTF-8")) %>%
        mutate(SRC = filenames[i])
      names(flist)[i] <- filenames[i]
    } else if (option == "long") {
      flist[[i]] <- read_csv(paste0(path,filenames[i]), guess_max = 1e6, locale = readr::locale(encoding = "UTF-8")) %>%
        mutate(SRC = filenames[i]) %>% select(all_of(cols))
      longdata <- rbind(longdata, flist[[i]])
    }
  }
  
  if (option == "list") {
    final_data <- flist
  } else if (option == "long") {
    final_data <- longdata
  }
  
  return(final_data)
}

# Consolidating ----
## Lists
list_catch <- ConsolidateFiles(filenames = files_catch, path = "./LPS_Data/catch/", cols = col_catch, "list")
list_main  <- ConsolidateFiles(filenames = files_main,  path = "./LPS_Data/main/",  cols = col_main,  "list")
list_size  <- ConsolidateFiles(filenames = files_size,  path = "./LPS_Data/size/",  cols = col_size,  "list")

## Dataframes
long_catch <- ConsolidateFiles(filenames = files_catch, path = "./data/LPS_Data/catch/", cols = col_catch, "long")
long_main  <- ConsolidateFiles(filenames = files_main,  path = "./data/LPS_Data/main/",  cols = col_main,  "long")
long_size  <- ConsolidateFiles(filenames = files_size,  path = "./data/LPS_Data/size/",  cols = col_size,  "long")

# Load Species Code Ref ----
ref <- read_xlsx("./data/hms-reference-tables.xlsx", sheet = "species_codes", guess_max = 1e5) %>%
  mutate(Species_Code = sprintf("%04d", Species_Code))

hms_codes <- ref %>% filter(HMS == TRUE) %>% pull(Species_Code)

# Data Exploration and QAQC----
## CATCH
long_catch$species <- sprintf("%04d", long_catch$species)

sppcode <- vector()
sciname <- vector()
comname <- vector()

for (i in 1:nrow(long_catch)) {
  print(i)
  species_code <- long_catch$species[i]
  sppcode[i] <- species_code
  sciname[i] <- ref$Scientific_Name[match(species_code, ref$Species_Code)]
  comname[i] <- ref$Common_Name[match(species_code, ref$Species_Code)]
}

sppcode[grepl("NA", sppcode)] <- NA

long_catch$SPPCODE <- sppcode
long_catch$SCINAME <- sciname
long_catch$COMNAME <- comname

long_catch <- long_catch %>% 
  mutate(kept    = ifelse(kept    == 9996, NA, kept),
         observe = ifelse(observe == 9996, NA, observe),
         alive   = ifelse(alive   == 9996, NA, alive),
         dead    = ifelse(dead    == 9996, NA, dead),
         YRMO    = paste(year,"_",month,sep="")) %>%
  relocate(tracker, SRC, SPPCODE, SCINAME, COMNAME, YRMO, kept, observe, alive, dead, sell, weighed) %>%
  filter(SPPCODE %in% hms_codes) %>%
  arrange(SPPCODE, YRMO)

# All Catches Export
long_export <- long_catch %>%
  left_join(long_main, by = c("tracker","year","month","stcode","control","docno","id")) %>%
  mutate(latddmm = ifelse(latddmm %in% c(9996,9997,9998,9999), NA, latddmm),
         londdmm = ifelse(londdmm %in% c(9996,9997,9998,9999), NA, londdmm)) %>%
  mutate(latdec = as.numeric(substr(latddmm,1,2)) + (as.numeric(substr(latddmm,3,4))/60),
         londec = as.numeric(substr(londdmm,1,2)) + (as.numeric(substr(londdmm,3,4))/60),
         londec = londec * -1) %>%
  transmute(TRACKER = tracker,
            SPCODE = SPPCODE,
            SCI_NAME = SCINAME,
            COM_NAME = COMNAME,
            LAT = latdec, LON = londec,
            STCODE = stcode,
            YR = year,
            MO = month,
            KEPT = kept,
            ALIVE = alive,
            DEAD = dead) %>%
  filter(!is.na(LON), !is.na(LAT)) %>%
  arrange(SCI_NAME)

write_xlsx(long_export, "./output/MRIP-LPS-catch-data.xlsx")

# Continued

summary_catch <- long_catch %>%
  group_by(SPPCODE, SCINAME, COMNAME) %>%
  summarize(NROW  = n(),
            KEPT  = sum(kept,  na.rm = T),
            ALIVE = sum(alive, na.rm = T),
            DEAD  = sum(dead,  na.rm = T))

## MAIN
long_main <- long_main %>%
  mutate(latddmm = ifelse(latddmm %in% c(9996,9997,9998,9999), NA, latddmm),
         londdmm = ifelse(londdmm %in% c(9996,9997,9998,9999), NA, londdmm)) %>%
  mutate(latdec = as.numeric(substr(latddmm,1,2)) + (as.numeric(substr(latddmm,3,4))/60),
         londec = as.numeric(substr(londdmm,1,2)) + (as.numeric(substr(londdmm,3,4))/60),
         londec = londec * -1)

ggplot(long_main, aes(londec, latdec)) +
  geom_point(size = 0.5) +
  annotation_map(map_data("world"), col = "gray60", fill = "gray80", linewidth = 0.5) +
  coord_cartesian(xlim = c(-80,-60), ylim = c(35, 45)) +
  theme_bw() + theme(panel.grid = element_blank())

## SIZE
long_size$species <- sprintf("%04d", long_size$species)

sppcode <- vector()
sciname <- vector()
comname <- vector()

for (i in 1:nrow(long_size)) {
  print(i)
  species_code <- long_size$species[i]
  sppcode[i] <- species_code
  sciname[i] <- ref$Scientific_Name[match(species_code, ref$Species_Code)]
  comname[i] <- ref$Common_Name[match(species_code, ref$Species_Code)]
}

sppcode[grepl("NA", sppcode)] <- NA

long_size$SPPCODE <- sppcode
long_size$SCINAME <- sciname
long_size$COMNAME <- comname

long_size <- long_size %>% 
  mutate(YRMO = paste(year,"_",month,sep="")) %>%
  relocate(tracker, SPPCODE, SCINAME, COMNAME, YRMO) %>%
  filter(SPPCODE %in% hms_codes) %>%
  arrange(SPPCODE, YRMO)

summary_size <- long_size %>%
  group_by(SPPCODE, SCINAME, COMNAME) %>%
  summarize(NROW = n())

# Merging and QAQC ----
MRIP_catch <- long_catch %>%
  left_join(long_main, by = c("tracker","year","month","stcode","control","docno","id")) %>%
  select(-c("observe","sell")) %>%
  pivot_longer(cols = c("kept","alive","dead"), names_to = "obs_type", values_to = "obs_count") %>%
  filter(obs_count > 0, !grepl("Bonito", COMNAME, ignore.case = TRUE)) %>%
  transmute(SPPCODE, SCI_NAME = SCINAME, COM_NAME = COMNAME,
            INTCODE = intcode, SITETYPE = sitetype,
            DATE = as.Date(gsub("_","-",paste(YRMO,"_",day,sep=""))), 
            OBS_TYPE = obs_type, OBS_COUNT = obs_count,
            LONDDMM = londdmm, LATDDMM = latddmm,
            LON = latdec, LAT = latdec,
            PPSTATE = ppstate,
            TARG1 = prim1, TARG2 = prim2,
            LOC = location, MILES = miles,
            SST = sst, DEPTH = depth) %>%
  filter(!is.na(LON), !is.na(LAT)) %>%
  uncount(OBS_COUNT) %>%
  mutate(MILES = ifelse(MILES %in% c(997,998,999), NA, MILES),
         SST   = ifelse(SST %in% c(97,98,99), NA, SST),
         DEPTH = ifelse(DEPTH %in% c(99997,99998,99999), NA, DEPTH))

MRIP_catch$LOC[17312] <- "JEFFREYS LEDGE"

MRIP_sizes <- long_size %>%
  left_join(long_main, by = c("tracker","year","month","stcode","control","docno","id")) %>%
  transmute(SPPCODE, SCI_NAME = SCINAME, COM_NAME = COMNAME,
            DATE = as.Date(gsub("_","-",paste(YRMO,"_",day,sep=""))),
            INTCODE = intcode, SITETYPE = sitetype,
            LONDDMM = londdmm, LATDDMM = latddmm,
            LON = latdec, LAT = latdec,
            LENGTH = length, CURVE = curve, UPPRBILL = upprbill, LOWERJAW = lowerjaw, GENDER = gender,
            PPSTATE = ppstate,
            TARG1 = prim1, TARG2 = prim2,
            LOC = location, MILES = miles,
            SST = sst, DEPTH = depth) %>%
  filter(!is.na(LON), !is.na(LAT)) %>%
  mutate(MILES = ifelse(MILES %in% c(997,998,999), NA, MILES),
         SST   = ifelse(SST %in% c(97,98,99), NA, SST),
         DEPTH = ifelse(DEPTH %in% c(99997,99998,99999), NA, DEPTH))

# Export ----
compileWorkbook(list(summary_catch, summary_size), sheetLab = c("LPS-Catch","LPS-Size"),
                save = TRUE, name = "MRIP-LPIS-hms-summary", path = ".")
write_xlsx(MRIP_catch, path = "./output/MRIP-catch.xlsx")
write_xlsx(MRIP_sizes, path = "./output/MRIP-sizes.xlsx")