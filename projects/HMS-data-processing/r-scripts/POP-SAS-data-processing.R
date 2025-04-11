# Processing Pelagic Observer Program (POP) data received via SAS/SQL
# Data received from George, SAS scripts are stored on the SF drive

# Setup ----
library(ddaye)
library(haven)
library(tidyverse)

path <- "G:/SF1/DATA/Observer Data/POP/March 2025/"

# Functions ----
RowSummary <- function (data, column) {
  data %>%
    select(column) %>%
    table() %>%
    data.frame()
}


# HMS Codes ----
spp_include = c("ALB","BAS","BET","BFT","BIL","BLK","BSH","BTH","BUM","DGS","DGY","DOL","DUS",
                "FAL","GHH","LMA","OCS","POR","PTH","SAI","SAS","SBG","SBK","SBN","SBU","SDG",
                "SFT","SHH","SHX","SKJ","SLN","SMA","SMK","SNI","SNR","SPF","SPG","SPL","SPX",
                "SRF","SRQ","SSB","SSG","SSP","SST","SWH","SWO","TIG","TUN","WAH","WHM","WHX",
                "WSH","XHH","XMA","XTH","YFT")

# Loading SAS Files ----
anim_log <- read_sas(paste0(path,"animal_log.sas7bdat"))
bait_log <- read_sas(paste0(path,  "bait_log.sas7bdat"))
haul_log <- read_sas(paste0(path,  "haul_log.sas7bdat"))
hook_log <- read_sas(paste0(path, "hooks_log.sas7bdat"))
gear_log <- read_sas(paste0(path,  "gear_log.sas7bdat"))
trip_log <- read_sas(paste0(path,  "trip_log.sas7bdat"))

anim_log %>%
  group_by(ALPHA_SPECIES_CODE, SPECIES_NAME) %>%
  summarize(n = n()) %>%
  arrange(ALPHA_SPECIES_CODE) %>%
  print(n=200)

# QAQC ----
anim <- anim_log %>%
  filter(ALPHA_SPECIES_CODE %in% spp_include) %>%
  transmute(ANIMAL_LOG_KEY, HAUL_LOG_KEY, CARCASS_TAG_NUMBER,
            TRIP_NUMBER, STRING_NUMBER, HAUL_NUMBER,
            ALPHA_SPECIES_CODE, SPECIES_NAME,
            LENGTH = LENGTH_MEASUREMENT_ONE,
            LEN_TYPE = LENGTH_TYPE_ONE,
            DRESSED_WEIGHT, TAG_NUMBER,
            SEX = SEX_DESC,
            BOARDING_STATUS = BOARDING_STATUS_DESC,
            KEPT_RELEASED = KEPT_OR_RELEASED_DESC,
            COMMENTS, AREA)

haul <- haul_log %>%
  select(HAUL_LOG_KEY, GEAR_LOG_KEY, HAUL_NUMBER, GEAR_CODE,
         WEATHER_DESCRIPTION_DESC, WIND_SPEED, WIND_DIRECTION, WAVE_HEIGHT,
         GEAR_CONDITION, MAINLINE_LENGTH,
         TARGET_SPECIES, SOAK_DURATION,
         NUMBER_OF_FLOATS, NUMBER_LIGHT_STICKS, NUMBER_HOOKS_SET,
         BEGIN_SET_DATE, END_SET_DATE)

gear <- gear_log %>%
  select(GEAR_LOG_KEY, STRING_NUMBER, NUMBER_HOOKS, TRIP_NUMBER,
         NUMBER_POLYBALL_FLOATS, NUMBER_BULLET_FLOATS)

trip <- trip_log %>%
  transmute(TRIP_NUMBER, POP_VESSEL_CODE,
            VESSEL_NAME, DEPARTURE_DATE, LANDING_DATE, SEA_DAYS,
            TRIP_COMMENTS = COMMENTS, OBSERVER_TYPE,
            TRIP_SUMMARY, TOTAL_SETS, TOTAL_HAULS)

#' bait_log has multiple different baits used on the same set line which results in multiple observations
#' (rows) per set. This will collapse all observations into one row per set so that they can be associated 
#' with each catch via TRIP/STRING/HAUL NUMBERS
bait_log %>%
  arrange(TRIP_NUMBER, STRING_NUMBER, HAUL_NUMBER, BAIT_KIND_DESC)

bait <- bait_log %>%
  select(BAITS_USED_KEY, HAUL_LOG_KEY, 
         TRIP_NUMBER, STRING_NUMBER, HAUL_NUMBER, 
         BAIT_KIND_DESC, BAIT_TYPE_DESC, BAIT_CONDITION_DESC, BAIT_NUMBER_USED, BAIT_WEIGHT) %>%
  mutate(BAIT_USED = paste(BAIT_TYPE_DESC,BAIT_KIND_DESC,BAIT_CONDITION_DESC,sep = " ")) %>%
  arrange(TRIP_NUMBER, STRING_NUMBER, HAUL_NUMBER, BAIT_KIND_DESC) %>%
  group_by(TRIP_NUMBER, STRING_NUMBER, HAUL_NUMBER) %>%
  summarize(BAIT_USED = paste(BAIT_USED, collapse = "; "))

# same with hook_log
hook_log %>%
  arrange(TRIP_NUMBER, STRING_NUMBER) %>% View()
hook <- hook_log %>%
  select(HOOKS_USED_KEY, GEAR_LOG_KEY, 
         HOOK_BRAND, HOOK_MODEL, HOOK_SIZE, HOOK_TYPE, 
         TRIP_NUMBER, STRING_NUMBER) %>%
  mutate(HOOK_USED = paste(HOOK_BRAND, HOOK_SIZE, HOOK_TYPE, sep = " ")) %>%
  group_by(TRIP_NUMBER, STRING_NUMBER) %>%
  summarize(HOOK_USED = paste(unique(HOOK_USED), collapse = "; "))

# Merging Files ----
full_table <- anim %>%
  left_join(haul, by = c("HAUL_LOG_KEY", "HAUL_NUMBER")) %>%
  left_join(gear, by = c("GEAR_LOG_KEY","STRING_NUMBER","TRIP_NUMBER")) %>%
  left_join(trip, by = c("TRIP_NUMBER")) %>%
  left_join(bait, by = c("TRIP_NUMBER","STRING_NUMBER","HAUL_NUMBER")) %>%
  left_join(hook, by = c("TRIP_NUMBER","STRING_NUMBER"))

# Saving as Excel Workbook ----
compileWorkbook(dataList <- list("POP-Combo"  = full_table,
                                 "animal_log" = anim_log,
                                 "bait_log"   = bait_log,
                                 "gear_log"   = gear_log,
                                 "haul_log"   = haul_log,
                                 "hooks_log"  = hook_log,
                                 "trip_log"   = trip_log),
                sheetLab = c("POP-Combo", "animal_log", "bait_log", "gear_log",
                             "haul_log", "hooks_log", "trip_log"),
                save = TRUE, name = "POP_DATA_HMS",
                path = "./output/")

write_xlsx(full_table, "./output/POP-Combo.xlsx", format_headers = TRUE)