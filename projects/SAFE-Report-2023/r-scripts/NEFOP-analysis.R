# NEFOP Analysis for 2023 Safe Report
# Originally done by Heather Baertlein & Delisse Ortiz

library(openxlsx)
library(readxl)
library(tidyverse)

# Load data files ----
safe_HMS <- read_excel("./data/2022_HMS.xlsx")
safe_sDog_lngth <- read_excel("./data/2022_smooth_dogfish_lengths.xlsx")
safe_sDog_trips <- read_excel("./data/2022_smooth_dogfish_trips.xlsx")


# Process NEFOP data ----
HMS_lookup_prog <- read_excel("./data/SAFE_HMS_lookupTables.xlsx", sheet = "program_codes") %>%
  mutate(PROGRAM = str_pad(Program, 3, pad = "0"))
HMS_lookup_gear <- read_excel("./data/SAFE_HMS_lookupTables.xlsx", sheet = "gearname")
HMS_lookup_disp <- read_excel("./data/SAFE_HMS_lookupTables.xlsx", sheet = "fishdisp")
HMS_lookup_spec <- read_excel("./data/SAFE_HMS_lookupTables.xlsx", sheet = "species_codes", trim_ws = FALSE) %>%
  select(Species_Code, Common_Name, Scientific_Name, Log) %>%
  mutate(Species_Code = str_pad(Species_Code, 4, pad = "0"))
HMS_lookup_stat <- read_excel("./data/SAFE_HMS_lookupTables.xlsx", sheet = "status")


# Add columns from the lookup tables
# HMS
ind.prog <- match(safe_HMS$PROGRAM, HMS_lookup_prog$PROGRAM)
ind.gear <- match(safe_HMS$GEARNM, HMS_lookup_gear$GEARNM)
#ind.disp <- match(safe_HMS$FISHDISP, HMS_lookup_disp$FISHDISP)
ind.targ <- match(safe_HMS$TARGSPEC1, HMS_lookup_spec$Species_Code)
ind.stat <- match(safe_HMS$STATEND, HMS_lookup_stat$STATEND)

safe_HMS$PROGNAME   <- HMS_lookup_prog$Program_Name[ind.prog]
safe_HMS$HMSGEARCAT <- HMS_lookup_gear$HMS_Gear_Cat[ind.gear]
#safe_HMS$DispDesc  <- HMS_lookup_disp$disp_desc[ind.disp]
safe_HMS$DISPDESC   <- ifelse(substr(safe_HMS$FISHDISP, 1, 1) == 1, "KEPT", "DISC")
safe_HMS$TARG1_COMMONNAME <- HMS_lookup_spec$Common_Name[ind.targ]
safe_HMS$STATUS     <- HMS_lookup_stat$STATUS[ind.stat]

rm(ind.prog, ind.gear, ind.targ, ind.stat)

# Smoothdogs
ind.prog <- match(safe_sDog_trips$PROGRAM, HMS_lookup_prog$PROGRAM)
ind.gear <- match(safe_sDog_trips$GEARNM, HMS_lookup_gear$GEARNM)
#ind.disp <- match(safe_sDog_trips$FISHDISP, HMS_lookup_disp$FISHDISP)
ind.targ <- match(safe_sDog_trips$T_TARGET1, HMS_lookup_spec$Species_Code)

safe_sDog_trips$PROGNAME   <- HMS_lookup_prog$Program_Name[ind.prog]
safe_sDog_trips$HMSGEARCAT <- HMS_lookup_gear$HMS_Gear_Cat[ind.gear]
#safe_HMS$DispDesc  <- HMS_lookup_disp$disp_desc[ind.disp]
safe_sDog_trips$DISPDESC   <- ifelse(substr(safe_sDog_trips$FISHDISP, 1, 1) == 1, "KEPT", "DISC")
safe_sDog_trips$TARG1_COMMONNAME <- HMS_lookup_spec$Common_Name[ind.targ]

rm(ind.prog, ind.gear, ind.targ)


# Assigning whether this is a smooth dogfish trip or not
smoothDogCodes <- c(3511, 3518, 3512)
smoothDogTrip <- vector()
targCols <- which(grepl("TARGSPEC", colnames(safe_HMS)))

for (i in 1:nrow(safe_HMS)) {
  if(sum(smoothDogCodes %in% safe_HMS[i, targCols]) > 0) {
    smoothDogTrip[i] = TRUE
  } else {
    smoothDogTrip[i] = FALSE
  }
}
safe_HMS$SMOOTHDOGTRIP <- smoothDogTrip

# QAQC ----
# Checking for consistency
safe_HMS %>%
  select(PROGRAM, PROGNAME, GEARNM, HMSGEARCAT, FISHDISP, DISPDESC,
         TARGSPEC1, TARG1_COMMONNAME, STATEND, STATUS)

safe_HMS %>%
  select(TARGSPEC1, TARGSPEC2, TARGSPEC3, TARGSPEC4, TARGSPEC5, SMOOTHDOGTRIP)

table(safe_HMS$PROGNAME)
table(safe_HMS$HMSGEARCAT)
table(safe_HMS$DISPDESC)
table(safe_HMS$TARG1_COMMONNAME)
table(safe_HMS$STATUS)

# NA Checks - Good if sum = 0
## Program
sum(is.na(safe_HMS$PROGNAME))
## Gear Cat
sum(is.na(safe_HMS$HMSGEARCAT))
## Disp
sum(is.na(safe_HMS$FISHDISP))  # good
## Common Name
sum(is.na(safe_HMS$TARG1_COMMONNAME))
## Status
sum(is.na(safe_HMS$STATUS))  #good

# All look good, will remove unnecessary values to clean up environment
rm(i, smoothDogCodes, smoothDogTrip, targCols)

#

# SAFE TABLE UPDATES ----

## T 5.45 ----
# Gillnet Gear Effort in US NE and MAR Targeting Smooth Dogfish, 2022
# Source: 2022_smooth_dogfish_trips.xlsx
smd_ggear_effort <- safe_sDog_trips %>%
  group_by(TRIPID) %>%
  summarize(NUM_ROWS = n(),
            NUM_SETS = length(unique(HAULNUM)),
            UID_SETS = paste(unique(HAULNUM), collapse =":"))
smd_ggear_effort %>% print(n = 50)

nrow(smd_ggear_effort)          #  34 Trips
sum(smd_ggear_effort$NUM_SETS)  # 110 Sets


## T 5.46 ----
# Catch and Landings of Smooth Dogfish Using Gillnet Gear
# Req: Total Caught (lb dw), Kept (%), Disc (%)
# Source: 2022_smooth_dogfish_trips.xlsx
# If the FISHDISP code begins with "1", catch is "KEPT"; else, "DISC"
safe_sDog_trips$DISPDESC <- ifelse(substr(safe_sDog_trips$FISHDISP, 1, 1) == 1, 
                                   "KEPT", "DISC")
smd_catch_landings <- safe_sDog_trips %>%
  group_by(DISPDESC) %>%
  filter(COMNAME == "DOGFISH, SMOOTH") %>%
  summarize(tot = sum(HAILWT))

smd_catch_landings  # 73,091 KEPT; 144 DISC
data.frame(Tot.Caught = sum(smd_catch_landings$tot),
           Num.Kept = smd_catch_landings$tot[2],
           Pct.Kept = round(smd_catch_landings$tot[2]*100/sum(smd_catch_landings$tot),1),
           Num.Disc = smd_catch_landings$tot[1],
           Pct.Disc = round(smd_catch_landings$tot[1]*100/sum(smd_catch_landings$tot),1))


## T 5.48 ----
# Atlantic HMS caught and kept on NEFOP BLL trips targeting TILEFISH and other FINFISH
safe_HMS %>%
  filter(HMSGEARCAT == "BOTTOM LONGLINE",
         grepl("TILEFISH|HADDOCK", TARG1_COMMONNAME)) %>%
  group_by(COMNAME, DISPDESC) %>%
  summarise(NUM = n()) %>%
  spread(DISPDESC, NUM) %>%
  replace(is.na(.), 0) %>%
  transmute(TOT_CAUGHT = DISC + KEPT, 
            KEPT, KEPT.PCT = round(KEPT*100/TOT_CAUGHT, digits = 1),
            DISC, DISC.PCT = round(DISC*100/TOT_CAUGHT, digits = 1)) %>%
  arrange(desc(TOT_CAUGHT))


## T 5.49 ----
# Non-target Shark Species caught/kept on NEFOPGillnet trips targeting teleosts
safe_HMS %>%
  filter(grepl("GILLNET", HMSGEARCAT),
         grepl("SHARK|TUNA", COMNAME),
         !grepl("ATLANTIC SHARPNOSE|DOGFISH|SKATE", TARG1_COMMONNAME)) %>%# select(TARG1_COMMONNAME) %>% table()
  group_by(COMNAME, DISPDESC) %>%
  summarise(NUM = n()) %>%
  spread(DISPDESC, NUM) %>%
  replace(is.na(.), 0) %>%
  transmute(TOT = KEPT + DISC,
            KEPT, KEPT.PCT = round(KEPT*100/TOT, digits = 1),
            DISC, DISC.PCT = round(DISC*100/TOT, digits = 1)) %>%
  arrange(desc(TOT)) %>% print(n = 50)


## T 6.25 ----
# Observed Protected Species Interactions in NE and Mid ATL Gillnet Fishery
# Targeting Smoothhounds
safe_sDog_trips %>%
  group_by(COMNAME, DISPDESC) %>%
  summarise(NUM_INT = n()) %>%
  spread(DISPDESC, NUM_INT) %>%
  filter(!(grepl("SHARK|DOGFISH|FLOUNDER|SKATE|DEBRIS|MACKEREL", COMNAME)),
         !(grepl("RAY|EGGS|BASS|SEA ROBIN|CRAB|BLUEFISH|JELLYFISH", COMNAME)),
         !(grepl("BONITO|COBIA|LOBSTER|MENHADEN|SCUP|MONKFISH", COMNAME)),
         !(grepl("STARGAZER|STARFISH|WEAKFISH|SHELL|TAUTOG|TUNA", COMNAME))) %>%
  View()


## T 6.28 ----
# Total Otter Trawl Shark Catches from Non-Smooth Dogfish Targeted Sets by species, and 
# Disposition in Order of Decreasing Abundance for all Trips
# Source: 2022_HMS.xlsx
otter_sharks <- safe_HMS %>%
  filter(HMSGEARCAT == "TRAWL" & SMOOTHDOGTRIP == FALSE) %>%
  filter(grepl("SHARK", COMNAME)) %>%
  group_by(COMNAME, STATUS, DISPDESC) %>%
  summarize(TOT = n()) %>%
  pivot_wider(names_from = c(STATUS, DISPDESC), values_from = TOT, values_fill = 0) %>%
  transmute(TOT = ALIVE_DISC + DEAD_DISC + NA_DISC + DEAD_KEPT,
            ALIVE_DISC, pALIVE_DISC = round(ALIVE_DISC*100/TOT, 1),
            DEAD_DISC, pDEAD_DISC = round(DEAD_DISC*100/TOT, 1),
            NA_DISC, pNA_DISC = round(NA_DISC*100/TOT, 1),
            DEAD_KEPT) %>%
  arrange(desc(TOT))
otter_sharks
sum(otter_sharks$TOT)


## T 6.30 ----
# Prohibited Shark HMS spp. Caught and Discarded on Observed BLL trips targeting 
# Golden Tilefish and Other Finfish in N Atl
# Source: 2022_HMS.xlsx
# BLL Sets had the following TARGSPEC1: 
# Tilefish (golden and blueline), Haddock, Spiny Dogfish
safe_HMS %>% 
  filter(HMSGEARCAT == "BOTTOM LONGLINE") %>% # select(TARG1_COMMONNAME) %>% table()
  filter(grepl("TILEFISH|HADDOCK", TARG1_COMMONNAME)) %>% # select(HULLNUM1, VESSELNAME, TARG1_COMMONNAME)
  group_by(COMNAME, DISPDESC) %>%
  summarise(TOT = n()) %>%
  spread(DISPDESC, TOT) %>%
  transmute(TOT = sum(DISC, KEPT, na.rm = T), KEPT, DISC) %>%
  arrange(desc(TOT))



## T 6.31 ----
# Sharks caught and discarded on observed trips across all gillnet gear types targeting
# mixed teleosts (NOT SHARKS AND SMOOTHDOG)
# Source: 2022_HMS.xlsx
# Heather getting 355, get 348 if removing "SKATE" along with others
sharks <- safe_HMS %>%
  filter(grepl("GILLNET", HMSGEARCAT),
         !grepl("SHARK|DOGFISH|SKATE", TARG1_COMMONNAME),
         !grepl("ANGEL|BASKING|DUSKY|NIGHT|GREENLAND|SAND TIGER|SHARK, WHITE", COMNAME),
         !grepl("TUNA", COMNAME)) %>% 
  group_by(COMNAME, DISPDESC) %>%
  summarise(count = n()) %>%
  spread(DISPDESC, count) %>%
  arrange(desc(DISC)) %>% 
  replace(is.na(.), 0) %>%
  mutate(TOTAL = DISC + KEPT,
         PCT.DISC = round(DISC*100/TOTAL, digits = 1)) %>%
  select(TOTAL, PCT.DISC) %>%
  print(n = 50)
sum(sharks$TOTAL)

#

# SAFE TEXT UPDATES ----


## Text 5.3.6.2 ----
## Top species dominating the NEFOP smooth dogfish gillnet fishery
## SOURCE: 2022_smooth_dogfish_lengths.xlsx
safe_sDog_trips %>%
  group_by(COMNAME, DISPDESC) %>%
  summarise(TOT_WGT = sum(HAILWT, na.rm = T)) %>%
  spread(DISPDESC, TOT_WGT) %>%
  replace(is.na(.), 0) %>%
  mutate(TOT_HAILWGT = DISC + KEPT + `<NA>`)%>%
  transmute(TOT_HAILWGT, KEPT, DISC, DISP_NA = `<NA>`) %>%
  arrange(desc(TOT_HAILWGT)) %>% print(n = 10)

## NUM_VESSELS Making NUM_SETS on NUM_TRIPS targeting smooth dog
## SOURCE: 2022_smooth_dogfish_lengths.xlsx
safe_sDog_trips %>% select(VESSELNAME) %>% distinct() %>% arrange()

## NUM_TRIPS where smooth dog were recorded caught
## SOURCE: 2022_smooth_dogfish_lengths.xlsx
sDog_tripCatches <- safe_sDog_trips %>% 
  filter(COMNAME == "DOGFISH, SMOOTH") %>%
  group_by(TRIPID) %>%
  summarize(NSETS = length(unique(HAULNUM)),
            UID_SETS = paste(unique(HAULNUM), collapse =":"),
            UID_CATCH = paste(unique(COMNAME), collapse = ":"))
sDog_tripCatches %>% print(n = 5)
sum(sDog_tripCatches$NSETS)


## Text 5.4.1 ----
## NUM_SETS on NUM_TRIPS in BLL targeting TILEFISH
safe_HMS %>%
  filter(HMSGEARCAT == "BOTTOM LONGLINE",
         grepl("TILEFISH|HADDOCK", TARG1_COMMONNAME)) %>%
  group_by(TRIPID) %>%
  summarize(NUM_ROWS = n(),
            VESS = unique(VESSELNAME),
            NUM_SETS = length(unique(HAULNUM)),
            UID_SETS = paste(unique(HAULNUM), collapse =":"))
safe_HMS %>%
  filter(HMSGEARCAT == "BOTTOM LONGLINE",
         grepl("TILEFISH, GOLDEN", TARG1_COMMONNAME)) %>%
  group_by(TRIPID) %>%
  summarize(NUM_ROWS = n(),
            VESS = unique(VESSELNAME),
            NUM_SETS = length(unique(HAULNUM)),
            UID_SETS = paste(unique(HAULNUM), collapse =":"))

## Text 5.4.2 ----
## NUM TRIPS, SETS, VESSELS observed interacting with HMS in gillnet fishery
safe_HMS %>%
  filter(grepl("GILLNET", HMSGEARCAT)) %>% #select(TARG1_COMMONNAME) %>%table()
  group_by(TRIPID, VESSELNAME)

## 5.4.2.1 ----
# Num sets and vessels interacting with HMS on gillnets
# Discrepancy between me and heather regarding removing prohibited species from the data if 
# occurring in the "COMMONNAME" column. since comname corresponds to the name of the bycatch
tab <- safe_HMS %>%
  filter(grepl("GILLNET", HMSGEARCAT),
         #grepl("SHARK|TUNA", COMNAME),
         !grepl("ATLANTIC SHARPNOSE|DOGFISH|SKATE", TARG1_COMMONNAME)) %>%  # select(TARG1_COMMONNAME) %>% table()
  group_by(HULLNUM1) %>%
  mutate(TRIPHAUL = paste(TRIPID, HAULNUM)) %>%
  summarise(n.trip = length(unique(TRIPID)),
            n.haul = length(unique(HAULNUM)),
            n.sets = length(unique(TRIPHAUL)))
tab %>% print(n = nrow(tab))
length(unique(tab$HULLNUM1))
sum(tab$n.trip)
sum(tab$n.sets)

safe_HMS %>%
  filter(grepl("GILLNET", HMSGEARCAT),
         #grepl("SHARK|TUNA", COMNAME),
         !grepl("ATLANTIC SHARPNOSE|DOGFISH|SKATE", TARG1_COMMONNAME)) %>%  # select(TARG1_COMMONNAME) %>% table()
  group_by(COMNAME) %>%
  summarise(num = n()) %>%
  arrange(desc(num))

# Drift Gillnet sets
drift <- safe_HMS %>%
  filter(grepl("DRIFT GILLNET", HMSGEARCAT),
         grepl("SHARK|TUNA", COMNAME),
         !grepl("ATLANTIC SHARPNOSE|DOGFISH|SKATE", TARG1_COMMONNAME)) %>% # select(HMSGEARCAT) %>% table()
  group_by(HULLNUM1, VESSELNAME) %>%
  mutate(TRIPHAUL = paste(TRIPID, HAULNUM)) %>%
  summarise(n.trip = length(unique(TRIPID)),
            n.haul = length(unique(HAULNUM)),
            n.sets = length(unique(TRIPHAUL)))
drift %>% print(n = nrow(tab))
sum(drift$n.sets)

# Sink Gillnet sets
sink <- safe_HMS %>%
  filter(grepl("SINK GILLNET", HMSGEARCAT),
         grepl("SHARK|TUNA", COMNAME),
         !grepl("ATLANTIC SHARPNOSE|DOGFISH|SKATE", TARG1_COMMONNAME)) %>% # select(HMSGEARCAT) %>% table()
  group_by(HULLNUM1, VESSELNAME) %>%
  mutate(TRIPHAUL = paste(TRIPID, HAULNUM)) %>%
  summarise(n.trip = length(unique(TRIPID)),
            n.haul = length(unique(HAULNUM)),
            n.sets = length(unique(TRIPHAUL)))
sink %>% print(n = nrow(tab)) 
length(unique(sink$HULLNUM1))
sum(sink$n.sets)
sum(sink$n.trip)


## Text 5.4.3 ----
# Check HMS encounters and retention in trawl
safe_HMS %>%
  filter(grepl("TRAWL", HMSGEARCAT)) %>% #select(TARG1_COMMONNAME) %>%table()
  group_by(TARG1_COMMONNAME) %>%
  summarize(n = n()) %>% arrange(desc(n))

safe_HMS %>%
  filter(grepl("TRAWL", HMSGEARCAT)) %>% #select(TARG1_COMMONNAME) %>%table()
  group_by(COMNAME) %>%
  summarize(n = n()) %>% arrange(desc(n)) %>% print(n = nrow(.))

safe_HMS %>%
  filter(grepl("TRAWL", HMSGEARCAT),
         grepl("SWO|TUNA|SHARK", COMNAME),
         DISPDESC == "KEPT") %>% #select(TARG1_COMMONNAME) %>%table()
  group_by(TARG1_COMMONNAME) %>% #View()
  summarize(num = n(),
            spec = paste0(unique(COMNAME), collapse = ":")) %>% arrange(desc(n)) %>% print(n = nrow(.)) 



## Text 6.5.1 ----
# Mixed-species otter trawl TRIP and SET count
# THIS IS NOW CORRECT AND IN-LINE WITH HEATHER'S MATH
otter_vess <- safe_HMS %>%
  filter(HMSGEARCAT == "TRAWL") %>% #select(GEARNM) %>% table()# all TRAWL in this data are OTTER TRAWL  #select(TARG1_COMMONNAME) %>% table()
  filter(!grepl("ANGEL|BASKING|DUSKY|NIGHT|GREENLAND|SAND TIGER|SHARK, WHITE", COMNAME)) %>% #select(COMNAME) %>% table()
  group_by(HULLNUM1, TRIPID) %>%
  summarize(VESNAME = unique(VESSELNAME),
            N_OBS = n(),
            N_SET = length(unique(paste(TRIPID, HAULNUM))),
            TARG = paste(unique(TARG1_COMMONNAME), collapse = ";"))

length(unique(otter_vess$HULLNUM1))  # 105 unique vessel
length(unique(otter_vess$TRIPID))    # 215 trips
sum(otter_vess$N_SET)                # 412 sets


## Other ----
## Which targeted species have the most bycatch?
bycatch_speccount <- safe_HMS %>%
  group_by(TARG1_COMMONNAME, COMNAME) %>%
  summarise(NUM_HMSCATCH = n()) %>%
  spread(COMNAME, NUM_HMSCATCH) %>%
  replace(is.na(.), 0)

bycatch_haulcount <- safe_HMS %>%
  group_by(TARG1_COMMONNAME) %>%
  summarise(NUM_HMSCATCH = n(),
            NUM_TRIPS = length(unique(TRIPID)),
            NUM_HAULS = length(unique(paste0(TRIPID,HAULNUM))),
            NUM_VESSL = length(unique(VESSELNAME)))

NEFOP_HMS_TARG_2023 <- bycatch_haulcount %>%
  left_join(bycatch_speccount, by = "TARG1_COMMONNAME") %>%
  arrange(desc(NUM_HMSCATCH)) %>% filter(!grepl("SHARK", TARG1_COMMONNAME))

## NUM_SETS on NUM_TRIPS targeting other sharks from SMOOTHDOG
safe_HMS %>%
  filter(grepl("GILLNET", HMSGEARCAT),
         grepl("SHARK|DOGFISH", TARG1_COMMONNAME),
         TARG1_COMMONNAME != "DOGFISH, SMOOTH") %>%
  group_by(VESSELNAME) %>%
  summarise(TOT_ROWS = n(),
            NUM_TRIP = length(unique(TRIPID)),
            NUM_SETS = length(unique(paste0(TRIPID,HAULNUM))),
            TRP_TARG = paste(unique(TARG1_COMMONNAME), collapse =":"),
            TRP_CTCH = paste(unique(COMNAME), collapse =":"))


# 6.5.4.1 ----
# first paragraph desc
safe_sDog_trips %>%
  filter(grepl("GILLNET", HMSGEARCAT),
         DISPDESC == "KEPT") %>%
  group_by(COMNAME) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# second paragraph desc
safe_HMS %>%
  filter(grepl("GILLNET", HMSGEARCAT)) %>% select(TARG1_COMMONNAME) %>% table()

# drift-sink is NOT sink - is drift
gill <- safe_HMS %>%
  filter(grepl("GILLNET", HMSGEARCAT),
         !grepl("SHARK|DOGFISH|SKATE", TARG1_COMMONNAME)) %>% #select(TARG1_COMMONNAME) %>% table()
  group_by(HULLNUM1) %>%
  summarise(NTRIP = length(unique(TRIPID)),
            NSETS = length(unique(paste(TRIPID, HAULNUM))))
nrow(gill)
sum(gill$NTRIP)
sum(gill$NSETS)
safe_HMS %>%
  filter(grepl("GILLNET", HMSGEARCAT),
         !grepl("SHARK|DOGFISH|SKATE", TARG1_COMMONNAME)) %>%
  group_by(COMNAME) %>%
  summarize(n = n()) %>%
  arrange(desc(n))


drift <- safe_HMS %>%
  filter(grepl("DRIFT", HMSGEARCAT),
         !grepl("SHARK|DOGFISH|SKATE", TARG1_COMMONNAME)) %>% select(GEARNM) %>% table()
  group_by(HULLNUM1) %>%
  summarise(NTRIP = length(unique(TRIPID)),
            NSETS = length(unique(paste(TRIPID, HAULNUM))))
nrow(drift)
sum(drift$NTRIP)
sum(drift$NSETS)
safe_HMS %>%
  filter(grepl("DRIFT", HMSGEARCAT),
         !grepl("SHARK|DOGFISH|SKATE", TARG1_COMMONNAME)) %>%
  group_by(COMNAME) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
  


# GOOD  
sink <- safe_HMS %>%
  filter(grepl("SINK", HMSGEARCAT),
         !grepl("DRIFT-SINK", HMSGEARCAT),
         !grepl("SHARK|DOGFISH|SKATE", TARG1_COMMONNAME),
         !grepl("ANGEL|BASKING|DUSKY|NIGHT|GREENLAND|SAND TIGER|SHARK, WHITE", COMNAME)) %>%  # select(GEARNM) %>% table()
  group_by(HULLNUM1) %>%
  summarise(NTRIP = length(unique(TRIPID)),
            NSETS = length(unique(paste(TRIPID, HAULNUM))))
nrow(sink)
sum(sink$NTRIP)
sum(sink$NSETS)
safe_HMS %>%
  filter(grepl("SINK", HMSGEARCAT),
         !grepl("DRIFT-SINK", HMSGEARCAT),
         !grepl("SHARK|DOGFISH|SKATE", TARG1_COMMONNAME),
         !grepl("ANGEL|BASKING|DUSKY|NIGHT|GREENLAND|SAND TIGER|SHARK, WHITE", COMNAME)) %>%
  group_by(COMNAME) %>%
  summarize(n = n()) %>%
  arrange(desc(n))



# Save to Excel
wb <- openxlsx::createWorkbook()
sheet1 <- addWorksheet(wb, "HMS_BY_TARG1")
writeData(wb, sheet1, NEFOP_HMS_TARG_2023, rowNames = FALSE, 
          headerStyle = createStyle(textDecoration = "Bold"))
setColWidths(wb, sheet = 1, cols = 1:ncol(NEFOP_HMS_TARG_2023), widths = "auto")
saveWorkbook(wb, file = "./exports/2024_NEFOP_HMS_catch_by_TARGSPECIES.xlsx", overwrite = TRUE)
