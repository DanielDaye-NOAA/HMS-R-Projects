# UDP Logbook Analysis for 2023 SAFE Report (Helping Tobey)

library(tidyverse)

# Example for how TransposeTable function works:
examp <- data.frame(x = c("A", "B", "C"), y = c(1, 2, 3), z = c("A1", "B2", "C3"))

TransposeTable <- function(data, colToRowName = TRUE) {
  if (colToRowName) {
    # If colToRowName == TRUE, the leftmost column will be made into
    # column names for the flipped table  
    data <- data %>%
      t() %>% as.data.frame()
    names(data) <- data[1,]
    data <- data[-1,]} 
  
  else {
    # Otherwise, row names will be made into column names
    data <- data %>%
      t() %>% as.data.frame()
  }
  return(data)
}

examp
examp %>% transpose_table()
examp %>% transpose_table(colToRowName = F)


# Load data ----
udp2024 <- read.csv("./FILEPATH/TO/UDP_ALL_LOGBOOK_as_of_20231204.csv")

# I caught a handful of entries that had errors with their SDATE
errors_SDATE <- c(39192, 39557, 54433, 81612, 112891, 
                  175067, 203959, 263486, 271508, 
                  283514, 337927, 337950, 367018)
View(udp2024[errors_SDATE,]) %>% udp2024$SET_YEAR[errors_SDATE]

# Going to breakdown the SDATE into YEAR/MONTH/DAY to help with filtering easier
data <- udp2024 %>%
  mutate(SYEAR  = substr(SDATE, 1, 4),
         SMONTH = substr(SDATE, 5, 6),
         SDAY   = substr(SDATE, 7, 8))

# Storing the years we are interested in here (past 5 years)
years = c("2018", "2019", "2020", "2021", "2022")

# ASSIGN REGION ----
# Not going to rewrite this section since it's all here already, will stick in some
# brackets so that it cycles through all at once though
{#Run this line and it will do all in brackets below
  dat <- data
  dat$REGION = NA
  dat$REGION[which(dat$LATDEG >=  0 & dat$LATDEG <  5 & dat$LONDEG >= 20 & dat$LONDEG <  53)] <- "TUS"
  dat$REGION[which(dat$LATDEG >=  5 & dat$LATDEG < 13 & dat$LONDEG >= 20 & dat$LONDEG <  60)] <- "TUN"
  dat$REGION[which(dat$LATDEG >=  0 & dat$LATDEG < 22 & dat$LONDEG >= 60 & dat$LONDEG <  87)] <- "CAR"
  dat$REGION[which(dat$LATDEG >= 13 & dat$LATDEG < 35 & dat$LONDEG >= 20 & dat$LONDEG <  60)] <- "NCA"
  dat$REGION[which(dat$LATDEG >= 18 & dat$LATDEG < 31 & dat$LONDEG >= 87 & dat$LONDEG < 100)] <- "GOM"
  dat$REGION[which(dat$LATDEG >= 22 & dat$LATDEG < 30 & dat$LONDEG >= 71 & dat$LONDEG <  82)] <- "FEC"
  dat$REGION[which(dat$LATDEG >= 22 & dat$LATDEG < 31 & dat$LONDEG >= 82 & dat$LONDEG <  87)] <- "GOM"
  dat$REGION[which(dat$LATDEG >= 22 & dat$LATDEG < 35 & dat$LONDEG >= 60 & dat$LONDEG <  71)] <- "SAR"
  dat$REGION[which(dat$LATDEG >= 30 & dat$LATDEG < 35 & dat$LONDEG >= 71 & dat$LONDEG <  82)] <- "SAB"
  dat$REGION[which(dat$LATDEG >= 35 & dat$LATDEG < 43 & dat$LONDEG >= 71 & dat$LONDEG <  80)] <- "MAB"
  dat$REGION[which(dat$LATDEG >= 35 & dat$LATDEG < 45 & dat$LONDEG >= 60 & dat$LONDEG <  71)] <- "NEC"
  dat$REGION[which(dat$LATDEG >= 35 & dat$LATDEG < 50 & dat$LONDEG >= 60 & dat$LONDEG <  65)] <- "NEC"
  dat$REGION[which(dat$LATDEG >= 35 & dat$LATDEG < 55 & dat$LONDEG >= 20 & dat$LONDEG <  60)] <- "NED"
  data$REGION = dat$REGION
  rm(dat)
}

table(data$REGION)

# Example from our call (4/25/24)
# Sum, by year, of alive and dead SMA
# SMAA = alive
# SMAD = dead
data %>%
  filter(SYEAR %in% years) %>%
  filter(PLL == "Y") %>%
  group_by(SYEAR) %>%
  summarise(count_alive.SMA = sum(SMAA, na.rm = T),
            count_dead.SMA  = sum(SMAD, na.rm = T),
            count_alive.SWO = sum(SWOA, na.rm = T),
            count_dead.SWO  = sum(SWOD, na.rm = T)) %>%
  View()

# Table 5.20 [updated] ----
# Average Number of Hooks per PLL set in 2018-2022
years
data %>%
  filter(SYEAR %in% years) %>%
  filter(PLL == "Y",
         TSWO=="Y"|TYFT=="Y"|TBET=="Y"|TMIX =="Y"|TSRK =="Y"|TOTH=="Y"|TDOL=="Y") %>% 
  select(ID, SYEAR, TSWO, TBET, TYFT, TMIX, TSRK, TDOL, TOTH, HOOKS) %>%
  pivot_longer(cols = TSWO:TOTH) %>%
  filter(value == "Y") %>%
  group_by(SYEAR, name) %>%
  summarize(numves = length(unique(ID))) %>%
  spread(name, numves)

tab_5.20 <- data %>%
  filter(SYEAR %in% years) %>%
  filter(PLL == "Y",
         TSWO=="Y"|TYFT=="Y"|TBET=="Y"|TMIX =="Y"|TSRK =="Y"|TOTH=="Y"|TDOL=="Y") %>% 
  select(SYEAR, TSWO, TBET, TYFT, TMIX, TSRK, TDOL, TOTH, HOOKS) %>%
  pivot_longer(cols = TSWO:TOTH) %>%
  filter(value == "Y") %>%
  group_by(SYEAR, name) %>%
  summarize(HOOKAVG = mean(HOOKS, na.rm = T)) %>%
  spread(name, HOOKAVG) %>%
  select(SYEAR, TSWO, TBET, TYFT, TMIX, TSRK, TDOL, TOTH) %>%
  t() %>% data.frame()

names(tab_5.20) <- tab_5.20[1,]
tab_5.20 <- tab_5.20[-1,]
tab_5.20 <- tab_5.20%>%
  mutate_all(as.numeric) %>% 
  mutate_all(round)
tab_5.20
# looks like data might not have been made confidential in previous years if the "TOTH" group 
# was used to populate 


# Table 5.22 [updated]----
# Reported Catch and Hook Numbers in ATL PLL

# Again, data is the full UDP dataset
dim(data)  # 435k rows, 325 cols

# Will need to filter down to the years of interest
years      # 2018 -> 2022

# Here's the pipe to subset to the data of interest for the table:
# %in% allows you to check for values that match the years we want, returns TRUE 
#   for each match, and filter will select only the rows where the criteria is TRUE
# Will run a second filter to subset to PLL (could do this in one filter using &
#   between the two criteria)
# Use group_by to group data by year - this allows use of summarize to calculate
#   some metrics based on each year. 
# For each sum() function in summarize, you will probably need to put na.rm = T.
#   This removed NA values, otherwise your sums will not calculate properly
# It's easy enough to add other species - do the same as for SWO (I think BAYS is 
#   the only one where multiple were combined)
# I find that code gets hard to read when a line goes on too long, so I try to break up 
#   long lines in places where it makes sense and helps with readibility (like where
#   I declare BAYS_disc below)

tab_5.22 <- data %>%
  filter(SYEAR %in% years) %>%
  filter(PLL == "Y") %>%
  group_by(SYEAR) %>%
  summarize(SWO_kept = sum(SWOK, na.rm = T),
            SWO_disc = sum(c(SWOD, SWOA), na.rm = T),
            BUM_disc = sum(c(BUMD, BUMA), na.rm = T),  # I think this is Blue Marlin?
            WHM_disc = sum(c(WHMD, WHMA), na.rm = T),
            SAI_disc = sum(c(SAID, SAIA), na.rm = T),
            SPX_disc = sum(c(SPXD, SPXA), na.rm = T),
            BFT_kept = sum(BFTK, na.rm = T),
            BFT_disc = sum(c(BFTD, BFTA), na.rm = T),
            BAYS_kept = sum(c(BETK, ALBK, YFTK, SKJK), na.rm = T),
            BAYS_disc = sum(c(BETD, ALBD, YFTD, SKJD,
                              BETA, ALBA, YFTA, SKJA), na.rm = T),
            PSHK_kept = sum(c(BSHK, XTHK, OCSK, PORK, SMAK), na.rm = T),  # Pelagic Sharks
            PSHK_disc = sum(c(BSHD, BSHA, XTHD, XTHA, OCSD, OCSA, 
                              PORD, PORA, SMAD, SMAA), na.rm = T),
            LSHK_kept = sum(c(SBKK, SPLK, SHHK, FALK, 
                              SSPK, TIGK, GHHK, SBUK), na.rm = T),  # Large Coastal Sharks
            LSHK_disc = sum(c(SBKD, SBKA, SPLD, SPLA, SHHD, SHHA, FALD, FALA,
                              SSPD, SSPA, TIGD, TIGA, GHHD, GHHA, SBUD, SBUA), na.rm = T),
            DOL_kept = sum(DOLK, na.rm = T),
            DOL_disc = sum(c(DOLD, DOLA), na.rm = T),
            WAH_kept = sum(WAHK, na.rm = T),
            WAH_disc = sum(c(WAHD, WAHA), na.rm = T),
            SEA_TURT = sum(c(TLB, TTL, TTG, KRT, THB, TTX), na.rm = T), # Involved (total), Injured, and Dead - just use total
            HOOKS_x1000 = round(sum(HOOKS, na.rm = T)/1000, 1)) %>% 
  t() %>% data.frame()

# transposing with t() messes up the first row, so I'm fixing the names here and then 
# dropping the first row where the names were stored
names(tab_5.22) <- paste0("YEAR_", tab_5.22[1,])
tab_5.22 <- tab_5.22[-1,]

# Here's the final table formatted for the SAFE report (Will need to add the 
# extra rows for other species)
tab_5.22

# Table 5.28 ----
# Reported Buoy Gear Effort
# Num Vessels and Num Trips
data %>%
  filter(SYEAR %in% years,
         BUOY == "Y") %>%  #View()
  group_by(SYEAR) %>%
  summarise(NTRIP = length(unique(TRIPN)),
            NVESS = length(unique(ID)),
            AVG_BUOY = sum(BUOY_GEARS_DEPLOYED, na.rm = T)/NTRIP,
            T_HOOKS = sum(BUOY_HOOKS_FISHED, na.rm = T),
            AVG_HOOK = T_HOOKS/(AVG_BUOY*NTRIP)) %>%
  transmute(YEAR = SYEAR, NVESS, NTRIP, AVG_BUOY, T_HOOKS, AVG_HOOK) %>%
  TransposeTable(colToRowName = TRUE)

# Table 5.31 [updated] ----
# Reported Buoy Gear Landings by Weight (lb dw), 2018-2022
data %>%
  filter(SYEAR %in% years & BUOY =="Y") %>%
  group_by(SYEAR) %>%
  summarise(LBSWO = sum(SWOLB, na.rm = T),
            LBDOL = sum(DOLPHIN_POUNDS, na.rm = T),
            LBOIL = sum(ESCOLAR_POUNDS, na.rm = T),
            LBWAH = sum(WAHOO_POUNDS, na.rm = T),
            LBBET = sum(BETLB, na.rm = T),
            LBKMC = sum(KING_MACKEREL_POUNDS, na.rm = T),
            LBYFT = sum(YFTLB, na.rm = T),
            LBBON = sum(BONLB, na.rm = T),
            LBBLK = sum(BLKLB, na.rm = T)) %>%
  transpose_table()

# Table 6.9 ----
# Bluefin Discards in GOM, ATL, NE Distant Waters
data %>%
  filter(SYEAR %in% years) %>% 
  group_by(REGION) %>%
  summarize(BFT_LB = sum(BFTLB, na.rm = T))

# Table 6.11 [updated] ----
# Num. SWO, BFT, YFT, BET, BAYS landings and discards in PLL
tab_6.11 <- data %>%
  filter(SYEAR %in% years & PLL == "Y") %>%
  group_by(SYEAR) %>%
  summarize(NUM_HOOK = round(sum(HOOKS, na.rm = T)/1000,digits = 2),
            SWO_kept = sum(SWOK, na.rm = T),
            SWO_disc = sum(c(SWOD, SWOA), na.rm = T),
            BFT_kept = sum(BFTA, na.rm = T),
            BFT_disc = sum(c(BFTD, BFTA), na.rm = T),
            YFT_kept = sum(YFTK, na.rm = T),
            YFT_disc = sum(c(YFTD, YFTA), na.rm = T),
            BET_kept = sum(BETK, na.rm = T),
            BET_disc = sum(c(BETD, BETA), na.rm = T),
            BAYS_kept = sum(c(BETK, ALBK, YFTK, SKJK), na.rm = T),
            BAYS_disc = sum(c(BETD, ALBD, YFTD, SKJD,
                              BETA, ALBA, YFTA, SKJA), na.rm = T)) %>%
  t() %>% data.frame()

# I'm going to transpose here to add in the means and diff from 1997-1999
# (I didn't include those years here, I assume they shouldn't change so am just 
# adding them in manually)
names(tab_6.11) <- paste0("YEAR_", tab_6.11[1,])
tab_6.11 <- tab_6.11[-1,]

# Second pipe to get the table in the same format as what's currently in the SAFE
# Report. It gets a bit confusing to follow but I used select to arrange the data
# properly before transposing it back into the correct orientation
tab_6.11 <- tab_6.11 %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(`1997_1999` = c(8533.10, 69131, 21519, 238, 877, 72342, 2489, 21308, 1133, 101477, 4224),
         `2001_2003` = c(7364.10, 50838, 13240, 212, 607, 55166, 1827, 13524, 395, 76116, 3069),
         `2018_2022` = round((YEAR_2018+YEAR_2019+YEAR_2020+YEAR_2021+YEAR_2022)/5,2),
         PCT_A = round((`2001_2003` - `1997_1999`)*100/`1997_1999`, 1),
         PCT_B = round((`2018_2022` - `1997_1999`)*100/`1997_1999`, 1)) %>%
  select(`1997_1999`, `2001_2003`, 
         YEAR_2018, YEAR_2019, YEAR_2020, YEAR_2021, YEAR_2022,
         `2018_2022`, PCT_A, PCT_B) %>% 
  mutate_if(is.numeric, as.character) %>% 
  t() %>% 
  data.frame()

tab_6.11

# Table 6.12 [updated] ----
# NUM PELAGIC_SHARKS, LARGE_COASTAL_SHARKS, DOL, WAH, LANDINGS and DISCARDS and
# NUM BILLFISH and TURTLES CAUGHT and DICARDED in ATL_PLL, %change since 97-99
data %>%
  filter(SYEAR %in% years,
         PLL == "Y") %>%
  group_by(SYEAR) %>%
  summarise(PSHK_kept = sum(c(BSHK, XTHK, OCSK, PORK, SMAK), na.rm = T),  # Pelagic Sharks kept
            PSHK_disc = sum(c(BSHD, BSHA, XTHD, XTHA, OCSD, OCSA,         # Pelagic Sharks disc
                              PORD, PORA, SMAD, SMAA), na.rm = T),
            LSHK_kept = sum(c(SBKK, SPLK, SHHK, FALK,                          # Large coastal kept
                              SSPK, TIGK, GHHK, SBUK), na.rm = T),       
            LSHK_disc = sum(c(SBKD, SBKA, SPLD, SPLA, SHHD, SHHA, FALD, FALA,  # Large coastal disc
                              SSPD, SSPA, TIGD, TIGA, GHHD, GHHA, SBUD, SBUA), na.rm = T),
            DOLP_kept = sum(DOLK, na.rm = T),
            DOLP_disc = sum(c(DOLD, DOLA), na.rm = T),
            WAHO_kept = sum(WAHK, na.rm = T),
            WAHO_disc = sum(c(WAHD, WAHA), na.rm = T),
            BLUM_disc = sum(c(BUMD, BUMA), na.rm = T),
            WHTE_disc = sum(c(WHMD, WHMA), na.rm = T),
            SAIL_disc = sum(c(SAID, SAIA), na.rm = T),
            SPEA_disc = sum(c(SPXD, SPXA), na.rm = T),
            TURT_ints = sum(c(TLB, TTL, TTG, KRT, THB, TTX), na.rm = T)) %>%
  transpose_table() %>%
  mutate_if(is.character, as.numeric) %>% 
  mutate(y9799 = c(3898, 52093, 8860, 6308, 39711, 608, 5172, 175, 1621, 1973, 1342, 213, 596),
         ref_A = c(3237, 23017, 5306, 4581, 29361, 322, 3776, 74, 815, 1045, 341, 139, 429),
         ref_B = (`2018`+`2019`+`2020`+`2021`+`2022`)/5,
         dif_a = round((ref_A-y9799)*100/y9799, digits = 1),
         dif_b = round((ref_B-y9799)*100/y9799, digits = 1)) %>%
  select(y9799,ref_A,`2018`,`2019`,`2020`,`2021`,`2022`,ref_B,dif_a, dif_b) %>%
  transpose_table(colToRowName = F)


# Table 6.13 [updated] ----
# Distribution of Hooks Set by Area 2018-2022, PCT change since 1997-1999 in ATL PLL
# It looks like column_to_rownames does what I was doing manually earlier
# I used select() to arrange the columns in the same order as the table in the '22 SAFE
# so I can manually add the missing rows
tab_6.13 <- data %>%
  filter(SYEAR %in% years & PLL == "Y") %>%
  group_by(REGION, SYEAR) %>%
  summarize(TOT_HOOKS = sum(HOOKS, na.rm = T)) %>%
  spread(REGION, TOT_HOOKS) %>%
  replace(is.na(.), 0) %>%
  mutate(TUN_TUS = TUN + TUS,
         TOTAL = CAR + GOM + FEC + SAB + MAB + NEC + NED + SAR + NCA + TUN_TUS) %>%
  select(SYEAR, CAR, GOM, FEC, SAB, MAB, NEC, NED, SAR, NCA, TUN_TUS, TOTAL) %>%
  column_to_rownames("SYEAR") %>%  
  t() %>% data.frame()

# Still not a perfect solution as the column names have an X at the start, will remove
names(tab_6.13) = gsub("X", "YEAR_", names(tab_6.13))

tab_6.13

# Manually adding in the data for '97-'99 and '01-'03 again
tab_6.13 <- tab_6.13 %>%
  mutate(YEAR_97_99 = c(328110, 3346298, 722580, 813111, 1267409, 901593, 511431, 14312, 191478, 436826, 8533148),
         YEAR_01_03 = c(175195, 3682536, 488838, 569965, 944929, 624497, 452430, 76130, 22070, 127497, 7364086),
         YEAR_18_22 = round((YEAR_2018 + YEAR_2019 + YEAR_2020 + YEAR_2021 + YEAR_2022)/5, 0),
         PCT_A = round((YEAR_01_03-YEAR_97_99)*100/YEAR_97_99, 1),
         PCT_B = round((YEAR_18_22-YEAR_97_99)*100/YEAR_97_99, 1)) %>%
  select(YEAR_97_99, YEAR_01_03, 
         YEAR_2018, YEAR_2019, YEAR_2020, YEAR_2021, YEAR_2022, 
         YEAR_18_22, PCT_A, PCT_B) %>%
  mutate_if(is.numeric, as.character) %>% 
  t() %>% 
  data.frame() 

tab_6.13

# Table 6.14 [in review] ----
# Num BFT, SWO, PELAGIC and L_COASTAL SHARKS, BILLFISH, TURTLES, KEPT and DISC 
# in Mid-Atlantic Bight and Northeast Coastal Areas
data %>%
  filter(REGION %in% c("MAB", "NEC"),
         PLL == "Y", 
         SYEAR %in% years) %>%  #dim() # 9,148 x 323
  group_by(SYEAR) %>%
  summarise(TOT_HOOK = sum(HOOKS, na.rm = T),
            THO_HOOK = round(TOT_HOOK/1000, digits = 6),
            BFT_K = sum(BFTK, na.rm = T),
            BFT_D = sum(c(BFTD, BFTA), na.rm = T),
            SWO_K = sum(SWOK, na.rm = T),
            SWO_D = sum(c(SWOD, SWOA), na.rm = T),
            PSHK_kept = sum(c(BSHK, XTHK, OCSK, PORK, SMAK), na.rm = T),  # Pelagic Sharks kept
            PSHK_disc = sum(c(BSHD, BSHA, XTHD, XTHA, OCSD, OCSA,         # Pelagic Sharks disc
                              PORD, PORA, SMAD, SMAA), na.rm = T),
            LSHK_kept = sum(c(SBKK, SPLK, SHHK, FALK,                          # Large coastal kept
                              SSPK, TIGK, GHHK, SBUK), na.rm = T),       
            LSHK_disc = sum(c(SBKD, SBKA, SPLD, SPLA, SHHD, SHHA, FALD, FALA,  # Large coastal disc
                              SSPD, SSPA, TIGD, TIGA, GHHD, GHHA, SBUD, SBUA), na.rm = T),
            BFSH_D = sum(c(BUMD, BUMA, WHMD, WHMA, SAID, SAIA, SPXD, SPXA), na.rm = T),
            TURT_I = sum(c(TLB, TTL, TTG, KRT, THB, TTX), na.rm = T)) %>% View()

# Table 6.15 [updated] ----
# BFT,SWO, PLL and LCS, BILLFISH, TURT in all region - NEC and MAB
data %>%
  filter(!REGION %in% c("MAB", "NEC"),
         PLL == "Y", 
         SYEAR %in% years) %>%  #dim() # 9,148 x 323 
  group_by(SYEAR) %>%
  summarise(TOT_HOOK = sum(HOOKS, na.rm = T),
            THO_HOOK = round(TOT_HOOK/1000, digits = 6),
            BFT_K = sum(BFTK, na.rm = T),
            BFT_D = sum(c(BFTD, BFTA), na.rm = T),
            SWO_K = sum(SWOK, na.rm = T),
            SWO_D = sum(c(SWOD, SWOA), na.rm = T),
            PSHK_kept = sum(c(BSHK, XTHK, OCSK, PORK, SMAK), na.rm = T),  # Pelagic Sharks kept
            PSHK_disc = sum(c(BSHD, BSHA, XTHD, XTHA, OCSD, OCSA,         # Pelagic Sharks disc
                              PORD, PORA, SMAD, SMAA), na.rm = T),
            LSHK_kept = sum(c(SBKK, SPLK, SHHK, FALK,                          # Large coastal kept
                              SSPK, TIGK, GHHK, SBUK), na.rm = T),       
            LSHK_disc = sum(c(SBKD, SBKA, SPLD, SPLA, SHHD, SHHA, FALD, FALA,  # Large coastal disc
                              SSPD, SSPA, TIGD, TIGA, GHHD, GHHA, SBUD, SBUA), na.rm = T),
            BFSH_D = sum(c(BUMD, BUMA, WHMD, WHMA, SAID, SAIA, SPXD, SPXA), na.rm = T),
            TURT_I = sum(c(TLB, TTL, TTG, KRT, THB, TTX), na.rm = T)) %>% View()

# Save as .CSV
savePath <- "G:/SF1/DATA/Tobey/2023_SAFE_script_exports/"

write.csv(tab_5.22, paste0(savePath,"table_5-22.csv"))
write.csv(tab_6.11, paste0(savePath,"table_6-11.csv"))
write.csv(tab_6.13, paste0(savePath,"table_6-13.csv"))


# Figure 6.1 [in review]----
library(ggplot2)
library(ggspatial)
library(ggtext)
library(sf)
library(terra)
library(tidyterra)
library(tidyverse)

state <- map_data("state")
world <- map_data("world")

CB <- vect("C:/Users/daniel.daye/Documents/DDaye/local_GIS_shapefiles/CBump_a.shp")
EF <- vect("C:/Users/daniel.daye/Documents/DDaye/local_GIS_shapefiles/EFC_a.shp")
DS <- vect("C:/Users/daniel.daye/Documents/DDaye/local_GIS_shapefiles/DeSoto_a.shp")
eez <- vect("C:/Users/daniel.daye/Documents/DDaye/local_GIS_shapefiles/US Fed Waters.shp")

ref = data.frame(x = -70, y = 35)

ggplot(data = world, aes(long, lat, group = group)) +
  geom_polygon(color = "black", fill = "gray60", lwd = 0.25) +
  annotation_map(state, fill = "gray90", col = "black", lwd = 0.25) +
  geom_rect(aes(xmin=-60,xmax=-20,ymin=35,ymax=55),
            col = "black", fill = "#80b4fc", lwd = 0.25) +
  coord_sf(xlim = c(-97, -58), ylim = c(15, 47)) +
  scale_x_continuous(breaks = seq(-90, -60, by = 10),
                     labels = paste0(c("90","80","70","60"),
                                     "\u00B0","0'",'0"W')) +
  scale_y_continuous(breaks = seq(20, 40, by = 10),
                     labels = paste0(c("20","30","40"),
                                     "\u00B0","0'",'0"N')) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(plot.margin = margin(1,0.5,1,0.1, unit = "cm"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10, family = "sans"))

xlabs <- paste0(c("90","80","70","60"),"\u00B0","0'",'0"W')
ylabs <- paste0(c("20","30","40"),"\u00B0","0'",'0"N')

fig_6.1 <- ggplot() +
  geom_sf(data = eez, col = "black", fill = NA) +
  annotation_map(world, fill = "gray60", col = "black", lwd = 0.25) +
  annotation_map(state, fill = "gray90", col = "black", lwd = 0.25) +
  # Text boxes
  geom_textbox(aes(x = c(-90, -75, -71.5, -61),
                   y = c( 25,  26,  32.0,  31),
                   label = c("**DeSoto Canyon**    \nYear-round",
                             "**East Florida Coast Closed Area**      \nYear-round",
                             "**Charleston Bump**       \nFeb 1 - April 30",
                             "**Northeast Distant Restricted Fishing Area**     \nYear-round with exceptions"),
                   box.colour = NA),
               width = unit(1.5, "inch"),
               col = "black", fill = "white") +
  geom_sf(data = CB, col = "black", fill = "#f68584") +
  geom_sf(data = EF, col = "black", fill = "#f68584") +
  geom_sf(data = DS, col = "black", fill = "#f68584") +
  geom_rect(aes(xmin = -60, xmax = -20,
                ymin =  35, ymax =  55),
            col = "black", fill = "#80b4fc", lwd = 0.25) +
  # Legend
  geom_rect(aes(xmin = -98, xmax = -85,
                ymin =  35, ymax =  41.5),
            col = "black", fill = "white", lwd = 0.25) +
  geom_rect(aes(xmin = -97.7, xmax = -96.2,
                ymin = c(35.8  , 37.5), 
                ymax = c(37.3, 39)),
            col = "black", lwd = 0.25,
            fill = c("#f68584", "#80b4fc")) +
  geom_text(aes(x = -97.7, y = 40, label = "Legend"),
            size = 8, hjust = 0, vjust = 0) +
  geom_text(aes(x = c(-96, -96, -96), 
                y = c(38.4, 36.7, 35.9),
                label = c("Pelagic Longline Closures", 
                          "Pelagic Longline",
                          "Gear Restricted Areas"),
                hjust = 0, vjust = 0)) +
  # Arrows
  annotate(geom = "segment", 
           x = c(-88.5, -88.5, -75.5, -75.5, -61.0),
           y = c( 26.0,  26.0,  27.5,  32.0,  33.5),
           xend = c(-85.8, -87.5, -79.0, -76.8, -58),
           yend = c( 26.5,  28.2,  30.0,  32.0,  36),
           color = "black", linewidth = 0.5, arrow = arrow(angle = 15,
                                                           length = unit(0.1, "inches"))) +
  # Formatting
  coord_sf(xlim = c(-97, -58), ylim = c(20, 41)) +
  scale_x_continuous(breaks = seq(-90, -60, by = 10),
                     labels = xlabs) +
  scale_y_continuous(breaks = seq(20, 40, by = 10),
                     labels = ylabs) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(plot.margin = margin(.5,0,0,0, unit = "cm"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10, family = "sans"))
fig_6.1
ggsave("exports/figures/figure_6-1.png", fig_6.1)

