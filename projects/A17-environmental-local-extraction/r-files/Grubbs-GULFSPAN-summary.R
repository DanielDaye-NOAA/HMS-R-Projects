#' Analysis to summarize environmental data collected by Dean Grubbs via GULFSPAN surveys in
#' Region 3 for EFH text updates

# Setup ----
library(readxl)
library(tidyverse)
library(writexl)

# Load catch and set data
grubbs_catch <- read_xlsx("./data/2024-HMS-EFH_Dean-Grubbs-FSU_GULFSPAN.xlsx",
                          sheet = "LONGLINE CATCH", guess_max = 1e5)
grubbs_sets  <- read_xlsx("./data/2024-HMS-EFH_Dean-Grubbs-FSU_GULFSPAN.xlsx",
                          sheet = "LONGLINE SETS", guess_max = 1e5)

# data.frame(table(grubbs_catch$MAT))

# QAQC
grubbs_catch_fmt <- grubbs_catch %>%
  transmute(SETNUM = `SET NUMBER`,
            DATE, YEAR, MONTH, DAY,
            SPECIES = toupper(SPECIES),
            LSTAGE = ifelse(MAT %in% c("I","I*","JUV","JUV*","JUV?"),"JUV",NA),
            LSTAGE = ifelse(MAT %in% c("M","MAT","MAT?","MATURING"),"ADU",LSTAGE),
            LSTAGE = ifelse(MAT %in% c("NEO","YOY"),"NEO-YOY",LSTAGE),
            PCL, FL, TL)

# grubbs_catch_fmt %>% filter(is.na(LSTAGE)) %>% select(SPECIES) %>% table() %>% data.frame()

# QAQC
grubbs_sets_fmt <- grubbs_sets %>%
  transmute(SETNUM = `SET NUMBER`,
            DATE,
            LON1 = `Long St`, LAT1 = `Lat St`,
            LON2 = `Long End`, Lat2 = `Lat End`,
            MIN_DEPTH = MINDEPTH,
            MAX_DEPTH = MAXDEPTH,
            SST = SURFTEMP,
            MID_TEMP = MIDTEMP,
            BOTTOM_TEMP = BOTTEMP,
            SDO = SURFDO,
            MID_DO = MIDDO,
            BOTTOM_DO = BOTTOMDO,
            SSS = SURFSAL,
            MID_SAL = MIDSAL,
            BOTTOM_SAL = BOTTOMSAL,
            CLARITY,
            BOTTOM_TYPE = `BOTOM TYPE`,
            TIDE) %>%
  mutate(across(MIN_DEPTH:BOTTOM_SAL, as.numeric))

# data.frame(table(grubbs_catch_fmt$SPECIES)) %>% arrange()

#' Merge catch data with set data to get environmental information, and calculate environmental
#' metrics: MEAN, STDEV, MEDIAN, IQR (Q1 & Q3), MIN, MAX, RANGE
species_env_summary <- grubbs_catch_fmt %>%
  left_join(grubbs_sets_fmt) %>%
  pivot_longer(MIN_DEPTH:BOTTOM_SAL, names_to = "ENV_VAR") %>%
  filter(!is.na(value)) %>%
  group_by(SPECIES, ENV_VAR) %>%
  summarize(N_RECORDS = n(),
            MEAN = mean(value),
            STDEV = sd(value),
            MEDIAN = median(value),
            Q1 = quantile(value, 0.25, names = FALSE),
            Q3 = quantile(value, 0.75, names = FALSE),
            MIN = min(value),
            MAX = max(value),
            RANGE = MAX-MIN) %>%
  ungroup() %>%
  arrange(SPECIES, ENV_VAR)

# Same as above, but at the lifestage level
species_env_summary_ls <- grubbs_catch_fmt %>%
  left_join(grubbs_sets_fmt) %>%
  pivot_longer(MIN_DEPTH:BOTTOM_SAL, names_to = "ENV_VAR") %>%
  filter(!is.na(value)) %>%
  group_by(SPECIES, LSTAGE, ENV_VAR) %>%
  summarize(N_RECORDS = n(),
            MEAN = mean(value),
            STDEV = sd(value),
            MEDIAN = median(value),
            Q1 = quantile(value, 0.25, names = FALSE),
            Q3 = quantile(value, 0.75, names = FALSE),
            MIN = min(value),
            MAX = max(value),
            RANGE = MAX-MIN) %>%
  ungroup() %>%
  arrange(SPECIES, LSTAGE, ENV_VAR)

# Save ----
write_xlsx(species_env_summary,
           "./GULFSPAN-summaries/GULFSPAN-Region-3_Grubbs-summary.xlsx")
write_xlsx(species_env_summary_ls,
           "./GULFSPAN-summaries/GULFSPAN-Region-3_Grubbs-summary-lifestages.xlsx")