#' Analysis to summarize environmental data collected by Jill Hendon via GULFSPAN surveys in
#' Region 1 for EFH text updates

# Setup ----
library(readxl)
library(tidyverse)

hendon <- read_xlsx("./data/2024-HMS_Hendon-GULFSPAN-EFH-Data.xlsx",
                    sheet = "merged-data", guess_max = 1e5)

# Subset to required columns
pivot <- hendon %>%
  transmute(UID,
            COMNAME = `Species Common Name`,
            SCINAME = `Species Scientific Name`,
            DATE = Date,
            BOTTOM_TYPE, CLOUD_COVER, 
            WAVE_FT, DEPTH_FT, DEPTH_M, 
            S_TEMP, B_TEMP, S_SAL, B_SAL, 
            S_DO, B_DO, S_PH, B_PH) %>%
  mutate(across(CLOUD_COVER:B_PH, as.numeric)) %>%
  pivot_longer(CLOUD_COVER:B_PH, names_to = "ENV_VAR") %>%
  
  # Summarize data similarly to how it was done for Dean Grubbs / GULFSPAN Region 3
  group_by(COMNAME, ENV_VAR) %>%
  summarize(COUNT = n(),
            MEAN = mean(value, na.rm = TRUE),
            STDEV = sd(value, na.rm = TRUE),
            MEDIAN = median(value, na.rm = TRUE),
            Q1 = quantile(value, 0.25, na.rm = TRUE),
            Q3 = quantile(value, 0.75, na.rm = TRUE),
            MIN = min(value, na.rm = TRUE),
            MAX = max(value, na.rm = TRUE),
            RANGE = MAX - MIN)

# Same, this time by lifestage
pivot_ls <- hendon %>%
  transmute(UID,
            COMNAME = `Species Common Name`,
            SCINAME = `Species Scientific Name`,
            LSTAGE,
            DATE = Date,
            BOTTOM_TYPE, CLOUD_COVER, 
            WAVE_FT, DEPTH_FT, DEPTH_M, 
            S_TEMP, B_TEMP, S_SAL, B_SAL, 
            S_DO, B_DO, S_PH, B_PH) %>%
  mutate(across(CLOUD_COVER:B_PH, as.numeric)) %>%
  pivot_longer(CLOUD_COVER:B_PH, names_to = "ENV_VAR") %>%
  group_by(COMNAME, LSTAGE, ENV_VAR) %>%
  summarize(COUNT = n(),
            MEAN = mean(value, na.rm = TRUE),
            STDEV = sd(value, na.rm = TRUE),
            MEDIAN = median(value, na.rm = TRUE),
            Q1 = quantile(value, 0.25, na.rm = TRUE),
            Q3 = quantile(value, 0.75, na.rm = TRUE),
            MIN = min(value, na.rm = TRUE),
            MAX = max(value, na.rm = TRUE),
            RANGE = MAX - MIN)

# Save ----
write_excel_csv(pivot,
                file = "./GULFSPAN-summaries/GULFSPAN-Region-1_Hendon-summary.csv")
write_excel_csv(pivot_ls,
                file = "./GULFSPAN-summaries/GULFSPAN-Region-1_Hendon-summary-lifestages.csv")