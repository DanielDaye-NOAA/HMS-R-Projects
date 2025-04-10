# FCC ULS Data Importing
library(tidyverse)

SH <- "./data/ULS/LIC_2024_06_08/SH.dat"
HD <- "./data/ULS/LIC_2024_06_08/HD.dat"

data_SH <- readLines(SH)
data_HD <- readLines(HD)

ULS_SH <- data.frame(LINE_IN = data_SH) %>%
  mutate(REC_TYPE = str_split_i(LINE_IN, "[|]", 1),
         FCC_UID  = str_split_i(LINE_IN, "[|]", 2),
         CALLSIGN = str_split_i(LINE_IN, "[|]", 5),
         VESSNMBR = str_split_i(LINE_IN, "[|]", 11),
         VESSNAME = str_split_i(LINE_IN, "[|]", 10),
         GT       = str_split_i(LINE_IN, "[|]", 16),
         LEN      = str_split_i(LINE_IN, "[|]", 17))

ULS_HD <- data.frame(LINE_IN = data_HD) %>%
  mutate(REC_TYPE = str_split_i(LINE_IN, "[|]", 1),
         FCC_UID  = str_split_i(LINE_IN, "[|]", 2),
         CALLSIGN = str_split_i(LINE_IN, "[|]", 5),
         STATUS   = toupper(str_split_i(LINE_IN, "[|]", 6)),
         EFF_DATE = str_split_i(LINE_IN, "[|]", 8),
         EXP_DATE = str_split_i(LINE_IN, "[|]", 9),
         CAN_DATE = str_split_i(LINE_IN, "[|]", 10))

# ULS LICENSE STATUS CODES
# [A]ctive; [C]ancelled; [E]xpired; [T]erminated
table(ULS_HD$STATUS)

ULS <-  ULS_SH %>% select(-c(LINE_IN, REC_TYPE)) %>%
  left_join(ULS_HD %>% select(-c(LINE_IN, REC_TYPE)), by = c("FCC_UID", "CALLSIGN"), suffix = c("SH", "HD"))