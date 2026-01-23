# FINAL USING DATA FROM LI
library(tidyverse)

test_permits <- c(10046313,10059014,10059018,10059592,10059593,
                  10059595,10066833,10088069,10088070,10088538,
                  10093066,10130719,10140720,10140721,10181414)

# Loading in the data and making the column names a bit more useful

file <- "./data/export2018-2025.csv"
file <- "./data/export-2025.csv"

all_oap <- read_csv(file) %>%
  transmute(PERMITNBR, PERMITYEAR, CATEGORY, 
            VESNAME = VESSELNAME,
            VESID = USCG_OR_STATE_REG, 
            GEARTYPE,
            LENGTH_FT = VESSEL_LENGTH,
            LENGTH_M = LENGTH_FT * 0.3048,
            OVER_20 = ifelse(LENGTH_M >= 20, TRUE, FALSE),
            TONNAGE, 
            CHBEND = CHBENDORSEMENT) %>%
  
  filter(!(PERMITNBR %in% test_permits)) %>%
  
  # Arranging this way keeps all permits together in chronological order
  arrange(PERMITNBR, PERMITYEAR)

# QAQC
table(all_oap$CATEGORY) %>% data.frame() %>% arrange()

table_oap <- all_oap %>%
  
  # Fix all permit CATEGORY values for ANGLING and CHARTER permits
  mutate(OLD_CAT = CATEGORY,
         CATEGORY = ifelse(grepl("ANGLING", CATEGORY), "ANGLING", CATEGORY),
         CATEGORY = ifelse(grepl("CHARTER", CATEGORY), "CHARTER/HB", CATEGORY),
         
         # Assign HANDLINE/HARPOON/REC status of permit
         TGROUP = ifelse(CATEGORY == "CHARTER/HB" & CHBEND == "Y", "HANDLINE", NA),
         TGROUP = ifelse(grepl("GENERAL", CATEGORY), "HANDLINE", TGROUP),
         TGROUP = ifelse(CATEGORY == "CHARTER/HB" & CHBEND == "N", "REC", TGROUP),
         TGROUP = ifelse(CATEGORY == "ANGLING", "REC", TGROUP),
         TGROUP = ifelse(grepl("HARP", CATEGORY), "HARPOON", TGROUP)) %>%
  
  # CHECK if HANDLINE/HARPOON have vessels > 20m, convert to new "20+" categories
  mutate(TGROUP = ifelse(TGROUP == "HANDLINE" & OVER_20, "HANDLINE_20+", TGROUP),
         TGROUP = ifelse(TGROUP == "HARPOON" & OVER_20, "HARPOON_20+", TGROUP)) %>%
  
  # Drop all unassigned categories (SWORDFISH and TRAP)
  filter(!is.na(TGROUP))

# QAQC
table_oap %>% select(OLD_CAT, CATEGORY) %>% table(useNA = "ifany")
table_oap %>% select(CATEGORY, TGROUP)  %>% table(useNA = "ifany")

# Total vessels (unique PERMITNBR) for each CATEGORY per PERMIT_YEAR
table_oap %>%
  group_by(TGROUP, PERMITYEAR) %>%
  summarize(COUNT = length(unique(PERMITNBR))) %>%
  pivot_wider(values_from = COUNT, names_from = PERMITYEAR) %>%
  arrange(factor(TGROUP, levels = c("HANDLINE_20+","HANDLINE","HARPOON","REC")))

# Total vessels (unique PERMITNBR) for each PERMIT_YEAR
table_oap %>%
  group_by(PERMITYEAR) %>%
  summarize(COUNT = length(unique(VESID))) %>%
  pivot_wider(values_from = COUNT, names_from = PERMITYEAR)
