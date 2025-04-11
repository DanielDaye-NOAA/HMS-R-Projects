#' This is partially set up to work iteratively, but column names are not dynamically updated so there
#' is some work needed to get this working with future years' data files.

# Setup

year = "2024-2025"

library(readxl)
library(tidyverse)
library(ddaye)

path = paste0("./data/",year,"/")

ind <- grepl("IBQ_summary", list.files(path))

# Load data
data = read_xlsx(paste0(path,list.files(path)[ind]), guess_max = 1e6, sheet = "VESID_sets_long") %>%
  transmute(VESID = `Vessel ID`, YEAR = year, 
            ATL, 
            GOM = ifelse(is.na(GOM), 0, GOM),
            TOT = Total)

# Summary Table
summary(data)

# Load permit info
permit_info = read_xlsx(paste0(path,list.files(path)[ind]), guess_max = 1e6, sheet = "2025 WORKING") %>%
  transmute(PERMIT = `Permit number`, VESID = `Vessel ID`)


paste(sum(data$VESID %in% permit_info$VESID),"of",nrow(data),"entries with matching permit numbers")
{ # Check if all Vessells in WORKING sheet are found in the set data
  print("Vessels in WORKING sheet that aren't found in VESID_sets:")
  permit_info$VESID[!permit_info$VESID %in% data$VESID]
}


combo <- data %>%
  left_join(permit_info, by = "VESID") %>%
  relocate(PERMIT, .after = VESID) %>%
  arrange(PERMIT, YEAR)

no_permit_match <- combo %>%
  filter(is.na(PERMIT)) %>%
  arrange(VESID, YEAR) %>%
  select(VESID) %>% distinct()

final <- combo %>%
  pivot_wider(values_from = c("GOM","ATL","TOT"), names_from = YEAR) %>%
  transmute(VESID, PERMIT,
            NUM = as.numeric(gsub("ATL-","",PERMIT)),
            ATL_2021_logbook, GOM_2021_logbook, TOT_2021_logbook,
            ATL_2022_logbook, GOM_2022_logbook, TOT_2022_logbook,
            ATL_2023_logbook, GOM_2023_logbook, TOT_2023_logbook,
            ATL_2024_vms, GOM_2024_vms, TOT_2024_vms,
            ATL_2024_logbook, GOM_2024_logbook, TOT_2024_logbook) %>%
  arrange(NUM, VESID) %>%
  mutate(across(ATL_2021_logbook:TOT_2024_logbook, ~replace_na(.x, 0))) %>%
  select(-NUM)

compileWorkbook(list(final, no_permit_match), sheetLab = c("Data_Formatted", "VESID_no_PERMIT"),
                save = TRUE, name = "IBQ_sets_2022-2024",
                path = path)