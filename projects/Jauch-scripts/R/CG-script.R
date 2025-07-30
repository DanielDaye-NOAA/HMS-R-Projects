library(tidyverse)
library(readxl)
library(dplyr)

# Read in full CG vessel list (download from Merchant Vessels of the United States)
# https://www.dco.uscg.mil/Our-Organization/Assistant-Commandant-for-Prevention-Policy-CG-5P/Inspections-Compliance-CG-5PC-/Office-of-Investigations-Casualty-Analysis/Merchant-Vessels-of-the-United-States/
allcg <- read_xlsx("C:/Users/rebecca.jauch/Desktop/vesdocJul25.xlsx", guess_max=1e7)

# Subset with pertinent info for matching
smallcg <- subset(allcg, select = c(vesselname, official_number, gross_ton, length, vesl_year_built))

# Change CG variables to match HMS
colnames(smallcg)[colnames(smallcg) == 'official_number'] <- 'COASTGUARDNBR'
smallcg$COASTGUARDNBR <- as.character(smallcg$COASTGUARDNBR)

# Read in unmatched HMS vessel list downloaded from SQL (see separate SQL script)
noaacg <- read.csv("C:/Users/rebecca.jauch/Desktop/july_un.csv")
noaacg$COASTGUARDNBR <- as.character(noaacg$COASTGUARDNBR)

# Join HMS and USCG data to determine any matches that were not identified in SQL
july_un <- dplyr::left_join(noaacg, smallcg, by = "COASTGUARDNBR")
write.csv(july_un, "C:/Users/rebecca.jauch/Desktop/july_un.csv")


# Make sure all vessel names are upper case for best matching by name
july_un$VESSELNAME <- toupper(unmatched$VESSELNAME)
colnames(smallcg)[colnames(smallcg) == 'vesselname'] <- 'VESSELNAME'
newdat <- dplyr::left_join(july_un, smallcg, by = "VESSELNAME")

write.csv(newdat, "C:/Users/rebecca.jauch/Desktop/julynames.csv")

#' Becky: I generally scroll through this name matched sheet to find obvious matches. I am sure
#' there is a better way to do this with fuzzy matching etc but I never had time to dig in


#' Compare HMS commercial permits to SAFIS landings for CG compliance assistance
#' this identifies those commercial vessels with actual commercial landings (ie actively fishing/selling)
#' download full SAFIS landings and full HMS commercial vessel list (either from SQL or ALRS)
safis <- read_xlsx("C:/Users/rebecca.jauch/Desktop/SAFIS_July23.xlsx")
hms <- read_xlsx("C:/Users/rebecca.jauch/Desktop/Comm_ves_July23.xlsx")

colnames(safis)[colnames(safis) == 'Vessel Permit Nbr'] <- 'PERMITNBR'

comm_ves_match <- dplyr::left_join(safis, hms, by = "PERMITNBR")
write.csv(comm_ves_match, "C:/Users/rebecca.jauch/Desktop/comm_ves_match_july23.csv")