# Comparison of VESL data (provided by VESL) to ALRS data (full landings download)
library(tidyverse)
library(readxl)
library(dplyr)

vesl <- read_xlsx("C:/Users/rebecca.jauch/Desktop/SEFHEIR_data_VESL.xlsx")
alrs <- read_xlsx("C:/Users/rebecca.jauch/Desktop/ALRS25.xlsx", skip=3)

#get dates in standard, matching format
vesl$`Submitted Date` <- format(as.Date(vesl$`Submitted Date`, format="%Y-%m-%d"), "%m/%d/%Y")
vesl$`Trip Start Date` <- format(as.Date(vesl$`Trip Start Date`, format="%Y-%m-%d"), "%m/%d/%Y")
vesl$`Trip End Date` <- format(as.Date(vesl$`Trip End Date`, format="%Y-%m-%d"), "%m/%d/%Y")

#subset to pertinent fields
sm_alrs <- subset(alrs, select = c(`SUBMITTED DATE`, `TRIP DEPART DATE`,
                                   `TRIP END DATE`, `DATA SOURCE`, `VESSEL NAME`, `USCG OR STATE/FOREIGN REG`))
#subset
sm_vesl <- subset(vesl, select = c(`SpeciesName`, `Submitted Date`, `Trip Start Date`, `Trip End Date`,
                                   `Registration`, `VesselName`))

#standardize variable names
colnames(sm_vesl)[colnames(sm_vesl) == "SpeciesName"] <- "SPECIES NAME"
colnames(sm_vesl)[colnames(sm_vesl) == "Submitted Date"] <- "SUBMITTED DATE"
colnames(sm_vesl)[colnames(sm_vesl) == "Trip Start Date"] <- "TRIP DEPART DATE"
colnames(sm_vesl)[colnames(sm_vesl) == "Trip End Date"] <- "TRIP END DATE"
colnames(sm_vesl)[colnames(sm_vesl) == "Registration"] <- "USCG OR STATE/FOREIGN REG"
colnames(sm_vesl)[colnames(sm_vesl) == "VesselName"] <- "VESSEL NAME"

#join to check for matches
matched <- dplyr::left_join(sm_vesl, sm_alrs, by = c("TRIP END DATE", "VESSEL NAME", "USCG OR STATE/FOREIGN REG"))

write.csv(matched, "C:/Users/rebecca.jauch/Desktop/vesl_alrs_match.csv")