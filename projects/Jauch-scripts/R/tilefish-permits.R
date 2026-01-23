#' This was code I used to answer a GARFO request for tilefish permit holders who also had HMS permits
#' I used tilefish data provided by GARFO and then matched it to 2024 HMS permit data I pulled from SQL

library(tidyverse)
library(dplyr)
library(sqldf)


hms <- read.csv("C:/Users/rebecca.jauch/Desktop/2024_permits_hms.csv")
hms1 <- read.csv("C:/Users/rebecca.jauch/Desktop/2024_permits_hms_hist.csv")

hms2024 <- bind_rows(hms, hms1)
write.csv(hms2024, "C:/Users/rebecca.jauch/Desktop/hms2024.csv")

tilefish <- read.csv("C:/Users/rebecca.jauch/Desktop/permits_dnf.csv")

statematch <- match(tilefish$VPS_HULLID, hms2024$STATEREGISTRATIONNBR)
cgmatch <- match(tilefish$VPS_HULLID, hms2024$COASTGUARDNBR)

matchtile <- sqldf('select * from tilefish join hms2024 on tilefish.HULL_ID in (hms2024.COASTGUARDNBR, 
                   hms2024.STATEREGISTRATIONNBR)')

write.csv(matchtile, "C:/Users/rebecca.jauch/Desktop/matched_tilefish_hms.csv")

tiles <- read.csv("C:/Users/rebecca.jauch/Desktop/matched_tilefish_hms.csv")
table(tiles$CATEGORY)
table(tiles$COMMERCIAL.ENDORSEMENT)

length(tiles$PERMIT)

hms2024 <- read.csv("C:/Users/rebecca.jauch/Desktop/hms2024.csv")
statetile <- dplyr::left_join(tilefish, hms2024, by = c("VPS_HULLID" = "STATEREGISTRATIONNBR"))
cgtile <- dplyr::left_join(tilefish, hms2024, by = c("VPS_HULLID" = "COASTGUARDNBR"))

fulltile <- bind_rows(statetile, cgtile)
write.csv(fulltile, "C:/Users/rebecca.jauch/Desktop/fulltile.csv")
