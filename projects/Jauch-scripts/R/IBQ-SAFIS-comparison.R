library(tidyverse)
library(readxl)


# Pull BFT Catch report in IBQ for all of 2025 and save it in a folder on my desktop
ibq <- read_xlsx("C:/Users/rebecca.jauch/Desktop/IBQ_final.xlsx", skip=1)


# Pull SAFIS data by filtering on 'landing year=2025' (since some categories are blank)
safis <- read_xlsx("C:/Users/rebecca.jauch/Desktop/SAFIS_final.xlsx")


# Change column name in IBQ to match SAFIS for easy joining
colnames(ibq)[colnames(ibq) == 'Fish Tag'] <- 'Tag'


# I make one spreadsheet joining SAFIS onto IBQ (preserving IBQ data)
ibq_safis_match <- dplyr::left_join(ibq, safis, by = "Tag")
write.csv(ibq_safis_match, "C:/Users/rebecca.jauch/Desktop/ibq_safis.csv")


# I make one spreadsheet joining IBQ onto SAFIS (preserving SAFIS data)
saf_ibq_match <- dplyr::left_join(safis, ibq, by = "Tag")
write.csv(saf_ibq_match, "C:/Users/rebecca.jauch/Desktop/safis_ibq.csv")


#' I sort each sheet by Tag number and do a quick visual inspection of records that don't match, then 
#' dig deeper as necessary - often in IBQ the errors are accidental 0 entries that can be deleted often
#' errors in SAFIS are incorrect tag numbers or fish that were never entered in IBQ