#' Through monthly ICCAT 20m vessel list creation, discovered that some PIMS permitted vessels were being
#' excluded due to a mismatch between ISS_DATE and EFF_DATE

library(openxlsx)
library(tidyverse)

data <- read.xlsx("./data/PIMS_2024_Jan01_Jun07.xlsx", startRow = 5)

data %>%
  mutate(IssueDate = convertToDate(IssueDate),
         EffDate = convertToDate(EffDate),
         IssMonth = format(IssueDate, f = "%m"),
         EffMonth = format(EffDate,   f = "%m"),
         mismatch = ifelse(IssMonth != EffMonth, TRUE, FALSE),
         LengthM = as.numeric(`Length(Meters)`)*0.3048) %>%
  filter(mismatch,
         grepl("ATL", Permit),
         !grepl("NOVESID", VESID),
         LengthM >= 20) %>%
  select(VESID, VESNAME, LengthM, Permit, IssueDate, EffDate, IssMonth, EffMonth, mismatch) %>%
  View()
