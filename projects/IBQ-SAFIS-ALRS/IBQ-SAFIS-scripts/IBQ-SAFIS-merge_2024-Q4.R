# Q4 IBQ SAFIS Data Merge

# SETUP ----
library(openxlsx)
library(readxl)
library(RecordLinkage)
library(tidyverse)

# 01: Load Data ----
# IBQ downloaded as an excel file; need to use readxl::read_excel
IBQ_raw <- read_excel("./data/IBQ/BFT_catch_report_2024_Jan01_Dec16.xlsx", skip = 1)
str(IBQ_raw)

IBQ <- IBQ_raw %>%
  transmute(fish_tag = as.character(`Fish Tag`),
            landing_date = as.character(`Catch Date`),
            dealer_num = as.character(`Dealer Permit`),
            dealer_name = as.character(`Dealer Name`),
            vessel_num = as.character(`Vessel Name`),  # needs to be split off
            vessel_name = toupper(as.character(`Vessel Name`)),
            round_wgt_lb = as.numeric(`Round Weight (lb)`)) %>%
  arrange(fish_tag) %>%
  as.data.frame() %>%
  # OG Vessel Name contains both the vessel name and number, need to split this.
  # The number always follows the name, so use strsplit to break into components
  # and index to grab name and number
  mutate(vessel_num = gsub("^.* ", "", vessel_num),
         vessel_name = gsub(paste0(" ",vessel_num, collapse = "|"), "", vessel_name))

# Replacing blanks with NA so that errors are coded properly
IBQ[IBQ == ""] <- NA
IBQ$fish_tag[is.na(IBQ$fish_tag)] <- "missing:IBQ"
summary(IBQ)

# SAFIS 
SAFIS_raw <- read.csv("./data/SAFIS/2024 SAFIS LL trips.csv")
names(SAFIS_raw) <- gsub(".", " ", names(SAFIS_raw), fixed = TRUE) 
str(SAFIS_raw)

SAFIS <- SAFIS_raw %>%
  filter(grepl("LONG LINE", `Gear Name`)) %>%
  mutate(`Coast guard nbr` = ifelse(is.na(`Coast guard nbr`), "", `Coast guard nbr`),
         `State reg nbr` = ifelse(is.na(`State reg nbr`), "", `State reg nbr`)) %>%
  transmute(fish_tag = as.character(`Tag`),
            landing_date = as.character(`Landing Date`),
            dealer_num = as.character(`Dealer Nbr`),
            dealer_name = as.character(`Dealer Name`),
            vessel_num = paste0(`Coast guard nbr`, `State reg nbr`),
            vessel_name = toupper(as.character(`Vessel Permit Name`)),
            round_wgt_lb = as.numeric(`Round weight`),
            dealer_rpt_id = as.character(`Dealer Rpt Id`)) %>%
  arrange(fish_tag) %>%
  as.data.frame()


# Replacing blanks with NA so that errors are coded properly
SAFIS[SAFIS == ""] <- NA
SAFIS$fish_tag[is.na(SAFIS$fish_tag)] <- "missing:SAFIS"
summary(SAFIS)

# Check for duplicates, should return <0 rows> if no issues
which(duplicated(IBQ))
which(duplicated(SAFIS))

compDupes_IBQ <- IBQ[sort(c(which(duplicated(IBQ))-1,which(duplicated(IBQ)))),]
compDupes_SAF <- SAFIS[sort(c(which(duplicated(SAFIS))-1,which(duplicated(SAFIS)))),]

IBQ <- IBQ %>% distinct()
SAFIS <- SAFIS %>% distinct()

sum(duplicated(IBQ$fish_tag))    # 0 duplication issues
sum(duplicated(SAFIS$fish_tag))  # 1 duplication issues


IBQ[sort(c(which(duplicated(IBQ$fish_tag))-1,which(duplicated(IBQ$fish_tag)))),]
SAFIS[sort(unique(c(which(duplicated(SAFIS$fish_tag))-1,which(duplicated(SAFIS$fish_tag))))),]


# Pull out errors
errors_IBQ <- IBQ[(duplicated(IBQ$fish_tag) | IBQ$fish_tag == "missing:IBQ"),]
errors_SAFIS <- SAFIS[(duplicated(SAFIS$fish_tag) | SAFIS$fish_tag == "missing:SAFIS"),]
IBQ <- IBQ[!(duplicated(IBQ$fish_tag) | IBQ$fish_tag == "missing:IBQ"),]
SAFIS <- SAFIS[!(duplicated(SAFIS$fish_tag) | SAFIS$fish_tag == "missing:SAFIS"),]

# Check for NA values
table(is.na(IBQ))
table(is.na(SAFIS))

GenError <- function(IBQ, SFS) {
  # Working backwards to that is.na values get triggered once
  # print(length(IBQ))
  # print(length(SFS))
  if (length(IBQ) != length(SFS)) {stop("length mismatch")}
  error_code <- NULL
  for(i in 1:length(IBQ)) {
    
  if (is.na(IBQ[i]) & is.na(SFS[i])) {
    error_code[i] = 4  # both missing
  } else if (!is.na(IBQ[i]) & is.na(SFS[i])) {
    error_code[i] = 3  # SAFIS missing
  } else if (is.na(IBQ[i]) & !is.na(SFS[i])) {
    error_code[i] = 2  # IBQ[i] missing
  } else if (IBQ[i] != SFS[i]) {
    error_code[i] = 1
  } else {
    error_code[i] = 0
  }
  }
  return(error_code)
}

IBQ_SAFIS <- merge(IBQ, SAFIS, by = "fish_tag", suffixes = c("_IBQ", "_SFS")) %>% 
  transmute(fish_tag,
            landing_date_IBQ, landing_date_SFS,
            landing_date_code = GenError(landing_date_IBQ, landing_date_SFS),
            dealer_num_IBQ, dealer_num_SFS,
            dealer_num_code = GenError(dealer_num_IBQ, dealer_num_SFS),
            dealer_name_IBQ = gsub("[[:punct:]]", "", dealer_name_IBQ), 
            dealer_name_SFS = gsub("[[:punct:]]", "", dealer_name_SFS),
            dealer_name_code = GenError(dealer_name_IBQ, dealer_name_SFS),
            vessel_num_IBQ, vessel_num_SFS,
            vessel_num_code = GenError(vessel_num_IBQ, vessel_num_SFS),
            vessel_name_IBQ, vessel_name_SFS,
            vessel_name_code = GenError(vessel_name_IBQ, vessel_name_SFS),
            round_wgt_lb_IBQ, round_wgt_lb_SFS,
            round_wgt_code = GenError(round_wgt_lb_IBQ, round_wgt_lb_SFS),
            dealer_rpt_id)

IBQ_noMatch <- IBQ[!(IBQ$fish_tag %in% IBQ_SAFIS$fish_tag),]
SAFIS_noMatch <- SAFIS[!(SAFIS$fish_tag %in% IBQ_SAFIS$fish_tag),]

# Making sure that all entries are accounted for; these should be TRUE

# IBQ
nrow(IBQ_noMatch) + nrow(IBQ_SAFIS) == nrow(IBQ)
# SAFIS
nrow(SAFIS_noMatch) + nrow(IBQ_SAFIS) == nrow(SAFIS)

# Double checking weight mismatch
ind <- which(IBQ_SAFIS$round_wgt_code == 1)
IBQ_SAFIS[ind,c("round_wgt_lb_IBQ", "round_wgt_lb_SFS", "round_wgt_code")]

wgt_diff = abs(IBQ_SAFIS$round_wgt_lb_IBQ[ind] - IBQ_SAFIS$round_wgt_lb_SFS[ind])
wgt_mean = (IBQ_SAFIS$round_wgt_lb_IBQ[ind] + IBQ_SAFIS$round_wgt_lb_SFS[ind]) / 2
wgt_perc = round(wgt_diff * 100 / wgt_mean, 4)

# checking numbers
cbind(IBQ_SAFIS[ind,c("round_wgt_lb_IBQ", "round_wgt_lb_SFS", "round_wgt_code")], wgt_diff, wgt_mean, wgt_perc)

# set code = 0 when difference is < 1%
IBQ_SAFIS$round_wgt_code[ind[which(wgt_perc < 1)]] <- 0
cbind(IBQ_SAFIS[ind,c("round_wgt_lb_IBQ", "round_wgt_lb_SFS", "round_wgt_code")], wgt_diff, wgt_mean, wgt_perc) %>%
  arrange(desc(round_wgt_code)) %>% print(nrow = 100)

# String matching algorithm ----
names <- IBQ_SAFIS[IBQ_SAFIS$dealer_name_code == 1,]
names$ls <- levenshteinSim(names$dealer_name_IBQ, names$dealer_name_SFS)

names %>%
  select(dealer_name_IBQ, dealer_name_SFS, ls) %>% 
  arrange(desc(ls)) %>%
  distinct()

IBQ_SAFIS$dealer_name_code[IBQ_SAFIS$dealer_name_code == 1][names$ls > .5] <- 0


# Save to Excel ----
wb <- openxlsx::createWorkbook()

sheet1 <- addWorksheet(wb, "IBQ_SAFIS")
sheet2 <- addWorksheet(wb, "IBQ_NoMatch")
sheet3 <- addWorksheet(wb, "SAFIS_NoMatch")
sheet4 <- addWorksheet(wb, "IBQ_errors")
sheet5 <- addWorksheet(wb, "SAFIS_errors")
sheet6 <- addWorksheet(wb, "All_IBQ_SAFIS_Issues")

writeData(wb, sheet1, IBQ_SAFIS,     rowNames = FALSE,
          headerStyle = createStyle(textDecoration = "Bold"))
writeData(wb, sheet2, IBQ_noMatch,   rowNames = FALSE)
writeData(wb, sheet3, SAFIS_noMatch, rowNames = FALSE)
writeData(wb, sheet4, errors_IBQ,    rowNames = FALSE)
writeData(wb, sheet5, errors_SAFIS,  rowNames = FALSE)

all_issues <- rbind(IBQ_noMatch %>% mutate(dealer_rpt_id = NA), SAFIS_noMatch, 
                    errors_IBQ %>% mutate(dealer_rpt_id = NA), errors_SAFIS)
all_issues$source = c(rep("IBQ", nrow(IBQ_noMatch)),
                      rep("SAFIS", nrow(SAFIS_noMatch)),
                      rep("IBQ", nrow(errors_IBQ)),
                      rep("SAFIS", nrow(errors_SAFIS)))
all_issues$issue = c(rep("fish_tag not found in SAFIS", nrow(IBQ_noMatch)),
                     rep("fish_tag not found in IBQ", nrow(SAFIS_noMatch)),
                     rep("fish_tag is duplicate of another IBQ fish_tag, or missing", nrow(errors_IBQ)),
                     rep("fish_tag is duplicate of another SAFIS fish_tag, or missing", nrow(errors_SAFIS)))
all_issues <- all_issues %>%
  transmute(SOURCE = source, DEALER_RPT_ID = dealer_rpt_id,
            FISH_TAG = fish_tag, LAND_DAT = landing_date,
            DEALER_NUM = dealer_num, DEALER_NAME = dealer_name,
            VESSEL_NUM = vessel_num, VESSEL_NAME = vessel_name,
            ROUND_W_LB = round_wgt_lb, ISSUE = issue )

writeData(wb, sheet6, all_issues, rowNames = FALSE)

setColWidths(wb, sheet = 1, cols = 1:ncol(IBQ_SAFIS),     widths = "auto")
setColWidths(wb, sheet = 2, cols = 1:ncol(IBQ_noMatch),   widths = "auto")
setColWidths(wb, sheet = 3, cols = 1:ncol(SAFIS_noMatch), widths = "auto")
setColWidths(wb, sheet = 4, cols = 1:ncol(errors_IBQ),    widths = "auto")
setColWidths(wb, sheet = 5, cols = 1:ncol(errors_SAFIS),  widths = "auto")
setColWidths(wb, sheet = 6, cols = 1:ncol(all_issues),  widths = "auto")

saveWorkbook(wb, file = "./data/IBQ_SAFIS_export/IBS_SAFIS_2024_Dec20.xlsx",
             overwrite = TRUE)
