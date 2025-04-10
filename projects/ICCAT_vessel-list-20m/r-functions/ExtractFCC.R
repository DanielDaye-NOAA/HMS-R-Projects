# ExtractFCC
#' Unlike the ScrapeFCC function, ExtractFCC relies on files that have been 
#' downloaded locally. Steps:
#' 1) https://www.fcc.gov/uls/transactions/daily-weekly
#' 2) Download the newest weekly Ship Radio Service Licenses .zip
#' 4) Extract and replace the files in ./data/FCC/

library(tidyverse)

ExtractFCC <- function(vesNum, vesName, prevName, verbose = FALSE) {
  # Read in the FCC data files
  print("Loading SH and HD FCC Files...")
  SH <- readLines("./data/FCC/SH.dat")
  HD <- readLines("./data/FCC/HD.dat")
  
  # Convert each line to the respective columns
  print("Converting SH and HD to proper format...")
  ulsSH <- data.frame(LINE_IN = SH) %>%
    mutate(REC_TYPE = str_split_i(LINE_IN, "[|]", 1),
           FCC_UID  = str_split_i(LINE_IN, "[|]", 2),
           CALLSIGN = str_split_i(LINE_IN, "[|]", 5),
           VESSNMBR = str_split_i(LINE_IN, "[|]", 11),
           VESSNAME = str_split_i(LINE_IN, "[|]", 10),
           GT       = str_split_i(LINE_IN, "[|]", 16),
           LEN      = str_split_i(LINE_IN, "[|]", 17))
  ulsHD <- data.frame(LINE_IN = HD) %>%
    mutate(REC_TYPE = str_split_i(LINE_IN, "[|]", 1),
           FCC_UID  = str_split_i(LINE_IN, "[|]", 2),
           CALLSIGN = str_split_i(LINE_IN, "[|]", 5),
           STATUS   = toupper(str_split_i(LINE_IN, "[|]", 6)),
           EFF_DATE = str_split_i(LINE_IN, "[|]", 8),
           EXP_DATE = str_split_i(LINE_IN, "[|]", 9),
           CAN_DATE = str_split_i(LINE_IN, "[|]", 10))
  
  # Provide list of all records
  print(data.frame(table(ulsHD$STATUS)) %>% rename(Lic_Status = Var1, Num_Lic = Freq), right = F)
  
  # Combine SH and HD records, filter to Active licences
  print("Combining SH and HD records; filtering to ACTIVE licenses.")
  ULS <- ulsSH %>% select(-c(LINE_IN, REC_TYPE)) %>%
    left_join(ulsHD %>% select(-c(LINE_IN, REC_TYPE)),
              by = c("FCC_UID", "CALLSIGN"), 
              suffix = c("SH", "HD")) %>%
    filter(STATUS == "A") %>%
    mutate(VESSNAME = toupper(VESSNAME)) %>%
    arrange(as.numeric(FCC_UID))
  
  # Move matched vesNum into dataframe and associate with FCC data
  data <- data.frame(VESSNMBR = as.character(vesNum),
                     CURRENT_NAME = toupper(as.character(vesName)), 
                     PREV_NAME = toupper(as.character(prevName))) %>%
    left_join(ULS, by = "VESSNMBR", relationship = "many-to-many") %>%
    arrange(VESSNMBR, VESSNAME, EXP_DATE) %>%
    mutate(VES_NOTE = "")
  
  # If number matches previous vessel name, ID that it's a prevName
  # VESSNMBR = vList
  # CURRENT_NAME = vList
  # PREV_NAME = vList
  # VESSNAME = FCC
  for(i in 1:nrow(data)) {
    if (verbose) {print(i)}
    if (is.na(data$VESSNAME[i])) {
      msg <- "No vessel name found for FCC entry"
      data$VES_NOTE[i] = "No FCC info, or FCC match has no vessel name"
    } else if (data$CURRENT_NAME[i] == data$VESSNAME[i]) {
      msg <- "FCC matches current name"
      data$VES_NOTE[i] = "FCC matches CURR NAME"
    } else if (is.na(data$PREV_NAME[i])) {
      msg <- "No PREV_NAME in vessel list"
      data$VES_NOTE[i] = "No PREV_NAME in vesslist"
    } else if (data$PREV_NAME[i] == data$VESSNAME[i]) {
      msg <- "FCC matches previous name"
      data$VES_NOTE[i] = "FCC matches PREV NAME"
    } else {
      msg <- "Previous vessel name doesn't match current"
      data$VES_NOTE[i] = "PREV does not match CURR NAME"
    }
    if (verbose) {print(msg)}
  }
  
  print(paste0(nrow(data)," matches found in FCC ULS data"))
  
  data <- data %>%
    transmute(VESSNMBR, CURRENT_NAME, PREV_NAME,
              FCC_VESSNAME = VESSNAME,
              CALLSIGN, LENGTH = LEN, GT,
              EFF_DATE, EXP_DATE, VES_NOTE)
  
  return(data)
}
