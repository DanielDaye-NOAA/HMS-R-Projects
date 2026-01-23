#' Ugly helper function to correct issues associated with EFH data (don't remember the context where
#' this was used). Includes Region (ATL, Gulf) mislabels, fixing sex and life stage errors

FinalizeLS <- function(data) {
  dfinal <- data %>%
    
    # Fix issues with ATL/GOM assigned regions
    mutate(REGION = ifelse(LAT <= (LON*-2.39 - 167.08) | LAT < 25.34, "GOM", "ATL")) %>%
    
    # Correcting SEX specific issues with formatting
    mutate(SEX = ifelse(toupper(SEX) %in% c("F","F?","FEMALE"), "F", SEX),
           SEX = ifelse(toupper(SEX) %in% c("M","MALE","MATURE MALE"), "M", SEX),
           SEX = ifelse(!toupper(SEX) %in% c("M","F"), "UNK", SEX)) %>%
    
    # Correcting LIFESTAGES for each species' proposed LIFESTAGES
    mutate(LSFINAL = ifelse(COM_NAME == "ALBACORE" & LIFESTAGE %in% c("JUV","ADU","UNK"), "JUV-ADU", NA),
           LSFINAL = ifelse(COM_NAME == "ATLANTIC_ANGEL", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "ATLANTIC_SHARPNOSE" & REGION == "ATL" & LIFESTAGE %in% c("NEO","JUV","ADU"), LIFESTAGE, LSFINAL),
           LSFINAL = ifelse(COM_NAME == "ATLANTIC_SHARPNOSE" & REGION == "GOM" & LIFESTAGE == "NEO", "NEO", LSFINAL),
           
           #' GOM ATLANTIC_SHARPNOSE are split out by sex
           LSFINAL = ifelse(COM_NAME == "ATLANTIC_SHARPNOSE" & REGION == "GOM" & (LIFESTAGE %in% c("JUV","UNK") | (LIFESTAGE == "ADU" & SEX %in% c("M","UNK"))), "JUV-ADUM", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "ATLANTIC_SHARPNOSE" & REGION == "GOM" & (LIFESTAGE == "ADU" & SEX == "F"), "ADUF", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "BASKING_SHARK", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "BIGEYE_SIXGILL", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "BIGEYE_THRESHER_SHARK", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "BIGEYE_TUNA" & LIFESTAGE == "SEL", "SEL", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "BIGEYE_TUNA" & LIFESTAGE %in% c("JUV","ADU","UNK"), "JUV-ADU", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "BIGNOSE_SHARK", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME %in% c("BLUE_MARLIN","WHITE_MARLIN","SAILFISH","LONGBILL_SPEARFISH","ROUNDSCALE_SPEARFISH") & LIFESTAGE == "SEL", "SEL", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "BLACKNOSE_SHARK" & REGION == "ATL", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "BLACKNOSE_SHARK" & REGION == "GOM" & LIFESTAGE == "NEO", "NEO", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "BLACKNOSE_SHARK" & REGION == "GOM" & LIFESTAGE %in% c("JUV","ADU","UNK"), "JUV-ADU", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "BLACKTIP_SHARK" & LIFESTAGE == "NEO", "NEO", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "BLACKTIP_SHARK" & LIFESTAGE %in% c("JUV","ADU","UNK"), "JUV-ADU", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "BLUE_MARLIN" & LIFESTAGE %in% c("JUV","ADU","UNK"), "JUV-ADU", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "BLUE_SHARK" & LIFESTAGE == "NEO", "NEO", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "BLUE_SHARK" & LIFESTAGE %in% c("JUV","ADU","UNK"), "JUV-ADU", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "BLUEFIN_TUNA" & LIFESTAGE == "SEL", "SEL", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "BLUEFIN_TUNA" & LIFESTAGE == "JUV", "JUV", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "BLUEFIN_TUNA" & LIFESTAGE == "ADU", "ADU", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "BLUNTNOSE_SIXGILL", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "BONNETHEAD_SHARK" & REGION == "ATL" & LIFESTAGE == "NEO", "NEO", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "BONNETHEAD_SHARK" & REGION == "ATL" & LIFESTAGE %in% c("JUV","ADU","UNK"), "JUV-ADU", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "BONNETHEAD_SHARK" & REGION == "GOM", "ALL-LS", LSFINAL),
           
           #' There are a handful of BULL_SHARK observations for JUV that don't have any sort of LENGTH
           #' information. Since we're splitting based on a LENGTH threshold for the LIFESTAGES here,
           #' these are assigned LSFINAL = NA so that they can be removed when grouping by LIFESTAGE
           #' but will be included when calculating summary statistics for BULL_SHARK as a whole.
           LSFINAL = ifelse(COM_NAME == "BULL_SHARK" & (LIFESTAGE == "NEO" | (LIFESTAGE == "JUV" & LENGTH < 150)), "NEO-SMJUV", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "BULL_SHARK" & (LIFESTAGE %in% c("ADU","UNK") | (LIFESTAGE == "JUV" & LENGTH >= 150)), "LGJUV-ADU", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "CARCHARHINUS_POROSUS", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "CARIBBEAN_REEF_SHARK", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "CARIBBEAN_SHARPNOSE", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "CAROLINA_HAMMERHEAD", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "COMMON_THRESHER_SHARK", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "DUSKY_SHARK" & LIFESTAGE == "NEO", "NEO", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "DUSKY_SHARK" & LIFESTAGE %in% c("JUV","ADU","UNK"), "JUV-ADU", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "FINETOOTH_SHARK", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "FLORIDA_SMOOTHHOUND", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "GALAPAGOS_SHARK", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "GREAT_HAMMERHEAD", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "GULF_SMOOTHHOUND", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "LEMON_SHARK" & LIFESTAGE %in% c("NEO","JUV","ADU"), LIFESTAGE, LSFINAL),
           LSFINAL = ifelse(COM_NAME == "LONGBILL_SPEARFISH" & LIFESTAGE %in% c("JUV","ADU","UNK"), "JUV-ADU", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "LONGFIN_MAKO_SHARK", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "NARROWTOOTH_SHARK", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "NIGHT_SHARK", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "NURSE_SHARK" & LIFESTAGE == "NEO", "NEO", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "NURSE_SHARK" & LIFESTAGE %in% c("JUV","ADU","UNK"), "JUV-ADU", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "OCEANIC_WHITETIP_SHARK", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "PORBEAGLE", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "ROUNDSCALE_SPEARFISH" & LIFESTAGE %in% c("JUV","ADU","UNK"), "JUV-ADU", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "SAILFISH" & LIFESTAGE %in% c("JUV","ADU","UNK"), "JUV-ADU", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "SAND_TIGER_SHARK" & LIFESTAGE %in% c("NEO","JUV","SUB-ADU"), "NEO-JUV", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "SAND_TIGER_SHARK" & LIFESTAGE == "ADU", "ADU", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "SANDBAR_SHARK" & LIFESTAGE == "NEO", "NEO", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "SANDBAR_SHARK" & LIFESTAGE %in% c("JUV","ADU","UNK"), "JUV-ADU", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "SCALLOPED_HAMMERHEAD" & LIFESTAGE == "NEO", "NEO", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "SCALLOPED_HAMMERHEAD" & LIFESTAGE %in% c("JUV","ADU","UNK"), "JUV-ADU", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "SHARPNOSE_SEVENGILL", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "SHORTFIN_MAKO_SHARK", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "SILKY_SHARK", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "SKIPJACK_TUNA" & LIFESTAGE == "SEL", "SEL", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "SKIPJACK_TUNA" & LIFESTAGE %in% c("JUV","ADU","UNK"), "JUV-ADU", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "SMOOTH_DOGFISH", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "SMOOTH_HAMMERHEAD", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "SMOOTHHOUND_COMPLEX", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "SPINNER_SHARK" & LIFESTAGE == "NEO", "NEO", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "SPINNER_SHARK" & LIFESTAGE %in% c("JUV","ADU","UNK"), "JUV-ADU", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "SWORDFISH" & LIFESTAGE == "SEL", "SEL", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "SWORDFISH" & LIFESTAGE %in% c("JUV","ADU","UNK"), "JUV-ADU", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "TIGER_SHARK" & LIFESTAGE == "NEO", "NEO", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "TIGER_SHARK" & LIFESTAGE %in% c("JUV","ADU","UNK"), "JUV-ADU", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "WHALE_SHARK", "ALL-LS", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "WHITE_MARLIN" & LIFESTAGE %in% c("JUV","ADU","UNK"), "JUV-ADU", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "WHITE_SHARK" & LIFESTAGE == "NEO", "NEO", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "WHITE_SHARK" & LIFESTAGE %in% c("JUV","ADU","UNK"), "JUV-ADU", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "YELLOWFIN_TUNA" & LIFESTAGE == "SEL", "SEL", LSFINAL),
           LSFINAL = ifelse(COM_NAME == "YELLOWFIN_TUNA" & LIFESTAGE %in% c("JUV","ADU","UNK"), "JUV-ADU", LSFINAL)) %>%
    
    # Fixing COM_NAME for special groups (combined larvae, smoothhounds)
    mutate(COMFINAL = ifelse(grepl("DOGFISH|HOUND", COM_NAME), "SMOOTHHOUND_COMPLEX", COM_NAME),
           COMFINAL = ifelse(grepl("MARLIN|SAILFISH|SPEARFISH", COM_NAME) & LIFESTAGE == "SEL", "BILLFISH_LARVAE", COMFINAL),
           LSFINAL = ifelse(COMFINAL == "SMOOTHHOUND_COMPLEX", "ALL-LS", LSFINAL))  %>%
    mutate(REGFINAL = ifelse(COMFINAL %in% c("ATLANTIC_SHARPNOSE","BLACKNOSE_SHARK","BLACKTIP_SHARK","BONNETHEAD_SHARK","SMOOTHHOUND_COMPLEX"), REGION, "ALL")) %>%
    mutate(LSFINAL = factor(LSFINAL, levels = c("SEL","NEO","NEO-JUV","NEO-SMJUV","JUV","JUV-ADU","JUV-ADUM","LGJUV-ADU","ADU","ADUF","ALL-LS"))) %>%
    arrange(COMFINAL, REGFINAL, LSFINAL)
  
  return(dfinal)
}