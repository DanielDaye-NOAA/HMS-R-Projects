BuildCP01 <- function (compiled_vessel_permits, vlist_params) {
  
  path.year <- paste0(path.report_files,vlist_params$year1,"/")
  path.mnth <- paste0(path.year,vlist_params$mabb1,"/")
  
  path_CP01 <- "./data/CP01/CP01-Template.xlsx"
  
  CP01_A <- read_xlsx(path_CP01, sheet = 1)
  CP01_B <- read_xlsx(path_CP01, sheet = 2)
  CP01_C <- read_xlsx(path_CP01, sheet = 3)
  
  compiled_vessel_permits <- compiled_vessel_permits %>%
    mutate(FLAG = "USA", FLAGPRV = "USA",
           LEN_TYPE = "LOA", TON_TYPE = "GRT",
           VMS_TYPE = ifelse(GEAR_TYPE == "LL", "VMS-GEN", "NO-VMS"),
           RENEW_MODE = "EXPL") %>%
    mutate(EFF_DATE2 = format(as.Date(EFFDATE), f = "%d/%m/%Y"),
           EXP_DATE2 = format(as.Date(EXPDATE), f = "%d/%m/%Y")) %>%
    
    # If EFF_DATE is earlier than the 13th, set to 13 for that month
    mutate(EFF_DATE2 = ifelse(as.numeric(substr(EFF_DATE2, 1, 2)) < 13,
                              paste0("13", substr(EFF_DATE2, 3, 10)),
                              EFF_DATE2)) %>%
    relocate(EFFDATE, EFF_DATE2, EXPDATE, EXP_DATE2, .after = last_col()) %>%
    mutate(TROP_EFF = ifelse(PERMIT_TYPE != "RO", EFF_DATE2, NA),
           TROP_EXP = ifelse(PERMIT_TYPE != "RO", EXP_DATE2, NA),
           TROP_RENEW = ifelse(PERMIT_TYPE != "RO", "EXPL", NA)) %>%
    mutate(EMAIL = NA, PHONE = NA,
           COUNTRY = "USA")
  
  
  # Additional formatting
  compiled_vessel_permits <- compiled_vessel_permits %>%
    
    # Length and Tonnage
    mutate(CP01_LENGTH = round(CP01_LENGTH, digits = 2),
           CP01_TONNAGE = round(CP01_TONNAGE, digits = 2)) %>%
    relocate(CP01_LENGTH, CP01_TONNAGE, .after = last_col()) %>%
    
    # Formatting Addresses
    mutate(ADDRESS = gsub("\\r|\\n", "", STREETADDRESS),
           STREET = sub("\\s*,.*", "", ADDRESS)) %>%
    rowwise() %>%
    mutate(CITY  = str_split_fixed(ADDRESS, ", ", 3)[,2],
           STATE = substr(str_split_fixed(ADDRESS, ", ", 3)[,3], 1, 2),
           ZIP   = substr(ADDRESS, nchar(ADDRESS)-4, nchar(ADDRESS)),
           ZIP   = ifelse(grepl("-", ZIP), substr(ADDRESS, nchar(ADDRESS)-9, nchar(ADDRESS)-5), ZIP)) %>%
    relocate(STREETADDRESS, ADDRESS, PERMIT_TYPE, STREET, CITY, STATE, ZIP) %>%
    ungroup() %>%
    
    # Make street confidential for RO
    mutate(STREET = ifelse(PERMIT_TYPE == "RO", "CONFIDENTIAL", STREET)) %>%
    
    # Finalizing IMO TYPES
    mutate(INT_TYPE = ifelse(substr(CP01_IMO,1,2)%in%c("12","13") | substr(CP01_IMO,1,1)=="2", "LRN", INT_TYPE),
           INT_TYPE = ifelse(substr(CP01_IMO,1,1)%in%c("7","8","9"), "IMO", INT_TYPE),
           INT_TYPE = ifelse(CP01_IMO=="0000001", "JUS", INT_TYPE)) %>%
    relocate(IMO_NUMBER, CG_IMO, INTREGNO, INT_TYPE, CP01_IMO)
  
  message("Are ICCAT numbers missing for these vessels?")
  print(table(compiled_vessel_permits$VNAME_FLAG, compiled_vessel_permits$CP01_ICCAT == "NEEDS_ICCAT_NUM"))
  Sys.sleep(2)
  
  # Checking HP
  compiled_vessel_permits <- compiled_vessel_permits %>%
    rowwise() %>%
    # IF reported HP matches ICCAT HP, insert that value.
    # IF reported HP doesn't match ICCAT, use the larger HP value
    # Otherwise, if only one value is present (reported or ICCAT), use that one
    mutate(CP01_HP = ifelse(!is.na(HP) & !is.na(HP) & HP == ICCAT_HP, HP, NA),
           CP01_HP = ifelse(!is.na(HP) & !is.na(HP) & HP != ICCAT_HP, max(c(HP, ICCAT_HP)), CP01_HP),
           CP01_HP = ifelse(!is.na(HP) & is.na(ICCAT_HP), HP, CP01_HP),
           CP01_HP = ifelse(is.na(HP) & !is.na(ICCAT_HP), ICCAT_HP, CP01_HP)) %>%
    ungroup()
  
  # Compiling CP01 Forms
  CP01A <- compiled_vessel_permits %>%
    transmute(ICCATSerialNo = ifelse(!is.na(ICCAT_NEW), ICCAT_NEW, CP01_ICCAT), 
              NatRegNo      = VESID, 
              IntRegNo      = CP01_IMO, 
              IRNoType      = INT_TYPE, 
              IRCS          = CP01_IRCS, 
              VesselNameCur = CP01_VNAME,
              VesselNamePrv = NA,
              # VesselNamePrv = CP01_PNAME,
              FlagCurCd     = FLAG, 
              FlagPrvCD     = NA, 
              # FlagPrvCD     = FLAGPRV, 
              OwnerID       = OWNERID, 
              OperatorID    = OPERATORID, 
              IsscfvID      = PERMIT_TYPE, 
              IsscfgID      = GEAR_TYPE, 
              LengthM       = CP01_LENGTH, 
              LenType       = "LOA", 
              Tonnage       = CP01_TONNAGE, 
              TonType       = "GRT",
              CarCapacity = NA, CCapUnitCd = NA, ExternalMark = NA,
              YrBuild  = YEAR_BUILT,
              ShipyNat = NA, HomePort = NA, DepthM = NA,
              EngineHP = CP01_HP, 
              VMSSysCD = VMS_TYPE)
  
  CP01B <- compiled_vessel_permits %>%
    transmute(ICCATSerialNo = CP01_ICCAT, 
              NatRegNo      = VESID, 
              P20mDtFr      = EFF_DATE2,
              P20mDtTo      = EXP_DATE2, 
              P20mRM        = "EXPL",
              SWOn = "X", SWOs = "", 
              ALBn = "X", ALBs = "", 
              TropDtFr = ifelse(GEAR_TYPE %in% c("LL","LX"), EFF_DATE2, ""), 
              TropDtTo = ifelse(GEAR_TYPE %in% c("LL","LX"), EXP_DATE2, ""), 
              TropRM   = ifelse(GEAR_TYPE %in% c("LL","LX"),    "EXPL", ""), 
              SWOmDtFr = "", SWOmDtTo = "", ALBmDtFr = "", ALBmDtTo = "", 
              CarrDtFr = "", CarrDtTo = "", BFEcDtFr = "", BFEcDtTo = "", 
              BFEcFisTyp = "", BFEcFusArea = "", BFEcAQuoKG = "", 
              BFEoDtFr = "", BFEoDtTo = "")
  
  CP01C <- compiled_vessel_permits %>%
    transmute(OwOpEntityID = OWNERID, 
              OwOpName     = PERMIT_HOLDER, 
              OwOpAddrs    = STREET, 
              OwOpCity     = CITY, 
              OwOpZipCd    = ZIP, 
              OwOpCtry     = "USA",
              OwOpEmail    = "", 
              OwOpTel      = "")

  workbook <- loadWorkbook("./data/CP01/CP01-Template.xlsx")
  
  writeData(workbook,        "CP01A (Vessels)", CP01A, startRow = 2, colNames = FALSE)
  writeData(workbook, "CP01B (Authorizations)", CP01B, startRow = 2, colNames = FALSE)
  writeData(workbook,      "CP01C (Ownership)", CP01C, startRow = 2, colNames = FALSE)
  
  savepath <- paste0(path.mnth, vlist_params$year1, "-", vlist_params$mabb1, "_CP01-template-vessel-list.xlsx")
  
  saveWorkbook(workbook, savepath, overwrite = TRUE)
  
  message("Compiled CP01 workbook template!")
  
  browseURL(path.mnth)
  
  invisible()
}
