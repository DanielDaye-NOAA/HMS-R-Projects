LoadICCATVesselRef <- function () {
  
  path_env_user <- gsub("[\\]", "/", paste0(Sys.getenv("USERPROFILE"), "/Downloads/"))
  
  activ <- sum(grepl("_active.xls", list.files(path_env_user))) > 0
  inact <- sum(grepl("_inactive.xls", list.files(path_env_user))) > 0
  inopr <- sum(grepl("_inoperative.xls", list.files(path_env_user))) > 0
  
  if (activ & inact & inopr) {
    message("ICCAT vessel list source files found!")
  } else {
    message("Downloading ICCAT Vessel Reference Files...")
    # Download ICCAT Reference files
    message("Downloading up")
    browseURL("https://www.iccat.int/en/Vesexport.asp?vessAll=True&vStatus=1")  # Active
    browseURL("https://www.iccat.int/en/Vesexport.asp?vStatus=2")               # Inactive
    browseURL("https://www.iccat.int/en/Vesexport.asp?vStatus=3")               # Inoperative
    
    message("Please each ICCAT files and re-save them as Excel 97 workbooks (.xls)")
    readline("Press Enter when ICCAT files have downloaded...")
  }

  # Grab file paths
  active_file <- paste0(path_env_user, list.files(path_env_user)[grepl("_active.xls", list.files(path_env_user))])
  inactv_file <- paste0(path_env_user, list.files(path_env_user)[grepl("_inactive.xls", list.files(path_env_user))])
  inoper_file <- paste0(path_env_user, list.files(path_env_user)[grepl("_inoperative.xls", list.files(path_env_user))])
  
  # Load ICCAT files, subset, QAQC
  active <- read_xls(active_file, guess_max = 1e6) %>%
    transmute(ICCAT_NUM = as.character(ICCATSerialNo),
              NATREGNO  = as.character(NatRegNo),
              INTREGNO  = as.character(IntRegNo),
              INT_TYPE  = as.character(IRNoTypeCode),
              IRCS, 
              VESNAME = VesselName,
              PRVNAME = VesselNamePrev,
              FLAG    = FlagVesCode,
              FLAGPRV = FlagVesCodePrev,
              LENGTH_M = as.numeric(gsub(",", ".", LOAm)),
              TONNAGE  = as.numeric(gsub(",", ".", Tonnage)),
              HP       = as.numeric(gsub(",", ".", EnginePowerHP)),
              BUILDYR  = as.numeric(YearBuilt)) %>%
    filter(FLAG == "USA") %>%
    mutate(STATUS = "ACTIVE")
  
  inactv <- read_xls(inactv_file, guess_max = 1e6) %>%
    transmute(ICCAT_NUM = as.character(ICCATSerialNo),
              NATREGNO  = as.character(NatRegNo),
              INTREGNO  = as.character(IntRegNo),
              INT_TYPE  = as.character(IRNoTypeCode),
              IRCS, 
              VESNAME = VesselName,
              PRVNAME = VesselNamePrev,
              FLAG    = FlagVesCode,
              FLAGPRV = FlagVesCodePrev,
              LENGTH_M = as.numeric(gsub(",", ".", LOAm)),
              TONNAGE  = as.numeric(gsub(",", ".", Tonnage)),
              HP       = as.numeric(gsub(",", ".", EnginePowerHP)),
              BUILDYR  = as.numeric(YearBuilt)) %>%
    filter(FLAG == "USA") %>%
    mutate(STATUS = "INACTIVE")
  
  inoper <- read_xls(inoper_file, guess_max = 1e6) %>%
    transmute(ICCAT_NUM = as.character(ICCATSerialNo),
              NATREGNO  = as.character(NatRegNo),
              INTREGNO  = as.character(IntRegNo),
              INT_TYPE  = as.character(IRNoTypeCode),
              IRCS, 
              VESNAME = VesselName,
              PRVNAME = VesselNamePrev,
              FLAG    = FlagVesCode,
              FLAGPRV = FlagVesCodePrev,
              LENGTH_M = as.numeric(gsub(",", ".", LOAm)),
              TONNAGE  = as.numeric(gsub(",", ".", Tonnage)),
              HP       = as.numeric(gsub(",", ".", EnginePowerHP)),
              BUILDYR  = as.numeric(YearBuilt)) %>%
    filter(FLAG == "USA") %>%
    mutate(STATUS = "INOPERATIVE")
  
  ICCAT_VesRef <- rbind(active, inactv, inoper) %>%
    mutate(INTREGNO = ifelse(INTREGNO == "1", "0000001", INTREGNO),
           IRCS = ifelse(IRCS %in% c("(blank)","(n/a)"), NA, IRCS),
           VESNAME = toupper(gsub("\u0092", "'", VESNAME)),
           PRVNAME = toupper(gsub("\u0092", "'", PRVNAME)),
           NATREGNO = ifelse(NATREGNO == "(blank)", NA, NATREGNO),
           NATREGNO = ifelse(substr(NATREGNO,1,2) == "DO", gsub("DO", "", NATREGNO), NATREGNO),
           INT_TYPE = ifelse(INT_TYPE == "unk", NA, INT_TYPE))
  
  return(ICCAT_VesRef)
}
