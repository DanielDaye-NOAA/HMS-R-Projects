MergeVesselDataSources <- function (oa_pims_permits, ICCAT_vesref, FOSS_vessels, vlist_params) {
  
  # QAQC
  message('Columns with missing permit information in oa_pims_permits:')
  colSums(is.na(oa_pims_permits)) %>% 
    data.frame() %>%
    rename(NUM_MISSING = ".") %>%
    arrange(desc(NUM_MISSING)) %>%
    head() %>% print()
  Sys.sleep(1)
  
  message('Columns with missing permit information in ICCAT_vesref:')
  colSums(is.na(ICCAT_vesref)) %>% 
    data.frame() %>%
    rename(NUM_MISSING = ".") %>%
    arrange(desc(NUM_MISSING)) %>%
    head() %>% print()
  Sys.sleep(1)
  
  message('Columns with missing permit information in FOSS_vessels:')
  colSums(is.na(FOSS_vessels)) %>% 
    data.frame() %>%
    rename(NUM_MISSING = ".") %>%
    arrange(desc(NUM_MISSING)) %>%
    head() %>% print()
  Sys.sleep(1)
  
  FOSS_vessels <- FOSS_vessels %>%
    mutate(VESID = as.character(CG_NUM),
           CG_IMO = as.character(CG_IMO),
           CG_LENGTH = round(CG_LENGTH * 0.3048, digits = 1))
  
  ICCAT_uniqueref <- ICCAT_vesref %>%
    filter(!is.na(NATREGNO)) %>%
    rename(VESID = NATREGNO) %>% 
    mutate(VESID = as.character(VESID)) %>%
    arrange(VESID, STATUS) %>%
    group_by(VESID) %>%
    summarize(ROWCT = n(),
              ICCAT_NUM = paste(unique(ICCAT_NUM), collapse = ":"),
              INTREGNO  = paste(unique(INTREGNO), collapse = ":"),
              INT_TYPE  = paste(unique(INT_TYPE), collapse = ":"),
              ICCAT_IRCS  = paste(unique(IRCS), collapse = ":"),
              ICCAT_VNAME = paste(unique(VESNAME), collapse = ":"),
              ICCAT_PRVNAME = paste(unique(PRVNAME), collapse = ":"),
              FLAG      = paste(unique(FLAG), collapse = ":"),
              ICCAT_LENGTH  = paste(unique(LENGTH_M), collapse = ":"),
              TONNAGE   = paste(unique(TONNAGE), collapse = ":"),
              ICCAT_HP  = paste(unique(HP), collapse = ":"),
              BUILDYR   = paste(unique(BUILDYR), collapse = ":"),
              STATUS = paste(unique(STATUS), collapse = ":")) %>%
    mutate(ICCAT_LENGTH = as.numeric(ICCAT_LENGTH),
           TONNAGE  = as.numeric(TONNAGE),
           ICCAT_HP = as.numeric(ICCAT_HP),
           BUILDYR  = as.numeric(BUILDYR),
           INTREGNO = ifelse(INTREGNO=="NA", NA, INTREGNO),
           INT_TYPE = ifelse(INT_TYPE=="NA", NA, INT_TYPE),
           ICCAT_IRCS     = ifelse(ICCAT_IRCS=="NA", NA, ICCAT_IRCS),
           ICCAT_PRVNAME  = ifelse(ICCAT_PRVNAME=="NA", NA, ICCAT_PRVNAME),
           STATUS = ifelse(STATUS=="ACTIVE:INACTIVE", "ACTIVE", STATUS),
           STATUS = ifelse(STATUS=="INACTIVE:INACTIVE", "INACTIVE", STATUS))
  
  table(ICCAT_uniqueref$STATUS)
  
  compiled_vessel_permits <- oa_pims_permits %>%
    filter(METERS >= 20,
           !grepl("NOVESID",VESID)) %>%
    mutate(VESID = ifelse(substr(VESID,1,2) == "DO", gsub("DO","",VESID), VESID)) %>%
    left_join(FOSS_vessels %>% select(-CG_NUM), by = "VESID") %>%
    left_join(ICCAT_uniqueref, by = "VESID") %>%
    relocate(ICCAT, ICCAT_NUM, IMO_NUMBER, CG_IMO, INTREGNO, INT_TYPE,
             VESID, VESNAME, ICCAT_VNAME, ICCAT_PRVNAME,
             PERMIT, CHBENDORSEMENT, PERMIT_TYPE, GEAR_TYPE, 
             IRCS, ICCAT_IRCS,
             METERS, CG_LENGTH, ICCAT_LENGTH, 
             GT, CG_TONNAGE, TONNAGE, HP, ICCAT_HP)
  
  permit_types <- compiled_vessel_permits %>%
    group_by(VESID) %>% 
    summarize(N = n(),
              PERMITS = paste(unique(str_extract(PERMIT, "[[:ALPHA:]]+")), collapse = ";")) %>%
    mutate(include_vess = ifelse(grepl("ATL|CHARTER|GENERAL|SFH|ANGLING", PERMITS), TRUE, FALSE))
  
  length(unique(compiled_vessel_permits$VESID))
  
  compiled_vess_unique <- compiled_vessel_permits %>%
    left_join(permit_types %>% select(-N), by = "VESID") %>%
    filter(grepl("ATL|CHARTER|GENERAL|SFH|ANGLING", PERMIT)) %>%
    mutate(VESNAME = gsub("DO|D0", "", VESNAME),
           VESNAME = gsub("\u0092", "'", VESNAME))
  
  # PRE - QAQC TABLES ----
  compiled_vess_unique %>% arrange(PERMITS) %>% select(PERMITS, PERMIT_TYPE) %>% table()
  
  # QAQC ----
  ## ICCAT_NUM ----
  compiled_vess_unique <- compiled_vess_unique %>%
    mutate(CP01_ICCAT = ICCAT,
           CP01_ICCAT = ifelse(!is.na(ICCAT)&!is.na(ICCAT_NUM)&ICCAT!=ICCAT_NUM, "ICCAT_MISMATCH", CP01_ICCAT),
           CP01_ICCAT = ifelse(is.na(CP01_ICCAT) & !is.na(ICCAT_NUM), ICCAT_NUM, CP01_ICCAT),
           CP01_ICCAT = ifelse(is.na(CP01_ICCAT), "NEEDS_ICCAT_NUM", CP01_ICCAT)) %>%
    rowwise() %>%
    mutate(CP01_ICCAT = ifelse(grepl(":",CP01_ICCAT), str_split_1(CP01_ICCAT,":"), CP01_ICCAT)) %>%
    ungroup() %>%
    relocate(ICCAT, ICCAT_NUM, CP01_ICCAT)
  
  ## IMO (INTREGNO) ----
  compiled_vess_unique <- compiled_vess_unique %>%
    mutate(CP01_IMO = IMO_NUMBER,
           IMO_FLAG = ifelse(IMO_NUMBER==VESID, "IMO = VESID", NA),
           IMO_FLAG = ifelse(is.na(IMO_NUMBER)&is.na(CG_IMO)&is.na(INTREGNO), "IMO_MISSING", IMO_FLAG),
           IMO_FLAG = ifelse(((!is.na(IMO_NUMBER) & !is.na(CG_IMO))|(!is.na(INTREGNO))) &
                               (IMO_NUMBER != CG_IMO | IMO_NUMBER != INTREGNO), "IMO_MISMATCH", IMO_FLAG),
           IMO_FLAG2 = ifelse(PERMIT_TYPE == "RO" & !is.na(INTREGNO) & !(INTREGNO%in%c("1","0000001")), "HISTORIC_IMO", NA)) %>%
    mutate(CP01_IMO = ifelse(is.na(CP01_IMO) & PERMIT_TYPE == "RO" & INTREGNO == "0000001", "0000001", CP01_IMO),
           CP01_IMO = ifelse(is.na(CP01_IMO) & IMO_FLAG2 == "HISTORIC_IMO", INTREGNO, CP01_IMO),
           CP01_IMO = ifelse(is.na(CP01_IMO) & PERMIT_TYPE == "RO", "0000001", CP01_IMO),
           CP01_IMO = ifelse(is.na(CP01_IMO) & INT_TYPE == "LRN" & !is.na(INTREGNO), INTREGNO, CP01_IMO)) %>%
    relocate(VESID, PERMIT_TYPE, STATUS, IMO_NUMBER, CG_IMO, INTREGNO, CP01_IMO, IMO_FLAG, IMO_FLAG2, .after = last_col())
  
  
  ## LENGTHS ----
  compiled_vess_unique <- compiled_vess_unique %>%
    mutate(CP01_LENGTH = CG_LENGTH,
           CP01_LENGTH = ifelse(is.na(CG_LENGTH) & !is.na(METERS), METERS, CP01_LENGTH),
           CP01_TONNAGE = CG_TONNAGE,
           CP01_TONNAGE = ifelse(is.na(CG_TONNAGE) & !is.na(GT), GT, CP01_TONNAGE)) %>%
    rowwise() %>%
    mutate(GT_FLAG = ifelse(sd(c(GT, CG_TONNAGE, TONNAGE), na.rm = T) > 5, "GT MISMATCH", NA)) %>%
    relocate(METERS, CG_LENGTH, ICCAT_LENGTH, CP01_LENGTH, GT, CG_TONNAGE, TONNAGE, CP01_TONNAGE, GT_FLAG, .after = last_col())
  
  ## NATREGNO ----
  #' VESID is only present in 1 spot, so will carry over directly into the CP01 form
  
  ## VESNAME ----
  compiled_vess_unique <- compiled_vess_unique %>%
    mutate(VESNAME = gsub("^\\s+|\\s+$", "", VESNAME),                  # Regex to remove leading
           ICCAT_VNAME = gsub("^\\s+|\\s+$", "", ICCAT_VNAME),          # and trailing whitespace in
           ICCAT_PRVNAME = gsub("^\\s+|\\s+$", "", ICCAT_PRVNAME),      # each VESNAME column
           CP01_VNAME = VESNAME,
           VNAME_FLAG = ifelse(VESNAME == ICCAT_VNAME, "SAME_NAME", NA),
           VNAME_FLAG = ifelse(VESNAME != ICCAT_VNAME, "NEW_NAME", VNAME_FLAG),
           VNAME_FLAG = ifelse(!is.na(VESNAME) & is.na(ICCAT_VNAME), "NEW_VESSEL", VNAME_FLAG),
           CP01_PNAME = ifelse(!is.na(ICCAT_PRVNAME) & (VESNAME != ICCAT_PRVNAME), ICCAT_PRVNAME, NA),
           CP01_PNAME = ifelse(VNAME_FLAG == "NEW_NAME", ICCAT_VNAME, CP01_PNAME)) %>%
    relocate(VESNAME, ICCAT_VNAME, ICCAT_PRVNAME, CP01_VNAME, CP01_PNAME, VNAME_FLAG, .after = last_col())
  
  ## IRCS ----
  compiled_vess_unique <- compiled_vess_unique %>% 
    mutate(CP01_IRCS = IRCS,
           CP01_IRCS = ifelse(is.na(CP01_IRCS), ICCAT_IRCS, CP01_IRCS),
           IRCS_FLAG = ifelse(!is.na(IRCS) & !is.na(ICCAT_IRCS) & (IRCS != ICCAT_IRCS), "IRCS != ICCAT_IRCS", NA)) %>%
    relocate(IRCS, ICCAT_IRCS, CP01_IRCS, IRCS_FLAG, .after = last_col())
  
  data_QAQC_summary <- data.frame(vessel_count = nrow(compiled_vess_unique),
                                  num_same_name = sum(compiled_vess_unique$VNAME_FLAG == "SAME_NAME"),
                                  num_new_name = sum(compiled_vess_unique$VNAME_FLAG == "NEW_NAME"),
                                  num_new_ves = sum(compiled_vess_unique$VNAME_FLAG == "NEW_VESSEL"),
                                  init_IRCS = sum(!is.na(compiled_vess_unique$IRCS)),
                                  finl_IRCS = sum(!is.na(compiled_vess_unique$CP01_IRCS)),
                                  flag_IRCS = sum(!is.na(compiled_vess_unique$IRCS_FLAG)))
  
  print(t(data_QAQC_summary))
  
  # If CP01_PNAME has multiple names listed, use the most recent name
  compiled_vess_unique <- compiled_vess_unique %>% 
    mutate(CP01_VNAME = VESNAME,
           CP01_PNAME = ifelse(VESNAME == ICCAT_VNAME & VESNAME != ICCAT_PRVNAME, ICCAT_PRVNAME, NA)) %>%
    relocate(VESNAME, ICCAT_VNAME, ICCAT_PRVNAME, CP01_VNAME, CP01_PNAME, .after = last_col())
  
  
  # Checking Length ~ Weights
  
  compiled_vess_unique %>%
    ggplot(aes(CP01_LENGTH, CP01_TONNAGE)) +
    geom_point() + theme_bw()
  
  # Numbering
  compiled_vess_unique <- compiled_vess_unique %>% 
    ungroup() %>%
    arrange(PERMIT_TYPE, CHBENDORSEMENT, CP01_VNAME) %>%
    mutate(OWNERID = 1:nrow(compiled_vess_unique),
           OPERATORID = 1:nrow(compiled_vess_unique)) %>%
    relocate(CP01_ICCAT, VESID, CP01_IMO, PERMITS, PERMIT_TYPE, GEAR_TYPE, CHBENDORSEMENT, 
             CP01_VNAME, CP01_PNAME, OWNERID, OPERATORID, CP01_LENGTH, CP01_TONNAGE)
  
  return(compiled_vess_unique)
}
