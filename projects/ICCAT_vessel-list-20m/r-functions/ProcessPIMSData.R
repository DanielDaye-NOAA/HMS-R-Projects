ProcessPIMSData <- function (vlist_params, oa_permits, verbose = FALSE) {
  
  path.year <- paste0(path.report_files,vlist_params$year1,"/")
  path.mnth <- paste0(path.year,vlist_params$mabb1,"/")
  
  pims_filename <- paste0("PIMS_", vlist_params$year1, "_", vlist_params$mabb1, ".xlsx")
  
  
  # Move the PIMS data file from /Downloads/ to the proper report-files folder
  if (sum(grepl(pims_filename, list.files(path.mnth))) > 0) {
    message(paste("PIMS data already located in", path.mnth))
  } else {
    message(paste("Moving PIMS data to", path.mnth))
    
    path_env_user <- paste0(Sys.getenv("USERPROFILE"), "/Downloads/")
    
    pims_index <- grepl(paste0("Permits - ", vlist_params$current_date), list.files(path_env_user))
    
    message(paste0("Located '", list.files(path_env_user)[pims_index], "' in Downloads folder"))
    
    # Copy to report-files and rename
    file.copy(paste0(path_env_user, list.files(path_env_user)[pims_index]), paste0(path.mnth))
    file.rename(paste0(path.mnth, list.files(path_env_user)[pims_index]), 
                paste0(path.mnth, "PIMS_", vlist_params$year1, "_", vlist_params$mabb1, ".xlsx"))
  }
  
  # Load in the PIMS data and standardize column names between NFPLRS and PIMS
  pims_permits <- read_xlsx(paste0(path.mnth, pims_filename), guess_max = 1e6, skip = 5) %>%
    select(-c(`Vessel Home Port (City, State, County)`, `IssueDate`))
  
  names(oa_permits)
  names(pims_permits)
  
  setdiff(names(oa_permits), names(pims_permits))
  
  names(pims_permits) <- names(oa_permits %>% select(-c(IRCS, CHBENDORSEMENT)))
  
  if (!sum(names(pims_permits) %in% names(oa_permits)) == length(names(pims_permits))) {
    stop("There is an issue with the column names between pims_permits and oa_permits")
  } else {
    message("All pims_permits column names located in oa_permits!")
  }
  
  table(substr(pims_permits$EFFDATE, 1, 7))
  
  pims_permits <- pims_permits %>%
    filter(substr(EFFDATE, 1, 7) == paste0(vlist_params$year1, "-", sprintf("%02d", vlist_params$month1))) 
  
  # Remove duplicate VESID values by collapsing PERMITS into one value
  pims_summary <- pims_permits %>%
    arrange(VESID, PERMIT) %>%
    group_by(VESID) %>%
    summarize(PERMITS = paste(unique(str_extract(PERMIT, "[[:ALPHA:]]+")), collapse = ":"),
              NPERMIT = n()) %>%
    mutate(PERMIT = ifelse(NPERMIT == 1 & grepl("SKI|SKD", PERMITS), NA, PERMITS),
           PERMIT = ifelse(grepl("ATL|SF*", PERMITS), PERMIT, NA))
  
  if (verbose) {
    pims_permits %>%
      select(-PERMIT) %>% 
      left_join(pims_summary %>% select(-PERMITS), by = "VESID") %>%
      distinct() %>% 
      mutate(METERS = round(as.numeric(METERS) * 0.3048), digits = 1) %>%
      filter(!is.na(PERMIT), METERS > 20) %>% 
      head() %>% print()
  }
  
  # QAQC
  setdiff(names(pims_permits), names(oa_permits))  # Should be character(0)
  setdiff(names(oa_permits), names(pims_permits))  # Should be [1] "IRCS" "CHBENDORSEMENT"
  
  # Setting all to character so they can be combined
  oa_permits <- oa_permits %>% 
    mutate(across(everything(), as.character))
  pims_permits <- pims_permits %>% 
    mutate(METERS = round(as.numeric(METERS) * 0.3048, digits = 1),
           across(everything(), as.character))
  
  oa_pims_permits <- bind_rows(oa_permits, pims_permits) %>%
    arrange(VESID) %>%
    mutate(METERS = as.numeric(METERS), 
           GT = as.numeric(GT), 
           HP = as.numeric(HP), 
           YEAR_BUILT = as.numeric(YEAR_BUILT))
  
  # SKI/SKD: Drillship Check ----
  # Vessels > 150m
  if (sum(oa_pims_permits$METERS > 150, na.rm = T) > 0) {
    cat("There are", sum(oa_pims_permits$METERS > 150, na.rm = T), "vessels longer than 150m \n")
    oa_pims_permits %>% 
      filter(METERS > 150) %>% 
      select(VESID, VESNAME, METERS, GT, HP, YEAR_BUILT) %>%
      print()
    
    oa_pims_permits <- oa_pims_permits %>% 
      filter(METERS < 150)
  } else {
    cat("There are no vessels longer than 150m \n")
  }
  
  # Permit Check
  oa_pims_permits <- oa_pims_permits %>%
    mutate(SKISKD_ONLY = ifelse(grepl("SKI|SKD", PERMIT) & !grepl("ATL", PERMIT), TRUE, FALSE))
  
  cat(sum(oa_pims_permits$SKISKD_ONLY), "vessels only have SKI or SKD \n")
  
  message("Removing SKI/SKD-ONLY Vessels...")
  oa_pims_permits <- oa_pims_permits %>%
    filter(!SKISKD_ONLY)
  
  # QAQC ----
  message(sum(is.na(oa_pims_permits$METERS) | oa_pims_permits$METERS == 0), " Vessels missing length info")
  
  # Vessel Type
  oa_pims_permits <- oa_pims_permits %>%
    mutate(PERMIT_TYPE = ifelse(grepl("ATL-", PERMIT), "LL", NA),
           PERMIT_TYPE = ifelse(grepl("CHARTER", PERMIT) & CHBENDORSEMENT == "Y", "LP", PERMIT_TYPE),
           PERMIT_TYPE = ifelse(grepl("SFH|GENERAL", PERMIT), "LP", PERMIT_TYPE),
           PERMIT_TYPE = ifelse(grepl("ANGLING", PERMIT), "RO", PERMIT_TYPE),
           PERMIT_TYPE = ifelse(grepl("CHARTER", PERMIT) & CHBENDORSEMENT == "N", "RO", PERMIT_TYPE),
           PERMIT_TYPE = ifelse(is.na(PERMIT_TYPE), "CHECK", PERMIT_TYPE)) %>%
    mutate(GEAR_TYPE = ifelse(PERMIT_TYPE == "LL", "LL", NA),
           GEAR_TYPE = ifelse(PERMIT_TYPE == "LP", "LX", GEAR_TYPE),
           GEAR_TYPE = ifelse(PERMIT_TYPE == "RO", "RG", GEAR_TYPE),
           GEAR_TYPE = ifelse(is.na(GEAR_TYPE), "CHECK", GEAR_TYPE))
  
  oa_pims_permits %>%
    select(PERMIT, CHBENDORSEMENT, PERMIT_TYPE) %>%
    arrange(PERMIT_TYPE, CHBENDORSEMENT, PERMIT)
  
  oa_pims_permits %>%
    select(PERMIT_TYPE, GEAR_TYPE) %>%
    table()
  
  
  
  # Saving oa_pims_permits
  message("Saving oa_pims_permits dataset...")
  oa_p_filename <- paste0(path.mnth, "oa_pims_permits")
  
  write_csv(oa_pims_permits,  file = paste0(oa_p_filename, ".csv"))
  write.xlsx(oa_pims_permits, file = paste0(oa_p_filename, ".xlsx"))
  
  message("PIMS data processed and combined with NFPLRS!")
  return(oa_pims_permits)
}
