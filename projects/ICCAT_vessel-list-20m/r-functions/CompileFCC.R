CompileFCC <- function (compiled_vessel_permits, option = "local", save = TRUE) {
  
  filepath <- paste0('./report-files/',vlist_params$year1,"/",vlist_params$mabb1,"/")
  filename <- paste0(vlist_params$year1,"-",vlist_params$mabb1,"_CP01-template-vessel-list-final.xlsx")
  
  vessels <- read_xlsx(paste0(filepath,filename), sheet = 1)
  
  if (!option %in% c("local","scrape")) {
    stop("Select either 'local' or 'scrape' for compiling option...")
  }
  
  
  if (option == "local") {
    # Local compiling ----
    
    FCC_extractions <- ExtractFCC(vessels$NatRegNo, vessels$VesselNameCur, vessels$VesselNamePrv)
    
    
  } else if (option == "scrape") {
    # Scraping FCC ULS ----
    
    stop("option = scrape currently not implemented")
    
    #' This is probably going to break and I'm not going to bother maintaining this code. The "local"
    #' option is much quicker and works reliably without any chance of crashing
    
    fcc.url <- 'https://wireless2.fcc.gov/UlsApp/UlsSearch/searchShip.jsp'
    
    vesid <- vessels$NatRegNo
    
    binman_cdriver <- binman::list_versions("chromedriver")[[1]]
    newest_cdriver <- sort(binman_cdriver, decreasing = TRUE)[1]
    
    driver_port <- free_port()
    
    # Start RSelenium
    rD <- rsDriver(port = driver_port, browser = "chrome", chromever = newest_cdriver, verbose = FALSE)
    remDr <- rD[["client"]]
    
    ## FCC ULS SCRAPE ----
    #' The loop crashes after hitting results for ~17 vessel searches, so best to
    #' split into batches and run every 15 or so
    
    vesselIDs <- vList$VESID_noDO
    
    source("./functions/ScrapeFCC.R")
    
    fcc_batch_1 <- ScrapeFCC(vesselIDs[1:10])
    fcc_batch_2 <- ScrapeFCC(vesselIDs[11:20])
    fcc_batch_3 <- ScrapeFCC(vesselIDs[21:30])
    fcc_batch_4 <- ScrapeFCC(vesselIDs[31:40])
    fcc_batch_5 <- ScrapeFCC(vesselIDs[41:45])
    
    fcc_df <- rbind(fcc_batch_1, fcc_batch_2, fcc_batch_3, fcc_batch_4)
    
    # add owner ID to map to CP01 form 
    fcc_df <- merge(fcc_df, vList[,c('VESID_noDO', 'OwnerID')], 
                    by.x = 'Vessel_ID', 
                    by.y = 'VESID_noDO')
    
    
    # we want only the 'best and most valid' data from the FCC data
    
    
    ## Compare IRCS numbers with what is in the IRCS for the vessels ####
    findat <- cp01a_vessels[c('ICCATSerialNo', 'NatRegNo', 'IRCS', 'VesselNameCur', 'VesselNamePrv')]
    
    str(findat)
    str(fcc_df)
    
    findat$NatRegNo %in% fcc_df$Vessel_ID
    
    # merge with the FCC data 
    fccMerge <- merge(findat, fcc_df, 
                      by.x = "NatRegNo", by.y = "Vessel_ID", all = TRUE)
    
    
    
    # mark which IRCS numbers match 
    fccMerge$ircs_match <- ifelse(fccMerge$IRCS == fccMerge$Permit_ID, '1', '0') %>%
      replace_na("0")
    fccMerge[c("IRCS", "Permit_ID", "ircs_match")]
    
    # mark which didn't have info from FCC database 
    fccMerge$no_fcc_data <- ifelse(is.na(fccMerge$Permit_ID), '1', '0')
    fccMerge[c("IRCS", "Permit_ID", "no_fcc_data", "ircs_match")]
    
    # mark which had fcc info, but nothing from OUR data 
    fccMerge$fccY_veslistN <- ifelse(is.na(fccMerge$IRCS) & !is.na(fccMerge$Permit_ID), '1', '0')
    fccMerge[c("IRCS", "Permit_ID", "fccY_veslistN")]
    
    
    # mark which are EXPIRED but HAVE AN ACTIVE ENTRY 
    fccMerge$expired_has_active <- ifelse((fccMerge$ircs_match != 1) & (fccMerge$Status == "Active"),
                                          "good", "bad")
    fccMerge[c("IRCS","Permit_ID","ircs_match", "Status","expired_has_active")]
    
    fccMerge <- fccMerge %>%
      transmute(NatRegNo, ICCAT_No = ICCATSerialNo, IRCS, FCC_Callsign = Permit_ID,
                OwnerID, Owner_name, Vessel_Name = toupper(Vessel_name), Address, Status, Eff_Date, Exp_Date)
    fccMerge <- fccMerge[!duplicated(fccMerge),]
    
    fccMerge$namechk <- ifelse(fccMerge$Vessel_Name %in% toupper(vList$VESNAME), 1, 0)
    fccMerge[c("NatRegNo", "IRCS", "Vessel_Name", "namechk")]
    
    write.csv(fccMerge, paste0(save.path, "fcc_IRCS_Info.csv"))
  }
  
  message("FCC Extractions Complete! Performing additional checks...")
  
  flagged <- c("No PREV_NAME in vesslist", "PREV does not match CURR NAME")
  
  # QAQC
  FCC_extractions <- FCC_extractions %>% 
    mutate(EFFDATE2 = format(as.Date(EFF_DATE, tryFormats = c("%m/%d/%Y")), f = "%Y-%m-%d")) %>%
    arrange(CURRENT_NAME, desc(EFFDATE2)) %>%
    group_by(VESSNMBR, CURRENT_NAME) %>%
    filter(row_number() == 1) %>%
    select(-EFFDATE2) %>%
    mutate(CURRENT_NAME = gsub("\\.| |F/V", "", CURRENT_NAME),
           PREV_NAME    = gsub("\\.| ", "", PREV_NAME),
           FCC_VESSNAME  = gsub("\\.| ", "", FCC_VESSNAME)) %>%
    mutate(VES_NOTE = ifelse(VES_NOTE %in% flagged & CURRENT_NAME == FCC_VESSNAME, "FCC matches CURR NAME", VES_NOTE))
  
  if (save) {
    write.xlsx(FCC_extractions, 
               file = paste0(filepath, vlist_params$year1, "-", vlist_params$mabb1,"_", "FCC-ULS_vessel-lookup.xlsx"))
  }
  
  message("All FCC processing completed!")
  invisible()
}