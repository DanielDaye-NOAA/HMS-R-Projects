# Extract Vessel Data from Fisheries One Stop Shop (FOSS)

#' If VESSELS.csv has already been downloaded (and is located within the current month's report folder),
#' then this function will skip the scraping step and load in the data from the downloaded version of
#' the report file

QueryFOSS <- function (oa_pims_permits, vlist_params, option = "api", rsbrowser = "chrome") {
  
  path.year <- paste0(path.report_files,vlist_params$year1,"/")
  path.mnth <- paste0(path.year,vlist_params$mabb1,"/")
  
  if (!option %in% c("api","scrape")) {
    stop("Please select either 'api' or 'scrape' for the query option.")
  }
  
  message("Removing ", sum(grepl("NOVESID", oa_pims_permits$VESID)), " entries with NOVESID")
  oa_pims_permits <- oa_pims_permits %>%
    filter(!grepl("NOVESID", VESID))
  
  # FOSS needs VESIDs to be colon-separated
  oa_pims_vesid <- paste(sort(unique(oa_pims_permits$VESID)), collapse = ":")
  
  #' TROUBLESHOOTING
  #' ---------------
  #' RSelenium relies on an up-to-date version of chromedriver to be downloaded for use by binman.
  #' RSelenium will launch a chrome browser and navigate to FOSS, input the provided VESID, and 
  #' scrape the required data for the vessel list. Additionally requires a local Java installation
  #' (not SQL Developer).
  #'  
  #' CHECKING BINMAN CHROMEDRIVER VERSION
  #' ------------------------------------
  #' Run the line below to check if a chromedriver update is required
  #' binman::list_versions("chromedriver")
  #' 
  #' ADDITIONAL NOTES
  #' ----------------
  #' Additional documentation for troubleshooting RSelenium and scraping FOSS is provided on 
  #' Confluence.
  
  if (!file.exists(paste0(path.mnth, "VESSELS.csv"))) {
    message("VESSELS.csv not found in ", path.mnth, " - querying FOSS...")
    
    if (option == "scrape") {
      
      # RSelenium ----
      message("Scraping FOSS via RSelenium...")
      
      binman_cdriver <- binman::list_versions("chromedriver")[[1]]
      newest_cdriver <- sort(binman_cdriver, decreasing = TRUE)[1]
      
      driver_port <- free_port()
      rs_driver <- rsDriver(port = driver_port, browser = rsbrowser, chromever = newest_cdriver, verbose = FALSE, phantomver = NULL)
      message("Waiting for Chrome to Load....")
      readline("Once the Chrome window has opened, maximize and press Enter to continue...")
      
      rem_driver <- remoteDriver(port = driver_port)
      remDr <-  rs_driver[["client"]]
      
      remDr$setTimeout(type = "page load", milliseconds = 60000)
      
      message("Navigating to FOSS")
      
      ## Scraping ----
      
      # Navigate to FOSS webpage
      remDr$navigate("https://www.fisheries.noaa.gov/foss/f?p=215:4")
      readline("Press Enter once FOSS has loaded...")
      
      # Select "Vessel Name" radio button
      rb.vesname <- remDr$findElement("css", "#P4_VES_LOV")
      rb.vesname$highlightElement()
      
      # Switches to the "Vessel Number" selection
      rb.vesname.child <- rb.vesname$findChildElement(using = 'css', value = 'input')
      rb.vesname.child$sendKeysToElement(list(key = 'left_arrow',    # selects the "Vessel ID"
                                              key = 'right_arrow'))  # option and box
      
      # Select the "Vessel Number" textbox and fill with vessel info
      input.vesnum <- remDr$findElement("xpath", '//*[@id="P4_USCG_NUMBER_input"]')
      input.vesnum$highlightElement()
      input.vesnum$sendKeysToElement(list(oa_pims_vesid))
      
      # Clicks "Run Report"
      button.report <- remDr$findElement("css", "#p4_go")
      button.report$highlightElement()
      button.report$clickElement()
      Sys.sleep(5)
      
      # Select "Vessel Check" from the dropdown
      dropdown <- remDr$findElement('css', "#interactive_report_vessels_saved_reports")
      dropdown$highlightElement()
      
      # Change to 2. Vessel Check in dropdown
      option.check <- dropdown$selectTag()
      option.select <- option.check$value[[2]]
      dropdown <- remDr$findElement('css', 
                                    paste0("#interactive_report_vessels_saved_reports option[value='", option.select, "']"))
      dropdown$clickElement()
      Sys.sleep(3)
      
      # Opens "Actions" dropdown
      actions <- remDr$findElement("css", "#interactive_report_vessels_actions_button")
      actions$highlightElement()
      actions$clickElement()
      Sys.sleep(3)
      
      # Click "Download"
      download <- remDr$findElement("css", "#interactive_report_vessels_actions_menu_14i")
      download$highlightElement()
      Sys.sleep(0.5)
      download$clickElement()
      Sys.sleep(2)
      
      # Click "CSV"
      option.csv <- remDr$findElement("xpath", "/html/body/div[5]/div[2]/div/ul/li[1]")
      option.csv$highlightElement()
      Sys.sleep(0.5)
      option.csv$clickElement() 
      Sys.sleep(2)
      
      # Click "Download" - sends to Downloads folder
      final.dl <- remDr$findElement("xpath", '//*[@id="t_PageBody"]/div[5]/div[3]/div/button[2]')
      final.dl$highlightElement()
      Sys.sleep(0.5)
      final.dl$clickElement()
      Sys.sleep(60)
      
      # Close the window and connection #
      remDr$close()
      rs_driver$server$stop()
      
      rm(binman_cdriver, newest_cdriver, driver_port, rs_driver, rem_driver, remDr, 
         rb.vesname, rb.vesname.child, input.vesnum, button.report, dropdown, actions, 
         download, option.csv, final.dl)
      
      gc()
      
      # Move VESSELS.csv to the proper report_files folder
      if (!file.exists(paste0(path.mnth, "VESSELS.CSV"))) {
        message("VESSELS.csv not found in ", path.mnth)
        path.dl <- gsub("[\\]", "/", paste0(Sys.getenv("USERPROFILE"), "/Downloads/VESSELS.csv"))
        
        if (file.exists(path.dl)) {
          cat(path.dl, "located \n")
          message("Attempting to move to ", path.mnth)
          file.copy(path.dl, path.mnth)
          
          if (file.exists(paste0(path.mnth, "VESSELS.csv"))) {
            message("File copied. Removing VESSELS.csv from Downloads folder...")
            file.remove(path.dl)
          }
        }
      } else {
        message("VESSELS.csv found in ", path.mnth)
      }
      
    } else if (option == "api") {
      
      # API call ----
      message("Querying FOSS via API call...")
      stop("API methods not yet implemented, please use option == 'scrape'")
      
    }
  } else {
    message("VESSELS.csv found in ", path.mnth, " - skipping FOSS query...")
  }
  
  # Read in data from VESSELS.csv, format column names, and return
  vessels_FOSS <- read_csv(paste0(path.mnth, "VESSELS.csv"), guess_max = 1e6, skip = 1) %>%
    transmute(CG_NUM     = `USCG Number`,
              CG_LENGTH  = `Reg. Length`,
              CG_TONNAGE = `Reg. Gross Tons`,
              CG_IMO     = `IMO Number`)
  
  # Spot-check
  print(head(vessels_FOSS))
  
  message("FOSS vessel list compiled!")
  return(vessels_FOSS)
}