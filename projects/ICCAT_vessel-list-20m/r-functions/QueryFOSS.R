QueryFOSS <- function (oa_pims_permits, vlist_params, option = "api", rsbrowser = "chrome") {
  
  path.year <- paste0(path.report_files,vlist_params$year1,"/")
  path.mnth <- paste0(path.year,vlist_params$mabb1,"/")
  
  if (!option %in% c("api","scrape")) {
    stop("Please select either 'api' or 'scrape' for the query option.")
  }
  
  message("Removing ", sum(grepl("NOVESID", oa_pims_permits$VESID)), " entries with NOVESID")
  oa_pims_permits <- oa_pims_permits %>%
    filter(!grepl("NOVESID", VESID))
  
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
  
  if (option == "scrape") {
    
    # RSelenium ----
    message("Scraping FOSS via RSelenium...")
    
    binman_cdriver <- binman::list_versions("chromedriver")[[1]]
    newest_cdriver <- sort(binman_cdriver, decreasing = TRUE)[1]
    
    driver_port <- free_port()
    rs_driver <- rsDriver(port = driver_port, browser = rsbrowser, chromever = newest_cdriver, verbose = FALSE)
    message("Waiting for Chrome to Load....")
    Sys.sleep(5)
    
    rem_driver <- remoteDriver(port = driver_port)
    remDr <-  rs_driver[["client"]]
    
    remDr$setTimeout(type = "page load", milliseconds = 60000)
    
    message("Navigating to FOSS")
    
    ## Scraping ----
    
    # Navigate to FOSS webpage
    remDr$navigate("https://www.fisheries.noaa.gov/foss/f?p=215:4")
    remDr$setTimeout(type = "page load", milliseconds = 60000)
    
    # CLICK ON THE 'VESSEL NUMBER' RADIO BUTTON ##
    t <- remDr$findElement("css", "#P4_VES_LOV")
    t$highlightElement()
    remDr$setTimeout(type = "page load", milliseconds = 60000)
    
    # Locate child element and send commands
    t.input <- t$findChildElement(using = 'css', value = 'input')
    t.input$sendKeysToElement(list(key = 'left_arrow',    # selects the "Vessel ID"
                                   key = 'right_arrow'))  # option and box
    # This section will select the vessel number textbox and paste the 
    # vessel list into the box
    t1.input <- remDr$findElement("xpath", '//*[@id="P4_USCG_NUMBER_input"]')
    t1.input$highlightElement()
    remDr$setTimeout(type = "page load", milliseconds = 60000)
    
    t1.input$sendKeysToElement(list(FOSS_IDs, key = 'enter')) # enter text 
    # Clicks "Run Report"
    t1 <- remDr$findElement("css", "#p4_go")
    t1$highlightElement()
    remDr$setTimeout(type = "page load", milliseconds = 60000)
    
    t1$clickElement()
    remDr$setTimeout(type = "page load", milliseconds = 60000)
    
    # Select the Vessel List
    t3 <- remDr$findElement('css', "#interactive_report_vessels_saved_reports")
    t3$highlightElement()
    remDr$setTimeout(type = "page load", milliseconds = 60000)
    
    # Change to 2. Vessel Check in dropdown
    t3.opts <- t3$selectTag()
    t3.optsel <- t3.opts$value[[2]]
    t3 <- remDr$findElement('css', 
                            paste0("#interactive_report_vessels_saved_reports option[value='", t3.optsel, "']"))
    t3$clickElement()
    remDr$setTimeout(type = "page load", milliseconds = 60000)
    
    # Opens "Actions" dropdown
    t4 <- remDr$findElement("css", "#interactive_report_vessels_actions_button")
    t4$highlightElement()
    remDr$setTimeout(type = "page load", milliseconds = 60000)
    
    t4$clickElement()
    remDr$setTimeout(type = "page load", milliseconds = 60000)
    
    # Click "Download"
    t5 <- remDr$findElement("css", "#interactive_report_vessels_actions_menu_14i")
    t5$highlightElement()
    remDr$setTimeout(type = "page load", milliseconds = 60000)
    t5$clickElement()
    remDr$setTimeout(type = "page load", milliseconds = 60000)
    # Click "CSV"
    t6 <- remDr$findElement("xpath", "/html/body/div[5]/div[2]/div/ul/li[1]")
    t6$highlightElement()
    remDr$setTimeout(type = "page load", milliseconds = 60000)
    t6$clickElement() 
    remDr$setTimeout(type = "page load", milliseconds = 60000)
    # Click "Download" - sends to Downloads folder
    t7 <- remDr$findElement("xpath", '//*[@id="t_PageBody"]/div[5]/div[3]/div/button[2]')
    t7$highlightElement()
    remDr$setTimeout(type = "page load", milliseconds = 60000)
    
    t7$clickElement()
    remDr$setTimeout(type = "page load", milliseconds = 60000)
    
    # Close the window and connection #
    remDr$close()
    rD$server$stop()
    system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
    rm(t, t1, t3, t4, t5, t6, t7, t.input, t1.input, t3.optsel, 
       rD, remDr, t3.opts)
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
    
  }
  
  vessels_FOSS <- read_csv(paste0(path.mnth, "VESSELS.csv"), guess_max = 1e6, skip = 1) %>%
    transmute(CG_NUM     = `USCG Number`,
              CG_LENGTH  = `Reg. Length`,
              CG_TONNAGE = `Reg. Gross Tons`,
              CG_IMO     = `IMO Number`)
  
  print(head(vessels_FOSS))
  return(vessels_foss)
}