QueryFOSS <- function (oa_pims_permits, option = "api", rsbrowser = "chrome") {
  
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
    remDr$navigate("https://www.fisheries.noaa.gov/foss/f?p=215:4")
    # PAGELOAD
    
  } else if (option == "api") {
    message("Querying FOSS via API call...")
    
    
  }
  
  
  
}