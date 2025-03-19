GenerateVesselListParameters <- function (verbose = FALSE) {
  
  current_date <- as.character(Sys.Date())
  message(paste0("Current Date: ", current_date))
  
  month2 <- as.numeric(substr(current_date, 6, 7))
  month1 <- ifelse(month2 == 1, 12, month2 - 1)
  
  year2 <- as.numeric(substr(current_date, 1, 4))
  year1 <- ifelse(month2 == 1, year2 - 1, year2)
  
  vlist_params <- data.frame(current_date = current_date, 
                             month1 = month1,
                             month2 = month2,
                             year1 = year1,
                             year2 = year2,
                             mabb1 = toupper(month.abb[month1]),
                             mabb2 = toupper(month.abb[month2]))
  
  message(paste0("Generating 20m vessel list parameters for ", vlist_params$mabb2, " ", year2))
  print(vlist_params)
  
  return(vlist_params)
}
