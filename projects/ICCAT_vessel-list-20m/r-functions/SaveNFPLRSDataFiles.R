SaveNFPLRSDataFiles <- function (oa_permits, vlist_params, verbose = FALSE) {
  
  path.year <- paste0(path.report_files,vlist_params$year1,"/")
  path.mnth <- paste0(path.year,vlist_params$mabb1,"/")
  
  # Creating folders for data if they don't yet exist
  if (!file.exists(path.year)) {
    message(paste0("Folder does not exist for ", vlist_params$year1, "\n", "creating now..."))
    dir.create(path.year)
  } else {message(paste0("Filepath located for ", vlist_params$year1, "!"))}
  Sys.sleep(1)
  
  if (!file.exists(path.mnth)) {
    message(paste0("Folder does not exist for ", vlist_params$mabb1, " ", vlist_params$year1,"\n","creating now..."))
    dir.create(path.mnth)
  } else {message(paste0("Filepath located for ", vlist_params$mabb1, " ", vlist_params$year1, "!"))}
  Sys.sleep(1)
  
  oap_filename <- paste0("NFPLRS-OAP_", vlist_params$year1, "-", vlist_params$mabb1)
  
  # Save as both CSV and XLSX
  write_csv(oa_permits, file = paste0(oap_filename, ".csv"))
  write.xlsx(oa_permits, file = paste0(oap_filename, ".xlsx"))
  
  message(paste0("NFPLRS permit info saved to ", path.mnth))
}
