# Atlantic HMS - ICCAT 20m Vessel List (Updated 2025 Workflow)
# Last updated: 2025-07-28


#' The old ICCAT 20m vessel list workflow has been updated and is now generated through a 
#' function-based workflow.


# SETUP ----

#' Monthly ICCAT vessels lists should be properly generated if all project files have been correctly
#' initalized. Vessels lists will always be generated for the month prior to the current month that 
#' the code is being run during (e.g. If running in July, will create the June vessel list).

# Load in database credentials and required libraries
source("./setup/libraries.R")
source("./setup/filepath-settings.R")
source("./setup/NFPLRS-database-credentials.R")

# Load all functions for creating the vessel list
source("./r-functions/BuildCP01.R")
source("./r-functions/BuildQueryNFPLRS.R")
source("./r-functions/CompileFCC.R")
source("./r-functions/ExtractFCC.R")
source("./r-functions/GenerateVesselListParameters.R")
source("./r-functions/LoadICCATVesselRef.R")
source("./r-functions/MergeVesselDataSources.R")
source("./r-functions/ProcessPIMSData.R")
source("./r-functions/QueryDBConnection.R")
source("./r-functions/QueryFOSS.R")
source("./r-functions/SaveNFPLRSDataFiles.R")

# CREATE VESSEL LIST ----

# Get monthly vessel list parameters ----
vlist_params <- GenerateVesselListParameters()
rm(GenerateVesselListParameters)

# Get SQL query string for NFPLRS database ----
query_text <- BuildQueryNFPLRS(vlist_params)
rm(BuildQueryNFPLRS)

# Query data from NFPLRS SQL db ----
oa_permits <- QueryDBConnection(db_Host, db_Port, db_Name, path.java_jre, path.ojdbc8_jar, db_Schema, query_text)
rm(db_Acct, db_Host, db_Name, db_Pass, db_Port, db_Schema,
   path.java_jre, path.ojdbc8_jar, query_text, QueryDBConnection)

# Save NFPLRS data ----
SaveNFPLRSDataFiles(oa_permits, vlist_params)
rm(SaveNFPLRSDataFiles)

# Access PIMS data ----

#' Instructions for Acquiring PIMS Data:
#' 
#' Log into PIMS via CAC, navigate to the "Permits" tab.
#' Select "Filter for Vessel HMS Export".
#' For the "ISSUED DATE" field, select all dates from the current month and prior month for the 
#' vessel list being generated (e.g. FEB: 01/01/YYYY - 03/01/YYYY ).
#' Sometimes, PIMS-permitted vessels may receive their permit in an earlier month than when the 
#' permit is made active, so including the prior month should capture these instances.
#' Select "Export to Excel"

# Load PIMS data into R ----
oa_pims_permits <- ProcessPIMSData(vlist_params, oa_permits)
remove(ProcessPIMSData)

# Load ICCAT vessel reference files ----
ICCAT_vesref <- LoadICCATVesselRef()
rm(LoadICCATVesselRef)

# Scrape FOSS vessel data ----
FOSS_vessels <- QueryFOSS(oa_pims_permits, vlist_params, option = "scrape")
rm(QueryFOSS)

# Merge NFPLRS, PIMS, ICCAT, and FOSS data ----
compiled_vessel_permits <- MergeVesselDataSources(oa_pims_permits, ICCAT_vesref, FOSS_vessels, vlist_params)
rm(MergeVesselDataSources)

# Compile data for CP_01 template ----
BuildCP01(compiled_vessel_permits, vlist_params)

# At this point, go into the completed template, and make any adjustments needed.
# Save as "YYYY-MON_CP01-templSate-vessel-list-final.xlsx

# Compile FCC ULS callsign info ----
CompileFCC(vlist_params, option = "local")
