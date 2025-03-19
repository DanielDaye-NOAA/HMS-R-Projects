# Updated, modular workflow for creating monthly ICCAT 20m Vessel Lists
# Last updated: 2025-03-18
# Status: In Development

#' The ICCAT 20m vessel list workflow is currently being migrated into a modular, function-based
#' workflow that can be uploaded to GitHub

# SETUP ----

#' These are the only objects that need to be updated on a month-to-month basis in order to create
#' the 20m vessel list.

# Load in database credentials
source("./setup/libraries.R")
source("./setup/filepath-settings.R")
source("./setup/NFPLRS-database-credentials.R")

# Load Functions
source("./r-functions/BuildCP01.R")
source("./r-functions/BuildQueryNFPLRS.R")
source("./r-functions/GenerateVesselListParameters.R")
source("./r-functions/LoadICCATVesselRef.R")
source("./r-functions/ProcessPIMSData.R")
source("./r-functions/QueryDBConnection.R")
source("./r-functions/SaveNFPLRSDataFiles.R")

# List Parameters ----

# Get monthly vessel list parameters
vlist_params <- GenerateVesselListParameters()
rm(GenerateVesselListParameters)

# Get query string for NFPLRS
query_text <- BuildQueryNFPLRS(vlist_params)
rm(BuildQueryNFPLRS)


# SQL DB Pull ----
oa_permits <- QueryDBConnection(db_Host, db_Port, db_Name, path.java_jre, path.ojdbc8_jar, db_Schema, query_text)
rm(db_Acct, db_Host, db_Name, db_Pass, db_Port, db_Schema,
   path.java_jre, path.ojdbc8_jar, query_text, QueryDBConnection)

# Save NFPLRS Data ----
SaveNFPLRSDataFiles(oa_permits, vlist_params)


# PIMS ----

#' Instructions for Acquiring PIMS Data:
#' 
#' Log into PIMS via CAC, navigate to the "Permits" tab.
#' Select "Filter for Vessel HMS Export".
#' For the "ISSUED DATE" field, select all dates from the current month and prior month for the 
#'   vessel list being generated (e.g. FEB: 01/01/YYYY - 03/01/YYYY ).
#' Sometimes, PIMS-permitted vessels may receive their permit in an earlier month than when the 
#'   permit is made active, so including the prior month should capture these instances.
#' Select "Export to Excel"

oa_pims_permits <- ProcessPIMSData(vlist_params, oa_permits)


# Vessel Reference
ICCAT_vesref <- LoadICCATVesselRef()

# Scrape FOSS ----
FOSS_vessels <- ScrapeFOSS(oa_pims_permits)

# Merge OA-PIMS, ICCAT, FOSS ----
compiled_vessel_permits <- MergeVesselDataSources(oa_pims_permits, ICCAT_vesref, FOSS_vessels)

# Build CP_01 Template ----
BuildCP01(compiled_vessel_permits)
