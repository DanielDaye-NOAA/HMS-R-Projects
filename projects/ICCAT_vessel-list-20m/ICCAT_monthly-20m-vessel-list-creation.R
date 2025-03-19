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
source("./r-functions/BuildQueryNFPLRS.R")
source("./r-functions/GenerateVesselListParameters.R")
source("./r-functions/QueryDBConnection.R")
source("./r-functions/SaveNFPLRSDataFiles.R")

# List Creation ----

# Get monthly vessel list parameters
vlist_params <- GenerateVesselListParameters()
rm(GenerateVesselListParameters)

# Get query string for NFPLRS
query_text <- BuildQueryNFPLRS(vlist_params)
rm(BuildQueryNFPLRS)

## SQL DB Pull ----
oa_permits <- QueryDBConnection(db_Host, db_Port, db_Name, path.java_jre, path.ojdbc8_jar, db_Schema, query_text)
rm(db_Acct, db_Host, db_Name, db_Pass, db_Port, db_Schema,
   path.java_jre, path.ojdbc8_jar, query_text, QueryDBConnection)

## Save NFPLRS Data ----
SaveNFPLRSDataFiles(oa_permits, vlist_params)




