QueryDBConnection <- function (db_Host, db_Port, db_Name, path.java_jre, path.ojdbc8_jar, db_Schema, query_text) {
  
  # Generate DB URL and set PATH
  db_URL <- paste("jdbc:oracle:thin:@//",db_Host,":",db_Port,"/",db_Name, sep = "")
  Sys.setenv(JAVA_HOME = path.java_jre)
  
  # Create DB driver
  jdbc_driver <- JDBC(driverClass = "oracle.jdbc.OracleDriver", classPath = path.ojdbc8_jar)
  
  # Create DB connection
  jdb_connection <- dbConnect(jdbc_driver, db_URL, user = db_Acct, password = db_Pass)
  
  # Print TABLE_NAMES to console
  message("head(SCHEMA-TABLE-NAMES)")
  print(head(data.frame(TABLE_NAMES = dbListTables(jdb_connection, schema = db_Schema))))
  Sys.sleep(3)
  message("tail(SCHEMA-TABLE-NAMES)")
  print(tail(data.frame(TABLE_NAMES = dbListTables(jdb_connection, schema = db_Schema))))
  Sys.sleep(3)
  
  NFPLRS_permits <- dbGetQuery(jdb_connection, query_text)
  
  dbDisconnect(jdb_connection)
  
  rm(jdbc_driver, jdb_connection)
  
  message("Returning queried database entries!")
  
  NFPLRS_permits <- NFPLRS_permits %>%
    mutate(VESNAME = toupper(VESNAME),
           PERMIT_HOLDER = toupper(PERMIT_HOLDER))
  
  return(NFPLRS_permits)
}