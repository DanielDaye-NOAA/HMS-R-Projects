#' Iterates through all EFH observation data that has been associated with remote environmental
#' data sets and provides text descriptions for each species and lifestage (if managed by lifestage
#' in EFH) to serve as an initial text description.
#' 
#' Saves data by SPECIES (all regions combined), REGION (ATL and GULF), and REGION + CARIBBEAN

GenerateTextDescriptions <- function(data, ref, verbose = TRUE, debug = FALSE) {
  
  if (verbose) {message("Setting up initial dataframes...")}
  
  # Variables to include in the text description
  vars <- c("BATHYMETRY", "RUGOSITY", "THETAO", "BT", "MLD", "SSS", "BS")  # , "CHLA") - omitting chla
  
  # Includes English description and units for each variable
  vardict <- data.frame(VAR = c("BATHYMETRY", "RUGOSITY", "THETAO", "BT", "MLD", "SSS", "BS"),
                        DESC = c("depth", "rugosity", "sea surface temperature", "bottom temperature",
                                 "mixed layer depth", "sea surface salinity", "bottom salinity"),
                        UNIT = c("m", "m", "°C", "°C", "m", "mg/m3", "mg/m3"))
  
  # Atlantic Ocean Regions
  regdict <- data.frame(REGION = c("ATL", "GOM"), 
                        DESC = paste(c("Atlantic","Gulf"),"Stock"))
  
  # All A17 life stages
  lstdict <- data.frame(LS = c("SEL", "NEO", "NEO-JUV", "NEO-SMJUV",
                               "JUV", "JUV-ADU", "JUV-ADUM", "LGJUV-ADU",
                               "ADU", "ADUF", "ALL-LS"),
                        DESC = c("Spawn, Eggs, and Larvae (SEL)", "Neonates & YOY",
                                 "Neonates, YOY, and Juveniles", "Neonates, YOY, and Small Juveniles (TL < 150 cm)",
                                 "Juveniles", "Juveniles & Adults", "Juveniles & Adult Males",
                                 "Large Juveniles (TL > 150 cm) and Adults",
                                 "Adults", "Adult Females", "All Lifestages Combined"))
  
  # EFH Regions
  if (verbose) {message("Converting data to long dataframe...")}
  long_2reg <- data %>%
    pivot_longer(cols = BATHYMETRY:CHLA, names_to = "ENV_VAR", values_to = "VALUE") %>%
    filter(ENV_VAR %in% vars) %>%
    mutate(ENV_VAR = factor(ENV_VAR, levels = vars)) %>%
    group_by(COMFINAL, LSFINAL, REGFINAL, ENV_VAR) %>%
    filter(!is.na(VALUE), !is.na(LSFINAL)) %>%
    summarize(N = n(),
              MEAN = mean(VALUE, na.rm = TRUE),
              MEDIAN = median(VALUE, na.rm = TRUE),
              Q25 = quantile(VALUE, 0.25, na.rm = TRUE),
              Q75 = quantile(VALUE, 0.75, na.rm = TRUE)) %>%
    ungroup() %>% data.frame() %>%
    arrange(COMFINAL, REGFINAL, LSFINAL)
  
  # ATL, GOM, and CARIB Regions
  if (verbose) {message("Converting to long dataframe with 3 regions...")}
  long_3reg <- data %>%
    pivot_longer(cols = BATHYMETRY:CHLA, names_to = "ENV_VAR", values_to = "VALUE") %>%
    filter(ENV_VAR %in% vars) %>%
    mutate(ENV_VAR = factor(ENV_VAR, levels = vars)) %>%
    group_by(COMFINAL, LSFINAL, REGION, ENV_VAR) %>%
    filter(!is.na(VALUE), !is.na(LSFINAL)) %>%
    summarize(N = n(),
              MEDIAN = median(VALUE, na.rm = TRUE),
              Q25 = quantile(VALUE, 0.25, na.rm = TRUE),
              Q75 = quantile(VALUE, 0.75, na.rm = TRUE)) %>%
    ungroup() %>% data.frame() %>%
    arrange(COMFINAL, REGION, LSFINAL)
  
  if (verbose) {message("Finishing set-up...")}
  gc()
  com_names <- sort(unique(ref$SPECIES))
  env_names <- vars
  
  fulltext <- ""
  
  # Generate text descriptions for each SPECIES
  for (name in com_names) {
    
    if (verbose) {message(name)}
    
    # Get all REGIONS (na if combined for all ATL)
    regions <- ref %>% filter(SPECIES == name) %>% pull(REGION)
    
    if (sum(!is.na(regions)) > 0) {
      
      cat(length(regions),"regions:",regions,'\n')
      
      for (region in regions) {
        
        # For each REGION, grab those lifestages specified (might differ between regions)
        lifestages <- str_split(ref %>% filter(SPECIES == name, REGION == region) %>% pull(LIFESTAGES), ":")[[1]]
        cat(length(lifestages),"lifestages in",region,"-",lifestages,'\n')
        
        for (lifestage in lifestages) {
          
          message("- Environmental Profile for ",region," ",name," (",lifestage,")")
          
          # For each LIFESTAGE, grab the summarized environmental information
          reglsdata <- long_2reg %>% filter(COMFINAL==name, REGFINAL==region, LSFINAL==lifestage)
          if (verbose) {print(reglsdata[,-c(1:3)])}
          
          h1 <- gsub("_", " ", str_to_title(name))
          h2 <- regdict %>% filter(REGION==region) %>% pull(DESC)
          h3 <- lstdict %>% filter(LS==lifestage) %>% pull(DESC)
          
          header <- paste(h1,"EFH for",h2,"-",h3,"\n")
          if (verbose) {print(header)}
          
          fulltext <- paste0(fulltext,header)
          
          for (envvar in reglsdata$ENV_VAR) {
            
            if (debug) {print(envvar)}
            
            # Generating text
            tvalu <- reglsdata %>% filter(ENV_VAR==envvar) 
            vdesc <- vardict %>% filter(VAR == envvar) %>% pull(DESC)
            vunit <- vardict %>% filter(VAR == envvar) %>% pull(UNIT)
            
            if (envvar == "BATHYMETRY") {
              # Beginning of paragraph
              p1 <- paste0(h1," (",h3,") across their ",str_to_sentence(h2)," were observed primarily between ",vdesc)
              p2 <- paste0(" values of ",tvalu$Q75*-1," ",vunit," to ",tvalu$Q25*-1," ",vunit,". ")
              
              fulltext <- paste0(fulltext,p1,p2)
              
            } else if (envvar == "RUGOSITY") {  
              if (tvalu$MEAN >= 20) {
                h3fmt <- gsub("yoy","YOY",gsub("tl","TL",tolower(h3)))
                h3fmt <- paste0(toupper(substr(h3fmt,1,1)),substr(h3fmt,2,nchar(h3fmt)))
                p1 <- paste0(" were associated with rugged habitat and a seafloor with a dynamic depth profile. ")
                fulltext <- paste0(fulltext,h3fmt,p1)
              } else {
                message("No association with rugosity")
              }
            } else {           
              p1 <- paste0(str_to_title(vdesc)," values between ",sprintf("%.3f",tvalu$Q25)," to ",sprintf("%.3f",tvalu$Q75)," ",vunit)
              p2 <- ifelse(envvar == reglsdata$ENV_VAR[length(reglsdata$ENV_VAR)], ".", ",")
              fulltext <- paste0(fulltext,p1,p2," ")
            }
          }
          fulltext <- paste0(fulltext,"\n\n")
        }
        envvars <- ref %>% filter(SPECIES == name, REGION == REGION) %>% pull()
        fulltext <- paste0(fulltext,"\n")
      }
      
    } else {
      cat("Data aggregated across all regions\n")
      lifestages <- str_split(ref$LIFESTAGES[match(name, ref$SPECIES)], ":")[[1]]
      cat(length(lifestages),"lifestages across extent:",lifestages,'\n')
      
      for (lifestage in lifestages) {
        
        message("- Environmental Profile for "," ",name," (",lifestage,")")
        
        # For each LIFESTAGE, grab the summarized environmental information
        reglsdata <- long_2reg %>% filter(COMFINAL==name, LSFINAL==lifestage)
        if (debug) {print(reglsdata[,-c(1:3)])}
        
        h1 <- gsub("_", " ", str_to_title(name))
        h2 <- "Full Range Extent"
        h3 <- lstdict %>% filter(LS==lifestage) %>% pull(DESC)
        
        header <- paste(h1,"EFH for",h2,"-",h3,"\n")
        if (verbose) {print(header)}
        
        fulltext <- paste0(fulltext, header)
        
        
        for (envvar in reglsdata$ENV_VAR) {
          
          if (debug) {print(envvar)}
          
          # Generating text
          tvalu <- reglsdata %>% filter(ENV_VAR==envvar) 
          vdesc <- vardict %>% filter(VAR == envvar) %>% pull(DESC)
          vunit <- vardict %>% filter(VAR == envvar) %>% pull(UNIT)
          
          if (envvar == "BATHYMETRY") {
            # Beginning of paragraph
            p1 <- paste0(h1," (",h3,") across their ",tolower(h2)," were observed primarily between ",vdesc)
            p2 <- paste0(" values of ",tvalu$Q75*-1," ",vunit," to ",tvalu$Q25*-1," ",vunit,". ")
            
            fulltext <- paste0(fulltext,p1,p2)
            
          } else if (envvar == "RUGOSITY") {
            
            if (tvalu$MEAN >= 20) {
              h3fmt <- gsub("yoy","YOY",gsub("tl","TL",tolower(h3)))
              h3fmt <- paste0(toupper(substr(h3fmt,1,1)),substr(h3fmt,2,nchar(h3fmt)))
              p1 <- paste0(" were associated with rugged habitat and a seafloor with a dynamic depth profile. ")
              fulltext <- paste0(fulltext,h3fmt,p1)
            } else {
              message("No association with rugosity")
            }
            
          } else {
            
            p1 <- paste0(str_to_title(vdesc)," values between ",sprintf("%.3f",tvalu$Q25)," to ",sprintf("%.3f",tvalu$Q75)," ",vunit)
            p2 <- ifelse(envvar == reglsdata$ENV_VAR[length(reglsdata$ENV_VAR)], ".", ",")
            fulltext <- paste0(fulltext,p1,p2," ")
            
          }
        }
        fulltext <- paste0(fulltext,"\n\n")
      }
      fulltext <- paste0(fulltext,"\n")
    }
    fulltext <- paste0(fulltext,"\n")
  }
  
  return(fulltext)
}