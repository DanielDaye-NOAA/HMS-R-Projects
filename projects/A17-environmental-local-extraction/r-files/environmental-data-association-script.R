# Environmental Data Association Batches

#' Script to extract and associate (and track progress) environmental data with EFH observations 
#' aggregated across previous A10 data and A17 data requests. Code run in parallel locally.

# SETUP ----
# Libraries
library(doParallel)
library(doSNOW)
library(ncdf4)
library(raster)
library(sp)
library(tidyverse)
library(writexl)

# Consolidated EFH data from 2010-2024
path_data <- "./data/A17_all_species_2010-2024-AUG.csv"

# D: Drive ----
dir_BATHY <- "D:/A17-EFH-environmental-data/"
dir_CMEMS <- "D:/A17-EFH-environmental-data/CMEMS/"
dir_HYCOM <- "D:/A17-EFH-environmental-data/HYCOM/"

# C: Drive ----
dir_BATHY <- "C:/Users/daniel.daye/Documents/DDaye/R/projects/A17-environmental-local-extraction/envdata/"
dir_CMEMS <- "C:/Users/daniel.daye/Documents/DDaye/R/projects/A17-environmental-local-extraction/envdata/CMEMS/"
dir_HYCOM <- "C:/Users/daniel.daye/Documents/DDaye/R/projects/A17-environmental-local-extraction/envdata/HYCOM/"

# Saved outfiles locally in project folder
dir_outfiles <- "C:/Users/daniel.daye/Documents/DDAYE/R/projects/A17-environmental-local-extraction/batch-outfiles/"

# Check how much data available for each month and day across the dataset
CheckData <- function(data) {
  data %>%
    mutate(MONTH = substr(DATE,6,7),
           DAY = substr(DATE,9,10)) %>%
    select(DAY, MONTH) %>%
    table(useNA = "ifany") %>%
    print()
}

# Load extraction functions
source("./R/BatchEnvAssign.R")
source("./R/ConvertListToDF.R")

# Checking that all data loads
alldata <- read_csv(path_data, guess_max = 1e6)


# Extractions ----
## 2010 (Done) ----
data_2010 <- read_csv(path_data, guess_max = 1e6) %>% filter(YEAR == "2010")
CheckData(data_2010)
outfile = paste0(dir_outfiles, "2010/")
outfile

# Set up cluster
cl <- makeCluster(4, outfile = "")
registerDoSNOW(cl)

# Run the script
# env_data_2010 <- BatchEnvAssign(data_2010, verbose = TRUE, dir_BATHY, dir_CMEMS, dir_HYCOM, dir_outfiles = outfile)
# saveRDS(env_data_2010, "./extractions/env_data_2010.RDS")
env_data_2010 <- readRDS("./extractions/env_data_2010.RDS")

env_df_2010 <- ConvertListToDF(env_data_2010)
# write_xlsx(env_df_2010, "./extractions/env_data_2010.xlsx")


## 2011 (Done) ----
data_2011 <- read_csv(path_data, guess_max = 1e6) %>% filter(YEAR == "2011")
CheckData(data_2011)
outfile = paste0(dir_outfiles, "2011/")
outfile

# Set up cluster
cl <- makeCluster(4, outfile = "")
registerDoSNOW(cl)

# Run the script
# env_data_2011 <- BatchEnvAssign(data_2011, verbose = TRUE, dir_BATHY, dir_CMEMS, dir_HYCOM, dir_outfiles = outfile)
# saveRDS(env_data_2011, "./extractions/env_data_2011.RDS")
env_data_2011 <- readRDS("./extractions/env_data_2011.RDS")

env_df_2011 <- ConvertListToDF(env_data_2011)
# write_xlsx(env_df_2011, "./extractions/env_data_2011.xlsx")


## 2012 (Done) ----
data_2012 <- read_csv(path_data, guess_max = 1e6) %>% filter(YEAR == "2012")
CheckData(data_2012)
outfile = paste0(dir_outfiles, "2012/")
outfile

# Set up cluster
cl <- makeCluster(4, outfile = "")
registerDoSNOW(cl)

# Run the script
# env_data_2012 <- BatchEnvAssign(data_2012, verbose = TRUE, dir_BATHY, dir_CMEMS, dir_HYCOM, dir_outfiles = outfile)
# saveRDS(env_data_2012, "./extractions/env_data_2012.RDS")
env_data_2012 <- readRDS("./extractions/env_data_2012.RDS")

env_df_2012 <- ConvertListToDF(env_data_2012)
# write_xlsx(env_df_2012, "./extractions/env_data_2012.xlsx")


## 2013-14 (Done) ----
data_2013_2014 <- read_csv(path_data, guess_max = 1e6) %>% filter(YEAR %in% c("2013","2014"))
CheckData(data_2013_2014)
outfile = paste0(dir_outfiles, "2013-2014/")
outfile

# Set up cluster
cl <- makeCluster(4, outfile = "")
registerDoSNOW(cl)

# Run the script
# env_data_2013_2014 <- BatchEnvAssign(data_2013_2014, verbose = TRUE, dir_BATHY, dir_CMEMS, dir_HYCOM, dir_outfiles = outfile)
# saveRDS(env_data_2013_2014, "./extractions/env_data_2013-2014.RDS")
env_data_2013_2014 <- readRDS("./extractions/env_data_2013-2014.RDS")

env_df_2013_2014 <- ConvertListToDF(env_data_2013_2014)
# write_xlsx(env_df_2013_2014, "./extractions/env_data_2013-2014.xlsx")


## 2015 (Done) ----
data_2015 <- read_csv(path_data, guess_max = 1e6) %>% filter(YEAR == "2015")
CheckData(data_2015)
outfile = paste0(dir_outfiles, "2015/")
outfile

# Set up cluster
cl <- makeCluster(4, outfile = "")
registerDoSNOW(cl)

# Run the script
# env_data_2015 <- BatchEnvAssign(data_2015, verbose = TRUE, dir_BATHY, dir_CMEMS, dir_HYCOM, dir_outfiles = outfile)
# saveRDS(env_data_2015, "./extractions/env_data_2015.RDS")
env_data_2015 <- readRDS("./extractions/env_data_2015.RDS")

env_df_2015 <- ConvertListToDF(env_data_2015)
# write_xlsx(env_df_2015, "./extractions/env_data_2015.xlsx")


## 2016 (Done) ----
data_2016 <- read_csv(path_data, guess_max = 1e6) %>% filter(YEAR == "2016")
CheckData(data_2016)
outfile = paste0(dir_outfiles, "2016/")
outfile

# Set up cluster
cl <- makeCluster(4, outfile = "")
registerDoSNOW(cl)

# Run the script
# env_data_2016 <- BatchEnvAssign(data_2016, verbose = TRUE, dir_BATHY, dir_CMEMS, dir_HYCOM, dir_outfiles = outfile)
# saveRDS(env_data_2016, "./extractions/env_data_2016.RDS")
env_data_2016 <- readRDS("./extractions/env_data_2016.RDS")

env_df_2016 <- ConvertListToDF(env_data_2016)
# write_xlsx(env_df_2016, "./extractions/env_data_2016.xlsx")


## 2017 JAN-JUN (Done) ----
data_2017 <- read_csv(path_data, guess_max = 1e6) %>% filter(YEAR == "2017", MONTH %in% c("01","02","03","04","05","06"))
CheckData(data_2017)
outfile = paste0(dir_outfiles, "2017-JANJUN/")
outfile

# Set up cluster
cl <- makeCluster(3, outfile = "")
registerDoSNOW(cl)

# Run the script
# env_data_2017 <- BatchEnvAssign(data_2017, verbose = TRUE, dir_BATHY, dir_CMEMS, dir_HYCOM, dir_outfiles = outfile)
# saveRDS(env_data_2017, "./extractions/env_data_2017-JANJUN.RDS")
env_data_2017 <- readRDS("./extractions/env_data_2017-JANJUN.RDS")

env_df_2017 <- ConvertListToDF(env_data_2017)
# write_xlsx(env_df_2017, "./extractions/env_data_2017-JANJUN.xlsx")


## 2017 JUL-SEP (Done) ----
data_2017 <- read_csv(path_data, guess_max = 1e6) %>% filter(YEAR == "2017", MONTH %in% c("07","08","09"))
CheckData(data_2017)
outfile = paste0(dir_outfiles, "2017-JULSEP/")
outfile

# Set up cluster
cl <- makeCluster(3, outfile = "")
registerDoSNOW(cl)

# Run the script
env_data_2017 <- BatchEnvAssign(data_2017, verbose = TRUE, dir_BATHY, dir_CMEMS, dir_HYCOM, dir_outfiles = outfile)
saveRDS(env_data_2017, "./extractions/env_data_2017-JULSEP.RDS")
env_data_2017 <- readRDS("./extractions/env_data_2017-JULSEP.RDS")

env_df_2017 <- ConvertListToDF(env_data_2017)
write_xlsx(env_df_2017, "./extractions/env_data_2017-JULSEP.xlsx")


## 2017 OCT-NOV (Done) ----
data_2017 <- read_csv(path_data, guess_max = 1e6) %>% filter(YEAR == "2017", MONTH %in% c("10","11"))
CheckData(data_2017)
outfile = paste0(dir_outfiles, "2017-OCTNOV/")
outfile

# Set up cluster
cl <- makeCluster(2, outfile = "")
registerDoSNOW(cl)

# Run the script
# env_data_2017 <- BatchEnvAssign(data_2017, verbose = TRUE, dir_BATHY, dir_CMEMS, dir_HYCOM, dir_outfiles = outfile)
# saveRDS(env_data_2017, "./extractions/env_data_2017-OCTNOV.RDS")
env_data_2017 <- readRDS("./extractions/env_data_2017-OCTNOV.RDS")

# env_df_2017 <- ConvertListToDF(env_data_2017)
# write_xlsx(env_df_2017, "./extractions/env_data_2017-OCTNOV.xlsx")


## 2017 DEC (Done)----
data_2017 <- read_csv(path_data, guess_max = 1e6) %>% filter(YEAR == "2017", MONTH %in% c("12"))
CheckData(data_2017)
outfile = paste0(dir_outfiles, "2017-DEC/")
outfile

# Set up cluster
cl <- makeCluster(1, outfile = "")
registerDoSNOW(cl)

# Run the script
# env_data_2017 <- BatchEnvAssign(data_2017, verbose = TRUE, dir_BATHY, dir_CMEMS, dir_HYCOM, dir_outfiles = outfile)
# saveRDS(env_data_2017, "./extractions/env_data_2017-DEC.RDS")
env_data_2017 <- readRDS("./extractions/env_data_2017-DEC.RDS")

env_df_2017 <- ConvertListToDF(env_data_2017)
# write_xlsx(env_df_2017, "./extractions/env_data_2017-DEC.xlsx")


## 2018 (Done) ----
data_2018 <- read_csv(path_data, guess_max = 1e6) %>% filter(YEAR == "2018")
CheckData(data_2018)
outfile = paste0(dir_outfiles, "2018/")
outfile

# Set up cluster
cl <- makeCluster(4, outfile = "")
registerDoSNOW(cl)

# Run the script
# env_data_2018 <- BatchEnvAssign(data_2018, verbose = TRUE, dir_BATHY, dir_CMEMS, dir_HYCOM, dir_outfiles = outfile)
# saveRDS(env_data_2018, "./extractions/env_data_2018.RDS")
env_data_2018 <- readRDS("./extractions/env_data_2018.RDS")

env_df_2018 <- ConvertListToDF(env_data_2018)
# write_xlsx(env_df_2018, "./extractions/env_data_2018.xlsx")


## 2019 (Done) ----
data_2019 <- read_csv(path_data, guess_max = 1e6) %>% filter(YEAR == "2019")
CheckData(data_2019)
outfile = paste0(dir_outfiles, "2019/")
outfile

# Set up cluster
cl <- makeCluster(4, outfile = "")
registerDoSNOW(cl)

# Run the script
# env_data_2019 <- BatchEnvAssign(data_2019, verbose = TRUE, dir_BATHY, dir_CMEMS, dir_HYCOM, dir_outfiles = outfile)
# saveRDS(env_data_2019, "./extractions/env_data_2019.RDS")
env_data_2019 <- readRDS("./extractions/env_data_2019.RDS")

env_df_2019 <- ConvertListToDF(env_data_2019)
# write_xlsx(env_df_2019, "./extractions/env_data_2019.xlsx")


## 2020 JAN-JUN (Done) ----
data_2020 <- read_csv(path_data, guess_max = 1e6) %>% filter(YEAR == "2020", MONTH %in% c("01","02","03","04","05","06"))
CheckData(data_2020)
outfile = paste0(dir_outfiles, "2020-JANJUN/")
outfile

# Set up cluster
cl <- makeCluster(3, outfile = "")
registerDoSNOW(cl)

# Run the script
# env_data_2020 <- BatchEnvAssign(data_2020, verbose = TRUE, dir_BATHY, dir_CMEMS, dir_HYCOM, dir_outfiles = outfile)
# saveRDS(env_data_2020, "./extractions/env_data_2020-JANJUN.RDS")
env_data_2020 <- readRDS("./extractions/env_data_2020-JANJUN.RDS")

env_df_2020 <- ConvertListToDF(env_data_2020)
# write_xlsx(env_df_2020, "./extractions/env_data_2020-JANJUN.xlsx")


## 2020 JUL - DEC (Done) ----
data_2020 <- read_csv(path_data, guess_max = 1e6) %>% filter(YEAR == "2020", MONTH %in% c("07","08","09","10","11","12"))
CheckData(data_2020)
outfile = paste0(dir_outfiles, "2020-JULDEC/")
outfile

# Set up cluster
cl <- makeCluster(3, outfile = "")
registerDoSNOW(cl)

# Run the script
# env_data_2020 <- BatchEnvAssign(data_2020, verbose = TRUE, dir_BATHY, dir_CMEMS, dir_HYCOM, dir_outfiles = outfile)
# saveRDS(env_data_2020, "./extractions/env_data_2020-JULDEC.RDS")
env_data_2020 <- readRDS("./extractions/env_data_2020-JULDEC.RDS")

env_df_2020 <- ConvertListToDF(env_data_2020)
# write_xlsx(env_df_2020, "./extractions/env_data_2020-JULDEC.xlsx")


## 2021 JAN - JUN (Done) ----
data_2021 <- read_csv(path_data, guess_max = 1e6) %>% filter(YEAR == "2021", MONTH %in% c("01","02","03","04","05","06"))
CheckData(data_2021)
outfile = paste0(dir_outfiles, "2021-JANJUN/")
outfile

# Set up cluster
cl <- makeCluster(3, outfile = "")
registerDoSNOW(cl)

# Run the script
# env_data_2021 <- BatchEnvAssign(data_2021, verbose = TRUE, dir_BATHY, dir_CMEMS, dir_HYCOM, dir_outfiles = outfile)
# saveRDS(env_data_2021, "./extractions/env_data_2021-JANJUN.RDS")
env_data_2021 <- readRDS("./extractions/env_data_2021-JANJUN.RDS")

# env_df_2021 <- ConvertListToDF(env_data_2021)
# write_xlsx(env_df_2021, "./extractions/env_data_2021-JANJUN.xlsx")


## 2021 JUL - DEC (Done) ----
data_2021 <- read_csv(path_data, guess_max = 1e6) %>% filter(YEAR == "2021", MONTH %in% c("07","08","09","10","11","12"))
CheckData(data_2021)
outfile = paste0(dir_outfiles, "2021-JULDEC/")
outfile

# Set up cluster
cl <- makeCluster(3, outfile = "")
registerDoSNOW(cl)

# Run the script
# env_data_2021 <- BatchEnvAssign(data_2021, verbose = TRUE, dir_BATHY, dir_CMEMS, dir_HYCOM, dir_outfiles = outfile)
# saveRDS(env_data_2021, "./extractions/env_data_2021-JULDEC.RDS")
env_data_2021 <- readRDS("./extractions/env_data_2021-JULDEC.RDS")

env_df_2021 <- ConvertListToDF(env_data_2021)
# write_xlsx(env_df_2021, "./extractions/env_data_2021-JULDEC.xlsx")


## 2022 (Done) ----
data_2022 <- read_csv(path_data, guess_max = 1e6) %>% filter(YEAR == "2022")
CheckData(data_2022)
outfile = paste0(dir_outfiles, "2022/")
outfile

# Set up cluster
cl <- makeCluster(4, outfile = "")
registerDoSNOW(cl)

# Run the script
# env_data_2022 <- BatchEnvAssign(data_2022, verbose = TRUE, dir_BATHY, dir_CMEMS, dir_HYCOM, dir_outfiles = outfile)
# saveRDS(env_data_2022, "./extractions/env_data_2022.RDS")
env_data_2022 <- readRDS("./extractions/env_data_2022.RDS")

env_df_2022 <- ConvertListToDF(env_data_2022)
# write_xlsx(env_df_2022, "./extractions/env_data_2022.xlsx")


## 2023 (Done) ----
data_2023 <- read_csv(path_data, guess_max = 1e6) %>% filter(YEAR == "2023")
CheckData(data_2023)
outfile = paste0(dir_outfiles, "2023/")
outfile

# Set up cluster
cl <- makeCluster(4, outfile = "")
registerDoSNOW(cl)

# Run the script
# env_data_2023 <- BatchEnvAssign(data_2023, verbose = TRUE, dir_BATHY, dir_CMEMS, dir_HYCOM, dir_outfiles = outfile)
# saveRDS(env_data_2023, "./extractions/env_data_2023.RDS")
env_data_2023 <- readRDS("./extractions/env_data_2023.RDS")

env_df_2023 <- ConvertListToDF(env_data_2023)
# write_xlsx(env_df_2023, "./extractions/env_data_2023.xlsx")


## 2024 (Done) ----
data_2024 <- read_csv(path_data, guess_max = 1e6) %>% filter(YEAR == "2024")
CheckData(data_2024)
outfile = paste0(dir_outfiles, "2024/")
outfile

# Set up cluster
cl <- makeCluster(4, outfile = "")
registerDoSNOW(cl)

# Run the script
# env_data_2024 <- BatchEnvAssign(data_2024, verbose = TRUE, dir_BATHY, dir_CMEMS, dir_HYCOM, dir_outfiles = outfile)
# saveRDS(env_data_2024, "./extractions/env_data_2024.RDS")
env_data_2024 <- readRDS("./extractions/env_data_2024.RDS")

env_df_2024 <- ConvertListToDF(env_data_2024)
# write_xlsx(env_df_2024, "./extractions/env_data_2024.xlsx")