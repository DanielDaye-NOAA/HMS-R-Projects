#' Consolidate environmental data associated with EFH observations via BatchEnvAssign into data
#' frames. BatchEnvAssign was run in chunks due to the long extraction times and extractions were
#' prone to failure due to changes which previously allowed R code to run overnight.
#' 
#' Extraction Lists were consolidated into yearly (or multi-year) data frames here and then saved
#' as either Excel files or RDS files

# Setup ----
library(tidyverse)
library(writexl)

source("./R/ConvertListToDF.R")

# Converting all BatchEnvAssign extractions
data_2010 <- readRDS("./extractions/env_data_2010.RDS") %>% ConvertListToDF()
data_2011 <- readRDS("./extractions/env_data_2011.RDS") %>% ConvertListToDF()
data_2012 <- readRDS("./extractions/env_data_2012.RDS") %>% ConvertListToDF()
data_2013_14 <- readRDS("./extractions/env_data_2013-2014.RDS") %>% ConvertListToDF()
data_2015 <- readRDS("./extractions/env_data_2015.RDS") %>% ConvertListToDF()
data_2016 <- readRDS("./extractions/env_data_2016.RDS") %>% ConvertListToDF()
data_2017a <- readRDS("./extractions/env_data_2017-JANJUN.RDS") %>% ConvertListToDF()
data_2017b <- readRDS("./extractions/env_data_2017-JULSEP.RDS") %>% ConvertListToDF()
data_2017c <- readRDS("./extractions/env_data_2017-OCTNOV.RDS") %>% ConvertListToDF()
data_2017d <- readRDS("./extractions/env_data_2017-DEC.RDS") %>% ConvertListToDF()
data_2018 <- readRDS("./extractions/env_data_2018.RDS") %>% ConvertListToDF()
data_2019 <- readRDS("./extractions/env_data_2019.RDS") %>% ConvertListToDF()
data_2020a <- readRDS("./extractions/env_data_2020-JANJUN.RDS") %>% ConvertListToDF()
data_2020b <- readRDS("./extractions/env_data_2020-JULDEC.RDS") %>% ConvertListToDF()
data_2021a <- readRDS("./extractions/env_data_2021-JANJUN.RDS") %>% ConvertListToDF()
data_2021b <- readRDS("./extractions/env_data_2021-JULDEC.RDS") %>% ConvertListToDF()
data_2022 <- readRDS("./extractions/env_data_2022.RDS") %>% ConvertListToDF()
data_2023 <- readRDS("./extractions/env_data_2023.RDS") %>% ConvertListToDF()
data_2024 <- readRDS("./extractions/env_data_2024.RDS") %>% ConvertListToDF()

# Binding all together to save in one huge dataframe
consolidated_data <- rbind(data_2010,
                           data_2011,
                           data_2012,
                           data_2013_14,
                           data_2015,
                           data_2016,
                           data_2017a, data_2017b, data_2017c, data_2017d,
                           data_2018,
                           data_2019,
                           data_2020a, data_2020b,
                           data_2021a, data_2021b,
                           data_2022,
                           data_2023,
                           data_2024)

# rm(list = ls()[grepl("data_",ls())])
# gc()

# Save tabular data files
saveRDS(consolidated_data, "./extractions/consolidated-env-association.RDS")
write_xlsx(consolidated_data, "./extractions/consolidated-env-association.xlsx")

# QAQC check
consolidated_data %>% select(YEAR, MONTH) %>% table(useNA = "ifany")