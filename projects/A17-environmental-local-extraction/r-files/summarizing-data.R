#' R script to summarize environmental data for A17 associated through BatchEnvAssign. Produces
#' data summaries by Species and lifestage, exports to excel. Also responsible for creating the 
#' text descriptions which were shared with the A17 team
#' 
#' Need to add some more comments about what all is contained below the text description.

# Setup ----
library(tidyverse)

#' All extractions were consolidated using env-data-consolidation.R
#' Consolidated data is saved as RDS because it is too large (~1m data points) and opens really slow
#' as .xlsx
data <- readRDS("./data/consolidated-env-association.RDS") %>%
  mutate(BATHYMETRY = ifelse(BATHYMETRY >= 0, NA, BATHYMETRY))

# Setting up a reference table for the units associated with each env. var.
units <- data.frame(VARS = c("BATHYMETRY", "BS", "BT", "CHLA",
                             "MLD", "RUGOSITY", "SSH", "SSS",
                             "SST", "THETAO", "TURB", "ZOS"),
                    LANG = c("depth", "Bottom salinity", "Bottom temperature", "Chlorophyll-a concentrations",
                             "Mixed layer depth", "Seafloor rugosity", "Sea surface height", "Sea surface salinity", 
                             "SST", "SST", "Turbidity", "Sea surface height"),
                    UNIT = c("m","psu","degC","mgm3","m","m","m","psu","degC","degC","m","m"),
                    SOURCE = c("ETOPO", "HYCOM", "HYCOM", "CMEMS", "CMEMS", "ETOPO",
                               "HYCOM", "HYCOM", "HYCOM", "CMEMS", "CMEMS", "CMEMS")) %>%
  arrange(SOURCE, VARS)

# SRU: Seafloor Rugosity Units - standard deviation in bathymetric depth (meters) within a 15x15 grid centered
# on each bathymetry raster cell (not relevent because we didn't ultimately include)

# QAQC ----
# Fixing sailfish names
data <- data %>%
  mutate(COM_NAME = ifelse(COM_NAME == "ATLANTIC_SAILFISH", "SAILFISH", COM_NAME)) 


data %>%
  group_by(COM_NAME) %>%
  summarize(N_SCI = length(unique(SCI_NAME))) %>% arrange(desc(N_SCI))

## Normalizing tag data ----

# table(data$TAG_TYPE, useNA = "ifany") %>% data.frame() %>% View()
notable_tags <- c("acoustic-tag", "dart-tag", "Floy-tag", "no-tag-present", "PSAT-tag",
                  "roto-tag", "SPLASH-tag", "SPOT-tag", "unknown-tag")

# Correcting tag types
data <- data %>%
  mutate(TAG_TYPE2 = TAG_TYPE,
         TAG_TYPE2 = ifelse(TAG_TYPE2 %in% c("A","Acoustic","ACOUSTIC"), "acoustic-tag", TAG_TYPE2),
         TAG_TYPE2 = ifelse(TAG_TYPE2 %in% c("DART","DART TAG #"), "dart-tag", TAG_TYPE2),
         TAG_TYPE2 = ifelse(grepl("FLOY", TAG_TYPE2), "Floy-tag", TAG_TYPE2),
         TAG_TYPE2 = ifelse(grepl("SPOT", TAG_TYPE2), "SPOT-tag", TAG_TYPE2),
         TAG_TYPE2 = ifelse(grepl("PSAT", TAG_TYPE2), "PSAT-tag", TAG_TYPE2),
         TAG_TYPE2 = ifelse(grepl("PAT", TAG_TYPE2), "PSAT-tag", TAG_TYPE2),
         TAG_TYPE2 = ifelse(grepl("roto", TAG_TYPE2, ignore.case = TRUE), "roto-tag", TAG_TYPE2),
         TAG_TYPE2 = ifelse(grepl("SPLASH", TAG_TYPE2, ignore.case = TRUE), "SPLASH-tag", TAG_TYPE2),
         TAG_TYPE2 = ifelse(is.na(TAG_TYPE2), "no-tag-present", TAG_TYPE2),
         TAG_TYPE2 = ifelse(TAG_TYPE2 %in% c("UNF","UNK","Unknown"), "unknown-tag", TAG_TYPE2),
         TAG_TYPE2 = ifelse(!(TAG_TYPE2 %in% notable_tags), "other-tag-type", TAG_TYPE2))

# Separating out satellite/acoustic data to average values by date
data_norml <- data %>%
  filter(TAG_TYPE2 %in% c("acoustic-tag","PSAT-tag","SPLASH-tag","SPOT-tag") & !is.na(TAG_NO)) %>%
  group_by(SPECIES_CODE, SRC, COM_NAME, SCI_NAME, LIFESTAGE, LEN_UNIT, LEN_TYPE, SEX, YEAR, MONTH, DAY, 
           ORIG_LS, YEAR_MO, TAG_NO, TAG_TYPE2, LENGTH, DATE) %>%
  mutate(BATHYMETRY = ifelse(BATHYMETRY >= 0, NA, BATHYMETRY)) %>%
  summarize(N_OBS = n(),
            REGION = paste(unique(REGION, collapse = ":")),
            LON = mean(LON, na.rm = TRUE), LAT = mean(LAT, na.rm = TRUE),
            BATHYMETRY = mean(BATHYMETRY, na.rm = TRUE),
            RUGOSITY = mean(RUGOSITY, na.rm = TRUE),
            SST = mean(SST, na.rm = TRUE),
            SSS = mean(SSS, na.rm = TRUE),
            SSH = mean(SSH, na.rm = TRUE),
            BT = mean(BT, na.rm = TRUE),
            BS = mean(BS, na.rm = TRUE),
            MLD = mean(MLD, na.rm = TRUE),
            ZOS = mean(ZOS, na.rm = TRUE),
            THETAO = mean(THETAO, na.rm = TRUE),
            TURB = mean(TURB, na.rm = TRUE),
            CHLA = mean(CHLA, na.rm = TRUE)) %>% ungroup() %>%
  mutate(TID_SRC = "data-normalized",
         TAG_TYPE = "data-normalized",
         TAG_EVENT = "data-normalized",)

data_notag <- data %>%
  filter(!(TAG_TYPE2 %in% c("acoustic-tag","PSAT-tag","SPLASH-tag","SPOT-tag")) | is.na(TAG_NO)) %>%
  mutate(N_OBS = 1)

nrow(data) == sum(data_norml$N_OBS) + nrow(data_notag)

data_norm <- rbind(data_notag, data_norml)

## Saving ----
# saveRDS(data_norm, "consolidated-env-association-normalized.RDS")
# write_xlsx(data_norm, "consolidated-env-association-normalized.xlsx")


# Data Manipulation ----

## Load Normalized Data ----
data_norm <- readRDS("./data/consolidated-env-association-normalized.RDS")

# Summarized by species & lifestage
data_norm %>%
  pivot_longer(cols = BATHYMETRY:CHLA, names_to = "ENV_VAR", values_to = "VALUE") %>%
  group_by(COM_NAME, LIFESTAGE, ENV_VAR) %>%
  filter(!is.na(VALUE), LIFESTAGE != "UNK") %>%
  summarize(N = n(), 
            MIN = min(VALUE, na.rm = TRUE), 
            MEDIAN = median(VALUE, na.rm = TRUE), 
            MEAN = mean(VALUE, na.rm = TRUE), 
            MAX = max(VALUE, na.rm = TRUE)) %>%
  arrange(COM_NAME, ENV_VAR, LIFESTAGE) # %>% View()

# Summarized by species
data_norm %>%
  pivot_longer(cols = BATHYMETRY:CHLA, names_to = "ENV_VAR", values_to = "VALUE") %>%
  group_by(COM_NAME, ENV_VAR) %>%
  filter(!is.na(VALUE)) %>%
  summarize(N = n(), 
            MIN = min(VALUE, na.rm = TRUE), 
            MEDIAN = median(VALUE, na.rm = TRUE), 
            MEAN = mean(VALUE, na.rm = TRUE), 
            MAX = max(VALUE, na.rm = TRUE)) %>%
  arrange(COM_NAME, ENV_VAR) # %>% View()

data_norm %>% 
  ggplot(aes(BATHYMETRY, col = COM_NAME)) +
  geom_density() +
  xlim(-50, 10) +
  guides(col = "none") +
  theme_bw()

# PRODUCTS ----

## Env. Tables ----
library(readxl)
library(tidyverse)
library(writexl)

source("./r-files/FinalizeLS.R")

data_norm <- readRDS("./data/consolidated-env-association-normalized.RDS")
data_norm <- FinalizeLS(data_norm) %>%
  mutate(REGION = ifelse(REGION == "GOM" & LON > -78.05, "CARIB", REGION),
         REGION = ifelse(LAT <= (LON*0.54 +  65.29) & REGION == "GOM", "CARIB", REGION))

# Regions used for generating KDE
data_norm  %>%
  arrange(REGFINAL) %>%
  ggplot(aes(LON, LAT, color = REGFINAL)) +
  geom_point(size = 0.25) + annotation_map(map_data("world")) + theme_bw()

# Regions for text description (if needed)
data_norm  %>%
  ggplot(aes(LON, LAT, color = REGION)) +
  geom_point(size = 0.25) + annotation_map(map_data("world")) + theme_bw()

vars <- c("BATHYMETRY","RUGOSITY","THETAO","BT","MLD","SSS","BS","CHLA")

## Species-level
tab_SP <- data_norm %>%
  pivot_longer(cols = BATHYMETRY:CHLA, names_to = "ENV_VAR", values_to = "VALUE") %>%
  filter(ENV_VAR %in% vars) %>%
  group_by(COMFINAL, ENV_VAR) %>%
  filter(!is.na(VALUE)) %>%
  summarize(LIFESTAGE = "ALL-LS",
            N = n(), 
            MIN = min(VALUE, na.rm = TRUE), 
            MEDIAN = median(VALUE, na.rm = TRUE), 
            MEAN = mean(VALUE, na.rm = TRUE), 
            MAX = max(VALUE, na.rm = TRUE),
            SD = sd(VALUE, na.rm = TRUE),
            Q1 = quantile(VALUE, 0.25, na.rm = TRUE),
            Q3 = quantile(VALUE, 0.75, na.rm = TRUE)) %>%
  arrange(COMFINAL, ENV_VAR) %>%
  rename(`Common Name` = COMFINAL,
         `Lifestage` = LIFESTAGE,
         `Environmental Variable` = ENV_VAR,
         `N Obs.` = N,
         `Min. Value` = MIN,
         `Max. Value` = MAX,
         `Median Value` = MEDIAN,
         `Quartile 1` = Q1,
         `Quartile 3` = Q3,
         `Mean Value` = MEAN,
         `Standard Deviation` = SD)

## Species-Lifestage
tab_LS <- data_norm %>%
  pivot_longer(cols = BATHYMETRY:CHLA, names_to = "ENV_VAR", values_to = "VALUE") %>%
  filter(ENV_VAR %in% vars) %>%
  group_by(COMFINAL, LSFINAL, ENV_VAR) %>%
  filter(!is.na(VALUE)) %>%
  summarize(N = n(), 
            MIN = min(VALUE, na.rm = TRUE), 
            MEDIAN = median(VALUE, na.rm = TRUE), 
            MEAN = mean(VALUE, na.rm = TRUE), 
            MAX = max(VALUE, na.rm = TRUE),
            SD = sd(VALUE, na.rm = TRUE),
            Q1 = quantile(VALUE, 0.25, na.rm = TRUE),
            Q3 = quantile(VALUE, 0.75, na.rm = TRUE)) %>%
  arrange(COMFINAL, LSFINAL, ENV_VAR) %>%
  rename(`Common Name` = COMFINAL,
         `Lifestage` = LSFINAL,
         `Environmental Variable` = ENV_VAR,
         `N Obs.` = N,
         `Min. Value` = MIN,
         `Max. Value` = MAX,
         `Median Value` = MEDIAN,
         `Quartile 1` = Q1,
         `Quartile 3` = Q3,
         `Mean Value` = MEAN,
         `Standard Deviation` = SD)

## Species-Lifestage-Region
tab_LS_REG <- data_norm %>%
  pivot_longer(cols = BATHYMETRY:CHLA, names_to = "ENV_VAR", values_to = "VALUE") %>%
  filter(ENV_VAR %in% vars) %>%
  group_by(COMFINAL, REGFINAL, LSFINAL, ENV_VAR) %>%
  filter(!is.na(VALUE)) %>%
  summarize(N = n(), 
            MIN = min(VALUE, na.rm = TRUE), 
            MEDIAN = median(VALUE, na.rm = TRUE), 
            MEAN = mean(VALUE, na.rm = TRUE), 
            MAX = max(VALUE, na.rm = TRUE),
            SD = sd(VALUE, na.rm = TRUE),
            Q1 = quantile(VALUE, 0.25, na.rm = TRUE),
            Q3 = quantile(VALUE, 0.75, na.rm = TRUE)) %>%
  arrange(COMFINAL, REGFINAL, LSFINAL, ENV_VAR) %>%
  rename(`Common Name` = COMFINAL,
         `Region` = REGFINAL,
         `Lifestage` = LSFINAL,
         `Environmental Variable` = ENV_VAR,
         `N Obs.` = N,
         `Min. Value` = MIN,
         `Max. Value` = MAX,
         `Median Value` = MEDIAN,
         `Quartile 1` = Q1,
         `Quartile 3` = Q3,
         `Mean Value` = MEAN,
         `Standard Deviation` = SD)

## Caribbean
tab_LS_CAR <- data_norm %>%
  pivot_longer(cols = BATHYMETRY:CHLA, names_to = "ENV_VAR", values_to = "VALUE") %>%
  filter(ENV_VAR %in% vars) %>%
  group_by(COMFINAL, REGION, LSFINAL, ENV_VAR) %>%
  filter(!is.na(VALUE)) %>%
  summarize(N = n(), 
            MIN = min(VALUE, na.rm = TRUE), 
            MEDIAN = median(VALUE, na.rm = TRUE), 
            MEAN = mean(VALUE, na.rm = TRUE), 
            MAX = max(VALUE, na.rm = TRUE),
            SD = sd(VALUE, na.rm = TRUE),
            Q1 = quantile(VALUE, 0.25, na.rm = TRUE),
            Q3 = quantile(VALUE, 0.75, na.rm = TRUE)) %>%
  arrange(COMFINAL, REGION, LSFINAL, ENV_VAR) %>%
  rename(`Common Name` = COMFINAL,
         `Region` = REGION,
         `Lifestage` = LSFINAL,
         `Environmental Variable` = ENV_VAR,
         `N Obs.` = N,
         `Min. Value` = MIN,
         `Max. Value` = MAX,
         `Median Value` = MEDIAN,
         `Quartile 1` = Q1,
         `Quartile 3` = Q3,
         `Mean Value` = MEAN,
         `Standard Deviation` = SD)

### Saving as .xlsx ----
library(openxlsx)

wb <- createWorkbook(creator = "DDaye", title = "A17 Remote Env Data - Text Description Analysis")

addWorksheet(wb, "regional-ls")
addWorksheet(wb, "lifestage")
addWorksheet(wb, "all-obs")
addWorksheet(wb, "caribbean")

writeData(wb, sheet = "regional-ls", tab_LS_REG)
writeData(wb, sheet = "lifestage", tab_LS)
writeData(wb, sheet = "all-obs", tab_SP)
writeData(wb, sheet = "caribbean", tab_LS_CAR)

saveWorkbook(wb, "A17-text-description-env-tables.xlsx", overwrite = TRUE)


## Text Descriptions ----
### Setup ----
library(ggforce)
library(readxl)
library(tidyverse)
library(writexl)

source("./r-files/FinalizeLS.R")
source("./r-files/GenerateTextDescriptions.R")

# Load normalized data with env. information
data_norm <- readRDS("./data/consolidated-env-association-normalized.RDS")

# Assign points to CARIB REGION
data_norm <- FinalizeLS(data_norm) %>%
  mutate(REGION = ifelse(REGION == "GOM" & LON > -78.05, "CARIB", REGION),
         REGION = ifelse(LAT <= (LON*0.54 +  65.29) & REGION == "GOM", "CARIB", REGION))

# Load reference file with final A17 lifestage and region groupings
ref <- read_xlsx("./supplemental/text-description-species-regions-lifestages.xlsx")

## GenTextDescriptions ----
text_descriptions <- GenerateTextDescriptions(data_norm, ref)
cat(text_descriptions)

## Rugosity Info ----
rugos_data <- data_norm %>%
  mutate(GRP = ifelse(grepl("TUNA|ALBACORE", COMFINAL), "TUNAS", NA),
         GRP = ifelse(grepl("MARLIN|SAILFISH|SPEARFISH|BILLFISH", COMFINAL), "BILLFISH", GRP),
         GRP = ifelse(grepl("SWORDFISH", COMFINAL), "SWORDFISH", GRP),
         GRP = ifelse(grepl("SMOOTHHOUND", COMFINAL), "SMOOTHHOUNDS", GRP),
         GRP = ifelse(grepl("BLACKTIP|BULL|HAMMERHEAD|LEMON|NURSE|SANDBAR|SILKY|SPINNER|TIGER", COMFINAL), "LG COASTAL", GRP),
         GRP = ifelse(grepl("BLUE|OCEANIC_WHITETIP|PORBEAGLE|SHORTFIN_MAKO|COMMON_THRESHER", COMFINAL), "PELAGIC", GRP),
         GRP = ifelse(grepl("SHARPNOSE|BLACKNOSE|BONNETHEAD|FINETOOTH", COMFINAL), "SM COASTAL", GRP),
         GRP = ifelse(grepl("ANGEL|BASKING|BIGEYE_SAND|BIGEYE_THRESHER|BIGNOSE|CARIBBEAN_REEF|DUSKY|LONGFIN_MAKO|NARROWTOOTH|NIGHT|SIXGILL|SAND_TIGER|WHITE_SHARK|WHALE", COMFINAL), "PROHIBITED", GRP)) %>%
  group_by(COMFINAL) %>%
  summarize(n = n(),
            mean.log.depth = log(median(BATHYMETRY*-1, na.rm = TRUE)),
            rugos = median(RUGOSITY, na.rm = TRUE),
            r25 = quantile(RUGOSITY, 0.25, na.rm = TRUE),
            r75 = quantile(RUGOSITY, 0.75, na.rm = TRUE),
            IQR = abs(r25-r75),
            GRP = unique(GRP),
            sd.depth = sd(BATHYMETRY*-1, na.rm = T),
            sd.rugos = sd(RUGOSITY, na.rm = T)) %>%
  mutate(FILTER = ifelse(n >= 100, TRUE, FALSE),
         depth.m = exp(mean.log.depth)) %>% ungroup()

# Print species to console
rugos_data %>%
  filter(n > 100) %>%
  select(GRP, COMFINAL, n, rugos, mean.log.depth, depth.m) %>%
  filter(rugos >= 20) %>%
  arrange(GRP, mean.log.depth, rugos) %>% distinct() %>% print(n = 50)

# Plot depth x rugosity and categorize
rugos_data %>%
  filter(FILTER) %>%
  ggplot(aes(mean.log.depth, rugos, col = GRP)) +
  geom_vline(xintercept = log(c(50,200,500,1000)), col = "gray75") +
  geom_hline(yintercept = 20, col = "gray75") +
  coord_cartesian(xlim = c(0, log(6000)), ylim = c(0,250), expand = FALSE) +
  scale_x_continuous(breaks = log(c(50,200,500,1000,6000)), labels = c("50","200","500","1,000","6,000")) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = r25, ymax = r75), alpha = 0.1) +
  xlab("Median Depth (m) - log scaled") + ylab("Median Rugosity") +
  theme_bw() +
  theme(panel.grid = element_blank())

### Functional Groups ----
for (group in unique(rugos_data$GRP)) {
  message(group)
  plot <- rugos_data %>%
    filter(GRP == group) %>%
    ggplot(aes(mean.log.depth, rugos)) +
    geom_vline(xintercept = log(c(50,200,500,1000)), col = "gray85") +
    geom_hline(yintercept = 20, col = "gray85") +
    geom_point() +
    geom_text(. %>% filter(rugos >= 20), mapping = aes(label = COMFINAL), nudge_x = .1, nudge_y = 7.5, size = 3) +
    coord_cartesian(xlim = c(0, log(7000)), ylim = c(0,250), expand = FALSE) +
    scale_x_continuous(breaks = log(c(50,200,500,1000,6000)), labels = c("50","200","500","1,000","6,000")) +
    theme_bw() +
    xlab("Depth (m)") + ylab("Rugosity (m)") +
    labs(title = group) +
    theme(panel.grid = element_blank())
  plot(plot)
}


rugos_data %>%
  filter(n > 100) %>%
  group_by(GRP) %>%
  mutate(depth.m = exp(mean.log.depth)) %>%
  summarize(n.spp = n(),
            mean.depth = mean(depth.m, na.rm = T),
            sd.depth = sd(depth.m, na.rm = T),
            mean.rugos = mean(rugos, na.rm = T),
            sd.rugos = sd(rugos, na.rm = T)) %>%
  filter(!is.na(GRP)) %>%
  ggplot(aes(mean.depth, mean.rugos)) +
  geom_point() +
  geom_ellipse(mapping = aes(x0=mean.depth, y0=mean.rugos, a = sd.depth, b = sd.rugos, angle = 0)) +
  geom_text(mapping = aes(label = GRP), nudge_x = 75, nudge_y = 4) +
  theme_bw()

# PCA
library(FactoMineR)
library(factoextra)
library(ggcorrplot)
sp_PCA <- data_norm %>% 
  mutate(GRP = ifelse(grepl("TUNA|ALBACORE", COMFINAL), "TUNAS", NA),
         GRP = ifelse(grepl("MARLIN|SAILFISH|SPEARFISH|BILLFISH", COMFINAL), "BILLFISH", GRP),
         GRP = ifelse(grepl("SWORDFISH", COMFINAL), "SWORDFISH", GRP),
         GRP = ifelse(grepl("SMOOTHHOUND", COMFINAL), "SMOOTHHOUNDS", GRP),
         GRP = ifelse(grepl("BLACKTIP|BULL|HAMMERHEAD|LEMON|NURSE|SANDBAR|SILKY|SPINNER|TIGER", COMFINAL), "LG COASTAL", GRP),
         GRP = ifelse(grepl("BLUE_SHARK|OCEANIC_WHITETIP|PORBEAGLE|SHORTFIN_MAKO|COMMON_THRESHER", COMFINAL), "PELAGIC", GRP),
         GRP = ifelse(grepl("SHARPNOSE|BLACKNOSE|BONNETHEAD|FINETOOTH", COMFINAL), "SM COASTAL", GRP),
         GRP = ifelse(grepl("ANGEL|BASKING|BIGEYE_SAND|BIGEYE_THRESHER|BIGNOSE|CARIBBEAN_REEF|DUSKY|LONGFIN_MAKO|NARROWTOOTH|NIGHT|SIXGILL|SAND_TIGER|WHITE|WHALE", COMFINAL), "PROHIBITED", GRP)) %>%
  group_by(COMFINAL) %>%
  summarize(n = n(), GRP = unique(GRP),
            bmean = mean(BATHYMETRY*-1, na.rm = TRUE),
            bmed = median(BATHYMETRY*-1, na.rm = TRUE),
            b25 = quantile(BATHYMETRY*-1, 0.25, na.rm = TRUE),
            b75 = quantile(BATHYMETRY*-1, 0.75, na.rm = TRUE),
            rmean = mean(RUGOSITY, na.rm = TRUE),
            rmed = median(RUGOSITY, na.rm = TRUE),
            r25 = quantile(RUGOSITY, 0.25, na.rm = TRUE),
            r75 = quantile(RUGOSITY, 0.75, na.rm = TRUE)) %>%
  mutate(bIQR = abs(b75-b25),
         rIQR = abs(r75-r25)) %>% filter(n > 100)

corrplot(sp_PCA)
sp_PCA_norm <- scale(sp_PCA %>% select(-c(COMFINAL,n,GRP,b25,b75,r25,r75,bmean,rmean)), center = TRUE)
head(sp_PCA_norm)
data.pca <- princomp(sp_PCA_norm)
summary(data.pca)
data.pca$loadings[,1:3]
fviz_eig(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca, col.var = "black")

ggplot(data.frame(x=data.pca$scores[,1],y=data.pca$scores[,2]), aes(x,y, col = sp_PCA$GRP)) +
  geom_point() +
  theme_bw()


# Additional Plots ----
library(terra)
library(tidyterra)
library(ggplot2)
bathy150 <- vect("G:/SF1/GIS/A17 - EFH/EditingFiles/Editing files/daye_updated_masks/GEBCO2024-avg_contours-150m.shp")

data_norm %>%
  filter(COMFINAL == "SANDBAR_SHARK", LSFINAL %in% c("JUV-ADU")) %>%
  filter(LIFESTAGE != "UNK") %>%
  ggplot(aes(LON,LAT,color=LIFESTAGE)) +
  geom_point() + facet_wrap(~LIFESTAGE, ncol = 1) +
  geom_spatvector(data = bathy150, inherit.aes = FALSE) +
  annotation_map(map_data("world"), col = "gray70", fill = "gray85") +
  coord_sf(xlim = c(-100,-75), ylim = c(22, 34)) +
  theme_bw()

# Juv = 1173
# Adu = 5147
data_norm %>% 
  filter(REGION == "GOM", 
         COMFINAL == "SANDBAR_SHARK", 
         LIFESTAGE == "JUV") %>%
  ggplot(aes(LON,LAT)) +
  geom_point(alpha = 0.25) + labs(title = "JUV Sandbar (n = 1173)") +
  geom_spatvector(data = bathy150, col = "gray50", inherit.aes = FALSE) +
  annotation_map(map_data("world"), col = "gray70", fill = "gray85") +
  coord_sf(xlim = c(-100,-75), ylim = c(22, 34)) +
  theme_bw() + theme(panel.grid = element_blank())
data_norm %>% 
  filter(REGION == "GOM", 
         COMFINAL == "SANDBAR_SHARK", 
         LIFESTAGE == "ADU") %>%
  ggplot(aes(LON,LAT)) +
  geom_point(alpha = 0.057) + labs(title = "ADU Sandbar (n = 5147)") +
  geom_spatvector(data = bathy150, col = "gray50", inherit.aes = FALSE) +
  annotation_map(map_data("world"), col = "gray70", fill = "gray85") +
  coord_sf(xlim = c(-100,-75), ylim = c(22, 34)) +
  theme_bw() + theme(panel.grid = element_blank())



data_norm %>%
  filter(COMFINAL == "SANDBAR_SHARK", LSFINAL %in% c("JUV-ADU")) %>%
  filter(LIFESTAGE != "UNK") %>%
  ggplot(aes(LON,LAT,color=LIFESTAGE)) +
  geom_point() + facet_wrap(~LIFESTAGE, ncol = 1) +
  geom_spatvector(data = bathy150, inherit.aes = FALSE) +
  annotation_map(map_data("world"), col = "gray70", fill = "gray85") +
  coord_sf(xlim = c(-100,-75), ylim = c(22, 34)) +
  theme_bw()

quants <- data_norm %>%
  mutate(DEPTH = BATHYMETRY * -1) %>%
  filter(COMFINAL == "SHORTFIN_MAKO_SHARK") %>%
  group_by(REGION) %>%
  summarize(mean = mean(DEPTH, na.rm = T),
            median = median(DEPTH, na.rm = T),
            quant.025 = quantile(DEPTH, 0.025, na.rm = T),
            quant.250 = quantile(DEPTH, 0.250, na.rm = T),
            quant.750 = quantile(DEPTH, 0.750, na.rm = T),
            quant.975 = quantile(DEPTH, 0.975, na.rm = T),
            max = max(DEPTH, na.rm = T)) %>%
  filter(REGION != "CARIB") %>%
  mutate(ymin = 5, ymax = Inf)


# Log-transformed depth histograms
data_norm %>% filter(REGION != "CARIB") %>%
  mutate(DEPTH = BATHYMETRY * -1) %>%
  filter(COMFINAL == "SHORTFIN_MAKO_SHARK") %>%
  ggplot(aes(DEPTH)) +
  facet_wrap(~REGION, ncol = 1, scales = "free_y") +
  geom_vline(data = quants, mapping = aes(xintercept = quant025), inherit.aes = FALSE, lty = "dotted") +
  geom_vline(data = quants, mapping = aes(xintercept = quant975), inherit.aes = FALSE, lty = "dotted") +
  geom_rect(data = quants, aes(xmin = quant250, xmax = quant750, ymin = ymin, ymax = ymax), col = "lightblue3", fill = "lightblue2", inherit.aes = FALSE) +
  geom_histogram(binwidth = .05, center = 0.025) + ylab("Count") + xlab("Depth (m)") +
  scale_x_log10(limits = c(10, 10000)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11))

# Roundscale Spearfish only
rsp <- data %>%
  filter(COM_NAME == "ROUNDSCALE_SPEARFISH") %>%
  select(SPECIES_CODE, LIFESTAGE, BATHYMETRY,RUGOSITY,THETAO,BT,MLD,SSS,BS,CHLA) %>%
  pivot_longer(BATHYMETRY:CHLA, names_to = "VAR") %>%
  group_by(LIFESTAGE, VAR) %>%
  summarize(N = n(),
            MEDIAN = median(value, na.rm = TRUE),
            Q1 = quantile(value, 0.25, na.rm = TRUE),
            Q3 = quantile(value, 0.75, na.rm = TRUE),
            MEAN = mean(value, na.rm = TRUE),
            STD = sd(value, na.rm = TRUE),
            MIN = min(value, na.rm = TRUE),
            MAX = max(value, na.rm = TRUE)) %>%
  filter(LIFESTAGE != "UNK")