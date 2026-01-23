#' Data exploration for GULFSPAN contributions from J. Hendon, A. Hilton, and D. Grubbs for A17

# Setup ----
library(readxl)
library(tidyverse)

getwd()

dir_dl <- "C:/Users/ddaye/Downloads/"

hilton_PCity <- read_xlsx(paste0(dir_dl,"2024-HMS-EFH_Annsli-Hilton-Panama-City_GULFSPAN.xlsx"), guess_max = 1e5)
grubbs_FLUni <- read_xlsx(paste0(dir_dl,"2024-HMS-EFH_Dean-Grubbs-FSU_GULFSPAN.xlsx"), guess_max = 1e5)
grubbs_FLUni2 <- read_xlsx(paste0(dir_dl,"2024-HMS-EFH_Dean-Grubbs-FSU_GULFSPAN.xlsx"), sheet = 2, guess_max = 1e5)
hendon_datas <- read_xlsx(paste0(dir_dl,"2024-HMS-EFH_Jill-Hendon_GULFSPAN.xlsx"), sheet = "Data Request Template", guess_max = 1e5)

hendon <- hendon_datas %>%
  transmute(SETNUM = NA,
            LON = as.numeric(Longitude),
            LAT = as.numeric(Latitude),
            SRC = "Hendon")

hilton <- hilton_PCity %>%
  transmute(SETNUM = NA, 
            LON = as.numeric(`LONGITUDE (DDdddd)`),
            LAT = as.numeric(`LATITUDE (DDdddd)`),
            SRC = "Hilton")

grubbs <- grubbs_FLUni %>%
  transmute(SETNUM = `SET NUMBER`) %>%
  left_join(grubbs2) %>%
  mutate(SRC = "Grubbs")

grubbs2 <- grubbs_FLUni2 %>%
  transmute(SETNUM = `SET NUMBER`,
            LON = `Long St`,
            LAT = `Lat St`)

# Plot of Region 2
ggplot(hilton, aes(LON, LAT))+
  annotation_map(map_data("world")) +
  coord_cartesian(xlim = c(-86.5,-84.5), ylim = c(29, 31), ratio = 1) +
  geom_point() +
  theme_bw()

grubbs %>%
  mutate(LON = ifelse(LON > 0, LON * -1, LON)) %>%
  ggplot(aes(LON, LAT)) +
  annotation_map(map_data("world")) +
  coord_cartesian(xlim = c(-86,-80), ylim = c(25, 31)) +
  geom_point(col = "red") 

# Region 3
grubbs_FLUni2 %>%
  mutate(`Long St` = ifelse(`Long St` > 0, `Long St` * -1, `Long St`)) %>%
  ggplot(aes(`Long St`, `Lat St`, col = VESSEL)) +
  annotation_map(map_data("world")) +
  coord_cartesian(xlim = c(-86,-80), ylim = c(25, 31)) +
  geom_point() +
  theme_bw()

# Plot - all GULFSPAN contributions
rbind(grubbs, hilton, hendon) %>%
  mutate(LON = ifelse(LON > 0, LON * -1, LON)) %>%
  ggplot(aes(LON, LAT, col = SRC)) +
  annotation_map(map_data("world")) +
  coord_cartesian(xlim = c(-92,-80), ylim = c(25, 31)) +
  geom_point() +
  labs(col = "Source") +
  theme_bw() +
  theme(panel.grid = element_blank())