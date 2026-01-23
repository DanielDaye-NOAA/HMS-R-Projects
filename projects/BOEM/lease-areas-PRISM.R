library(readxl)
library(tidyverse)

# NES ----
NES_SPRING <- read_xls("./GIS_exports/NES_SPRING.xls") %>%
  mutate(NAME = "NES",
         HAB_AREA = "NES",
         SEASON = "SPRING") %>%
  select(-c(OBJECTID, LME_NAME, ZONE_CODE))
NES_FALL   <- read_xls("./GIS_exports/NES_FALL.xls") %>%
  mutate(NAME = "NES",
         HAB_AREA = "NES",
         SEASON = "FALL") %>%
  select(-c(OBJECTID, LME_NAME, ZONE_CODE))

# Proposed Areas ----
PLA_SPRING <- read_xls("./GIS_exports/PLA_SPRING.xls") %>%
  mutate(NAME = Primary,
         HAB_AREA = "P_LEASE",
         SEASON = "SPRING") %>%
  select(-c(OBJECTID, Primary, ZONE_CODE))
PLA_FALL   <- read_xls("./GIS_exports/PLA_FALL.xls") %>%
  mutate(NAME = Primary,
         HAB_AREA = "P_LEASE",
         SEASON = "FALL") %>%
  select(-c(OBJECTID, Primary, ZONE_CODE))

# Existing Areas ----
ELA_SPRING <- read_xls("./GIS_exports/ELA_SPRING.xls") %>%
  mutate(NAME = Primary,
         HAB_AREA = "E_LEASE",
         SEASON = "SPRING") %>%
  select(-c(OBJECTID, Primary, ZONE_CODE))
ELA_FALL   <- read_xls("./GIS_exports/ELA_FALL.xls") %>%
  mutate(NAME = Primary,
         HAB_AREA = "E_LEASE",
         SEASON = "FALL") %>%
  select(-c(OBJECTID, Primary, ZONE_CODE))

# Combine
areas <- rbind(NES_SPRING, NES_FALL, PLA_SPRING, PLA_FALL, ELA_SPRING, ELA_FALL) %>%
  relocate(NAME, HAB_AREA, SEASON) %>%
  arrange(factor(HAB_AREA, levels = c("NES","E_LEASE","P_LEASE")), NAME, SEASON) %>%
  mutate(NAME = factor(NAME, levels = c("NES",  "ELA1", "ELA2", "ELA3", "ELA4", "ELA5",
                                        "ELA6", "ELA7", "PLA1", "PLA2", "PLA3", "PLA4")))

# Plot ----
nudge = position_nudge(x = c(-.15,.15))
ggplot(areas, aes(NAME, color = SEASON, group = NAME)) +
  geom_segment(aes(y = MEAN, yend = MEAN - STD),
               lwd = 3, position = nudge) +
  geom_segment(aes(y = MEAN, yend = MEAN + STD),
               lwd = 3, position = nudge) +
  geom_point(aes(y = MEAN),
             position = nudge,
             pch = 15, cex = 3,
             col = "black") + 
  theme_bw(base_size = 12) +
  labs(x = "Area / Lease Area (ELA = Existing; PLA = Planned)", y = "PRiSM Score (Mean +/- SD)") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title = element_text(size = 12),
        axis.text  = element_text(size = 11))