# Required Libraries
library(tidyverse)

# Species codes for each group
shark_scs = c("SHX","CCN","RHT","CCO")
shark_lcs = c("CCL","SPK","NGB","GNC","CCP","SPL","SPZ","CCB","TIG")
shark_pel = c("ALV","BSH","POR","SMA")
shark_pro = c("CCA","CCS","CCR","DUS", "LMA", "CCT", "RHN","BTH", "OCS", "WSH")
tunas = c("ALB","BET","BLK","BFT","SKJ","YFT")
bills = c("BUM","WHM","RSP","SAI","SWO")

# Load POP CPUE data and format data columns
data <- read.csv("./data/HMS_POP_CPUE_2024.csv") %>%
  mutate(YEAR = as.numeric(YEAR),
         ANIMAL_CODE = as.character(ANIMAL_CODE),
         COUNT = as.numeric(gsub(",","",COUNT)),
         HAUL_NUM = as.numeric(HAUL_NUM),
         NUM_PER_HAUL = as.numeric(NUM_PER_HAUL),
         GROUP = ifelse(ANIMAL_CODE %in% shark_scs, "Small Coastal", NA),
         GROUP = ifelse(ANIMAL_CODE %in% shark_lcs, "Large Coastal", GROUP),
         GROUP = ifelse(ANIMAL_CODE %in% shark_pel, "Pelagic", GROUP),
         GROUP = ifelse(ANIMAL_CODE %in% shark_pro, "Prohibited", GROUP),
         GROUP = ifelse(ANIMAL_CODE %in% tunas, "TUNA", GROUP),
         GROUP = ifelse(ANIMAL_CODE %in% bills, "SWORD_BILL_FISHES", GROUP))

# Check ANIMAL_CODE values
data %>% filter(is.na(GROUP)) %>% select(ANIMAL_CODE) %>% table() %>% names()


# Plot CPUE for each group ----

## SHARK ----
data %>%
  filter(GROUP %in% c("Small Coastal","Large Coastal","Pelagic","Prohibited")) %>%
  group_by(GROUP, YEAR) %>%
  summarize(SUM_COUNT = sum(COUNT, na.rm = TRUE),
            SUM_HAULS = sum(HAUL_NUM, na.rm = TRUE),
            GRP_CPUE = SUM_COUNT/SUM_HAULS) %>%
  ggplot(aes(YEAR, GRP_CPUE, col = GROUP)) +
  geom_rect(aes(xmin = 2015, xmax = 2023, ymin = -1000, ymax = 3500), col = NA, fill = "gray95") +
  geom_line() +
  geom_point() +
  xlab("Year") + ylab("CPUE") + labs(color = "Category") +
  coord_cartesian(xlim = c(min(data$YEAR),max(data$YEAR)), ylim = c(0,8)) +
  theme_bw() +
  theme(panel.grid = element_blank())

## TUNAS ----
data %>% 
  filter(ANIMAL_CODE %in% tunas) %>%
  ggplot(aes(YEAR, COUNT, col = ANIMAL_CODE)) +
  geom_rect(aes(xmin = 2015, xmax = 2023, ymin = -1000, ymax = 3500), col = NA, fill = "gray95") +
  geom_line() +
  geom_point() +
  xlab("Year") + ylab("COUNT") + labs(color = "Category") +
  coord_cartesian(xlim = c(min(data$YEAR),max(data$YEAR)), ylim = c(-50, 3000)) +
  theme_bw() +
  theme(panel.grid = element_blank())

data %>% 
  filter(ANIMAL_CODE %in% tunas) %>%
  mutate(ANIMAL_NAME = ifelse(ANIMAL_CODE == "ALB", "ALBACORE", NA),
         ANIMAL_NAME = ifelse(ANIMAL_CODE == "BET", "BIGEYE TUNA", ANIMAL_NAME),
         ANIMAL_NAME = ifelse(ANIMAL_CODE == "BLK", "BLACKFIN TUNA", ANIMAL_NAME),
         ANIMAL_NAME = ifelse(ANIMAL_CODE == "BFT", "BLUEFIN TUNA", ANIMAL_NAME),
         ANIMAL_NAME = ifelse(ANIMAL_CODE == "SKJ", "SKIPJACK TUNA", ANIMAL_NAME),
         ANIMAL_NAME = ifelse(ANIMAL_CODE == "YFT", "YELLOWFIN TUNA", ANIMAL_NAME)) %>%
  ggplot(aes(YEAR, NUM_PER_HAUL, col = ANIMAL_NAME)) +
  geom_rect(aes(xmin = 2015, xmax = 2023, ymin = -1000, ymax = 3500), col = NA, fill = "gray95") +
  geom_line() +
  geom_point() +
  xlab("Year") + ylab("CPUE") + labs(color = "Category") +
  coord_cartesian(xlim = c(min(data$YEAR),max(data$YEAR)), ylim = c(0,20)) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())

# BILL + SWORD ----
data %>% 
  filter(GROUP == "SWORD_BILL_FISHES") %>%
  mutate(ANIMAL_NAME = ifelse(ANIMAL_CODE == "BUM", "BLUE MARLIN", NA),
         ANIMAL_NAME = ifelse(ANIMAL_CODE == "WHM", "WHITE MARLIN", ANIMAL_NAME),
         ANIMAL_NAME = ifelse(ANIMAL_CODE == "RSP", "ROUNDSCALE SPEARFISH", ANIMAL_NAME),
         ANIMAL_NAME = ifelse(ANIMAL_CODE == "SAI", "SAILFISH", ANIMAL_NAME),
         ANIMAL_NAME = ifelse(ANIMAL_CODE == "SWO", "SWORDFISH", ANIMAL_NAME)) %>%
  ggplot(aes(YEAR, NUM_PER_HAUL, col = ANIMAL_NAME)) +
  geom_rect(aes(xmin = 2015, xmax = 2023, ymin = -1000, ymax = 3500), col = NA, fill = "gray95") +
  geom_line() +
  geom_point() +
  xlab("Year") + ylab("CPUE") + labs(color = "Category") +
  coord_cartesian(xlim = c(min(data$YEAR),max(data$YEAR)), ylim = c(0,20)) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())
