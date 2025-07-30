# Amendment 16 Observer Data

library(tidyverse)

# Load observer datasets
bll_obs <- read_csv("./data/observer/V_SBLOP_ANIMAL_20250716.csv", guess_max = 1e6)
gillnet <- read_csv("./data/observer/V_GOP_EXPANDED_ANIMAL_20250716.csv", guess_max = 1e6)
pelagic <- read_csv("./data/observer/V_POP_ANIMAL_LOG_20250716.csv", guess_max = 1e6)

# str(bll_obs)
# str(gillnet)
# str(pelagic)

# Subset and standardize column names for combining
sub_bll <- bll_obs %>% transmute(SOURCE = "BLL", ANIMAL_LOG = ANIMAL_LOG_ID, HAUL_LOG = HAUL_LOG_ID, 
                                 TRIP_LOG = TRIP_LOG_ID, TRIP_NUMBER,
                                 SPECIES = toupper(SPECIES_NAME), SPC_CODE = SPECIES_CODE,
                                 LENGTH1 = LENGTH, LTYPE1 = MEASUREMENT_TYPE, LTYPE1DESC = MEASUREMENT_TYPE_DESC,
                                 ACTION1 = ACTION_1, ACTION1_DESC = ACTION_1_DESC, ACTION2 = ACTION_2, ACTION2_DESC = ACTION_2_DESC,
                                 YEAR = HAUL_BEGIN_DATE_YEAR)
sub_gil <- gillnet %>% transmute(SOURCE = "GILLNET", ANIMAL_LOG = paste(TRIP_ID, SPECIMEN_NUMBER, sep="-"), HAUL_LOG = HAUL_NBR,
                                 TRIP_LOG = TRIP_ID, TRIP_NUMBER = NA,
                                 SPECIES = toupper(SP_COMMON_NAME), SPC_CODE = SPECIES_ID,
                                 LENGTH1 = LENGTH, LTYPE1 = MEASUREMENT_TYPE, LTYPE1DESC = MEASUREMENT_TYPE_DESC,
                                 ACTION1 = ACTION_1, ACTION1_DESC = ACTION_1_DESC, ACTION2 = NA, ACTION2_DESC = NA,
                                 YEAR = YEAR_LANDED)
sub_pel <- pelagic %>% transmute(SOURCE = "PLL", ANIMAL_LOG = ANIMAL_LOG_KEY, HAUL_LOG = HAUL_LOG_KEY, 
                                 TRIP_LOG = NA, TRIP_NUMBER,
                                 SPECIES = toupper(SPECIES_NAME), SPC_CODE = ALPHA_SPECIES_CODE,
                                 LENGTH1 = LENGTH_MEASUREMENT_ONE, LTYPE1 = LENGTH_TYPE_ONE, LTYPE1DESC = NA,
                                 ACTION1 = KEPT_OR_RELEASED, ACTION1_DESC = KEPT_OR_RELEASED_DESC, ACTION2 = NA, ACTION2_DESC = NA,
                                 YEAR = substr(as.character(BEGIN_HAUL_DATE), 8, 9)) %>%
  mutate(YEAR = as.numeric(ifelse(as.numeric(YEAR) <= 25, paste("20",YEAR,sep=""), paste("19",YEAR,sep=""))))

# Combine and filter to HMS species only
spclist <- "DOGFISH|MARLIN|SHARK|TUNA"
combined <- rbind(sub_bll, sub_gil, sub_pel) %>%
  filter(grepl(spclist, SPECIES))


# ANALYSIS ----

## Blacktip ----
blacktip <- combined %>%
  filter(SPECIES == "BLACKTIP SHARK")

### Mean Length ----
summary(blacktip$LENGTH1)

unique(blacktip$LTYPE1)
unique(blacktip$LTYPE1DESC)

#### Longline ----
# Weighted mean by sample size for each year
blacktip_LL <- blacktip %>% filter(SOURCE == "BLL") %>%
  group_by(YEAR) %>%
  summarize(n = n(), LMEAN = mean(LENGTH1, na.rm = T))
wmean_LL <- sum(blacktip_LL$n * blacktip_LL$LMEAN) / sum(blacktip_LL$n)  # 121.7

#### Gillnet ----
# Weighted mean by log-transformed sample size to account for magnitude-difference, and weighting
# more recent data more than older data to account for a changing fishery
blacktip_GN <- blacktip %>% filter(SOURCE == "GILLNET") %>%
  group_by(YEAR) %>%
  summarize(n = n(), LMEAN = mean(LENGTH1, na.rm = T))
wyear_GN <- 1 + (.01 * (blacktip_GN$YEAR - max(blacktip_GN$YEAR)))
wmean_GN <- sum(log(blacktip_GN$n) * blacktip_GN$LMEAN * wyear_GN) / sum(log(blacktip_GN$n) * wyear_GN)  # 101.5

#### Plot ----
blacktip %>%
  group_by(YEAR, SOURCE) %>%
  summarize(n = n(), 
            LMEAN = mean(LENGTH1, na.rm = T),
            LMED = median(LENGTH1, na.rm = T),
            LSTDEV = sd(LENGTH1, na.rm = T)) %>%
  ggplot(aes(YEAR, LMEAN, col = SOURCE)) +
  geom_hline(yintercept = wmean_LL, lty = "dashed", col = "red", alpha = 0.5) +    # 121.7
  geom_hline(yintercept = wmean_GN, lty = "dashed", col = "cyan3", alpha = 0.5) +  # 115.3
  geom_line() +
  labs(title = "Blacktip Shark (C. limbatus)", col = "Fishery") +
  xlab("YEAR") + ylab("Mean Length") +
  coord_cartesian(ylim = c(0,150)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())
