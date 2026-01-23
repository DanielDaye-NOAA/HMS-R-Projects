#' Scratch work to figure out how ridgeplots from ggridges library functions

library(ggridges)
library(readxl)
library(tidyverse)

data <- read_xlsx("./extractions/env_data_2010.xlsx", guess_max = 1e6)

SST <- data %>%
  pivot_longer(cols = BATHYMETRY:CHLA,
               names_to = "ENV_VAR",
               values_to = "VALUE") %>%
  filter(ENV_VAR == "SST") %>%
  mutate(YEAR = factor(YEAR, levels = as.character(2010:2024)),
         MONTH = factor(MONTH, levels = rev(c("01","02","03","04","05","06","07","08","09","10","11","12")))) %>%
  arrange(COM_NAME, YEAR, DATE)

for (i in 1:length(unique(SST$COM_NAME))) {
  print(i)
  spc <- unique(SST$COM_NAME)[i]
  plt.i <- SST %>%
    filter(COM_NAME == spc, !is.na(VALUE)) %>%
    ggplot(aes(VALUE, MONTH, fill = stat(x))) +
    geom_density_ridges_gradient(scale=0.8, rel_min_height = 0.001, bandwidth=.5) +
    scale_fill_viridis_c(name = "SST [C]", option = "C") +
    xlim(min(SST$VALUE, na.rm = TRUE), max(SST$VALUE, na.rm = TRUE)) +
    theme_bw() + labs(title = spc) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) 
  plot(plt.i)
}