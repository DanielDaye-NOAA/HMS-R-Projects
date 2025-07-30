# Code to fill in various numbers for slides in the AP commercial tuna landings presentation

library(dplyr)
library(readxl)
library(tidyverse)


tuna <- read.csv("C:/Users/rebecca.jauch/Desktop/June_landings.csv")

df <- tuna %>%
  group_by(Landing.Date, Vessel.Permit.Nbr) %>%
  summarize(Tot.tuna = length(Vessel.Permit.Nbr))

table(df$Tot.tuna)
n <- length(df$Tot.tuna)
n
14/338

harp <- read_xlsx("C:/Users/rebecca.jauch/Desktop/Harpoons.xlsx", sheet = 'sheet1')

df <- harp %>%
  group_by('Landing Date') %>%
  summarize(Tot.tuna = length('Vessel Permit Nbr'))

table(df$Tot.tuna)


tuna <- read.csv("C:/Users/rebecca.jauch/Desktop/Harp_data.csv")

df <- tuna %>%
  filter(Landing.Year == 2022) %>%
  group_by(Landing.Date, Vessel.Permit.Nbr) %>%
  summarize(Tot.tuna = length(Vessel.Permit.Nbr))

table(df$Tot.tuna)


tuna %>% filter(Landing.Year == 2022) %>% summarize(num_fish = length(Tag))
n <- length(df$Tot.tuna)
n
96/n
52/n
27/n
21/n
7/n
7/n
