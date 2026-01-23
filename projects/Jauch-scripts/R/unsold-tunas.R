# This was to look at unsold tuna using full landing data from SAFIS

library(dplyr)
library(ggplot2)
library(tidyverse)


tuna <- read.csv("C:/Users/rebecca.jauch/Desktop/Tuna/Audit_Safis_Data.csv")

tuna_dat <- tuna[c('Landing.Date', 'Disposition.Desc', 'Port.Name', 'Category', 
                   'Reported.Quantity', 'Species.Length', 'Bft.Area.Name', 'Sale.Nature.Desc',
                   'Tag', 'Landing.Year', 'Landing.Month', 'Landing.Day')]
dim(tuna_dat)

plot(tuna_dat$Landing.Year, tuna_dat$Reported.Quantity)
plot(tuna_dat)

max(tuna_dat$Reported.Quantity)

df <- tuna_dat %>%
  group_by(Landing.Year) %>%
  summarize(Tot.unsold = sum(Reported.Quantity))

plot(df, pch=16)
text(labels=df$Tot.unsold)

tuna_dat %>% 
  group_by(Landing.Year) %>% 
  summarize(Tot.unsold = sum(Reported.Quantity)) %>% 
  ggplot(aes(x = Landing.Year, y = Tot.unsold, group = Landing.Year)) +
  geom_col() +
  geom_label(label = df$Tot.unsold) +
  labs(x = "Landing Year", 
       y = "Total Unsold Tuna in lbs", 
       title = "Unsold Tuna by Year")

tuna_dat %>% 
  group_by(Landing.Month, Landing.Year) %>% 
  summarize(m = sum(Reported.Quantity)) %>% 
  ggplot(aes(x = Landing.Month, y = m, group = Landing.Year, color = Landing.Year)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ Landing.Year, ncol = 2) +
  labs(x = "Landing Month", 
       y = "Total Lbs Unsold Tuna", 
       title = "Unsold Tuna by Month and Year") 

df1 <- tuna_dat %>%
  group_by(Landing.Year) %>%
  summarize(Tot.unsold = length(Tag))

tuna_dat %>% 
  group_by(Landing.Year) %>% 
  summarize(Tot.unsold = length(Tag)) %>% 
  ggplot(aes(x = Landing.Year, y = Tot.unsold, group = Landing.Year)) +
  scale_x_continuous(breaks = 2017:2024, labels = 2017:2024) +
  labs(x = "Landing Year",
       y = "Number of Unsold Tuna",
       title = "Unsold Tuna by Year") +
  coord_cartesian(ylim = c(0, 150), expand = FALSE) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  geom_col(fill = "#00467F") +
  geom_label(label = df1$Tot.unsold) 
 
tuna_dat$Port.Name

tuna_dat$State <- substr(tuna_dat$Port.Name, nchar(tuna_dat$Port.Name)-1, nchar(tuna_dat$Port.Name))
head(tuna_dat)

tuna_dat %>% 
  group_by(State, Landing.Year) %>% 
  summarize(Tot.unsold = length(Tag)) %>% 
  ggplot(aes(x = State, y = Tot.unsold, group = Landing.Year, color = Landing.Year)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ Landing.Year, ncol = 2) +
  labs(x = "Landing Month", 
       y = "Total Lbs Unsold Tuna", 
       title = "Unsold Tuna by Month and Year") 

tuna_states <- tuna_dat[c('Reported.Quantity', 'Landing.Year', 'State')]
tuna_states

noaacolors <- c("#00467F", "#BC4700", "#4C9C2E", "#1ECAD3", "#FF4438", "#0093D0",
                "#93D500", "#575195", "#FF8300", "#007934") 

ggplot(tuna_states, aes(fill=State, y=Reported.Quantity, x=Landing.Year)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = noaacolors) +
  scale_x_continuous(breaks = 2017:2024, labels = 2017:2024) +
  labs(x = "Landing Year",
       y = "Lbs Unsold Tuna",
       title = "Unsold Tuna by State") +
  coord_cartesian(ylim = c(0, 60000), expand = FALSE) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

tuna_states %>% 
  group_by(Landing.Year, State) %>% 
  reframe(m = sum(Reported.Quantity)/df$Tot.unsold) %>%
  ggplot(aes(x = State, y = m, group = Landing.Year, color = Landing.Year)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ Landing.Year, ncol = 2) +
  labs(x = "Landing Month", 
       y = "Percent Unsold Tuna", 
       title = "Percent Unsold Tuna by State and Year") 

tuna_cat <- tuna_dat[c('Category', 'Landing.Year', 'Reported.Quantity')]
tuna_cat$Category[tuna_cat$Category==""] <- 'Unknown'

noaacols <- c("#00467F", "#1ECAD3", "#4C9C2E", "#0093D0") 

ggplot(tuna_cat, aes(fill=Category, y=Reported.Quantity, x=Landing.Year)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = noaacols) +
  scale_x_continuous(breaks = 2017:2024, labels = 2017:2024) +
  labs(x = "Landing Year",
       y = "Lbs Unsold Tuna",
       title = "Unsold Tuna by Permit") +
  coord_cartesian(ylim = c(0, 60000), expand = FALSE) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())