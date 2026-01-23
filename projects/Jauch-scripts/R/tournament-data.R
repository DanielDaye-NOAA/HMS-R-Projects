# This was a comparison of two tournament datasets downloaded from ATR
# BFT tournament data 2004 - 2024

library(tidyverse)
library(dplyr)


# Read in bft tournament catch details
deets <- read.csv("C:/Users/rebecca.jauch/Desktop/Tuna/Catch_Details_2004_Present.csv")

# Read in bft tournament catches by state
states <- read.csv("C:/Users/rebecca.jauch/Desktop/Tuna/Catch_by_State_2004_Present.csv")

# Group the state data by year and state and plot 
states %>% 
  group_by(Tournament.Year, State) %>%  # summarize(Fish = sum(Total.Fish)) %>% 
  ggplot(aes(x = Tournament.Year, y = Total.Fish, color = State)) +
  scale_x_continuous(breaks = 2004:2024, labels = 2004:2024) +
  labs(x = "Tournament Year",
       y = "Number of Tuna",
       title = "Tournament Tuna by Year") +
  coord_cartesian(ylim = c(0, 800), expand = FALSE) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  geom_col()

states %>% 
  group_by(Tournament.Year, State) %>%  # reframe(m = sum(Reported.Quantity)/df$Tot.unsold) %>%
  ggplot(aes(x = State, y = Total.Fish, group = Tournament.Year, color = Tournament.Year)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ Tournament.Year, ncol = 3) +
  labs(x = "State", 
       y = "Number Tuna", 
       title = "Tournament Tuna by State and Year") 

noaacolors <- c("#00467F", "#007934", "#1ECAD3", "#FF4438", "#4C9C2E", "#0093D0",
                "#93D500", "#7F7FFF", "#FF8300", "#BC4700", "#575195", "#008998",
                "#B2292E", "#646464") 

ggplot(states, aes(fill=State, y=Total.Fish, x=Tournament.Year)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = noaacolors) +
  scale_x_continuous(breaks = 2004:2024, labels = 2004:2024) +
  labs(x = "Tournament Year",
       y = "Number Tuna",
       title = "Tournament Tuna by State") +
  coord_cartesian(ylim = c(0, 900), expand = FALSE) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

df <- deets %>%
  group_by(Year, State) %>%
  summarize(Tot.Fish = sum(Kept, Released))

ggplot(df, aes(fill=State, y=Tot.Fish, x=Year)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = noaacolors) +
  scale_x_continuous(breaks = 2004:2024, labels = 2004:2024) +
  labs(x = "Tournament Year",
       y = "Number Tuna",
       title = "Tournament Tuna by State") +
  coord_cartesian(ylim = c(0, 900), expand = FALSE) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

new_df <- data.frame(df$Year, df$State, states$State, df$Tot.Fish, states$Total.Fish)

# States df has a few more fish than the deets df 

num_tourn <- deets %>% 
  group_by(Year, State) %>% 
  summarize(Tot.Tourneys = length(State))

print(num_tourn, n=99)

df2 <- deets %>%
  group_by(Year, State) %>% 
  summarize(Tot.Tourneys = length(State))%>%
  summarize(Tot.Year.Tourneys = sum(Tot.Tourneys))

totals <- left_join(df2, num_tourn,  by = 'Year')

df3 <- deets %>%
  group_by(Year, State) %>%
  summarize(Tot.Fish = sum(Kept, Released))%>%
  summarize(Tot.Year.Fish = sum(Tot.Fish))

total_fish <- left_join(df3, num_tourn, by = 'Year')

ggplot(num_tourn, aes(fill=State, y=Tot.Tourneys, x=Year)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = noaacolors) +
  scale_x_continuous(breaks = 2004:2024, labels = 2004:2024) +
  labs(x = "Tournament Year",
       y = "Number Tournaments",
       title = "Tournaments by State") +
  coord_cartesian(ylim = c(0, 20), expand = FALSE) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

df1 <- data.frame('Year' = df$Year, 'State' = df$State, 'Num.Fish' = df$Tot.Fish, 
                  'Tot.Fish' = total_fish$Tot.Year.Fish, 
                  'Percent.Fish' =  round(df$Tot.Fish/total_fish$Tot.Year.Fish, digits = 2),
                  'Num.Tourneys' = num_tourn$Tot.Tourneys, 'Tot.Tourns'= totals$Tot.Year.Tourneys,
                  'Percent.Tourneys' = round(num_tourn$Tot.Tourneys/totals$Tot.Year.Tourneys, digits=2))

# Stacked bar chart of percentage of total tournament bft caught by state
ggplot(df1, aes(fill=State, y=Percent.Fish, x=Year)) + 
  # geom_line(size=1.25) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = noaacolors) +
  scale_x_continuous(breaks = 2004:2024, labels = 2004:2024) +
  labs(x = "Tournament Year",
       y = "Percent Fish Caught",
       title = "Percentage of Tournament Fish by State") +
  coord_cartesian(ylim = c(0, 1), expand = FALSE) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) 

# Stacked bar chart of percentage of total tournaments by state
ggplot(df1, aes(fill=State, y=Percent.Tourneys, x=Year)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = noaacolors) +
  scale_x_continuous(breaks = 2004:2024, labels = 2004:2024) +
  labs(x = "Tournament Year",
       y = "Percent Total Tourneys",
       title = "Percentage of Tournaments by State") +
  coord_cartesian(ylim = c(0, 1), expand = FALSE) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())
