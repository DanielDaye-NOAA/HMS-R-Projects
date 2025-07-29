# Reprex for Scale Color Interpolation issues

library(tidyverse)
source("ScaleColor.R")
set.seed(100)

# Creating datasets to use as examples, # groups = # colors in palette
data1 <- data.frame(x = rnorm(60), y = rnorm(60), group = factor(sample(1:6, 60, replace = TRUE))) 
data2 <- data.frame(x = rnorm(60), y = rnorm(60), group = factor(sample(1:3, 60, replace = TRUE)))
data3 <- data.frame(x = rnorm(60), y = rnorm(60), group = factor(sample(1:7, 60, replace = TRUE))) 


# Both work properly since the number of groups = number of colors in the palette
data1 %>% mutate(group = as.numeric(group)) %>%
  ggplot(aes(x,y,color = group)) +
  geom_point() + scale_color(discrete = FALSE)
data1 %>%
  ggplot(aes(x,y,color = group)) +
  geom_point() + scale_color(discrete = TRUE)


# Number groups < number colors in palette ----

# the non-discrete version creates a 3 color palette using the extremes and interpolated midpoint 
# of the provided palette (this one doesn't use the exact palette colors)
data2 %>% mutate(group = as.numeric(group)) %>%
  ggplot(aes(x,y,color = group)) +
  geom_point() + scale_color(discrete = FALSE)
# the discrete version works properly and uses the first 3 colors of the provided palette
data2 %>%
  ggplot(aes(x,y,color = group)) +
  geom_point() + scale_color(discrete = TRUE)


# Number groups > number colors in palette ----
# non-discrete version will create a new 7 color palette that doesnt use the same colors provided
data3 %>% mutate(group = as.numeric(group)) %>%
  ggplot(aes(x,y,color = group)) +
  geom_point() + scale_color(discrete = FALSE)
# discrete version won't work since the palette has less colors than are provided for the number of groups
data3 %>%
  ggplot(aes(x,y,color = group)) +
  geom_point() + scale_color(discrete = TRUE)
