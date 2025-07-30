# Linear Regression for recreational ALRS and LPS fish, by size class

library(tidyverse)
library(readxl)

# Read in data that excludes late reports of ALRS fish
rec_bft <- read_xlsx("C:/Users/rebecca.jauch/Desktop/rec_bft.xlsx", sheet = 'Sheet3')

# Run linear regression of ALRS school size fish and LPS school size fish
mod_sch <- lm(lps_school ~ alrs_school, data=rec_bft)
summary(mod_sch)
cor(rec_bft$lps_school, rec_bft$alrs_school)

# Enter number of ALRS school size fish to predict LPS school size fish
predict(mod_sch, data.frame(alrs_school = 150), interval=c('prediction'), lvl=0.95, type="response")

# Do the same for ls/sm size fish
mod_lssm <- lm(lps_lssm ~ alrs_lssm, rec_bft)
summary(mod_lssm)
cor(rec_bft$alrs_lssm, rec_bft$lps_lssm)

# Enter number of ALRS ls/sm fish to predict LPS ls/sm fish
predict(mod_lssm, data.frame(alrs_lssm = 150), interval=c('prediction'), lvl=0.95, type="response")