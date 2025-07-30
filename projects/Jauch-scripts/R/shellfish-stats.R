# Script for analyzing shellfish data for seafood lab, R. Jauch Sept 2024#

# these are packages that I called in case I need them
library(tidyverse)
library(chemCal)

# I saved 'results' tab as a csv named 'shellfish' on my desktop
shellfish <- read.csv("C:/Users/rebecca.jauch/Desktop/Shellfish_full.csv", header=T)

# This lets you look at the first 6 entries 
head(shellfish)
tail(shellfish)
# This tells you the names of the columns
colnames(shellfish)

#' This code takes the logs of your data and puts them into columns the name on the left is the new
#' column name for the log data allowing R to calculate the logs ensures we are using the most accurate
#' numbers
shellfish$TLH.log <- log10(shellfish$TLH)
shellfish$TRH.log <- log10(shellfish$TRH)
shellfish$TDH.log <- log10(shellfish$TDH)
shellfish$VVH.log <- log10(shellfish$VVH)


shellfish$VP.log <- na.omit(rep(log10(shellfish$VP_CFU.g), each=3))
shellfish$VV.log <- na.omit(rep(log10(shellfish$VV_CFU.g), each=3))

#' Since I really am most interested in comparing the averaged samples and the log plate counts I made
#' a new dataframe using just those columns the na.omit removes the NAs from your original data sheet.
#' The NAs were a result of fewer data points in those columns since they were averages of the other
#' columns' data
cleanshell <- na.omit(subset(shellfish, select = c(Sample, Month, VP, VV)))
cleanshell$VP.log <- log10(cleanshell$VP)
cleanshell$VV.log <- log10(cleanshell$VV)


cleanshell$TLH.m <- c(mean(shellfish$TLH.log[1:3]), mean(shellfish$TLH.log[4:6]), 
                      mean(shellfish$TLH.log[7:9]), mean(shellfish$TLH.log[10:12]), 
                      mean(shellfish$TLH.log[13:15]), mean(shellfish$TLH.log[16:18]), 
                      mean(shellfish$TLH.log[19:21]), mean(shellfish$TLH.log[22:24]),
                      mean(shellfish$TLH.log[25:27]), mean(shellfish$TLH.log[28:30]), 
                      mean(shellfish$TLH.log[31:33]), mean(shellfish$TLH.log[34:36]), 
                      mean(shellfish$TLH.log[37:39]), mean(shellfish$TLH.log[40:42]),
                      mean(shellfish$TLH.log[43:45]))

cleanshell$TRH.m <- c(mean(shellfish$TRH.log[1:3]), mean(shellfish$TRH.log[4:6]), 
                      mean(shellfish$TRH.log[7:9]), mean(shellfish$TRH.log[10:12]), 
                      mean(shellfish$TRH.log[13:15]), mean(shellfish$TRH.log[16:18]), 
                      mean(shellfish$TRH.log[19:21]), mean(shellfish$TRH.log[22:24]),
                      mean(shellfish$TRH.log[25:27]), mean(shellfish$TRH.log[28:30]), 
                      mean(shellfish$TRH.log[31:33]), mean(shellfish$TRH.log[34:36]), 
                      mean(shellfish$TRH.log[37:39]), mean(shellfish$TRH.log[40:42]),
                      mean(shellfish$TRH.log[43:45]))

cleanshell$TDH.m <- c(mean(shellfish$TDH.log[1:3]), mean(shellfish$TDH.log[4:6]), 
                      mean(shellfish$TDH.log[7:9]), mean(shellfish$TDH.log[10:12]), 
                      mean(shellfish$TDH.log[13:15]), mean(shellfish$TDH.log[16:18]), 
                      mean(shellfish$TDH.log[19:21]), mean(shellfish$TDH.log[22:24]),
                      mean(shellfish$TDH.log[25:27]), mean(shellfish$TDH.log[28:30]), 
                      mean(shellfish$TDH.log[31:33]), mean(shellfish$TDH.log[34:36]), 
                      mean(shellfish$TDH.log[37:39]), mean(shellfish$TDH.log[40:42]),
                      mean(shellfish$TDH.log[43:45]))

cleanshell$VVH.m <- c(mean(shellfish$VVH.log[1:3]), mean(shellfish$VVH.log[4:6]), 
                      mean(shellfish$VVH.log[7:9]), mean(shellfish$VVH.log[10:12]), 
                      mean(shellfish$VVH.log[13:15]), mean(shellfish$VVH.log[16:18]), 
                      mean(shellfish$VVH.log[19:21]), mean(shellfish$VVH.log[22:24]),
                      mean(shellfish$VVH.log[25:27]), mean(shellfish$VVH.log[28:30]), 
                      mean(shellfish$VVH.log[31:33]), mean(shellfish$VVH.log[34:36]), 
                      mean(shellfish$VVH.log[37:39]), mean(shellfish$VVH.log[40:42]),
                      mean(shellfish$VVH.log[43:45]))


head(cleanshell)

# To accurately work with the dilution factors, I created a column with the dilution amounts
# you can see I named the column 'dil' as in 'dilution'
cleanshell$dil <- as.factor(c(0.1, 0.01, 0.0001, 0.00001, 0.000001, 0.1, 0.01, 0.0001, 0.00001, 0.000001,
                              0.1, 0.01, 0.0001, 0.00001, 0.000001))
cleanshell$Sample <- as.factor(cleanshell$Sample)
cleanshell$Month <- as.factor(cleanshell$Month)
# If you run this code you will see the columns are now tlh.m, trh.m, tdh.m, vvh.m, VP.log, 
# VV.log, and dil
head(cleanshell)

?confint()
cleanshell$VV.log
# To start working with the data I ran a linear regression with the tlh on the y-axis and VP plate 
# counts on the x-axis
# TLH
m <- lm(TLH.m ~ VP.log, data = cleanshell)

# This is how to plot the data with confidence and prediction bands
calplot(m)

# This is how to get the linear regression summary information
summary(m) 

# This pulls out the coefficients which you need for your y=mx+b equation
m$coefficients
# intercept is 0.1174725


# To find the antilog of the intercept, plug the intercept into 10^(x)
10^(0.1239645)


# To find the 95% confidence interval for the intercept
confint(m, level = 0.95)
# (Intercept) -0.4020136 0.6369586

# To find pearson's coefficient as well as its p-value
cor.test(cleanshell$VP.log, cleanshell$TLH.m, method = 'pearson')
# 0.9545384



# When working with linear regression it's a good idea to check to see if your data is normally
# distributed - so I ran a shapiro test on both the VP and VV plate count data - both are normal
shapiro.test(cleanshell$VP.log) #p-val is 0.1405 (null hypothesis is that data is normally distributed)
shapiro.test(cleanshell$VV.log) #p-val is 0.3079 (normally distributed)


# I then repeated the analysis steps with each of the other samples
# VVH
mv <- lm(VVH.m ~ VV.log, data = cleanshell)

summary(mv)

calplot(mv)

mv$coefficients
# intercept is 0.5597829

10^(0.5597829)
# antilog is 3.628966 

confint(mv, level = .95)
# (Intercept)  -0.1178223 1.237388

cor.test(cleanshell$VV.log, cleanshell$VVH.m, method = 'pearson')
# 0.928116



# TRH
mr <- lm(TRH.m ~ VP.log, data = cleanshell)
anova(mr)
calplot(mr)

summary(mr)

mr$coefficients
# intercept is -0.005401756

10^(-0.005401756)
# antilog is 0.987639

confint(mr, level = .95)
# (Intercept) -0.4778989 0.4670954

cor.test(cleanshell$VP.log, cleanshell$TRH.m, method = 'pearson')
# 0.9731793

cleanshell

# TDH
md <- lm(TDH.m ~ VP.log, data = cleanshell)

calplot(md)

summary(md)

md$coefficients
# intercept is 0.0971992

10^(0.0971992)
# antilog is 1.250833

confint(md, level = 0.95)
# (Intercept) -0.3334648 0.5278632

cor.test(cleanshell$VP.log, cleanshell$TDH.m, method = 'pearson')
# 0.978805


# to perform a T Test at the 95% confidence interval, you can run this code
t.test(cleanshell$VP.log, cleanshell$TLH.m, conf.level = 0.95)
t.test(cleanshell$VP.log, cleanshell$TRH.m, conf.level = 0.95)
t.test(cleanshell$VP.log, cleanshell$TDH.m, conf.level = 0.95)
t.test(cleanshell$VV.log, cleanshell$VVH.m, conf.level = 0.95)



# to run ANOVA, you can run this code on whichever variables you want to analyze 
rec <- lm(cleanshell$TRH.m ~ cleanshell$VP.log)
anova(rec)
summary(rec)

rec.aov <- aov(cleanshell$TRH.m ~ cleanshell$VP.log)
summary(rec.aov)
TukeyHSD(rec.aov)
pairwise.t.test(cleanshell$TRH.m, cleanshell$VP.log, p.adjust.method='BH')
oneway.test(cleanshell$TRH.m ~ cleanshell$VP.log)
plot(rec.aov)


rec <- lm(cleanshell$TRH.m ~ cleanshell$dil)
anova(rec)

# Precision, n=# of tubes of dilution, D = log of dilution ratio
# MPN precision equation is 0.5487/n^0.5(D)
0.5487/(6^0.5)*(log10(2))
# 0.06743248

t <- lm(cleanshell$TDH.m ~ cleanshell$Month)
anova(t)
summary(t)
# same as Dan's figures on precision page
# used all samples (not averages) and repped plate counts (idk if this is correct method)
# use averages for ANOVA

# ANOVAs
tl <- lm(cleanshell$TLH.m ~ cleanshell$VP.log)
anova(tl)
summary(anova(tl))


tr <- lm(cleanshell$TRH.m ~ cleanshell$VP.log)
anova(tr)
tr$coefficients

td <- lm(cleanshell$TDH.m ~ cleanshell$VP.log)
anova(td)
td$coefficients

vv <- aov(cleanshell$VVH.m ~ cleanshell$VV.log)
anova(vv)
vv$coefficients

m <- aov(cleanshell$TLH.m ~ cleanshell$VP.log, data=cleanshell)
summary(m)
m$coefficients

head(cleanshell)

#Full Shellfish calculations ----
#Anova ----

TDH.AOV <- lm(TDH.m ~ Month, data = cleanshell)
summary(TDH.AOV)

tlh.aov <- aov(TLH.m ~ Month + dil, data = cleanshell)
summary(tlh.aov)
TukeyHSD(tlh.aov)

trh.aov <- aov(TRH.m ~ Month + dil, data = cleanshell)
summary(trh.aov)

vvh.aov <- aov(VVH.m ~ Month + dil, data = cleanshell)
summary(vvh.aov)

m <- lm(TLH.m ~ Month + dil, data = cleanshell)
summary(anova(m))

install.packages("ggpubr")
library("ggpubr")
ggboxplot(cleanshell, x = "dil", y = "TLH.m", color = "Month",
          palette = c("#4C9C2E", "#FF8300", "#1ECAD3"))
ggline(cleanshell, x = "dil", y = "TLH.m", color = "Month",
       add = c("mean_se", "dotplot"),
       palette = c("#4C9C2E", "#FF8300", "#1ECAD3"))


interaction.plot(x.factor = cleanshell$dil, trace.factor = cleanshell$Month,
                 response = cleanshell$TLH.m, fun = mean,
                 type = "b", legend = TRUE,
                 xlab = "Dilution", ylab="TLH",
                 pch=c(1,19), col = c("#4C9C2E", "#FF8300", "#1ECAD3"))

interaction.plot(x.factor = cleanshell$dil, trace.factor = cleanshell$Month,
                 response = cleanshell$TRH.m, fun = mean,
                 type = "b", legend = TRUE,
                 xlab = "Dilution", ylab="TRH",
                 pch=c(1,19), col = c("#4C9C2E", "#FF8300", "#1ECAD3"))

interaction.plot(x.factor = cleanshell$dil, trace.factor = cleanshell$Month,
                 response = cleanshell$TDH.m, fun = mean,
                 type = "b", legend = TRUE,
                 xlab = "Dilution", ylab="TDH",
                 pch=c(1,19), col = c("#4C9C2E", "#FF8300", "#1ECAD3"))

interaction.plot(x.factor = cleanshell$dil, trace.factor = cleanshell$Month,
                 response = cleanshell$VVH.m, fun = mean,
                 type = "b", legend = TRUE,
                 xlab = "Dilution", ylab="VVH",
                 pch=c(1,19), col = c("#4C9C2E", "#FF8300", "#1ECAD3"))


# Recovery
tlh.rec <- mean(cleanshell$TLH.m)
# 3.007289
vp.rec <- mean(cleanshell$VP.log)
tlh.rec/vp.rec
# 0.9589011

trh.rec <- mean(cleanshell$TRH.m)
trh.rec/vp.rec
# 0.9123105

tdh.rec <- mean(cleanshell$TDH.m)
tdh.rec/vp.rec
# 0.9722427

vvh.rec <- mean(cleanshell$VVH.m)
vv.rec <- mean(cleanshell$VV.log)
vvh.rec/vv.rec
# 1.157613