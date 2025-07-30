# Courtney & Rice Meta-Analysis, ABC & OFL Calculations
library(tidyverse)

# Provided in Courtney & Rice
sigma.min = 0.4151
# To align with the hypothetical OFL given in the paper
scalar = 5000

x <- seq(-10, 10, by = 0.001)

# Generating lognormal distributions based on sigma.min
y1 <- dlnorm(x, meanlog = 0, sdlog = sigma.min)
y2 <- dlnorm(x, meanlog = 0, sdlog = sigma.min*1.5)
y3 <- dlnorm(x, meanlog = 0, sdlog = sigma.min*2.0)
y4 <- dlnorm(x, meanlog = 0, sdlog = sigma.min*4.0)

# combining data
data <- data.frame(x, y1, y2, y3, y4)

# Plotting distributions
data %>%
  rename(Tier1=y1,Tier2=y2,Tier3=y3,Tier4=y4) %>%
  pivot_longer(Tier1:Tier4) %>%
  ggplot(aes(x, value, group = name)) +
  geom_line(aes(col = name)) + 
  coord_cartesian(xlim = c(0, 2), ylim = c(0, 1.25), expand = FALSE) +
  geom_vline(xintercept = 1) +
  theme_bw() + theme(panel.grid = element_blank())

# Calculating OFL/ABC Buffer
p.threshold = 0.3

# Calculating using the cumulative distribution
data <- data %>%
  mutate(cumd1 = cumsum(y1),
         cumd2 = cumsum(y2),
         cumd3 = cumsum(y3),
         cumd4 = cumsum(y4)) %>%
  mutate(cump1 = cumd1/sum(data$y1),
         cump2 = cumd2/sum(data$y2),
         cump3 = cumd3/sum(data$y3),
         cump4 = cumd4/sum(data$y4))

data$x[min(which(data$cump1 >= p.threshold))] # 0.804
data$x[min(which(data$cump2 >= p.threshold))] # 0.721
data$x[min(which(data$cump3 >= p.threshold))] # 0.646
data$x[min(which(data$cump4 >= p.threshold))] # 0.371

# Calculating using qlnorm
c1 <- qlnorm(p.threshold, 0, sigma.min * 1.0)  # 80.4%
c2 <- qlnorm(p.threshold, 0, sigma.min * 1.5)  # 72.2%
c3 <- qlnorm(p.threshold, 0, sigma.min * 2.0)  # 64.7%
c4 <- qlnorm(p.threshold, 0, sigma.min * 4.0)  # 41.9%

# Figure 13, Courtney & Rice ----
data %>%
  rename(Tier_1=y1,Tier_2=y2,Tier_3=y3,Tier_4=y4) %>%
  pivot_longer(Tier_1:Tier_4, names_to = "Tier") %>%
  ggplot(aes(x*scalar, value, group = Tier)) +
  geom_line(data = .%>% filter(Tier != "Tier_4"), mapping = aes(col = Tier)) + 
  geom_line(data = .%>% filter(Tier == "Tier_4"), mapping = aes(col = Tier), alpha = 0.25) +
  coord_cartesian(xlim = c(0, 2)*scalar, ylim = c(0, 1.25), expand = FALSE) +
  geom_vline(xintercept = 1*scalar) +
  geom_vline(xintercept = c(c1,c2,c3)*scalar, col = c("darkred","forestgreen","cyan3"), lty = "dashed") +
  scale_x_continuous(breaks = c(0.4, 0.8, 1.2, 1.6)*scalar, labels = c(0.4,0.8,1.2,1.6)*scalar) +
  xlab("Catch") + ylab("") +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


sigma.min      # T1 - 0.4151 
sigma.min*1.5  # T2 - 0.6227
sigma.min*2.0  # T3 - 0.8302
sigma.min*4.0  # T4 - 1.6604


# Calculating the ABC/OFL ratio at each P* level
p.seq = seq(0.05, 0.5, by = 0.05)
qdata <- data.frame(p = p.seq, T1=NA, T2=NA, T3=NA, T4=NA)
for (i in 1:length(p.seq)) {
  print(p.seq[i])
  qdata$T1[i] <- qlnorm(p.seq[i], meanlog = 0, sdlog = sigma.min * 1.0)
  qdata$T2[i] <- qlnorm(p.seq[i], meanlog = 0, sdlog = sigma.min * 1.5)
  qdata$T3[i] <- qlnorm(p.seq[i], meanlog = 0, sdlog = sigma.min * 2.0)
  qdata$T4[i] <- qlnorm(p.seq[i], meanlog = 0, sdlog = sigma.min * 4.0)
}

# Figure 12, Courtney & Rice ----
qdata %>%
  pivot_longer(T1:T4, names_to = "TIER") %>%
  ggplot(aes(p, value, group = TIER)) +
  geom_line(aes(col = TIER, linetype = TIER)) +
  geom_point() + 
  xlab("P* (Probability of Overfishing") + ylab("ABC/OFL") +
  theme_bw() + theme(panel.grid = element_line(colour = "gray95"))
