# Load Packages & Dataset
library(ggplot2)
library(tidyverse)
mrktval <- read.csv("~/Desktop/Market Value of 25 players Before v. After.csv")
View(mrktval)

# Assign Variables
age <- mrktval$Age
before <- mrktval$Before.Market.Value
after <-mrktval$After.Market.Val
change <- mrktval$Difference

# General Graphs
mrktval %>% ggplot(aes(x = age, y = after)) +
  geom_jitter() +
  theme_minimal() +
  labs(x="Age", y="Post-World Cup Market Value", title = "Age vs. Market Value for the Top 25 World Cup Players")

mrktval %>% ggplot(aes(x = change, y = after)) +
  geom_jitter() +
  theme_minimal() +
  labs(x="Change in Market Value over the World Cup", y="Post-World Cup Market Value", title = "Change in Market Value vs. Current Value for the top 25 World Cup Players")

# Tests for Normality
qqnorm(before)
qqline(before)

qqnorm(after)
qqline(after)

shapiro.test(before)
shapiro.test(after)
shapiro.test(change)

# Hypothesis Test
t.test(before,after, paired = TRUE, alternative = "less")

# Wilcox Test
wilcox.test(x = change,
            alternative = "two.sided",
            mu = 0, paired = F,  
            conf.int = T, conf.level = 0.95)
