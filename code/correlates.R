# CORRELATES

# Exploring correlations between variables to help with model selection

# Author: Will Shepherd

library(tidyverse)
library(ggplot2)
library(janitor)
library(readxl)
library(dplyr)
library(openxlsx)
library(here)

# Read in the LA cross section table

la_data <- read_csv(here("data","la_cross_section.csv"))

#-----------------------------------------
# First let's look at variation in the graduate retention rate
#-----------------------------------------
summary(la_data$graduate_retention_rate)

ggplot(la_data, aes(x=graduate_retention_rate)) +
  geom_histogram()

# Rural - Urban differences

la_data %>% group_by(Broad_RUC11) %>% summarize(mean(graduate_retention_rate))

ggplot(la_data, aes(x=graduate_retention_rate, y=RUC11)) +
  geom_point()

# Median weekly pay

ggplot(la_data, aes(x=graduate_retention_rate,y=median_weekly_pay_2018)) +
  geom_point() +
  geom_smooth(method='lm',formula=y~x)