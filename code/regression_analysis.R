# Regression analysis

# Understanding the drivers of graduate retention across English local authorities

# Author: Will Shepherd

library(tidyverse)
library(ggplot2)
library(janitor)
library(readxl)
library(dplyr)
library(openxlsx)
library(here)
library(knitr)

# Read in the LA cross section table
la_data <- read_csv(here("data","la_cross_section.csv"))

# First basic model

model1 <- lm(graduate_retention_rate ~ median_weekly_pay_2018 + median_house_price_2018 + 
               l4_plus_share_2011 + la_imd_decile + arts_council_per_head_201819 + jobs_density_2018 + gva_2018, data=la_data)

summary(model1)

