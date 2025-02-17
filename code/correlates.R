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
library(knitr)
library(ggrepel)

# Read in the LA cross section table
la_data <- read_csv(here("data","la_cross_section.csv"))

#-----------------------------------------
# First let's look at variation in the graduate retention rate
#-----------------------------------------
summary(la_data$graduate_retention_rate)

ggplot(la_data, aes(x=graduate_retention_rate)) +
  geom_histogram()

#-----------------------------------------
# Explanatory variable correlations
#---------------------------------------

# Multicorrelation matrix
selected_la_df <- la_data %>% select(median_weekly_pay_2018, median_house_price_2018, l4_plus_share_2011,
                                     enterprise_count_2018, la_imd_decile, arts_council_per_head_201819, jobs_density_2018,
                                     gva_2018)

multicorrelation_matrix <- cor(selected_la_df, use = "complete.obs")
write.csv(multicorrelation_matrix, here("results","correlation_matrix.csv"))

# Rural - Urban differences
la_data %>% group_by(Broad_RUC11) %>% summarize(mean(graduate_retention_rate))

ggplot(la_data, aes(x=graduate_retention_rate, y=RUC11)) +
  geom_point()

# Median weekly pay
la_data %>% select(graduate_retention_rate, median_weekly_pay_2018) %>% cor(use='complete.obs')

ggplot(la_data, aes(x=graduate_retention_rate,y=median_weekly_pay_2018, label=lad11nm)) +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) +
  geom_text_repel(aes(label=ifelse(graduate_retention_rate>0.7,as.character(lad11nm),'')),
            size=2) 
# Median house prices
la_data %>% select(graduate_retention_rate, median_house_price_2018) %>% cor(use='complete.obs')

ggplot(la_data, aes(x=graduate_retention_rate,y=median_house_price_2018, label=lad11nm)) +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) +
  geom_text_repel(aes(label=ifelse(graduate_retention_rate>0.7,as.character(lad11nm),'')),
                  size=2) 

# House prices excluding London
london <- c("Inner London BUA","Outer London BUA")
la_data_excluding_london <- la_data %>% filter(!lad11nm %in% london)

la_data_excluding_london %>% select(graduate_retention_rate, median_house_price_2018) %>% cor(use='complete.obs')

ggplot(la_data_excluding_london, aes(x=graduate_retention_rate,y=median_house_price_2018, label=lad11nm)) +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) +
  geom_text_repel(aes(label=ifelse(graduate_retention_rate>0.7,as.character(lad11nm),'')),
                  size=2) 

# Level 4 plus share
la_data %>% select(graduate_retention_rate, l4_plus_share_2011) %>% cor(use='complete.obs')

ggplot(la_data, aes(x=graduate_retention_rate,y=l4_plus_share_2011, label=lad11nm)) +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) +
  geom_text_repel(aes(label=ifelse(graduate_retention_rate>0.7,as.character(lad11nm),'')),
                  size=2) 

#Enterprise count

la_data %>% select(graduate_retention_rate, enterprise_count_2018) %>% cor(use='complete.obs')

ggplot(la_data, aes(x=graduate_retention_rate,y=enterprise_count_2018, label=lad11nm)) +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) +
  geom_text_repel(aes(label=ifelse(graduate_retention_rate>0.7,as.character(lad11nm),'')),
                  size=2) 

# IMD
la_data %>% group_by(la_imd_decile) %>% summarize(mean(graduate_retention_rate))

ggplot(la_data, aes(x=graduate_retention_rate, y=la_imd_decile)) +
  geom_point()

# Arts council funding
la_data %>% select(graduate_retention_rate, arts_council_per_head_201819) %>% cor(use='complete.obs')

ggplot(la_data, aes(x=graduate_retention_rate,y=arts_council_per_head_201819, label=lad11nm)) +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) +
  geom_text_repel(aes(label=ifelse(arts_council_per_head_201819>2500,as.character(lad11nm),'')),
                  size=2) 

# jobs density
la_data %>% select(graduate_retention_rate, jobs_density_2018) %>% cor(use='complete.obs')

ggplot(la_data, aes(x=graduate_retention_rate,y=jobs_density_2018, label=lad11nm)) +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) +
  geom_text_repel(aes(label=ifelse(jobs_density_2018>1.2,as.character(lad11nm),'')),
                  size=2)

# GVA
la_data %>% select(graduate_retention_rate, gva_2018) %>% cor(use='complete.obs')

ggplot(la_data, aes(x=graduate_retention_rate,y=gva_2018, label=lad11nm)) +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) +
  geom_text_repel(aes(label=ifelse(gva_2018>45,as.character(lad11nm),'')),
                  size=2)

