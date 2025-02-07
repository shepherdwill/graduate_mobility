# This script links together all local authority data sources to create a cross section

# Data sources:
#   - Graduate retention rate (geographical mobility ONS)
#   - Rural urban classification
#   - Average earnings
#   - Highest level of qualification share
#   - IMD
#   - Median house prices


# Author: Will Shepherd

library(tidyverse)
library(ggplot2)
library(janitor)
library(readxl)
library(dplyr)
library(openxlsx)
library(here)

#Define inner and outer london
inner_london <- c("Camden","Greenwich","Hackney","Hammersmith and Fulham","Islington","Kensington and Chelsea",
                  "Lambeth","Lewisham","Southwark","Tower Hamlets","Wandsworth","Westminster","City of London")

outer_london <- c("Barking and Dagenham","Barnet","Bexley","Brent","Bromley","Croydon","Ealing","Enfield","Haringey",
                  "Harrow","Havering","Hillingdon","Hounslow","Kingston upon Thames","Merton","Newham","Redbridge",
                  "Richmond upon Thames","Sutton","Waltham Forest")

oa_bua_lad <- read_csv(here("data","OA(2011)_to_BUAsubdivision_to_BUA_to_LAD_to_Region_(December_2011)_Lookup_in_England_and_Wales.csv"))
oa_bua_lad <- clean_names(oa_bua_lad)

oa_bua_lad %>% filter(lad11nm %in% inner_london) %>% select(lad11cd) %>% distinct()
oa_bua_lad %>% filter(lad11nm %in% outer_london) %>% select(lad11cd) %>% distinct()

inner_london_codes <- c("E09000001","E09000007","E09000011","E09000012","E09000013","E09000019","E09000020",
                        "E09000022","E09000023","E09000028","E09000030","E09000032","E09000033")

outer_london_codes <- c("E09000002","E09000003","E09000004","E09000005","E09000006","E09000008","E09000009",
                        "E09000010","E09000014","E09000015","E09000016","E09000017","E09000018","E09000021",
                        "E09000024","E09000025","E09000026","E09000027","E09000029","E09000031")
#----------------------------------------------
# Read in the core graduate retention dataset, 294 local authorities
graduate_retention <- read_csv(here("data","la_graduate_retention.csv"))

#---------------------------------------------
# Read in rural urban classification
ruc <- read_csv(here("data","RUC_LAD_2011_EN_LU.csv"))

ruc <- ruc %>% select("LAD11CD", "RUC11", "Broad RUC11") %>%
               rename(lad11cd = LAD11CD,
                      Broad_RUC11 = 'Broad RUC11')

#33/34 LA's present in RUC but not main table are in London
ruc_mismatch_lad <- ruc %>% anti_join(graduate_retention, by='lad11cd')

# Join RUC to graduate retention to begin creation of LA cross-section
la_cross_section <- graduate_retention %>% left_join(ruc, by='lad11cd')

# Add value for Inner and Outer London
la_cross_section <- la_cross_section %>%
  mutate(RUC11 = case_when(
    lad11nm == "Inner London BUA" ~ "Urban with Major Conurbation",
    lad11nm == "Outer London BUA" ~ "Urban with Major Conurbation",
    TRUE ~ RUC11),
        Broad_RUC11 = case_when(
    lad11nm == "Inner London BUA" ~ "Predominantly Urban",
    lad11nm == "Outer London BUA" ~ "Predominantly Urban",
    TRUE ~ Broad_RUC11))

#-----------------------------------------------
# Read in LA population estimates

la_population <- read_xls(here("data","la_population_2011_census.xls"), sheet=2,skip=9) %>%
  select(1,6) %>% filter(rowSums(is.na(.)) != ncol(.)) %>% slice(-1) %>%
  rename(lad11cd = 1,
         la_population_2011 = 2)

# Aggregate to Inner and Outer London
la_population <- la_population %>% mutate(london_flag = case_when(
  lad11cd %in% inner_london_codes ~ "Inner London BUA",
  lad11cd %in% outer_london_codes ~ "Outer London BUA",
  TRUE ~ 'Other'
))

london_pop_share <- la_population %>% filter(london_flag != 'Other') %>%
  mutate(la_population_2011 = as.double(la_population_2011)) %>%
  group_by(london_flag) %>% summarize(la_population_2011 = sum(la_population_2011)) %>%
  rename(lad11cd = london_flag)

la_population <- la_population %>% filter(london_flag == "Other") %>% select(-london_flag) %>%
  rbind(london_pop_share)

la_population <- la_population %>% mutate(la_population_2011 = as.numeric(la_population_2011))

# Join with la cross section
la_cross_section <- la_cross_section %>% left_join(la_population, by='lad11cd')

la_cross_section %>% filter(is.na(la_population_2011)) %>% summarize(n=n())

#---------------------------------------------------
# Read in gross weekly pay
weekly_pay <- read_xlsx(here("data","gross_weekly_pay_nomis.xlsx"), skip=7)

# 386 local authorities
weekly_pay <- weekly_pay %>% select('local authority: district / unitary (prior to April 2015)','2018') %>%
  slice(-1) %>% rename(lad11nm = 'local authority: district / unitary (prior to April 2015)',
                       median_weekly_pay_2018 = '2018')

# Which LA don't match with grad retention dataset 95
pay_mismatch_lad <- weekly_pay %>% anti_join(graduate_retention, by='lad11nm')

#Take average of weekly pay for Inner and Outer London and join back on
weekly_pay <- weekly_pay %>% mutate(london_flag = case_when(
  lad11nm %in% inner_london ~ "Inner London BUA",
  lad11nm %in% outer_london ~ "Outer London BUA",
  TRUE ~ 'Other'
))

london_weekly_pay <- weekly_pay %>% filter(london_flag != 'Other') %>%
  mutate(median_weekly_pay_2018 = as.double(median_weekly_pay_2018)) %>%
  group_by(london_flag) %>% summarize(median_weekly_pay_2018 = median(median_weekly_pay_2018)) %>%
  rename(lad11nm = london_flag)

weekly_pay <- weekly_pay %>% filter(london_flag == "Other") %>% select(-london_flag) %>%
                             rbind(london_weekly_pay)

# Join onto the LA cross section
la_cross_section <- la_cross_section %>% left_join(weekly_pay, by='lad11nm')

la_cross_section %>% filter(is.na(median_weekly_pay_2018)) %>% summarize(n=n()) #1 sHEPWAY

#-----------------------------------------------------------------------
#Read in house prices - 348 local authorities

house_prices <- read_xls(here("data","hpssadataset9medianpricepaidforadministrativegeographies.xls"),sheet=10,skip=6)

house_prices <- house_prices %>% select('Local authority code','Local authority name','Year ending Mar 2018') %>%
  rename(lad11cd = 'Local authority code',
         lad11nm = 'Local authority name',
         median_house_price_2018 = 'Year ending Mar 2018')

# Which LA don't match? 57
hp_mismatch_la <- house_prices %>% anti_join(graduate_retention, by='lad11nm')

# Aggregate to Inner and Outer London
house_prices <- house_prices %>% mutate(london_flag = case_when(
  lad11nm %in% inner_london ~ "Inner London BUA",
  lad11nm %in% outer_london ~ "Outer London BUA",
  TRUE ~ 'Other'
))

london_house_prices <- house_prices %>% filter(london_flag != 'Other') %>%
  mutate(median_weekly_pay_2018 = as.double(median_house_price_2018)) %>%
  group_by(london_flag) %>% summarize(median_house_price_2018 = median(median_house_price_2018)) %>%
  rename(lad11nm = london_flag)

house_prices <- house_prices %>% filter(london_flag == "Other") %>% select(-london_flag,-lad11cd) %>%
  rbind(london_house_prices)

# Join onto la cross section
la_cross_section <- la_cross_section %>% left_join(house_prices, by='lad11nm')

la_cross_section %>% filter(is.na(median_house_price_2018)) %>% summarize(n=n()) #1 Shepway

#------------------------------------------------
# Read in qualification share from census 2011 - 350 local authorities
qual_share <- read_xlsx(here("data","qualifications_census2011.xlsx"), skip=8)

qual_share <- qual_share %>% slice(-1) %>% select(1,5) %>%
  rename(lad11nm = 1,
         l4_plus_share_2011 = 2)

# How many local authorities don't match 59
qual_mismatch_la <- qual_share %>% anti_join(graduate_retention, by='lad11nm')

# Aggregate to Inner and Outer London
qual_share <- qual_share %>% mutate(london_flag = case_when(
  lad11nm %in% inner_london ~ "Inner London BUA",
  lad11nm %in% outer_london ~ "Outer London BUA",
  TRUE ~ 'Other'
))

london_qual_share <- qual_share %>% filter(london_flag != 'Other') %>%
  mutate(l4_plus_share_2011 = as.double(l4_plus_share_2011)) %>%
  group_by(london_flag) %>% summarize(l4_plus_share_2011 = round(mean(l4_plus_share_2011),2)) %>%
  rename(lad11nm = london_flag)

qual_share <- qual_share %>% filter(london_flag == "Other") %>% select(-london_flag) %>%
  rbind(london_qual_share)

# Join onto la cross section
la_cross_section <- la_cross_section %>% left_join(qual_share, by='lad11nm')

la_cross_section %>% filter(is.na(l4_plus_share_2011)) %>% summarize(n=n())

#--------------------------------------------------
# Read in UK business counts - 411 local authorities

business_counts <- read_xlsx(here("data","uk_business_counts_nomis.xlsx"), skip=7)

business_counts <- business_counts %>% select('local authority: district / unitary (prior to April 2015)','2018') %>%
  rename(lad11nm = 1,
         enterprise_count_2018 = 2)

# Which don't match 120 (includes scotland and wales)
business_mismatch_la <- business_counts %>% anti_join(graduate_retention, by='lad11nm')

# Aggregate to Inner and Outer London
business_counts <- business_counts %>% mutate(london_flag = case_when(
  lad11nm %in% inner_london ~ "Inner London BUA",
  lad11nm %in% outer_london ~ "Outer London BUA",
  TRUE ~ 'Other'
))

london_business_counts <- business_counts %>% filter(london_flag != 'Other') %>%
  mutate(enterprise_count_2018 = as.double(enterprise_count_2018)) %>%
  group_by(london_flag) %>% summarize(enterprise_count_2018 = sum(enterprise_count_2018)) %>%
  rename(lad11nm = london_flag)

business_counts <- business_counts %>% filter(london_flag == "Other") %>% select(-london_flag) %>%
  rbind(london_business_counts)

# Join onto la cross section
la_cross_section <- la_cross_section %>% left_join(business_counts, by='lad11nm')

la_cross_section %>% filter(is.na(enterprise_count_2018)) %>% summarize(n=n())

#-------------------------------------------------------------------
# Read in Index of Multiple Deprivation (IMD)
imd <- read_csv(here("data","Index_of_Multiple_Deprivation(IMD2019).csv")) %>%
  select(1,2,5,6) %>%
  rename(lsoa_code = 1,
         lsoa_name = 2,
         imd_rank = 3,
         imd_decile = 4)

# Read LSOA populations (sheet = 5)
lsoa_populations <- read_csv(here("data","lsoa_populations.csv"),skip=3) %>%
  select('LSOA 2021 Code','LSOA 2021 Name','Total')%>%
  rename(lsoa_code = 1,
         lsoa_name = 2,
         lsoa_population = 3)
 
#Check how many rows match between LSOA 2011 AND 2021 - 9.4% of LSOA missing population
imd <- imd %>% left_join(lsoa_populations, by=c('lsoa_code','lsoa_name'))

imd %>% filter(is.na(lsoa_population)) %>% summarize(n=n())
imd %>% summarize(n=n())

# Calculate weighted averages of IMD rank
# Use LSOA to LAD (2011) lookup to match the rest of the data
imd <- imd %>% mutate(weighted_imd_decile = imd_decile * lsoa_population)

lsoa_to_lad <- read_csv(here("data","Lower_Layer_Super_Output_Area_(2001)_to_Lower_Layer_Super_Output_Area_(2011)_to_Local_Authority_District_(2011)_Lookup_in_England_and_Wales.csv")) %>%
                          select(LSOA11CD, LAD11CD, LAD11NM) %>%
                          rename(lsoa_code = LSOA11CD,
                                 lad11cd = LAD11CD,
                                 lad11nm = LAD11NM)

imd <- imd %>% left_join(lsoa_to_lad, by='lsoa_code')

la_imd <- imd %>% group_by(lad11cd, lad11nm) %>% summarize(la_weighted_imd_decile = sum(weighted_imd_decile, na.rm=TRUE),
                                                               la_population = sum(lsoa_population, na.rm=TRUE)) %>%
  mutate(la_imd_decile = round(la_weighted_imd_decile/la_population, 0))

# Join to LA cross section
la_cross_section <- la_cross_section %>% left_join(la_imd, by=c("lad11cd","lad11nm"))

# The LA missing IMD decile is because we don't have LSOA population because the LSOA population is too new
# Next step: Obtain LSOA population for LSOA 2011
la_cross_section %>% filter(is.na(la_imd_decile)) %>% summarize(n=n()) #28

#------------------------------------------
# Read in Arts Council Funding - 296 LA
arts_council <- read_csv(here("data","arts_council_funding_la.csv"),skip=3) %>%
  select(2,8) %>% rename(lad11nm = 1,
                         arts_council_funding_201819 = 2) %>%
  
  mutate(arts_council_funding_201819 = gsub("£","", arts_council_funding_201819),
    arts_council_funding_201819 = as.numeric(arts_council_funding_201819))

# Aggregate to Inner and Outer London
arts_council <- arts_council %>% mutate(london_flag = case_when(
  lad11nm %in% inner_london ~ "Inner London BUA",
  lad11nm %in% outer_london ~ "Outer London BUA",
  TRUE ~ 'Other'
))

london_arts_council <- arts_council %>% filter(london_flag != 'Other') %>%
  mutate(arts_council_funding_201819 = as.double(arts_council_funding_201819)) %>%
  group_by(london_flag) %>% summarize(arts_council_funding_201819 = sum(arts_council_funding_201819)) %>%
  rename(lad11nm = london_flag)

arts_council <- arts_council %>% filter(london_flag == "Other") %>% select(-london_flag) %>%
  rbind(london_arts_council)

# Which LA don't match
arts_council_mismatch <- arts_council %>% anti_join(graduate_retention, by='lad11nm')

# Join to LA cross section
la_cross_section <- la_cross_section %>% left_join(arts_council, by='lad11nm')

la_cross_section %>% filter(is.na(arts_council_funding_201819)) %>% summarize(n=n()) #42

# Divide by LA population for per head measure
la_cross_section <- la_cross_section %>% mutate(arts_council_per_head_201819 = arts_council_funding_201819 / la_population_2011 * 100) 
#---------------------------------
# Read in economic activity



#----------------------
# Read in jobs density



#-------------------------
# Read in labour productivity

#-----------------------
# Save to CSV

write.csv(la_cross_section, here("data","la_cross_section.csv"), )
