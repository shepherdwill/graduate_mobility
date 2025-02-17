# Create BUA to Local Authority lookup using population weighting

library(tidyverse)
library(ggplot2)
library(janitor)
library(readxl)
library(dplyr)
library(openxlsx)
library(here)

#-----------------------------------------------------------------------
# Read in OA to BUA to LAD lookup which we will use to go between the levels of geography
#----------------------------------------------------------------------

# Using OA to BUA to LAD lookup
oa_bua_lad <- read_csv(here("data","OA(2011)_to_BUAsubdivision_to_BUA_to_LAD_to_Region_(December_2011)_Lookup_in_England_and_Wales.csv"))
oa_bua_lad <- clean_names(oa_bua_lad)

#Filter out Wales LA's because the grad mobility data is England only
oa_bua_lad <- oa_bua_lad %>% filter(rgn11nm != 'Wales')

# Number of unique OAs 171372
# Number of unique BUASD 1543
# Number of unique BUA 5063
# Number of unique LAD 326

oa_bua_lad %>% summarize(n_distinct(oa11cd),n_distinct(buasd11cd),n_distinct(bua11cd),n_distinct(lad11cd))

# How many BUA span more than one LAD? 150 (2.96%)
oa_bua_lad %>% group_by(bua11cd) %>% summarize(la_span = n_distinct(lad11cd)) %>% filter(la_span > 1) %>% summarize(n=n())

# How many BUASD span more than one LAD? 193 (12.5%)
oa_bua_lad %>% group_by(buasd11cd) %>% summarize(la_span = n_distinct(lad11cd)) %>% filter(la_span > 1) %>% summarize(n=n())

# What is the average number of BUA within an LAD? 23.7
oa_bua_lad %>% group_by(lad11cd) %>% summarize(buasd_count = n_distinct(buasd11cd), 
                                               bua_count = n_distinct(bua11cd)) %>% 
  mutate(total_bua_buasd = buasd_count + bua_count) %>%
  summarize(mean(total_bua_buasd))

oa_bua_lad %>% group_by(lad11cd) %>% summarize(buasd_count = n_distinct(buasd11cd), 
                                               bua_count = n_distinct(bua11cd)) %>% 
  mutate(total_bua_buasd = buasd_count + bua_count) %>%
  ggplot(aes(x=total_bua_buasd)) +
  geom_histogram()

# Create single combined BUA and BUASD column
oa_bua_lad <- oa_bua_lad %>% mutate(town_and_city_name = if_else(is.na(buasd11nm), bua11nm, buasd11nm),
                                    town_and_city_code = if_else(is.na(buasd11cd), bua11cd, buasd11cd))

# 234 towns and cities span multiple LAs
oa_bua_lad %>% group_by(town_and_city_code) %>% summarize(la_span = n_distinct(lad11cd)) %>% filter(la_span > 1) %>% summarize(n=n())
#-----------------------------------------------------------------------------------------
# Now obtain from the graduate mobility data the list of BUA that we want to convert to LA
#-----------------------------------------------------------------------------------------
# Read in graduate mobility data
grad_migration <- read_xlsx((here("data","BUA_geographical_mobility.xlsx")), sheet=2)
grad_migration <-clean_names(grad_migration)

# 1104 BUA/BUASD in towns and cities
grad_migration %>% summarize(n_distinct(town_and_city_code))

# Inner join on combined BUA to see how many are shared between grad migration and lookup - 1100
grad_migration %>% inner_join(oa_bua_lad, by='town_and_city_code') %>% summarize(n_distinct(town_and_city_code))

common_places <-grad_migration %>% inner_join(oa_bua_lad, by='town_and_city_code')

uncommon_places <- grad_migration %>% anti_join(common_places, by='town_and_city_code')
# Inner London BUA
# Outer London BUA
# Other Small BUAs
# Not BUA

# How do we account for these?

#-----------------------------------
# Focus first on the direct matches
#-----------------------------------

# Identify those towns in the lookup that have a direct match to one LA - then join to the data to see which of these are present
# 923 towns in the data fall entirely within one LA
town_la_direct_match_in_lookup <- oa_bua_lad %>% group_by(town_and_city_code, town_and_city_name) %>% summarize(la_span = n_distinct(lad11cd)) %>% filter(la_span == 1)
town_la_direct_match_in_data <- grad_migration %>% inner_join(town_la_direct_match_in_lookup, by='town_and_city_code')

# Identify those towns in the lookup that fall into multiple LA - join to the data
# 177 towns fall into more than one LA
town_la_partial_match_in_lookup <- oa_bua_lad %>% group_by(town_and_city_code, town_and_city_name) %>% summarize(la_span = n_distinct(lad11cd)) %>% filter(la_span > 1)
town_la_partial_match_in_data <- grad_migration %>% inner_join(town_la_partial_match_in_lookup, by=c('town_and_city_code','town_and_city_name'))

# 923 + 177 = 1100 so all our towns in the data fall into either direct or partial match

# Join the matched data df back to the lookup on an inner join so that we have LA matches
# 923 rows this will form the basis of our conversion

town_LAD_lookup <- town_la_direct_match_in_data %>% inner_join(oa_bua_lad, by='town_and_city_code') %>% 
  select(town_and_city_code, town_and_city_name, lad11cd, lad11nm) %>% distinct()

#--------------------------------------------------------------------
# Bring in OA population to find majority LA weighting for areas that span multiple LAD's
#--------------------------------------------------------------------

# Read in regional OA residence population and join together
east_midlands <- read_csv(here("data/oa_populations","east_midlands_oa_pop.csv")) %>% 
  select('geography code', 'Variable: All usual residents; measures: Value') %>% 
  rename(oa11cd = 'geography code',
         oa_population = 'Variable: All usual residents; measures: Value')

east_of_england <- read_csv(here("data/oa_populations","east_of_england_oa_pop.csv")) %>% 
  select('geography code', 'Variable: All usual residents; measures: Value') %>% 
  rename(oa11cd = 'geography code',
         oa_population = 'Variable: All usual residents; measures: Value')

north_east <- read_csv(here("data/oa_populations","north_east_oa_pop.csv")) %>% 
  select('geography code', 'Variable: All usual residents; measures: Value') %>% 
  rename(oa11cd = 'geography code',
         oa_population = 'Variable: All usual residents; measures: Value')

north_west <- read_csv(here("data/oa_populations","north_west_oa_pop.csv")) %>% 
  select('geography code', 'Variable: All usual residents; measures: Value') %>% 
  rename(oa11cd = 'geography code',
         oa_population = 'Variable: All usual residents; measures: Value')

south_east <- read_csv(here("data/oa_populations","south_east_oa_pop.csv")) %>% 
  select('geography code', 'Variable: All usual residents; measures: Value') %>% 
  rename(oa11cd = 'geography code',
         oa_population = 'Variable: All usual residents; measures: Value')

south_west <- read_csv(here("data/oa_populations","south_west_oa_pop.csv")) %>% 
  select('geography code', 'Variable: All usual residents; measures: Value') %>% 
  rename(oa11cd = 'geography code',
         oa_population = 'Variable: All usual residents; measures: Value')

west_midlands <- read_csv(here("data/oa_populations","west_midlands_oa_pop.csv")) %>% 
  select('geography code', 'Variable: All usual residents; measures: Value') %>% 
  rename(oa11cd = 'geography code',
         oa_population = 'Variable: All usual residents; measures: Value')

yorkshire_and_humber <- read_csv(here("data/oa_populations","yorkshire_and_humber_oa_pop.csv")) %>%
  select('geography code','Variable: All usual residents; measures: Value') %>%
  rename(oa11cd = 'geography code',
         oa_population = 'Variable: All usual residents; measures: Value')

oa_population <- rbind(east_midlands, east_of_england, north_east, north_west, south_east, south_west, west_midlands, yorkshire_and_humber)

# We identified towns that span multiple LA earlier - join back onto lookup to get relevant OA and LAD
OAs_town_la_partial_match_in_data <- inner_join(town_la_partial_match_in_data, oa_bua_lad, by=c('town_and_city_code','town_and_city_name'))
# 52240 rows, 177 towns

# Join OA population to lookup
OApop_town_la_partial_match_in_data <- OAs_town_la_partial_match_in_data %>% inner_join(oa_population, by='oa11cd') 
#52237 rows, 177 towns

# We have lost 8 town in this inner join - do all the OAs belong to this 1 town?

before_join <- OAs_town_la_partial_match_in_data 
after_join <- OApop_town_la_partial_match_in_data

lost_rows <- before_join %>% anti_join(after_join, by='oa11cd')
lost_rows %>% group_by(town_and_city_name) %>% summarize(n=n()) %>% arrange(desc(n)) 

# The following towns are missing some small numbers of OA population
# Esher BUASD (2)
# Borehamwood BUASD (1)

#----------------------------------
# Three cases we need to account for:
#   1. Direct one to one match, where we assign all cohort size and graduate numbers to one LA
#   2. BUA spans multiple LA, where we split cohort size and graduate numbers by a population weighting such as 0.6
#   3. Multiple BUA within one LA, where we aggregate all cohort size and graduate numbers
#------------------------------------------------------------------------------------------------------------

# 1 is addressed in dataframe town LAD lookup, below we address 2 and 3

#------------------------------------
# Create rule to select dominant LAD from town or provide a weighting for instances where the split is below 80%
#------------------------------------
#Group by town, sum total OA population and join back on
town_total_pop <- OApop_town_la_partial_match_in_data %>% group_by(town_and_city_code) %>% summarize(town_population = sum(oa_population)) #177 rows

OApop_town_la_partial_match_in_data <- left_join(OApop_town_la_partial_match_in_data, town_total_pop, by='town_and_city_code') #52237 rows, 177 towns

#Group by town and LA, sum OA population and collapse so we have rows for just BUA and LA
la_total_pop_within_town <- OApop_town_la_partial_match_in_data %>% group_by(town_and_city_code, lad11cd) %>% summarize(town_lad_population = sum(oa_population)) #409 rows, 177 towns

OApop_town_la_partial_match_in_data <- left_join(OApop_town_la_partial_match_in_data, la_total_pop_within_town, by=c('town_and_city_code','lad11cd')) #52237 rows, 177 towns

town_la_partial_match_in_data <- OApop_town_la_partial_match_in_data %>% select(town_and_city_code, town_and_city_name, lad11cd, lad11nm, town_population, town_lad_population) %>% distinct() #409 rows, 177 towns

#Calculate share of population lying in each local authority for BUA
town_la_partial_match_in_data <- town_la_partial_match_in_data %>% mutate(la_pop_share = town_lad_population/town_population) #409 rows, 177 towns

#For cases with an LA population share of <10% remove these rows (129 towns)
town_la_partial_match_in_data %>% filter(la_pop_share < 0.1) %>% summarize(n_distinct(town_and_city_code))

town_la_partial_match_in_data_filter <- town_la_partial_match_in_data %>% filter(la_pop_share > 0.1) #236 rows, 177 towns

#After removing minority shares - how many towns are left that span multiple LA? 56 out of 177
town_la_partial_match_in_data_filter %>% group_by(town_and_city_code) %>% summarize(la_span = n_distinct(lad11cd)) %>% filter(la_span > 1) %>% 
  summarize(n_distinct(town_and_city_code))

la_span_recheck <- town_la_partial_match_in_data_filter %>% group_by(town_and_city_code) %>% summarize(la_span = n_distinct(lad11cd)) #177 rows
town_la_partial_match_in_data_filter <- town_la_partial_match_in_data_filter %>% left_join(la_span_recheck, by='town_and_city_code')

town_LA_majority_match <- town_la_partial_match_in_data_filter %>% filter(la_span == 1) #121 towns - this df can be appended to the direct matches
town_LA_partial_match <- town_la_partial_match_in_data_filter %>% filter(la_span > 1) #56 towns- for this df we need to use population weights

#--------------------------------------------------------------------
#Construct an entire lookup for the graduate mobility data BUA TO LAD
#--------------------------------------------------------------------

# Append the majority matches to the direct matches
town_LA_majority_match <- town_LA_majority_match %>% select(town_and_city_code, town_and_city_name, lad11cd, lad11nm)

town_LAD_lookup <- rbind(town_LAD_lookup, town_LA_majority_match) #1044

# Append the partial matches whereby there are two BUAs per LA - we might need to make columns consistent for this

#   Create blank columns for population weight
town_LA_partial_match <- town_LA_partial_match %>% select(town_and_city_code, town_and_city_name, lad11cd, lad11nm, la_pop_share)

town_LAD_lookup[,'la_pop_share'] <- 1
town_LAD_lookup <- rbind(town_LAD_lookup, town_LA_partial_match) #1159 rows, 1100 towns (the extra 4 are the custom categories)

# Join the list of towns and LAs to the graduate mobility data
town_LAD_lookup_on_data <- inner_join(town_LAD_lookup, grad_migration, by=c('town_and_city_code','town_and_city_name')) %>% select(town_and_city_code, town_and_city_name, lad11cd, lad11nm, la_pop_share, number_of_graduates, graduate_retention_rate)

#------------------------------------
# Now we have a corresponding LA (for 56 >1 LA) for every town now - let's aggregate up to LA level for our final dataset
#------------------------------------

# Multiply the number of graduates by the population share so we have individual graduate numbers for every town
# Including cases where a town spans multiple LAs

town_LAD_lookup_on_data <- town_LAD_lookup_on_data %>% mutate(pop_weight_number_of_graduates = round(number_of_graduates * la_pop_share, 0))

# Calculate the absolute number of graduates retained
town_LAD_lookup_on_data <- town_LAD_lookup_on_data %>% mutate(graduate_retention_rate = as.numeric(graduate_retention_rate),
                                                              number_of_retained_graduates = round(pop_weight_number_of_graduates * graduate_retention_rate, 0))

# Where graduate retention rate is NA - do we want to include this towns number of graduates in the denominator for total LA graduate retention?

# 1100 towns and cities - all are being assigned to a local authority



# Group by local authority and sum both number of graduates and graduates retained
local_authority_graduates <- town_LAD_lookup_on_data %>% group_by(lad11cd, lad11nm) %>% 
  summarize(number_of_graduates = sum(pop_weight_number_of_graduates, na.rm=TRUE), 
            number_of_retained_graduates = sum(number_of_retained_graduates, na.rm=TRUE),
            graduate_retention_rate = number_of_retained_graduates / number_of_graduates) %>%
  ungroup()

#292 Local Authorities / 326 originally in the lookup. 

original_la_list <- oa_bua_lad %>% select(lad11nm) %>% distinct()

final_la_list <- local_authority_graduates %>% select(lad11nm)

la_difference <- original_la_list %>% anti_join(final_la_list, by='lad11nm')

# London
# Graduate mobility data aggregates London boroughs to Inner Outer London so we need to do the same?

#---------------------------------------------------
# Add Inner London and Outer London back in from the graduate mobility data
#-------------------------------------------------------------

london <- grad_migration %>% filter((town_and_city_name == 'Inner London BUA') | (town_and_city_name == 'Outer London BUA')) %>%
  rename(lad11nm = town_and_city_name,
         lad11cd = town_and_city_code) %>%
  mutate(graduate_retention_rate = as.numeric(graduate_retention_rate),
         number_of_retained_graduates = round(number_of_graduates * graduate_retention_rate, 0)) %>%
  select(lad11cd, lad11nm, number_of_graduates, number_of_retained_graduates, graduate_retention_rate) 

local_authority_graduates <- rbind(local_authority_graduates, london)

#Final counts
# 294 local authorities

<<<<<<< HEAD
# Export to csv
to.csv(local_authority_graduates, here("data","la_grad_retention"))


=======
# Save to a CSV
write.csv(local_authority_graduates, here("data","la_graduate_retention.csv"))
>>>>>>> b0edee0cf30a9feb439e7a42f4fb49887362d6e7

#---------------------------------------------------
# Assess what % of the LA population the BUA makes up
#---------------------------------------------------

final_la_list <- local_authority_graduates %>% select(lad11nm) %>% distinct()

# Let's filter the OA to BUA to LA lookup to just the LA's we finish with in our data
filtered_lookup <- oa_bua_lad %>% inner_join(final_la_list, by='lad11nm')

# Join in the OA population
filtered_lookup <- filtered_lookup %>% left_join(oa_population, by='oa11cd') #145,457

# Group by LA and sum the OA population
la_populations <- filtered_lookup %>% group_by(lad11nm) %>% summarize(total_la_population = sum(oa_population, na.rm=TRUE))

# Group by LA, BUA and sum the OA population
filtered_lookup %>% group_by(lad11nm, bua11nm, buasd11nm) %>% summarize(sum(oa_population,na.rm=TRUE))

# For each LA we want to calculate the share of population from NA BUA & NA BUASD
la_not_bua_populations <- filtered_lookup %>% filter((is.na(buasd11cd) & (is.na(bua11cd)))) %>%
  group_by(lad11nm) %>% summarize(not_bua_population = sum(oa_population,na.rm=TRUE))

la_populations <- la_populations %>% left_join(la_not_bua_populations, by='lad11nm') %>%
  mutate(share_not_bua_population = not_bua_population / total_la_population * 100)

la_populations %>% arrange(desc(share_not_bua_population))

ggplot(la_populations, aes(x=share_not_bua_population)) +
  geom_histogram()



