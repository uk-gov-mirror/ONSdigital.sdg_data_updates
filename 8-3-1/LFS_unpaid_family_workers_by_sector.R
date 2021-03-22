# Authors: Emmma Wood, Varun Jakki
# Purpose: Get data from the APS dataset for the following disaggregations:
#             sex, sector (agriculture/non-agriculture), and sector by sex 
# Requires: Access to the LFS_SPSS drive.
#           This file is called by compile_tables.R, and requires config.R 

# About this code: This code creates a CSV for indicator 8.3.1 - The calculations completed are: 
# Proportion of informal employment in total employment = (Informal employment/Total employment) × 100
# Proportion of informal employment in agriculture = (Informal employment in agricultural activities/Total employment in agriculture) × 100
# Proportion of informal employment in non agricultural employment = (Informal employment in non agricultural activities/Total employment in non agricultural activities) × 100

# Code last updated: 22/03/2021

library(haven)
library(tidyr)
library(dplyr) 

###############
check_for_caseno_repeats <- function(dat) {
  dat %>%
    group_by(caseno) %>%
    summarise(caseno_count = n()) %>%
    filter(caseno_count > 1)
}


min_count_check <- function(group_var){
  
  counts <- unpaid_family_workers %>% 
    group_by(across(all_of(group_var))) %>% 
    summarise(count = n())
  
  min(counts$count)
}

sum_weights <- function(dat, group_var, new_col) {
  
  new_col <- enquo(new_col)
  new_col_name <- quo_name(new_col)
  
  dat %>% 
    group_by(across(all_of(group_var))) %>% 
    summarise(!!new_col_name := sum(weight))
}

##########

APS_data <- read_sav(paste0(filepath, year_filepath))

employed <- APS_data %>% 
  select(INECAC05, INDS07M, SEX, PWTA18, caseno) %>% 
  rename(industry = INDS07M,
         employment_status = INECAC05,
         weight = PWTA18) %>% 
  mutate(sector = ifelse(industry == 1, "Agriculture", "Non-Agriculture")) %>% 
  filter(!is.na(sector)) %>%  # removes anyone without an industry e.g. inactive people
  filter(employment_status %in% c(1:4)) # 1 = employed, 2 = self employed, 3 = Government schemes, 4 = unpaid family workers

unpaid_family_workers  <- employed %>% 
  filter(employment_status  == 4) # 4 refers to unpaid family workers

# get denominators
total_employment_by_sector <- sum_weights(employed, "sector", Total_employment)
total_employment_by_sector_by_sex <- sum_weights(employed, c("sector", "SEX"), Total_employment)
denominators <- bind_rows(total_employment_by_sector, total_employment_by_sector_by_sex)

# get numerators
informal_employment_by_sector <- sum_weights(unpaid_family_workers, "sector", informal_employment)
informal_employment_by_sector_sex <- sum_weights(unpaid_family_workers, c("sector", "SEX"), informal_employment) 
numerators <- bind_rows(informal_employment_by_sector, informal_employment_by_sector_sex)

# calculate total number of people in agriculture - 'Total employment in agriculture' - by male and Female
proportion_of_informal_employment <- numerators %>% 
  left_join(denominators, by = c("SEX", "sector")) %>%
  mutate(proprtion_of_informal_employment = (informal_employment/Total_employment)*100)
  
headline <- proportion_of_informal_employment %>% 
  group_by(SEX) %>% 
  summarise(Total_employment = sum(Total_employment),
            informal_employment = sum(informal_employment)) %>% 
  mutate(proprtion_of_informal_employment = (informal_employment/Total_employment)*100)
  
all_data <- bind_rows(headline, proportion_of_informal_employment)


min_count_sector <- min_count_check("sector")
min_count_sex <- min_count_check("SEX")
min_count_sex_by_sector <- min_count_check(c("SEX", "sector"))


quality_control <- all_data %>% 
  mutate(low_reliability = case_when(
    min_count_sector <= 25 & !is.na(sector) & is.na(SEX) ~ TRUE,
    min_count_sex <= 25 & is.na(sector) & !is.na(SEX) ~ TRUE,
    min_count_sex_by_sector <= 25 & !is.na(sector) & !is.na(SEX) ~ TRUE,
    TRUE ~ FALSE)) %>%  # In a case_when this final "TRUE" translates as "all other cases"
  filter(low_reliability == FALSE) %>% 
  select(-low_reliability)

csv <- quality_control %>%
  mutate(`Unit measure` = "Percentage (%)",
         `Unit multiplier`= "Units",
         `Observation status`= "Undefined",
         Year = substr(year_filepath, 1, 4),
         Region = "",
         GeoCode = "",
         sector = ifelse(is.na(sector), "", sector),
         Sex = case_when(
           SEX == 1 ~ "Male",
           SEX == 2 ~ "Female",
           is.na(SEX) ~ "")) %>% 
  rename(Sector = sector,
         Value = proprtion_of_informal_employment) %>% 
  select(Year, Region, Sex, Sector, `Observation status`, `Unit multiplier`, `Unit measure`, GeoCode, Value) 

#### Checks
repeat_check_employed <- check_for_caseno_repeats(employed)
repeat_check_unpaid <- check_for_caseno_repeats(unpaid_family_workers)
