# Purpose: Get data for the urban/ rural by sex and the the urban/ rural disaggregations from the LFS dataset
# Requires: Access to the LFS_SPSS drive

library(haven)
library(tidyr)
library(dplyr) 


# replace drive with relevant letter for your computer
filepath <- "Z:\\APS Databases\\2018 Reweighted Files\\"
APS_2020 <- "2020\\APSP_J19J20_CLIENT_PWTA18\\APSP_J19J20_CLIENT_PWTA18.sav"
APS_2019 <- "2019\\APSP_Jan_Dec2019_PWT18\\APSP_Jan_Dec2019_PWT18.sav"
APS_2018 <- "2018\\APSP_JAN18_DEC18_CLIENT_PWTA18\\APSP_JAN18_DEC18_CLIENT_PWTA18.sav"
APS_2017 <-  "2017\\APSP_JAN17_DEC17_CLIENT_PWTA18\\APSP_JAN17_DEC17_CLIENT_PWTA18.sav"
APS_2016 <-  "2016\\APSP_JAN16_DEC16_CLIENT_PWTA18\\APSP_JAN16_DEC16_CLIENT_PWTA18.sav"
APS_2015 <-  "2015\\APSP_JAN15_DEC15_CLIENT_PWTA18\\APSP_JAN15_DEC15_CLIENT_PWTA18.sav"
APS_2014 <-  "2014\\APSP_JAN14_DEC14_CLIENT_PWTA18\\APSP_JAN14_DEC14_CLIENT_PWTA18.sav"
APS_2013 <- "2013\\APSP_JAN13_DEC13_CLIENT_PWTA18\\APSP_JAN13_DEC13_CLIENT_PWTA18.sav"
APS_2012 <- "2012\\APSP_JAN12_DEC12_CLIENT_PWTA18\\APSP_JAN12_DEC12_CLIENT_PWTA18.sav"

APS_data <- paste0(filepath, APS_2020) 

unpaid_family_workers_INECAC05 <- APS_data %>% 
  select(INECAC05, INDS07M, SEX, PWTA18, caseno) %>% 
  filter(INECAC05 == 4)

total_employed <- APS_data %>% 
  select(INECAC05, INDS07M, SEX, PWTA18, caseno) %>% 
  filter(INECAC05 == 1 | INECAC05 == 2)


check_for_caseno_repeats <- function(dat) {
  dat %>%
    group_by(caseno) %>%
    summarise(caseno_count = n()) %>%
    filter(caseno_count > 1)
}

repeat_check_employed <- check_for_caseno_repeats(total_employed)
repeat_check_unpaid <- check_for_caseno_repeats(unpaid_family_workers_INECAC05)

if (nrow(repeat_check_employed) > 0| nrow(repeat_check_unpaid)>0) {
  stop("There are repeated cases in one or both of these dataset, please investigate")
}


number_no_sector <- unpaid_family_workers_INECAC05 %>% 
  filter(is.na(INDS07M)) %>% 
  nrow(.)

Sector_added <- unpaid_family_workers_INECAC05 %>% 
  mutate(sector = ifelse(INDS07M == 1, "Agriculture", "Non-Agriculture")) %>% 
  filter(!is.na(sector))

sex_count <- Sector_added %>% 
  group_by(sector, SEX) %>% 
  summarise(sex_count = n())

min_count_sex_by_sector <- min(sex_count$sex_count)

sector_count <- Sector_added %>% 
  group_by(sector) %>% 
  summarise(sector_count = n())

min_count_sector <- min(sector_count$sector_count)


weights_by_sector <- Sector_added %>% 
  group_by(sector) %>% 
  summarise(unpaid_weights_sum = sum(PWTA18))


weights_by_sector_sex <- Sector_added %>% 
  group_by(sector, SEX) %>% 
  summarise(unpaid_weights_sum = sum(PWTA18))


total_employed_weights_sum<- sum(total_employed$PWTA18)

total_employed_weights_by_sex <- total_employed %>% 
  group_by(SEX) %>% 
  summarise(employed_weights_sum = sum(PWTA18))


csv_data <- bind_rows(weights_by_sector, weights_by_sector_sex) %>%
  left_join(total_employed_weights_by_sex, by = "SEX") %>% 
  mutate(employed_weights_sum = ifelse(is.na(SEX), total_employed_weights_sum, employed_weights_sum)) %>% 
  mutate(Value = (unpaid_weights_sum/employed_weights_sum)*100, 
         Sex = case_when(
           SEX == 1 ~ "Male",
           SEX == 2 ~ "Female",
           is.na(SEX) ~ "")) %>%
    select(-c(SEX, unpaid_weights_sum, employed_weights_sum))
  
  
