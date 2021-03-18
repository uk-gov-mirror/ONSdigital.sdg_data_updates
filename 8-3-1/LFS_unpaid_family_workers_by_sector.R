# Purpose: Get data for the urban/ rural by sex and the the urban/ rural disaggregations from the LFS dataset
# Requires: Access to the LFS_SPSS drive

library(haven)
library(tidyr)
library(dplyr) 


check_for_caseno_repeats <- function(dat) {
  dat %>%
    group_by(caseno) %>%
    summarise(caseno_count = n()) %>%
    filter(caseno_count > 1)
}

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
APS_data <- read_sav(APS_data)

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


repeat_check_employed <- check_for_caseno_repeats(total_employed)
repeat_check_unpaid <- check_for_caseno_repeats(unpaid_family_workers )

if (nrow(repeat_check_employed) > 0| nrow(repeat_check_unpaid)>0) {
  stop("There are repeated cases in one or both of these dataset, please investigate")
}

# get denominators


total_employment_by_sector <- employed %>% 
  group_by(sector) %>% 
  summarise(Total_employment = sum(weight))

total_employment_by_sector_by_sex <- employed %>% # change to employed
  group_by(sector, SEX) %>% 
  summarise(Total_employment = sum(weight))

denominators <- bind_rows(total_employment_by_sector, total_employment_by_sector_by_sex)


# get numerators
informal_employment_by_sector <- unpaid_family_workers %>% 
  group_by(sector) %>% 
  summarise(informal_employment = sum(weight))

informal_employment_by_sector_sex <- unpaid_family_workers %>% 
  group_by(sector, SEX) %>% 
  summarise(informal_employment = sum(weight))

numerators <- bind_rows(informal_employment_by_sector, informal_employment_by_sector_sex)

# calculating total number of people in agriculture - 'Total employment in agriculture' - by male and Female



proportion_of_informal_employment <- numerators %>% 
  left_join(denominators, by = c("SEX", "sector")) %>%
  mutate(proprtion_of_informal_employment = (informal_employment/Total_employment)*100, 
         Sex = case_when(
           SEX == 1 ~ "Male",
           SEX == 2 ~ "Female",
           is.na(SEX) ~ ""))
  
  
headline <- proportion_of_informal_employment %>% 
  group_by(sector) %>% 
  summarise(Total_employment = sum(Total_employment),
             informal_employment = sum(informal_employment)) %>% 
  mutate(proprtion_of_informal_employment = (informal_employment/Total_employment)*100)
  
# CSV_2 holds 'Proportion of informal employment in agriculture' AND 'Proportion of informal employment in non agricultural employment'
   
CSV_2 <- bind_rows(headline, proportion_of_informal_employment)



total_employed_weights_sum<- sum(total_employed$weight)

total_employed_weights_by_sex <- total_employed %>% 
  group_by(SEX) %>% 
  summarise(employed_weights_sum = sum(weight))



# Calculation for 'Proportion of informal employment in total employment'

csv_data <- bind_rows(weights_by_sector, weights_by_sector_sex) %>%
  left_join(total_employed_weights_by_sex, by = "SEX") %>% 
  mutate(employed_weights_sum = ifelse(is.na(SEX), total_employed_weights_sum, employed_weights_sum)) %>% 
  mutate(Value = (unpaid_weights_sum/employed_weights_sum)*100, 
         Sex = case_when(
           SEX == 1 ~ "Male",
           SEX == 2 ~ "Female",
           is.na(SEX) ~ "")) %>%
    select(-c(SEX, unpaid_weights_sum, employed_weights_sum))
  


####Checks

sex_count <- employed %>% 
  group_by(sector, SEX) %>% 
  summarise(sex_count = n())

min_count_sex_by_sector <- min(sex_count$sex_count)

sector_count <- employed %>% 
  group_by(sector) %>% 
  summarise(sector_count = n())

min_count_sector <- min(sector_count$sector_count)

  
