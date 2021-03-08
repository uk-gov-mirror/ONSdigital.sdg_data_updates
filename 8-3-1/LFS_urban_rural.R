# Purpose: Get data for the urban/ rural by sex and the the urban/ rural disaggregations from the LFS dataset
# Requires: Access to the LFS_SPSS drive

library(haven)
library(tidyr)
library(dplyr) 

# replace drive with relevant letter for your computer 
lfs_data <- read_sav("X:\\LFS Person datasets\\2018 reweighted data\\2020\\LFSP_JS20_GOVERNMENT_SRS.sav")

# unpaid family workers: OWNBUS == 1 or RELBUS == 1 (see page 112 of LFS user guide volume 3)
# Employment status: STAT - 4 for unpaid family workers
# Employment status in main job (reported): STATR - 4 for unpid family workers
# SIC for main job: ICDM(could use for extra disagg)
# Industry section in main ob: INDS07M (all we really need? - 1 == A - agriculture, forestry, and fishing)
# SEX: 1 = M, 2 = F

# questions: does OWNBUS and RELBUS always refer to main job? ans = yes
#            is the sample size for 'agriculture' large enough? Or do we need to use 'Agriculture, foresty and fishing'
#
#            Can we recreate the headline figures? Can use this to answer:
#            can we use pre-2018 data using the old weights and present them alongside the 2018 onwards data?
#            which quarter are the headline figures from? Or is there an annual dataset? We should use the same dataset for this analysus.


unpaid_family_workers_OWNBUS_RELBUS <- lfs_data %>% 
  select(OWNBUS, RELBUS, STAT, STATR, ICDM, INDS07M, SEX, PWT18, CASENO) %>% 
  filter(OWNBUS == 1 | RELBUS == 1) %>% 
  mutate(OWNBUS_RELBUS = TRUE)

unpaid_family_workers_STAT_STATR <- lfs_data %>% 
  select(OWNBUS, RELBUS, STAT, STATR, ICDM, INDS07M, SEX, PWT18, CASENO) %>% 
  filter(STAT == 4 | STATR == 4) %>% 
  mutate(STAT_STATR = TRUE) 

find_repeats_STAT_STATR <- unpaid_family_workers_STAT_STATR %>% 
  group_by(CASENO) %>% 
  summarise(count = n()) %>% 
  mutate(STAT_STATR = TRUE)
find_repeats_OWNBUS_RELBUS <- unpaid_family_workers_OWNBUS_RELBUS %>% 
  group_by(CASENO) %>% 
  summarise(count = n()) %>% 
  mutate(OWNBUS_RELBUS = TRUE)

mismatched_rows <- find_repeats_STAT_STATR %>% 
  left_join(find_repeats_OWNBUS_RELBUS, by = "CASENO") %>% 
  mutate(OWNBUS_RELBUS = ifelse(is.na(OWNBUS_RELBUS), FALSE, OWNBUS_RELBUS)) %>% 
  filter(OWNBUS_RELBUS == FALSE) %>% 
  distinct(CASENO)

# WHY IS there is an extra 2 rows in unpaid_family_workers_STAT_STATR compared to unpaid_family_workers_OWNBUS_RELBUS???
extra_rows_from_STAT_STATR <- mismatched_rows %>% 
  left_join(unpaid_family_workers_STAT_STATR)
# non-response for OWNBUS and RELBUS - could increase sample size by two by using this approach, but the user guide says to define by OWNBUS and RELBUS

unpaid_family_workers <- data.frame(lapply(unpaid_family_workers_OWNBUS_RELBUS, as.character), stringsAsFactors=FALSE) %>% 
  mutate(PWT18 = as.numeric(PWT18))
str(unpaid_family_workers)  
