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
#            is the sample size for 'agriculture large enough?
#
#            Can we recreate the headline figures? Can use this to answer:
#            can we use pre-2018 data using the old weights and present them alongside the 2018 onwards data?
#            which quarter are the headline figures from? We should use the same dataset for this analysus.


unpaid_family_workers_OWNBUS_RELBUS <- lfs_data %>% 
  select(OWNBUS, RELBUS, STAT, STATR, ICDM, INDS07M, SEX, PWT18) %>% 
  filter(OWNBUS == 1 | RELBUS == 1)

unpaid_family_workers_STAT_STATR <- lfs_data %>% 
  select(OWNBUS, RELBUS, STAT, STATR, ICDM, INDS07M, SEX, PWT18) %>% 
  filter(STAT == 4 | STATR == 4)

# unpaid_family_worker_not_STAT4 <- unpaid_family_workers_OWNBUS_RELBUS %>% 
#   filter(STAT != 4)
# unpaid_family_worker_not_STATR4 <- unpaid_family_workers_OWNBUS_RELBUS %>% 
#   filter(STATR != 4)
# unpaid_family_worker_not_OWNBUS_RELBUS <- unpaid_family_workers_STAT_STATR %>% 
#   filter(OWNBUS != 1 & RELBUS != 1)
# # looks like STATR 4 and STAT 4 directly map to OWNBUS/RELBUS

# WHY IS there is an extra 2 rows in unpaid_family_workers_STAT_STATR compared to unpaid_family_workers_OWNBUS_RELBUS???


