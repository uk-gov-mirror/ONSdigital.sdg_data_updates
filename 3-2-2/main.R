# Author: Emma Wood
# Date: 08/01/2021
# Purpose: To create csv data for birthweight: mother age disaggregations for indicator 3.2.2
# Requirements: The data from the source should be saved as a csv, and all data columns MUST be number format without the comma separator
#               This script calls birthweight_by_mum_age_function - if edits are needed to the script that is where you will need to make them

# install.packages("tidyr", dependencies = TRUE)
# install.packages("dplyr", dependencies = TRUE)
# install.packages("tidyxl", dependencies = TRUE)
# install.packages("unpivotr", dependencies = TRUE)

rm(list = ls())

setwd("H:/Coding_repos")
# install.packages("SDGupdater", repos = NULL, type="source")
library(SDGupdater)

library(tidyr)
library(dplyr)
library(tidyxl)
library(unpivotr)

#----------------------------------- CONFIGURATIONS -------------------------------------------

working_directory <- "H:\\Coding_repos\\3-2-2_update\\" # the folder containing code for the update. This folder should contain folders called "Input" and "Output"
filename <- "Input/cms2017correctedfeb2020.xlsx"

birthweight_by_mum_age_tab_name <- "Table 10"
first_header_row_birthweight_by_mum_age <- 4

country_of_occurrence_by_sex_tab_name <- "Table 2"
first_header_row_country_by_sex <- 4

area_of_residence_tab_name <- "Table 3"
first_header_area_of_residence <- 4

#----------------------------------------------------------------------------------------------

setwd(working_directory)
source("birthweight_by_mum_age.R")
source("country_of_occurence_by_sex.R")
source("region.R")




