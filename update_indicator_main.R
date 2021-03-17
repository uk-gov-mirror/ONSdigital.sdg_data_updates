# Author: Emma Wood
# Date: 08/01/2021
# Purpose: To create csv data for indicators 3.2.2, ...
# Requirements: This script runs the code in the folder stated by indicator <- "indicator_folder_name" below. 
# Runtime: last run approx. 22 seconds

# install.packages("tidyr", dependencies = TRUE)
# install.packages("dplyr", dependencies = TRUE)
# install.packages("tidyxl", dependencies = TRUE)
# install.packages("unpivotr", dependencies = TRUE)

# setwd("H:/Coding_repos/sdg_data_updates")
# install.packages("SDGupdater", repos = NULL, type="source", force = TRUE)

rm(list = ls())

library(tidyr)
library(dplyr)
library(tidyxl)
library(unpivotr)
library(stringr)

library(SDGupdater)

#----------------------------------------------------------------------------------------------

indicator <- "13-2-2" # name of folder for indicator


setwd(paste0("H:/Coding_repos/sdg_data_updates/", indicator))
source("compile_tables.R")



