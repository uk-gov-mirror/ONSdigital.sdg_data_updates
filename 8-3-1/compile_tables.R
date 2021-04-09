# Authors: Emma Wood (emma.wood@ons.gov.uk) and Varun Jakki
# Purpose: Compile tables for SDG indicator 8-3-1
# Requires: Access to the LFS_SPSS drive.
#           This file is called by update_indicator_main.R 

library(haven)
library(openxlsx)
# could put in a stop if the user hasn't installed these packages - or will it just automatically stop

`%not_in%` <- Negate(`%in%`)

run_date_time <- Sys.time()
run_date <- Sys.Date()
run_date_formatted <- format.Date(run_date, "%d/%m/%y")

run_info_filename <- paste0("run_info_", run_date, ".txt")
csv_data_filename <- paste0("8-3-1_csv_", run_date, ".csv")
csv_data_filename <- paste0("8-3-1_csv_", run_date, ".csv")

source("config.R")

# check that all filepaths exist, if not don't continue 
# (as it takes ages to read in each dataset, and if even one is wrong, they all need to be re-run)
# pretty sure this can be simplified somehow...
for (i in 1:length(year_filepaths)) {
  
  year_filepath <- year_filepaths[i]
  input <- paste0(filepath, year_filepath)
  
  input_folder_exploded <- strsplit(year_filepath, "/")

  full_input_filepath_exploded <- strsplit(input, "/")
  input_file <- full_input_filepath_exploded[[1]][length(full_input_filepath_exploded[[1]])]
  
  year_folder <- input_folder_exploded[[1]][1]
  available_year_folders <- list.files(filepath)
  
  dataset_folder <- input_folder_exploded[[1]][2]
  available_dataset_folders <- list.files(paste0(filepath, year_folder))
  
  available_files <- list.files(paste0(filepath, year_folder, "/", dataset_folder))
  
  if (year_folder %not_in% available_year_folders) {
    
    stop(paste(input, "does not exist. Please correct year_filepaths and re-run"))
    
  } else if (dataset_folder %not_in% available_dataset_folders & input_file %not_in% available_dataset_folders) {
    
    stop(paste(dataset_folder, "does not exist in", paste0(filepath, year_folder), ". Please correct year_filepaths in config.R and re-run"))
    
  } else if (input_file %not_in% available_files & input_file %not_in% available_dataset_folders) {
    
    stop(paste(input_file, "does not exist in", paste0(filepath, "/", dataset_folder) , ". Please correct year_filepaths in config.R and re-run"))
    
  }   
  
} 

# Compile data and checks (usually there will only be one year of data, but this allows for multiple years to be run at once)
csv_data_compiled <- data.frame()

sector_by_country_compiled <- data.frame()
sector_by_region_compiled <- data.frame()
sector_by_sex_compiled <- data.frame()
country_by_sex_compiled <- data.frame()

repeat_checks_compiled <- data.frame()
disaggregations_with_low_counts_compiled <- data.frame()
suppressed_data_compiled <- data.frame()

all_years <- c()

for (i in 1:length(year_filepaths)) {
  
  year_filepath <- year_filepaths[i]
  input <- paste0(filepath, year_filepath)
  
  year <- substr(year_filepath, 1, 4)
  all_years[i] <- as.numeric(year)
  
  source("LFS_unpaid_family_workers_by_sector.R")
  
  csv_data_compiled <- bind_rows(csv_data_compiled, csv)
  
  sector_by_country_compiled <- bind_rows(sector_by_country_compiled, sector_by_country)
  sector_by_region_compiled <- bind_rows(sector_by_region_compiled, sector_by_region)
  sector_by_sex_compiled <- bind_rows(sector_by_sex_compiled, sector_by_sex)
  country_by_sex_compiled <- bind_rows(country_by_sex_compiled, country_by_sex)
  
  suppressed <- suppressed_data
  
  repeat_checks <- data.frame(year = substr(year_filepath, 1, 4),
                              dataset = c("all employed    ", "unpaid family workers"),
                              repeated_respondents = c(nrow(repeat_check_employed), nrow(repeat_check_unpaid))) 
  
  suppressed_data_compiled <- bind_rows(suppressed_data_compiled, suppressed)
  disaggregations_with_low_counts_compiled <- bind_rows(disaggregations_with_low_counts_compiled, disaggregations_with_low_counts)
  repeat_checks_compiled <- bind_rows(repeat_checks_compiled, repeat_checks)
  
  print(paste("data created for", i, "of", length(year_filepaths), "years"))
}

# Warn the user about failed checks (this info is also given in the run_info file)
if (any(repeat_checks_compiled$repeated_respondents > 0)) {
  warning("There were one or more repeated respondents in one or more years. Please investigate (see run repo)")
}

if (nrow(disaggregations_with_low_counts_compiled) > 0) {
  warning("There were fewer than 25 respondents in one or more groups. These can be found using the 'Number of respondents' column in the output csv")
}

if (nrow(suppressed_data_compiled) > 0) {
  warning("There were fewer than 3 respondents for some disaggregations. These values have been suppressed in the output csv")
}

# save data

source("Publication.R")
print(paste0("The Excel file for the ad-hoc publication has been saved as ", output_directory, "/ad_hoc_", run_date, ".xlsx"))

source("run_info.R") # output_directory is set in run_info.R

write.csv(csv_data_compiled, csv_data_filename, row.names = FALSE)
print(paste("data for 8-3-1 have been compiled and saved as", csv_data_filename))


