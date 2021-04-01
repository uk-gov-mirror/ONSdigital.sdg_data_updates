# Authors: Emma Wood (emma.wood@ons.gov.uk) and Varun Jakki
# Purpose: Compile tables for SDG indicator 8-3-1
# Requires: Access to the LFS_SPSS drive.
#           This file is called by update_indicator_main.R 

library(haven)
library(tidyr)
library(dplyr)
library(openxlsx)

# need to put in a stop if the user hasn't installed these packages - how though?

run_date_time <- Sys.time()
run_date <- Sys.Date()
run_date_formatted <- format.Date(run_date, "%m/%d/%y")

reformat_for_filename <- function(run_date_time) {
  run_date_time_no_spaces <- gsub(" ", "_", as.character(run_date_time))
  run_date_time_no_colons <- gsub(":", "-", as.character(run_date_time_no_spaces))
}

run_info_filename <- paste0("run_info_", reformat_for_filename(run_date_time), ".txt")
csv_data_filename <- paste0("8-3-1_csv_", reformat_for_filename(run_date_time), ".csv")
csv_data_filename <- paste0("8-3-1_csv_", reformat_for_filename(run_date_time), ".csv")

source("config.R")

# check that all filepaths exist, if not don't continue 
# (as it takes ages to read in each dataset, and if even one is wrong, they all need to be re-run)
# pretty sure this can be simplified somehow...
for (i in 1:length(year_filepaths)) {
  
  year_filepath <- year_filepaths[i]
  input <- paste0(filepath, year_filepath)
  
  input_folder_exploded <- strsplit(year_filepath, "/")
  input_folder <- paste0(input_folder_exploded[[1]][1], "/", input_folder_exploded[[1]][2])
  
  full_input_filepath_exploded <- strsplit(input, "/")
  input_file <- full_input_filepath_exploded[[1]][length(full_input_filepath_exploded[[1]])]
  
  first_folder <- input_folder_exploded[[1]][1]
  available_folders_level_1 <- list.files(filepath)
  
  second_folder <- input_folder_exploded[[1]][2]
  available_folders_level_2 <- list.files(paste0(filepath, first_folder))
  
  available_folders_level_3 <- list.files(paste0(filepath, first_folder, "/", second_folder))
  
  if (!first_folder %in% available_folders_level_1) {
    
    stop(paste(input, "does not exist. Please correct year_filepaths and re-run"))
    
  } else if (!second_folder %in% available_folders_level_2) {
    
    stop(paste(second_folder, "does not exist in", paste0(filepath, first_folder), ". Please correct year_filepaths in config.R and re-run"))
    
  } else if (!input_file %in% available_folders_level_3) {
    
    stop(input_file(input_file, "does not exist in", paste0(filepath, "/", second_folder) , ". Please correct year_filepaths in config.R and re-run"))
    
  }   
  
} 

# Compile data and checks (usually there will only be one year of data, but this allows for multiple years to be run at once)
csv_data_compiled <- data.frame()

sector_by_country_compiled <- data.frame()
sector_by_region_compiled <- data.frame()
sector_by_sex_compiled <- data.frame()

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
  sector_by_country_compiled <- bind_rows(sector_by_country, csv)
  sector_by_region_compiled <- bind_rows(sector_by_region, csv)
  sector_by_sex_compiled <- bind_rows(sector_by_sex, csv)
  
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
  warning("There were one or more repeated respondents in one or more years. Please investigate (see run repo")
}

if (nrow(disaggregations_with_low_counts_compiled) > 0) {
  warning("There were fewer than 25 respondents in one or more groups. These can be found using the 'Number of respondents' column in the output csv")
}

if (nrow(suppressed_data_compiled) > 0) {
  warning("There were fewer than 3 respondents for some disaggregations. These values have been suppressed in the output csv")
}

setwd(output_directory)

# Create and save file containing run info
low_counts_text <- ifelse(nrow(disaggregations_with_low_counts_compiled) == 0, 
                          "Respondent counts are all greater than 25 (check passed)", 
                          "WARNING: Some respondent counts for one or more disaggregations are low (<= 25) - see table below")
suppressed_data_text <- ifelse(nrow(suppressed_data_compiled) == 0, 
                               "Respondent counts are all greater than 3 (check passed)", 
                               "WARNING: Some respondent counts for one or more disaggregations are 3 or lower and have been suppressed - see table below")
repeat_respondents_text <- ifelse(all(repeat_checks_compiled$repeated_respondents == 0), 
                                  "There were no repeat respondents (check passed)",
                                  "WARNING: There were one or more repeated respondents in one or more years - see table below")

filepath_list <- paste0(filepath, year_filepaths, collapse = "\n")

run_info <- paste0("8-3-1 update \n\n",
                   "Date and time of run: ", run_date_time, "\n",
                   "File(s) used: ", filepath_list, "\n\n",
                   "Checks: \n",
                   low_counts_text, "\n",
                   suppressed_data_text, "\n",
                   repeat_respondents_text, "\n")

cat(run_info, file = run_info_filename, append = FALSE)
cat("\n Disaggregation combinations where one or more counts are 25 or lower:\n\n", file = run_info_filename, append = TRUE)
write.table(disaggregations_with_low_counts_compiled, 
            file = run_info_filename, append = TRUE, sep='\t\t', 
            row.names = FALSE, col.names = TRUE)
cat("\n Rows where count is 3 or lower:\n\n", file = run_info_filename, append = TRUE)
write.table(suppressed_data_compiled, 
            file = run_info_filename, append = TRUE, sep='\t\t', 
            row.names = FALSE, col.names = TRUE)
cat("\n Number of times respondents appear in the data:\n\n", file = run_info_filename, append = TRUE)
write.table(repeat_checks_compiled, 
            file = run_info_filename, append = TRUE, sep='\t', 
            row.names = FALSE, col.names = TRUE)

# save data
write.csv(csv_data_compiled, csv_data_filename, row.names = FALSE)
print(paste("data for 8-3-1 have been compiled and saved as", csv_data_filename))

source("Publication.R")
print(paste0("The Excel file for the ad-hoc publication has been saved as ", output_directory, "ad_hoc_", run_date, ".xlsx"))
