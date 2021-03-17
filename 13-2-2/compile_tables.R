# Author: Emma Wood 
# Contact: emma.wood@ons.gov.uk
# compile tables for indicator 13-2-2

source("configs.R")

#--- get input from user ---
filename <- ask_user_for_filename("Input")

if (get_characters_after_dot(filename) != "xlsx") {
  stop(paste("File must be an xlsx file. Save", filename, "as an xlsx and re-run script"))
}

headline_tab_name <- ask_user_for_tab_name("UK data by gas 1990 onwards", headline_tab_name)
headline_first_header_row <- ask_user_for_first_header_row(headline_tab_name, headline_first_header_row)

# by_source_tab_name <- ask_user_for_tab_name("UK data by source category (sector) 1990 onwards", by_source_tab_name)
# by_source_first_header_row <- ask_user_for_first_header_row(headline_tab_name, by_source_first_header_row)
# 
# by_user_tab_name <- ask_user_for_tab_name("UK data by end-user category (sector) 1990 onwards", by_user_tab_name)
# by_user_first_header_row <- ask_user_for_first_header_row(headline_tab_name, by_source_first_header_row)
# 
# #--- create data for disaggregation by source sector ---
# cat("creating data for disaggregation by source sector\n")
# tab_name <- by_source_tab_name
# first_header_row <- by_source_first_header_row
# expected_info_1 <- "source"
# expected_info_2 <- "territorial greenhouse gas"
# 
# source("by_sector.R")
# 
# by_source_csv_data <- csv_data %>% 
#   rename(`Sector (source)` = Sector,
#          `Sector detail (source)` = `Sector detail`) %>% 
#   mutate(`Sector (end-user)` = "",
#          `Sector detail (end-user)` = "")
# 
# if (expected_info_present != TRUE){
#   warning(wrong_tab_message(tab_name, "territorial greenhouse gas emissions by source sector"))
# }
# 
# if (earliest_year != "1990") {
#   warning(paste0("data should be from the table for 1990-", latest_year, ". 
# It looks like the wrong table may have been used for data by source category (sector). Was data for 1970-", latest_year, " accidentally used?  
#  Please check, and re-run the code if the wrong tab was used"))
# }
# 
# rm(csv_data, expected_info_present, tab_name, first_header_row, earliest_year, latest_year)
# 
# 
# #--- create data for disaggregation by end-user sector ---
# cat("creating data for disaggregation by end-user sector\n")
# tab_name <- by_user_tab_name
# first_header_row <- by_user_first_header_row
# expected_info_1 <- "end"
# expected_info_2 <- "territorial greenhouse gas"
# 
# source("by_sector.R")
# 
# by_user_csv_data <- csv_data %>% 
#   rename(`Sector (end-user)` = Sector,
#          `Sector detail (end-user)` = `Sector detail`) %>% 
#   mutate(`Sector (source)` = "",
#          `Sector detail (source)` = "")
# 
# if (expected_info_present != TRUE){
#   warning(wrong_tab_message(tab_name, "territorial greenhouse gas emissions by end-user sector"))
# }
# 
# if (earliest_year != "1990") {
#   warning(paste0("data should be from the table for 1990-", latest_year, ". 
# It looks like the wrong table may have been used for data by source category (sector). Was data for 1970-", latest_year, " accidentally used?  
#  Please check, and re-run the code if the wrong tab was used"))
# }
# 
# rm(csv_data, expected_info_present, tab_name, first_header_row, earliest_year, latest_year)

#--- create headline data ---
cat("creating headline data\n")
source("by_gas_type.R")

#--- compile data ---

# all_csv_data <- bind_rows(headline_csv_data_by_gas,
#                           by_source_csv_data, 
#                           by_user_csv_data)

all_csv_data <- headline_csv_data_by_gas
  
#--- write csv ---
current_directory <- getwd()

setwd('./Output')
write.csv(all_csv_data, paste0("13-2-2_data_", latest_year, ".csv"), row.names = FALSE)

cat(paste0("csv for ", latest_year, ", has been created and saved in '", current_directory, 
             "' as '13-2-2_data_", latest_year, ".csv'", "\n\nFiles created for individual tabs can be viewed by clicking on them in the Global Environment."))




