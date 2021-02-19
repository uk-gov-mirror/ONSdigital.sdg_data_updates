# Author: Emma Wood 
# Contact: emma.wood@ons.gov.uk
# compile tables for indicator 13-2-2

source("configs.R")

filename <- ask_user_for_filename("Input")
tab_name <- ask_user_for_tab_name("UK data by gas 1990 onwards", tab_name)
first_header_row <- ask_user_for_first_header_row(tab_name, first_header_row)


if (get_characters_after_dot(filename) != "xlsx") {
  stop(paste("File must be an xlsx file. Save", filename, "as an xlsx and re-run script"))
}

source("by_gas_type.R")

current_directory <- getwd()

setwd('./Output')
write.csv(csv_data, paste0("13-2-2_data_", latest_year, ".csv"), row.names = FALSE)

cat(paste0("csv for ", latest_year, ", has been created and saved in '", current_directory, 
             "' as '13-2-2_data_", latest_year, ".csv'", "\n\nFiles created for individual tabs can be viewed by clicking on them in the Global Environment."))



