source("configs.R")

filename <- ask_user_for_filename("Input")

if (get_characters_after_dot(filename) != "xlsx") {
  stop(paste("File must be an xlsx file. Save", filename, "as an xlsx and re-run script"))
}

source("region.R")
source("birthweight_by_mum_age.R")
source("country_of_occurence_by_sex.R")

all_csv_data <- bind_rows(clean_csv_data_area_of_residence,
                          clean_csv_data_birtweight_by_mum_age,
                          clean_csv_data_country_by_sex)



current_directory <- getwd()
year <- unique_to_string(get_all_years(all_csv_data$Year))

setwd('./Output')
write.csv(all_csv_data, paste0("2_data_", year, ".csv"), row.names = FALSE)

cat(paste0("csv for ", year, ", has been created and saved in '", current_directory, 
             "' as '3-2-2_data_", year, ".csv'", "\n\nFiles created for individual tabs can be viewed by clicking on them in the Global Environment."))



