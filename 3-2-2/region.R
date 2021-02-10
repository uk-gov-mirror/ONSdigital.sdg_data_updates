# Author: Emma Wood
# Date: 01/02/2021
# Purpose: To create csv data for area of residence (region) 3.2.2. (look into also giving ocal authority numbers - not rates as they are unreliable)
# Requirements: This script is called by main.R. It calls functions.R.

source("functions.R")


is_table_name_correct <- menu(c("Yes", "No"),
                              title = paste("Are area of residence data in:", area_of_residence_tab_name, "\n \n",
                                            "(please type relevant number and hit enter)"))

if(is_table_name_correct == 2) {
  stop("Please enter correct Table name between the quote marks after 'area_of_residence_tab_name <- ', then re-run the script")
} 

print(paste("reading in", filename, "from", getwd()))
source_data <- xlsx_cells(filename, sheets = area_of_residence_tab_name)

print("getting year")
info_cells <- get_info_cells(source_data)

Year <- get_year(info_cells)

print("tidying data") 
clean_data <- remove_superscripts_blanks_and_info_cells(source_data)

if ("Region" %in% clean_data$character){ # because it is different for 2017 and 2018 files
  tidy_data <- clean_data %>%
    behead("left-up", GeoCode) %>%
    behead("left-up", area_name) %>%
    behead("left-up", geography) %>%
    behead("up-left", measure) %>%
    behead("up-left", life_event) %>% 
    behead("up", baby_age) %>% 
    filter(!is.na(numeric)) %>% # to remove cells that are just ends of a header that have run on to the next row
    mutate(area_name = trimws(area_name,  which = "both"),
           geography = trimws(geography,  which = "both")) %>% 
    select(GeoCode, area_name, geography, measure, life_event, baby_age,
           numeric)
  
  only_regions_kept <- tidy_data %>% 
    filter(geography == "Region")
  
} else {
  
  tidy_data <- clean_data %>%
    behead("left-up", GeoCode) %>%
    behead("left-up", area_name) %>%
    # behead("left-up", geography) %>%
    behead("up-left", measure) %>%
    behead("up-left", life_event) %>% 
    behead("up", baby_age) %>% 
    filter(!is.na(numeric)) %>% # to remove cells that are just ends of a header that have run on to the next row
    mutate(area_name = trimws(area_name,  which = "both")) %>% 
    select(GeoCode, area_name, measure, life_event, baby_age,
           numeric)
  
  only_regions_kept <- tidy_data %>% 
    filter(any_lowercase_letters(area_name) == FALSE &
             substr(area_name, 1, 7) != "ENGLAND")
}

# no early-neonatal deaths are given, so can't calculate late_neonatoal as in other tables

print("formatting data")
clean_csv_data <- only_regions_kept %>% 
  filter(measure == "Rates",
         life_event == "Deaths",
         baby_age == "Neonatal") %>% 
  select(area_name, GeoCode, numeric) %>% 
  rename(Region = area_name,
         Value = numeric) %>% 
  mutate(Year = Year,
         Sex = "",
         `Neonatal period` = "",
         Birthweight = "",
         Age = "",
         `Health board` = "",
         Country = "England",
         Region = format_region_names(Region),
         `Unit measure` = "Rate per 1,000 live births",
         `Unit multiplier` = "Units", 
         `Observation status` = "Undefined",
         Value = ifelse(is.na(Value), "", Value)) %>% # this turns the value into a character string
  select(Year, Sex, Country, Region, `Health board`, Birthweight, Age, `Neonatal period`, `Unit measure`, `Unit multiplier`, `Observation status`, GeoCode, Value)

print("saving data")
setwd('./Output')
write.csv(clean_csv_data, paste0("area_of_residence_for_csv_", Year, ".csv"), row.names = FALSE)

current_directory <- getwd()
print(paste0("Area of residence (Region) data have been created and formatted for ", Year, ", and saved in '", current_directory, 
             "' as 'area_of_residence_", Year, ".csv'"))

rm(clean_csv_data, clean_data, only_regions_kept,
   info_cells, source_data,
   tidy_data, Years)

setwd('..')

