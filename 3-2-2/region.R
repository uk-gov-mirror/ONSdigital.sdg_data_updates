# Author: Emma Wood
# Date (start): 01/02/2021
# Purpose: To create csv data for area of residence (region) 3.2.2. 
# TO DO: look into also giving local authority numbers (We shouldn't display rates as they are unreliable)
# Requirements:This script is called by compile_tables.R, which is called by update_indicator_main.R

area_of_residence_tab_name <- ask_user_for_tab_name("area of residence (region)", area_of_residence_tab_name)
first_header_row <- ask_user_for_first_header_row(area_of_residence_tab_name, first_header_row_area_of_residence)

source_data <- xlsx_cells(paste0("Input/", filename), sheets = area_of_residence_tab_name)

# info cells are the cells above the column headings
info_cells <- get_info_cells(source_data, first_header_row_area_of_residence)
year <-unique_to_string(info_cells$Year)
country <- unique_to_string(info_cells$Country)

main_data <- source_data %>% 
  remove_blanks_and_info_cells(first_header_row_country_by_sex) %>%
  mutate(character = remove_superscripts(character))

if ("Region" %in% main_data$character){ # because headings are different for 2017 and 2018 files
  tidy_data <- main_data %>%
    behead("left-up", GeoCode) %>%
    behead("left-up", area_name) %>%
    behead("left-up", geography) %>%
    behead("up-left", measure) %>%
    behead("up-left", life_event) %>% 
    behead("up", baby_age) %>% 
    select(GeoCode, area_name, geography, measure, life_event, baby_age,
           numeric)
  
  clean_data <- tidy_data %>% 
    filter(!is.na(numeric)) %>% # to remove cells that are just ends of a header that have run on to the next row
    mutate(area_name = trimws(area_name,  which = "both"),
           geography = trimws(geography,  which = "both"))
  
  only_regions_kept <- clean_data %>% 
    filter(geography == "Region")
  
} else { # for data that don't have Region as a geography, England regions and Welsh health boards are in the same column
  
  tidy_data <- main_data %>%
    behead("left-up", GeoCode) %>%
    behead("left-up", area_name) %>%
    behead("up-left", measure) %>%
    behead("up-left", life_event) %>% 
    behead("up", baby_age) %>% 
    select(GeoCode, area_name, measure, life_event, baby_age,
           numeric)
  
  clean_data <- tidy_data %>% 
    filter(!is.na(numeric)) %>% # to remove cells that are just ends of a header that have run on to the next row
    mutate(area_name = trimws(area_name,  which = "both"))
  
  only_regions_kept <- clean_data %>% # because Wales health boards and LAs contain lowercase letters
    filter(grepl("[a-z]|ENGLAND", area_name) == FALSE)
}

# no early-neonatal deaths are given, so can't calculate late_neonatoal as in other tables

clean_csv_data_area_of_residence <- only_regions_kept %>% 
  filter(measure == "Rates",
         life_event == "Deaths",
         baby_age == "Neonatal") %>% 
  select(area_name, GeoCode, numeric) %>% 
  rename(Region = area_name,
         Value = numeric) %>% 
  mutate(Year = year,
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
         Value = ifelse(is.na(Value), "", as.character(Value))) %>% # this turns the value into a character string only if there are NAs so need to explicitly turn it into a character so all data can be combined (in compile_tables.R).
  select(Year, Sex, Country, Region, `Health board`, Birthweight, Age, `Neonatal period`, `Unit measure`, `Unit multiplier`, `Observation status`, GeoCode, Value)

multiple_year_warning(filename, area_of_residence_tab_name,"area of residence (region)")
multiple_country_warning(filename, area_of_residence_tab_name,"area of residence (region)")

# clean up environment as the same names are used for multiple scripts called in the same session
rm(clean_data, main_data,
   only_regions_kept, 
   info_cells, source_data,
   tidy_data,
   country, year, first_header_row)


