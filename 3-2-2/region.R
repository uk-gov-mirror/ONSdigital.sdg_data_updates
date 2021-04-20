# Author: Emma Wood
# Date (start): 01/02/2021
# Purpose: To create csv data for area of residence (region) 3.2.2.
# TO DO: look into also giving local authority numbers (We shouldn't display rates as they are unreliable)
# Requirements:This script is called by compile_tables.R, which is called by update_indicator_main.R

source_data <- tidyxl::xlsx_cells(paste0(config$input_folder, "/", filename),
                                  sheets = config$area_of_residence_tab_name)

# info cells are the cells above the column headings
info_cells <- SDGupdater::get_info_cells(source_data, config$first_header_row_area_of_residence)
year <- SDGupdater::unique_to_string(info_cells$Year)
country <- SDGupdater::unique_to_string(info_cells$Country)

main_data <- source_data %>%
  SDGupdater::remove_blanks_and_info_cells(config$first_header_row_country_by_sex) %>%
  dplyr::mutate(character = SDGupdater::remove_superscripts(character))

if ("Region" %in% main_data$character){ # because headings are different for 2017 and 2018 files
  tidy_data <- main_data %>%
    unpivotr::behead("left-up", GeoCode) %>%
    unpivotr::behead("left-up", area_name) %>%
    unpivotr::behead("left-up", geography) %>%
    unpivotr::behead("up-left", measure) %>%
    unpivotr::behead("up-left", life_event) %>%
    unpivotr::behead("up", baby_age) %>%
    dplyr::select(GeoCode, area_name, geography, measure, life_event, baby_age,
                  numeric)

  clean_data <- tidy_data %>%
    dplyr::filter(!is.na(numeric)) %>% # to remove cells that are just ends of a header that have run on to the next row
    dplyr::mutate(area_name = trimws(area_name,  which = "both"),
                  geography = trimws(geography,  which = "both"))

  only_regions_kept <- clean_data %>%
    dplyr::filter(geography == "Region")

} else { # for data that don't have Region as a geography, England regions and Welsh health boards are in the same column

  tidy_data <- main_data %>%
    unpivotr::behead("left-up", GeoCode) %>%
    unpivotr::behead("left-up", area_name) %>%
    unpivotr::behead("up-left", measure) %>%
    unpivotr::behead("up-left", life_event) %>%
    unpivotr::behead("up", baby_age) %>%
    dplyr::select(GeoCode, area_name, measure, life_event, baby_age,
                  numeric)

  clean_data <- tidy_data %>%
    dplyr::filter(!is.na(numeric)) %>% # to remove cells that are just ends of a header that have run on to the next row
    dplyr::mutate(area_name = trimws(area_name,  which = "both"))

  only_regions_kept <- clean_data %>% # because Wales health boards and LAs contain lowercase letters
    dplyr::filter(grepl("[a-z]|ENGLAND", area_name) == FALSE)
}

# no early-neonatal deaths are given, so can't calculate late_neonatoal as in other tables

clean_csv_data_area_of_residence <- only_regions_kept %>%
  dplyr::filter(measure == "Rates",
                life_event == "Deaths",
                baby_age == "Neonatal") %>%
  dplyr::select(area_name, GeoCode, numeric) %>%
  dplyr::rename(Region = area_name,
                Value = numeric) %>%
  dplyr::mutate(Year = year,
                Sex = "",
                `Neonatal period` = "",
                Birthweight = "",
                Age = "",
                `Health board` = "",
                Country = "England",
                Region = SDGupdater::format_region_names(Region),
                `Unit measure` = "Rate per 1,000 live births",
                `Unit multiplier` = "Units",
                `Observation status` = "Undefined",
                Value = ifelse(is.na(Value), "", as.character(Value))) %>% # this turns the value into a character string only if there are NAs so need to explicitly turn it into a character so all data can be combined (in compile_tables.R).
  dplyr::select(Year, Sex, Country, Region, `Health board`, Birthweight, Age, `Neonatal period`, `Unit measure`, `Unit multiplier`, `Observation status`, GeoCode, Value)

SDGupdater::multiple_year_warning(filename, config$area_of_residence_tab_name,"area of residence (region)")
SDGupdater::multiple_country_warning(filename, config$area_of_residence_tab_name,"area of residence (region)")

# clean up environment as the same names are used for multiple scripts called in the same session
rm(clean_data, main_data,
   only_regions_kept,
   info_cells, source_data,
   tidy_data,
   country, year)


