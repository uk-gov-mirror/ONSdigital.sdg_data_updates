# Author: Emma Wood
# Date (start): 26/01/2021
# Purpose: To create csv data for birthweight by mother age disaggregation for indicator 3.2.2
# Requirements: This script is called by compile_tables.R, which is called by update_indicator_main.R

birthweight_by_mum_age_tab_name <-  birthweight_by_mum_age_tab_name
first_header_row <-  first_header_row_birthweight_by_mum_age

source_data <- tidyxl::xlsx_cells(paste0(input_folder, "/", filename), sheets = birthweight_by_mum_age_tab_name)

info_cells <- SDGupdater::get_info_cells(source_data, first_header_row)
year <- SDGupdater::unique_to_string(info_cells$Year)
country <- SDGupdater::unique_to_string(info_cells$Country)

main_data <- source_data %>%
  SDGupdater::remove_blanks_and_info_cells(first_header_row) %>%
  dplyr::mutate(character = remove_superscripts(character))

tidy_data <- main_data %>%
  unpivotr::behead("left-up", birthweight) %>%
  unpivotr::behead("left", mother_age) %>%
  unpivotr::behead("up-left", measure) %>%
  unpivotr::behead("up-left", event) %>%
  unpivotr::behead("up", baby_age) %>%
  dplyr::select(birthweight, mother_age, measure, event, baby_age,
         numeric)

clean_data <- tidy_data %>%
  dplyr::filter(!is.na(numeric)) %>% # to remove cells that are just ends of a header that have run on to the next row
  dplyr::mutate(birthweight = trimws(birthweight,  which = "both")) %>%
  dplyr::mutate(birthweight = ifelse(birthweight == "4000 and" | birthweight == "over",
                              "4000 and over", birthweight))

data_for_calculations <- clean_data %>%
  tidyr::pivot_wider(names_from = c(measure, baby_age, event),
              values_from = numeric)

max_decimal_places_used_by_source <- count_decimal_places(data_for_calculations$Rates_Neonatal_Deaths)

late_neonatal <- data_for_calculations %>%
  dplyr::mutate(Numbers_Late_neonatal_Deaths = Numbers_Neonatal_Deaths - Numbers_Early_Deaths) %>%
  dplyr::mutate(Rates_Late_neonatal_Deaths = SDGupdater::calculate_valid_rates_per_1000(Numbers_Late_neonatal_Deaths,
                                                                     `Numbers_Live births_Births`, max_decimal_places_used_by_source),

         Rates_Early_neonatal_Deaths = SDGupdater::calculate_valid_rates_per_1000(Numbers_Early_Deaths,
                                                                      `Numbers_Live births_Births`, max_decimal_places_used_by_source),

         # overall neonatal rates are calculated already in the download, so we can check our calcs against these
         Rates_Neonatal_check = SDGupdater::calculate_valid_rates_per_1000(Numbers_Neonatal_Deaths,
                                                               `Numbers_Live births_Births`, max_decimal_places_used_by_source))

number_of_rate_calculation_mismatches <- SDGupdater::count_mismatches(late_neonatal$Rates_Neonatal_check, late_neonatal$Rates_Neonatal_Deaths)

data_in_csv_format <- late_neonatal %>%
  dplyr::select(birthweight, mother_age,
         # Early_neonatal, Late_neonatal, Neonatal,
         Rates_Early_neonatal_Deaths, Rates_Late_neonatal_Deaths, Rates_Neonatal_Deaths) %>%
  tidyr::pivot_longer(
    cols = starts_with("Rates"),
    names_to = "Neonatal_period",
    values_to = "Value")

clean_csv_data_birtweight_by_mum_age <- data_in_csv_format %>%
  dplyr::mutate(Neonatal_period = gsub("Rates_", "", Neonatal_period),
         Neonatal_period = gsub("_Deaths", "", Neonatal_period),
         Neonatal_period = gsub("_", " ", Neonatal_period),
         Neonatal_period = ifelse(Neonatal_period == "Neonatal", "", Neonatal_period),
         mother_age = gsub("-", " to ", mother_age),
         mother_age = gsub("&", " and ", mother_age),
         mother_age = gsub("<", "Less than ", mother_age),
         mother_age = ifelse(mother_age == "Notstated", "Not stated", mother_age),
         mother_age = ifelse(mother_age == "All", "", mother_age),
         mother_age = trimws(mother_age,  which = "both"),
         birthweight = gsub("-", " to ", birthweight),
         birthweight = gsub("<", "Less than ", birthweight),
         birthweight = ifelse(birthweight == "<", "Less than", birthweight),
         birthweight = ifelse(birthweight == "Notstated", "Not stated", birthweight),
         birthweight = ifelse(birthweight == "All", "", birthweight)) %>%
  dplyr::rename(`Neonatal period` = Neonatal_period,
         Age = mother_age,
         Birthweight = birthweight) %>%
  dplyr::mutate(Year = year,
         Sex = "",
         Country = country,
         Region = "",
         `Health board` = "",
         `Unit measure` = "Rate per 1,000 live births",
         `Unit multiplier` = "Units",
         `Observation status` = "Undefined",
         GeoCode = "K04000001",
         Value = ifelse(is.na(Value), "", Value)) %>% # this turns the value into a character string
  dplyr::select(Year, Sex, Country, Region, `Health board`, Birthweight, Age, `Neonatal period`, `Unit measure`, `Unit multiplier`, `Observation status`, GeoCode, Value)

SDGupdater::multiple_year_warning(filename, birthweight_by_mum_age_tab_name,"birthweight by age")
SDGupdater::multiple_country_warning(filename, birthweight_by_mum_age_tab_name,"birthweight by age")

if(number_of_rate_calculation_mismatches != 0){
  warning(paste("check of rate caclulations has failed.",
                number_of_rate_calculation_mismatches, "of", nrow(late_neonatal), "neonatal death rates do not match.",
                "Calculations are performed in the block of code where 'late_neonatal' is created."))
}

# clean up environment as the same names are used for multiple scripts called in the same session
rm(clean_data, main_data,
   data_for_calculations, data_in_csv_format,
   info_cells, late_neonatal, source_data,
   tidy_data,
   country, year, first_header_row,
   max_decimal_places_used_by_source, number_of_rate_calculation_mismatches)
