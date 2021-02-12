# Author: Emma Wood
# Date (start): 29/01/2021
# Purpose: To create csv data for country of occurrence: baby sex disaggregations for indicator 3.2.2
# Requirements:This script is called by compile_tables.R, which is called by main.R

country_of_occurrence_by_sex_tab_name <- ask_user_for_tab_name("country of occurrence by sex", country_of_occurrence_by_sex_tab_name)
first_header_row <- ask_user_for_first_header_row(country_of_occurrence_by_sex_tab_name, first_header_row_country_by_sex)

source_data <- xlsx_cells(paste0("Input/", filename), sheets = country_of_occurrence_by_sex_tab_name)

info_cells <- get_info_cells(source_data, first_header_row)
year <- unique_to_string(info_cells$Year)
country <- unique_to_string(info_cells$Country)

main_data <- source_data %>% 
  remove_blanks_and_info_cells(first_header_row) %>%
  mutate(character = remove_superscripts(character))

tidy_data <- main_data %>%
  behead("left-up", area_code) %>%
  behead("left-up", country) %>%
  behead("left", sex) %>%
  behead("up-left", measure) %>%
  behead("up-left", rate_type) %>% 
  behead("up-left", life_event_age) %>% 
  behead("up", baby_age) %>% 
  select(area_code, country, sex, measure, rate_type, life_event_age, baby_age,
         numeric)

clean_data <- tidy_data %>% 
  filter(!is.na(numeric)) %>% # to remove cells that are just ends of a header that have run on to the next row
  mutate(country = trimws(country,  which = "both"),
         sex = trimws(sex,  which = "both")) 

data_for_calculations <- clean_data %>% 
  pivot_wider(names_from = c(measure, rate_type, life_event_age, baby_age), 
              values_from = numeric) 

# TO DO: write function to remove NAs from column headings

# In 2018 data 'Numbers' heading hasn't been pulled all the way to the left, so Births aren't included under that heading.
if ("NA_NA_Births_Live births" %in% colnames(data_for_calculations)) {
  headings_standardised <- rename(data_for_calculations, `Numbers_NA_Births_Live births` = `NA_NA_Births_Live births`)
} else {
  headings_standardised <- data_for_calculations
}

max_decimal_places_used_by_source <- count_max_decimal_places(data_for_calculations$`Rates_Per 1,000  live births_Childhood deaths_Neonatal`) 

late_neonatal <- headings_standardised %>% 
  mutate(Numbers_Deaths_Late_neonatal = `Numbers_NA_Deaths under 1_Neonatal` - `Numbers_NA_Deaths under 1_Early`) %>% 
  mutate(Rates_Late_neonatal = calculate_valid_rates_per_1000(Numbers_Deaths_Late_neonatal, 
                                                        `Numbers_NA_Births_Live births`, max_decimal_places_used_by_source),
         # overall neonatal rates are calculated already in the download, so we can check our calcs against these
         Rates_Neonatal_check = calculate_valid_rates_per_1000(`Numbers_NA_Deaths under 1_Neonatal`, 
                                                         `Numbers_NA_Births_Live births`, max_decimal_places_used_by_source))

number_of_rate_calculation_mismatches <- count_mismatches(late_neonatal$Rates_Neonatal_check, late_neonatal$`Rates_Per 1,000  live births_Childhood deaths_Neonatal`)

relevant_columns <- late_neonatal %>% 
  select(country, sex, area_code,
         `Rates_Per 1,000  live births_Childhood deaths_Early`,
         Rates_Late_neonatal,
         `Rates_Per 1,000  live births_Childhood deaths_Neonatal`)

data_in_csv_format <- relevant_columns %>% 
  pivot_longer(
    cols = starts_with("Rates"),
    names_to = "Neonatal_period",
    values_to = "Value")  

clean_csv_data_country_by_sex <- data_in_csv_format %>% 
  mutate(Neonatal_period = case_when(
    Neonatal_period == "Rates_Per 1,000  live births_Childhood deaths_Early" ~ "Early neonatal",
    Neonatal_period == "Rates_Late_neonatal" ~ "Late neonatal",
    Neonatal_period == "Rates_Per 1,000  live births_Childhood deaths_Neonatal" ~ ""),
    sex = ifelse(sex == "All", "", sex)) %>% 
  rename(`Neonatal period` = Neonatal_period,
         Sex = sex,
         Country = country,
         GeoCode = area_code) %>% 
  mutate(Year = year,
         Birthweight = "",
         Age = "",
         Region = "",
         `Health board` = "",
         Country = ifelse(Country == "United Kingdom", "", Country),
         Sex = ifelse(Sex == "P", "", Sex),
         `Unit measure` = "Rate per 1,000 live births",
         `Unit multiplier` = "Units", 
         `Observation status` = "Undefined",
         Value = ifelse(is.na(Value), "", as.character(Value))) %>% # this turns the value into a character string
  select(Year, Sex, Country, Region, `Health board`, Birthweight, Age, `Neonatal period`, `Unit measure`, `Unit multiplier`, `Observation status`, GeoCode, Value)


multiple_year_warning(filename, country_of_occurrence_by_sex_tab_name,"country of occurrence by sex")
multiple_country_warning(filename, country_of_occurrence_by_sex_tab_name,"country of occurrence by sex")

if(number_of_rate_calculation_mismatches != 0){
  warning(paste("check of rate caclulations has failed.", 
                number_of_rate_calculation_mismatches, "of", nrow(late_neonatal), "neonatal death rates do not match.",
                "Calculations are performed in the block of code where 'late_neonatal' is created."))
} 

# clean up environment as the same names are used for multiple scripts called in the same session
rm(clean_data, main_data,
   data_for_calculations, data_in_csv_format, 
   headings_standardised, 
   info_cells, late_neonatal, source_data,
   tidy_data, relevant_columns,
   country, year, first_header_row,
   max_decimal_places_used_by_source, number_of_rate_calculation_mismatches)


