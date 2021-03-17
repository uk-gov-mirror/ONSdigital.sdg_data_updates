# Author: Emma Wood
# Contact: emma.wood@ons.gov.uk
# Date (start): 18/02/2021
# Purpose: To create csv-format headline figures for 13-2-2
# Requirements: This script is called by compile_tables.R, which is called by main.R

source_data <- xlsx_cells(paste0("Input/", filename), sheets = headline_tab_name) 

info_cells <- get_info_cells(source_data, headline_first_header_row)

units <- info_cells %>% 
  filter(is.na(Year) & is.na(Country)) %>% 
  select(character) %>% 
  pull()

years <- unique_to_string(info_cells$Year)
latest_year <- max(as.numeric(strsplit(years, ",")[[1]]))
earliest_year <- min(as.numeric(strsplit(years, ",")[[1]]))

main_data <- source_data %>% 
  remove_blanks_and_info_cells(headline_first_header_row) %>%
  mutate(character = remove_superscripts(character))

tidy_data <- main_data %>%
  behead("up", Year) %>%
  behead("left", Gas) %>% 
  select(Year, Gas, numeric)

headline_csv_data_by_gas <- tidy_data %>% 
  mutate_all(~replace(., is.na(.), "")) %>% 
  rename(Value = numeric) %>% 
  mutate(Gas = ifelse(Gas == "Total greenhouse gas emissions", "", Gas),
         `Sector (end-user)` = "",
         `Sector detail (end-user)` = "",
         `Sector (source)` = "",
         `Sector detail (source)` = "",
         `Observation status` = "Undefined",
         `Unit multiplier` = "Units",
         `Unit measure` = units) %>% 
  select(Year, Gas, 
         `Sector (end-user)`, `Sector detail (end-user)`, `Sector (source)`, `Sector detail (source)`,
         `Observation status`,
         `Unit multiplier`, `Unit measure`,
         Value)

rm(info_cells, main_data, source_data, tidy_data, units, years)
