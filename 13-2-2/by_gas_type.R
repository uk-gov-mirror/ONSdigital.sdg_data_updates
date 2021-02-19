# Author: Emma Wood
# Contact: emma.wood@ons.gov.uk
# Date (start): 18/02/2021
# Purpose: To create csv data for 13-2-2
# Requirements: This script is called by compile_tables.R, which is called by main.R

library(readxl)

# source_data <- read_excel(paste0("Input/", filename), sheet = tab_name, skip = first_header_row-1,
#                           trim_ws = TRUE) 
source_data <- xlsx_cells(paste0("Input/", filename), sheets = tab_name) 

info_cells <- get_info_cells(source_data, first_header_row)

units <- info_cells %>% 
  filter(is.na(Year) & is.na(Country)) %>% 
  select(character) %>% 
  pull()

years <- unique_to_string(info_cells$Year)
latest_year <- max(as.numeric(strsplit(years, ",")[[1]]))

main_data <- source_data %>% 
  remove_blanks_and_info_cells(first_header_row) %>%
  mutate(character = remove_superscripts(character))

tidy_data <- main_data %>%
  behead("up", Year) %>%
  behead("left", Gas) %>% 
  select(Year, Gas, numeric)

csv_data <- tidy_data %>% 
  rename(Value = numeric) %>% 
  mutate(Gas = ifelse(Gas == "Total greenhouse gas emissions", "", Gas),
         Country = "United Kingdom",
         `Observation status` = "Undefined",
         `Unit multiplier` = "Units",
         `Unit measure` = units) %>% 
  select(Year, Country, Gas, 
         `Observation status`,
         `Unit multiplier`, `Unit measure`,
         Value)
