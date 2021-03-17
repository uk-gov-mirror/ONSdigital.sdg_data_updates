# Author: Emma Wood
# Contact: emma.wood@ons.gov.uk
# Date (start): 19/02/2021
# Purpose: To create csv-format data for 13-2-2, broken down by source sector
# Requirements: This script is called by compile_tables.R, which is called by main.R

source_data <- xlsx_cells(paste0("Input/", filename), sheets = tab_name) 

info_cells <- get_info_cells(source_data, first_header_row)

expected_info_1_present <- grepl(expected_info_1, info_cells$character)
expected_info_2_present <- grepl(expected_info_2, info_cells$character)
expected_info_present <- ifelse(sum(expected_info_1_present) > 0 &
                                  sum(expected_info_2_present) > 0, TRUE, FALSE)

units <- info_cells %>% 
  filter(is.na(Year) & is.na(Country)) %>% 
  select(character) %>% 
  pull()

years <- unique_to_string(info_cells$Year)
latest_year <- max(as.numeric(strsplit(years, ",")[[1]]))
earliest_year <- min(as.numeric(strsplit(years, ",")[[1]]))

main_data <- source_data %>% 
  remove_blanks_and_info_cells(first_header_row) %>%
  mutate(character = remove_superscripts(character))

transport_subsectors <- c("Aviation", "Road", "Railways", "Shipping", "Other mobile")
agriculture_subsectors <- c("Enteric fermentation", "Wastes", "Other (agriculture)")
landuse_subsectors <- c("Forest land", "Cropland", "Grassland", "Wetlands", "Settlements", "Other (LULUCF)")

subsectors <- c(transport_subsectors, agriculture_subsectors, landuse_subsectors)

tidy_data <- main_data %>%
  behead("up", Year) %>%
  behead("left-up", Sector) %>% 
  behead("left", `Sector detail`) %>% 
  select(Year, Sector, `Sector detail`, numeric)

sectors_corrected <- tidy_data %>% 
  mutate(`Sector detail` = ifelse(Sector %in% subsectors, paste(Sector, "-", `Sector detail`), 
                                  as.character(`Sector detail`))) %>% 
  mutate(Sector = case_when(
    Sector %in% transport_subsectors ~ "Transport",
    Sector %in% agriculture_subsectors ~ "Agriculture",
    Sector %in% landuse_subsectors ~ "Land use, land use change and forestry",
    TRUE ~ Sector
  ))

# TODO: This works but it is horrible! Make it nicer.
sector_detail_without_adendum <- sectors_corrected %>% 
  mutate(first_word = word(`Sector detail`, 1),
         last_word = word(`Sector detail`, -1),
         word_count = str_count(`Sector detail`, " ") + 1) %>% 
  mutate(letters_in_last_word = nchar(last_word),
         letter_count = nchar(`Sector detail`)) %>% 
  mutate(end_of_adendum_prefix = letter_count - letters_in_last_word,
         start_of_adendum_prefix = end_of_adendum_prefix - 2) %>% 
  mutate(adendum_prefix = substr(`Sector detail`, start_of_adendum_prefix, end_of_adendum_prefix)) %>% 
  mutate(first_word_equals_last = tolower(first_word) == tolower(last_word) & word_count > 1) %>% 
  mutate(`Sector detail` = ifelse(first_word_equals_last == TRUE & adendum_prefix == " - ",
                                  substr(`Sector detail`, 1, start_of_adendum_prefix - 1),
                                  `Sector detail`))
sector_detail_without_two_word_adendum <- sector_detail_without_adendum %>% 
  mutate(second_word = word(`Sector detail`, 2)) %>% 
  mutate(letters_in_penultimate_word = nchar(first_word)) %>% 
  mutate(end_of_adendum_prefix = letter_count - letters_in_last_word - letters_in_penultimate_word - 1,
         start_of_adendum_prefix = end_of_adendum_prefix - 2) %>% 
  mutate(adendum_prefix = substr(`Sector detail`, start_of_adendum_prefix, end_of_adendum_prefix)) %>% 
  mutate(second_word_equals_last = tolower(second_word) == tolower(last_word) & word_count > 1) %>% 
  mutate(`Sector detail` = ifelse(second_word_equals_last == TRUE & adendum_prefix == " - ",
                                  substr(`Sector detail`, 1, start_of_adendum_prefix - 1),
                                  `Sector detail`)) %>% 
  select(-c(first_word, second_word, last_word,
            word_count, letter_count, letters_in_last_word, letters_in_penultimate_word,
            first_word_equals_last, second_word_equals_last,
            adendum_prefix, start_of_adendum_prefix, end_of_adendum_prefix))

csv_data <- sector_detail_without_two_word_adendum %>% 
  filter(Sector != "Grand Total") %>% # because this is captured in the headline frgures created in 'by_gas_type.R'
  mutate_all(~replace(., is.na(.), "")) %>% 
  mutate(`Sector detail` = ifelse(`Sector detail` == Sector, "", `Sector detail`)) %>% 
  rename(Value = numeric) %>% 
  mutate(Gas = "",
         `Observation status` = "Undefined",
         `Unit multiplier` = "Units",
         `Unit measure` = units)

rm(info_cells, main_data, sector_detail_without_adendum, sectors_corrected, source_data, tidy_data,
   units, years)
