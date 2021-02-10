# Author: Emma Wood
# Date: 26/01/2021
# Purpose: To create csv data for birthweight: mother age disaggregations for indicator 3.2.2
# Requirements: This script is called by main.R


source("functions.R")

is_table_name_correct <- menu(c("Yes", "No"), 
                              title = paste("Are data for birthweight by mother age in", birthweight_by_mum_age_tab_name, "(please type relevant number and hit enter)"))

if(is_table_name_correct == 2) {
  stop("Please enter correct tab name between the quote marks after 'birthweight_by_mum_age_tab_name <- ', then re-run the script")
} 

print(paste("reading in", filename, "from", getwd()))

source_data <- xlsx_cells(filename, sheets = birthweight_by_mum_age_tab_name)

possible_geographies <- c("England", "Wales", "Scotland", "Northern Ireland", "UK", "United Kingdom")

print("getting country and year")
info_cells <- source_data %>%
  filter(row %in% 1:(first_header_row_birthweight_by_mum_age - 1)) %>%
  distinct(character) %>%
  filter(!is.na(character)) %>%
  mutate(character = trimws(character,  which = "both")) %>%
  mutate(Year = ifelse(substr(character, nchar(character) - 3, nchar(character) - 2) == "20",
                       substr(character, nchar(character) - 3, nchar(character)),
                       NA)) %>%
  mutate(Country = ifelse(grepl(possible_geographies, character, fixed = TRUE), character, NA))

Years <- filter(info_cells, !is.na(Year)) %>% 
  select(Year)
Years_list <- as.list(unique(Years))

Countries <- filter(info_cells, !is.na(Country)) %>% 
  select(Country)
Countries_list <- as.list(unique(Countries))

Country <- str_c(Countries_list, collapse = " ")

Year <- Years_list[[1]]

print("tidying data") 
superscript_regex_codes <- "[\u{2070}\u{00B9}\u{00B2}\u{00B3}\u{2074}-\u{2079}]"

all_letters <- c(LETTERS, letters)

tidy_data <- source_data %>%
  mutate(character = ifelse(substr(character, 1, 1) %in% all_letters, 
                            gsub(superscript_regex_codes, '', character), 
                            character)) %>% # because we dont want to keep superscripts
  mutate(character = ifelse(substr(character, 1, 1) %in% all_letters, 
                            gsub("[1-9]", '', character), 
                            character)) %>%  # because sometimes superscripts seem to be read as just plain numbers
  mutate(is_blank = ifelse(character == "" & data_type == "character", TRUE, is_blank)) %>%
  filter(is_blank == FALSE & row %not_in% 1:(first_header_row_birthweight_by_mum_age - 1)) %>%
  behead("left-up", birthweight) %>%
  behead("left", mother_age) %>%
  behead("up-left", measure) %>%
  behead("up-left", event) %>%
  behead("up", baby_age) %>% 
  filter(!is.na(numeric)) %>% 
  mutate(birthweight = trimws(birthweight,  which = "both")) %>% 
  mutate(birthweight = ifelse(birthweight == "4000 and" | birthweight == "over",
                              "4000 and over", birthweight)) %>% 
  select(birthweight, mother_age, measure, event, baby_age,
         numeric)

data_for_calculations <- tidy_data %>% 
  pivot_wider(names_from = c(measure, baby_age, event), 
              values_from = numeric) 

# should count dp and do to that number here

print("calculating late neonatal deaths")
late_neonatal <- data_for_calculations %>% 
  mutate(Numbers_Late_neonatal_Deaths = Numbers_Neonatal_Deaths - Numbers_Early_Deaths) %>% 
  mutate(Rates_Late_neonatal_Deaths = ifelse(Numbers_Late_neonatal_Deaths > 3, 
                                             round(Numbers_Late_neonatal_Deaths / (`Numbers_Live births_Births` / 1000), 1), # calculating late neonatal
                                             NA),
         Rates_Early_neonatal_Deaths = ifelse(Numbers_Early_Deaths > 3, 
                                              round(Numbers_Early_Deaths / (`Numbers_Live births_Births` / 1000), 1), # calculating early neonatal
                                              NA),
         # overall neonatal rates are calculated already in the download, so we can check our calcs against these
         Rates_Neonatal_check = ifelse(Numbers_Neonatal_Deaths > 3,
                                       round(Numbers_Neonatal_Deaths / (`Numbers_Live births_Births` / 1000), 1),
                                       NA))

check_rate_calcs <- late_neonatal %>% 
  mutate(rate_calcs_comparison = ifelse(Rates_Neonatal_check == Rates_Neonatal_Deaths, 0, 1)) %>% 
  summarise(check_rate_calcs = sum(rate_calcs_comparison, na.rm = TRUE))
check_rate_calcs_pass <- ifelse(check_rate_calcs[1, 1] == 0, TRUE, FALSE)

print("formatting data")
data_in_csv_format <- late_neonatal %>% 
  select(birthweight, mother_age, 
         # Early_neonatal, Late_neonatal, Neonatal, 
         Rates_Early_neonatal_Deaths, Rates_Late_neonatal_Deaths, Rates_Neonatal_Deaths) %>% 
  pivot_longer(
    cols = starts_with("Rates"),
    names_to = "Neonatal_period",
    values_to = "Value")  

clean_csv_data <- data_in_csv_format %>% 
  mutate(Neonatal_period = gsub("Rates_", "", Neonatal_period),
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
  rename(`Neonatal period` = Neonatal_period,
         Age = mother_age,
         Birthweight = birthweight) %>% 
  mutate(Year = Year,
         Sex = "",
         Country = Country,
         Region = "",
         `Health board` = "",
         `Unit measure` = "Rate per 1,000 live births",
         `Unit multiplier` = "Units", 
         `Observation status` = "Undefined",
         GeoCode = "K04000001",
         Value = ifelse(is.na(Value), "", Value)) %>% # this turns the value into a character string
  select(Year, Sex, Country, Region, `Health board`, Birthweight, Age, `Neonatal period`, `Unit measure`, `Unit multiplier`, `Observation status`, GeoCode, Value)

print("saving data")
setwd('./Output')
write.csv(clean_csv_data, paste0("birthweight_by_mother_age_for_csv_", Year, ".csv"), row.names = FALSE)

current_directory <- getwd()
print(paste0("Birthweight by mother age data have been created and formatted for ", Year, ", and saved in '", current_directory, 
             "' as 'birthweight_by_mother_age_for_csv_", Year, ".csv'"))

if (length(Countries_list) > 1) {
  message("Code has identified more than one geography, where only one was expected. Please check that Country is correct in the output")
} 

if (length(Years_list) > 1) {
  message("Code has identified more than one year, where only one was expected. Please check that Year is correct in the output")
}

if(check_rate_calcs_pass == FALSE){
  message(paste("check of rate caclulations has failed.", 
                check_rate_calcs[1, 1], "of", nrow(late_neonatal), "neonatal death rates do not match.",
                "Calculations are performed in the block of code where 'late_neonatal' is created."))
}

rm(check_rate_calcs, check_rate_calcs_pass,
   clean_csv_data, Countries,
   data_for_calculations, data_in_csv_format, 
   info_cells, late_neonatal, source_data,
   tidy_data, Years)

setwd('..')

