# Authors: Emmma Wood, Varun Jakki
# Purpose: Get data from the APS dataset for the following disaggregations:
#             sex, sector (agriculture/non-agriculture), and sector by sex 
# Requires: Access to the LFS_SPSS drive.
#           This file is called by compile_tables.R, and requires config.R 

# About this code: This code creates a CSV for indicator 8.3.1 - The calculations completed are: 
# Proportion of informal employment in total employment = (Informal employment/Total employment) × 100
# Proportion of informal employment in agriculture = (Informal employment in agricultural activities/Total employment in agriculture) × 100
# Proportion of informal employment in non agricultural employment = (Informal employment in non agricultural activities/Total employment in non agricultural activities) × 100

# Code last updated: 22/03/2021



###############
check_for_caseno_repeats <- function(dat) {
  dat %>%
    group_by(CASENO) %>%
    summarise(caseno_count = n(), .groups = 'drop') %>%
    filter(caseno_count > 1)
}

count_respondents <- function(dat, group_var){
  
  dat %>% 
    group_by(across(all_of(group_var))) %>% 
    summarise(count = n(), .groups = 'drop')

}

sum_weights <- function(dat, group_var, new_col) {
  
  new_col <- enquo(new_col)
  new_col_name <- quo_name(new_col)
  
  dat %>% 
    group_by(across(all_of(group_var))) %>% 
    summarise(!!new_col_name := sum(weight), .groups = 'drop') 
}


##########


APS_data <- read_sav(input)

colnames(APS_data) <- toupper(colnames(APS_data)) # column name case varies from year to year

employed <- APS_data %>% 
  select(INECAC05, INDS07M, SEX, PWTA18, CASENO, GOR9D, CTRY9D) %>% 
  rename(industry = INDS07M,
         employment_status = INECAC05,
         weight = PWTA18,
         GeoCode = GOR9D,
         Country = CTRY9D) %>% 
  mutate(Sector = ifelse(industry == 1, "Agriculture", "Non-Agriculture")) %>% 
  filter(!is.na(Sector)) %>%  # removes anyone without an industry e.g. inactive people
  filter(employment_status %in% c(1:4))   # 1 = employed, 2 = self employed, 3 = Government schemes, 4 = unpaid family workers


unpaid_family_workers  <- employed %>% 
  filter(employment_status  == 4) # 4 refers to unpaid family workers

#####################
# get denominators
total_employed_by_sex <- sum_weights(employed, "SEX", Total_employment) 
total_employed_by_country <-  sum_weights(employed, "Country", Total_employment)
total_employed_by_region <-  sum_weights(employed, "GeoCode", Total_employment)
total_employed_by_sector <- sum_weights(employed, "Sector", Total_employment) 

total_employed_by_sector_by_sex <- sum_weights(employed, c("Sector", "SEX"), Total_employment)
total_employed_by_country_by_sex <- sum_weights(employed, c("Country", "SEX"), Total_employment)

total_employed_by_country_by_sector <-  sum_weights(employed, c("Country", "Sector"), Total_employment)
total_employed_by_region_by_sector <-  sum_weights(employed, c("GeoCode", "Sector"), Total_employment)

regional_employed_totals <- bind_rows(total_employed_by_country, total_employed_by_region, 
                                        total_employed_by_country_by_sex, 
                                        total_employed_by_country_by_sector, total_employed_by_region_by_sector) %>% 
  filter(!GeoCode %in% c("N99999999", "S99999999", "W99999999")) %>% 
  mutate(GeoCode = coalesce(GeoCode, Country)) %>% 
  select(-Country)

denominators <- bind_rows(total_employed_by_sex, total_employed_by_sector, total_employed_by_sector_by_sex, regional_employed_totals)

# get numerators
informal_employed_by_sex <- sum_weights(unpaid_family_workers, "SEX", informal_employment) 
informal_employed_by_country <- sum_weights(unpaid_family_workers, "Country", informal_employment)
informal_employed_by_region <- sum_weights(unpaid_family_workers, "GeoCode", informal_employment)
informal_employed_by_sector <- sum_weights(unpaid_family_workers, "Sector", informal_employment)

informal_employed_by_sector_by_sex <- sum_weights(unpaid_family_workers, c("Sector", "SEX"), informal_employment) 
informal_employed_by_country_by_sex <- sum_weights(unpaid_family_workers, c("Country", "SEX"), informal_employment)

informal_employed_by_country_by_sector <- sum_weights(unpaid_family_workers, c("Country", "Sector"), informal_employment)
informal_employed_by_region_by_sector <- sum_weights(unpaid_family_workers, c("GeoCode", "Sector"), informal_employment)

regional_unpaid_family_workers_totals <- bind_rows(informal_employed_by_country, informal_employed_by_region, 
                                                   informal_employed_by_country_by_sex, 
                                                   informal_employed_by_country_by_sector, informal_employed_by_region_by_sector) %>% 
  filter(!GeoCode %in% c("N99999999", "S99999999", "W99999999")) %>% 
  mutate(GeoCode = coalesce(GeoCode, Country)) %>% 
  select(-Country)

numerators <- bind_rows(informal_employed_by_sex, informal_employed_by_sector, informal_employed_by_sector_by_sex, regional_unpaid_family_workers_totals)

# Join data and do calculations 
disaggregation_data_for_calculations <- numerators %>% 
  left_join(denominators, by = c("SEX", "Sector", "GeoCode")) 

headline_data_for_calculations <- disaggregation_data_for_calculations %>% 
  filter(is.na(GeoCode) & is.na(SEX)) %>% 
  summarise(Total_employment = sum(Total_employment),
            informal_employment = sum(informal_employment), .groups = 'drop')

all_data <- bind_rows(headline_data_for_calculations, disaggregation_data_for_calculations) %>% 
  mutate(Value = (informal_employment/Total_employment)*100) 

# count number of respondents 
sector_counts <- count_respondents(unpaid_family_workers, "Sector")
sex_counts <- count_respondents(unpaid_family_workers, "SEX")
sex_by_sector_counts <- count_respondents(unpaid_family_workers, c("SEX", "Sector"))

region_counts <- count_respondents(unpaid_family_workers, "GeoCode")
region_by_sector_counts <- count_respondents(unpaid_family_workers, c("GeoCode", "Sector"))

country_counts <- count_respondents(unpaid_family_workers, "Country")
country_by_sector_counts <- count_respondents(unpaid_family_workers, c("Country", "Sector"))
country_by_sex_counts <- count_respondents(unpaid_family_workers, c("Country", "SEX"))

total_count <- summarise(unpaid_family_workers, count = n())

all_counts <- bind_rows(sector_counts, sex_counts, sex_by_sector_counts,
                        region_counts, region_by_sector_counts,
                        country_counts, country_by_sector_counts, country_by_sex_counts,
                        total_count) %>% 
  filter(!GeoCode %in% c("N99999999", "S99999999", "W99999999")) 

all_counts_one_geography <- all_counts %>% 
  mutate(GeoCode = coalesce(GeoCode, Country)) %>% 
  select(-Country)

# add count information and suppress low counts
quality_control <- all_data %>% 
  left_join(all_counts_one_geography, by = c("SEX", "Sector", "GeoCode")) %>% 
  rename(`Number of respondents` = count) %>% 
  mutate(`Number of respondents` = ifelse(`Number of respondents` < 3, 
                                          "suppressed", as.character(`Number of respondents`)),
         Value = ifelse(`Number of respondents` == "suppressed", NA, Value),
         Total_employment = ifelse(`Number of respondents` == "suppressed", NA, Total_employment),
         informal_employment = ifelse(`Number of respondents` == "suppressed", NA, informal_employment))

for_publication_and_csv <- quality_control %>%
  mutate(Year = year, # year is defined in compile_tables
         Sector = ifelse(is.na(Sector), "", Sector),
         GeoCode = ifelse(is.na(GeoCode), "", GeoCode),
         # Value = ifelse(is.na(Value), "", as.character(Value)),
         Sex = case_when(
           SEX == 1 ~ "Male",
           SEX == 2 ~ "Female",
           is.na(SEX) ~ ""),
         Region = case_when(
           GeoCode == "E12000001" ~ "North East",
           GeoCode == "E12000002" ~ "North West",
           GeoCode == "E12000003" ~ "Yorkshire and The Humber",
           GeoCode == "E12000004" ~ "East Midlands",
           GeoCode == "E12000005" ~ "West Midlands",
           GeoCode == "E12000006" ~ "East of England",
           GeoCode == "E12000007" ~ "London",
           GeoCode == "E12000008" ~ "South East",
           GeoCode == "E12000009" ~ "South west",
           TRUE ~ ""), # In a case_when this final "TRUE" translates as "all other cases"
         Country = case_when(
           GeoCode == "W92000004" ~ "Wales",
           GeoCode == "S92000003" ~ "Scotland",
           GeoCode == "N92000002" ~ "Northern Ireland",
           GeoCode == "E92000001" ~ "England",
           TRUE ~ "")) %>% 
  select(-SEX)

# final table for current year for indicator csv
csv <- for_publication_and_csv %>%
  mutate(`Unit measure` = "Percentage (%)",
         `Unit multiplier`= "Units",
         `Observation status`= "Undefined") %>%
  select(Year,Country, Region, Sex, Sector, 
         `Observation status`, `Unit multiplier`, `Unit measure`, GeoCode, Value)

#### publication ###
publication_data <- for_publication_and_csv %>%
  mutate(Country = ifelse(Country == "" & Region == "", "United Kingdom", Country),
         Geocode = ifelse(Country == "United Kingdom", "K02000001", GeoCode),
         Sex = ifelse(Sex == "", "Total", Sex),
         Sex = ifelse(Sex == "Male", "Men", Sex),
         Sex = ifelse(Sex == "Female", "Women", Sex),
         Sector = ifelse(Sector == "", "Total", Sector),
         `Number of people in informal employment` = ifelse(is.na(informal_employment), "-", informal_employment),
         `Number of people in employment` = ifelse(is.na(Total_employment), "-", Total_employment),
         `Proportion of employed people in informal employment` = ifelse(Value == "" | is.na(Value), "-", Value)) %>% 
  mutate(Country = ifelse(Region != "", "England", Country)) %>% 
  select(Year, Sector, Country, Region, Sex, GeoCode, 
         `Number of people in employment`, `Number of people in informal employment`, `Proportion of employed people in informal employment`, 
         `Number of respondents`)

# final tables for ad-hoc
sector_by_sex <- publication_data %>%
  filter(Country == "United Kingdom") %>% 
  arrange(Sector, Sex) %>% 
  select(-c(Country, Region, GeoCode))

sector_by_region <- publication_data %>%
  filter(Sex == "Total" & Region != "") %>% 
  arrange(Sector, GeoCode) %>% 
  select(-c(Sex, Country))

sector_by_country <- publication_data %>%
  filter(Region == "" & Sex == "Total") %>% 
  mutate(Country_order = case_when(
           Country == "England" ~ 1,
           Country == "Northern Ireland" ~ 2,
           Country == "Scotland" ~ 3,
           Country == "Wales" ~ 4,
           Country == "United Kingdom" ~ 5)) %>% 
  arrange(Sector, Country_order) %>% 
  select(-c(Region, Sex, Country_order))

country_by_sex <- publication_data %>%
  filter(Region == "" & Sex != "Total" & Sector == "Total") %>% 
  mutate(Country_order = case_when(
    Country == "England" ~ 1,
    Country == "Northern Ireland" ~ 2,
    Country == "Scotland" ~ 3,
    Country == "Wales" ~ 4,
    Country == "United Kingdom" ~ 5)) %>% 
  arrange(Country_order, Sex) %>% 
  select(-c(Region, Country_order, Sector))


#### Checks

not_all_na <- function(x) {any(!is.na(x))}

repeat_check_employed <- check_for_caseno_repeats(employed)
repeat_check_unpaid <- check_for_caseno_repeats(unpaid_family_workers)

low_counts <- all_counts %>% 
  filter(count <= 25) %>% 
  select_if(not_all_na)

disaggregations_with_low_counts <- low_counts %>% 
  select(-count) %>% 
  mutate(SEX = ifelse(is.na(SEX), "0", "Sex")) %>%
  mutate(Sector = ifelse(is.na(Sector), "0", "Sector")) %>%
  mutate(GeoCode = ifelse(is.na(GeoCode), "0", "Region")) %>%
  mutate(Country = ifelse(is.na(Country), "0", "Country")) %>% 
  mutate(disaggregation = paste(Country, "by", GeoCode, "by", Sector, "by", SEX)) %>% 
  mutate(disaggregation = gsub("0 by ", "", disaggregation)) %>% 
  mutate(disaggregation = gsub(" by 0", "", disaggregation)) %>% 
  distinct() %>% 
  select(disaggregation) %>% 
  mutate(year = substr(year_filepath, 1, 4))

suppressed_data <- quality_control %>% 
  filter(`Number of respondents` == "suppressed") %>%
  mutate(Year = substr(year_filepath, 1, 4)) %>% 
  select_if(not_all_na) %>% 
  select(-`Number of respondents`)

  




