# Author: Varun Jakki and Emma Wood
# Purpose: Data for 8-3-1 needs to be published as an ad-hoc in order for it to be put on the SDG platform. 
# This script takes data created in LFS_updaid_family_workers_by_sector.R and formats it for publication.


publication_data <- for_publication_and_csv %>%
  mutate(Country = ifelse(Country == "" & Region == "", "United Kingdom", Country),
         Sex = ifelse(Sex == "", "Total", Sex),
         Sector = ifelse(Sector == "", "Total", Sector),
         `Number of people in informal employment` = ifelse(is.na(informal_employment), "-", informal_employment),
         `Number of people in employment` = ifelse(is.na(Total_employment), "-", Total_employment),
         `Proportion of employed people in informal employment` = ifelse(Value == "" | is.na(Value), "-", Value)) %>% 
  mutate(Country = ifelse(Region != "", "England", Country)) %>% 
  select(Year, Sector, Country, Region, Sex, GeoCode, 
         `Number of people in employment`, `Number of people in informal employment`, `Proportion of employed people in informal employment`, 
         `Number of respondents`)

sector_by_sex <- publication_data %>%
  filter(Country == "United Kingdom") %>% 
  arrange(Sector, Sex) %>% 
  select(-c(Country, Region, GeoCode, Year))

sector_by_region <- publication_data %>%
  filter(Sex == "Total" & Region != "") %>% 
  arrange(Sector, Region) %>% 
  select(-c(Sex, Country, Year))

sector_by_country <- publication_data %>%
  filter(Region == "" & Sex == "Total") %>% 
  mutate(Country = ifelse(Country == "United Kingdom", "Total", Country),
         Country_order = case_when(
           Country == "England" ~ 1,
           Country == "Northern Ireland" ~ 2,
           Country == "Scotland" ~ 3,
           Country == "Wales" ~ 4,
           Country == "Total" ~ 5)) %>% 
  arrange(Sector, Country_order) %>% 
  select(-c(Region, Sex, Country_order))

period <- paste(min(all_years), "to", max(all_years))

note <- data.frame("Note" = c(
  "IMPORTANT NOTE",
  "",
  "Labour Force Survey (LFS) and Annual Population Survey (APS) responses are weighted to official population projections.", 
  "As the current projections are 2018-based they are based on demographic trends that pre-date the COVID-19 pandemic.", 
  "We are analysing the population totals used in the weighting process and intend to make adjustments where appropriate.", 
  "Rates published from the LFS/APS remain robust; however, levels and changes in levels should be used with caution.", 
  "This will particularly affect estimates for country of birth, nationality, ethnicity and disability."))

info <- data.frame("Info" = c(
  "Office for National Statistics",
  "", 
  "Proportion of informal employment in total employment, by sector (agriculture/ non-agriculture) and sex",
  "These data are used to report indicator 8.3.1 of the Sustainable Development Goals",
  "",
  "Geographical coverage: United Kingdom",
  paste("Period:", period),
  "Source: Annual Population Survey",
  paste("Created:", Sys.Date()),
  "",
  "Contact: SustainableDevelopment@ons.gov.uk",
  "",
  "Further regional labour market statistics can be found on the ONS website:",
  "All data related to Labour market in the regions of the UK"
))
  
  
###################
# KableExtra makes a beautiful table, but it seems it can't be saved to Excel (sigh)
# # install.packages("kableExtra")
# library(kableExtra)
#
# sector_by_country_table <- sector_by_country %>%
#   mutate(`Proportion of employed people in informal employment` = 
#            cell_spec(`Proportion of employed people in informal employment`, 
#                      background = ifelse(`Number of respondents` <= 25, "#EAEAEA", "white")),
#          `Number of people in informal employment` = 
#            cell_spec(`Number of people in informal employment`, 
#                      background = ifelse(`Number of respondents` <= 25, "#EAEAEA", "white")),
#          `Number of people in employment` = 
#            cell_spec(`Number of people in employment`, 
#                      background = ifelse(`Number of respondents` <= 25, "#EAEAEA", "white"))) %>% 
#   select(-`Number of respondents`) %>% 
#   kable(escape = F, booktabs = T) %>%
#   kable_styling() %>% 
#   collapse_rows() %>% 
#   footnote(general = c("Quality indicator",
#                        "Shaded estimates are based on a small sample size. This may result in less precise estimates, which should be used with caution.",
#                        "Unshaded estimates are based on a larger sample size. This is likely to result in estimates of higher precision, although they will still be subject to some sampling variability."))
# 

####
wb <- createWorkbook()

addWorksheet(wb, "Note")
addWorksheet(wb, "Info")
addWorksheet(wb, "Sector by country")
addWorksheet(wb, "Sector by region")
addWorksheet(wb, "Sector by sex")

# Note page
writeData(wb, "Note", 
          startRow = 1,
          note, colNames = FALSE)

# Info page
writeData(wb, "Info", 
          startRow = 1,
          info, colNames = FALSE)

# Sector by Country tab
writeData(wb, "Sector by country", 
          "Informal employment in the agricultural and non-agricultural sectors by country", 
          startRow = 1)
writeData(wb, "Sector by country", 
          sector_by_country, 
          startRow = 3, startCol = 2,
          colNames = TRUE, borders = "columns", borderColour = "grey")
writeData(wb, "Sector by country", 
          "Quality indicator", 
          startRow = nrow(sector_by_country) + 5)
writeData(wb, "Sector by country", 
          "Shaded estimates are based on a small sample size. This may result in less precise estimates, which should be used with caution.", 
          startRow = nrow(sector_by_country) + 6)
writeData(wb, "Sector by country", 
          "Unshaded estimates are based on a larger sample size. This is likely to result in estimates of higher precision, although they will still be subject to some sampling variability.", 
          startRow = nrow(sector_by_country) + 7)

# Sector by region
writeData(wb, "Sector by region", 
          "Informal employment in the agricultural and non-agricultural sectors by regions of England", 
          startRow = 1)
writeData(wb, "Sector by region", 
          sector_by_region,  
          startRow = 3, startCol = 2, 
          colNames = TRUE)
writeData(wb, "Sector by region", 
          "Quality indicator", 
          startRow = nrow(sector_by_region) + 5)
writeData(wb, "Sector by region", 
          "A dash (-) is given for suppressed estimates where the sample size is 3 or lower", 
          startRow = nrow(sector_by_region) + 6)
writeData(wb, "Sector by region", 
          "Shaded estimates are based on a small sample size. This may result in less precise estimates, which should be used with caution.", 
          startRow = nrow(sector_by_region) + 7)
writeData(wb, "Sector by region", 
          "Unshaded estimates are based on a larger sample size. This is likely to result in estimates of higher precision, although they will still be subject to some sampling variability.", 
          startRow = nrow(sector_by_region) + 8)

# Sector by sex
writeData(wb, "Sector by sex", 
          "Informal employment in the agricultural and non-agricultural sectors by sex", 
          startRow = 1)
writeData(wb, "Sector by sex", 
          sector_by_sex, 
          startRow = 3, startCol = 2, 
          colNames = TRUE)
writeData(wb, "Sector by sex", 
          "Quality indicator", 
          startRow = nrow(sector_by_sex) + 5)
writeData(wb, "Sector by sex", 
          "Shaded estimates are based on a small sample size. This may result in less precise estimates, which should be used with caution.", 
          startRow = nrow(sector_by_sex) + 6)
writeData(wb, "Sector by sex", 
          "Unshaded estimates are based on a larger sample size. This is likely to result in estimates of higher precision, although they will still be subject to some sampling variability.", 
          startRow = nrow(sector_by_sex) + 7)

saveWorkbook(wb, "H:/Coding_repos/sdg_data_updates/8-3-1/Output/ad_hoc_test.xlsx", overwrite = TRUE)
# openXL(wb)



