# Author: Varun Jakki and Emma Wood
# Purpose: Data for 8-3-1 needs to be published as an ad-hoc in order for it to be put on the SDG platform. 
# This script takes data created in LFS_updaid_family_workers_by_sector.R and compiled in compile_tables
# and saves it in excel for publication.
# Formatting and removal of respondent count column needs to be done in Excel.

if (min(all_years) == max(all_years)) {
  period <- all_years
} else {
  period <- paste(min(all_years), "to", max(all_years))
}

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
  paste("Created:", run_date_formatted),
  "",
  "Contact: SustainableDevelopment@ons.gov.uk",
  "",
  "Further regional labour market statistics can be found on the ONS website:",
  "All data related to Labour market in the regions of the UK"
))
  

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
          sector_by_country_compiled, 
          startRow = 3, startCol = 2,
          colNames = TRUE, borders = "columns", borderColour = "grey")
writeData(wb, "Sector by country", 
          "Quality indicator", 
          startRow = nrow(sector_by_country_compiled) + 5)
writeData(wb, "Sector by country", 
          "Shaded estimates are based on a small sample size. This may result in less precise estimates, which should be used with caution.", 
          startRow = nrow(sector_by_country_compiled) + 6)
writeData(wb, "Sector by country", 
          "Unshaded estimates are based on a larger sample size. This is likely to result in estimates of higher precision, although they will still be subject to some sampling variability.", 
          startRow = nrow(sector_by_country_compiled) + 7)

# Sector by region
writeData(wb, "Sector by region", 
          "Informal employment in the agricultural and non-agricultural sectors by regions of England", 
          startRow = 1)
writeData(wb, "Sector by region", 
          sector_by_region_compiled,  
          startRow = 3, startCol = 2, 
          colNames = TRUE)
writeData(wb, "Sector by region", 
          "Quality indicator", 
          startRow = nrow(sector_by_region_compiled) + 5)
writeData(wb, "Sector by region", 
          "A dash (-) is given for suppressed estimates where the sample size is 3 or lower", 
          startRow = nrow(sector_by_region_compiled) + 6)
writeData(wb, "Sector by region", 
          "Shaded estimates are based on a small sample size. This may result in less precise estimates, which should be used with caution.", 
          startRow = nrow(sector_by_region_compiled) + 7)
writeData(wb, "Sector by region", 
          "Unshaded estimates are based on a larger sample size. This is likely to result in estimates of higher precision, although they will still be subject to some sampling variability.", 
          startRow = nrow(sector_by_region_compiled) + 8)

# Sector by sex
writeData(wb, "Sector by sex", 
          "Informal employment in the agricultural and non-agricultural sectors by sex", 
          startRow = 1)
writeData(wb, "Sector by sex", 
          sector_by_sex_compiled, 
          startRow = 3, startCol = 2, 
          colNames = TRUE)
writeData(wb, "Sector by sex", 
          "Quality indicator", 
          startRow = nrow(sector_by_sex) + 5)
writeData(wb, "Sector by sex", 
          "Shaded estimates are based on a small sample size. This may result in less precise estimates, which should be used with caution.", 
          startRow = nrow(sector_by_sex_compiled) + 6)
writeData(wb, "Sector by sex", 
          "Unshaded estimates are based on a larger sample size. This is likely to result in estimates of higher precision, although they will still be subject to some sampling variability.", 
          startRow = nrow(sector_by_sex_compiled) + 7)

saveWorkbook(wb, paste0(output_directory, "ad_hoc_", run_date, ".xlsx"), overwrite = TRUE)

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




