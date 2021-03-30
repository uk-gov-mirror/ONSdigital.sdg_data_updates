csv_publish_by_sector <- csv_to_publish_prep %>%
  filter(Sector %in% c("Agriculture", "Non-Agriculture") & Country == "United Kingdom") %>% 
  select(-c(Region, GeoCode))

csv_publish_by_sector_by_region <- csv_to_publish_prep %>%
  filter(Sector %in% c("Agriculture", "Non-Agriculture") & Country != "United Kingdom")

csv_publish_by_region<- csv_to_publish_prep %>%
  filter(!Sector %in% c("Agriculture", "Non-Agriculture")) %>% 
  select(-Sector)

csv_NRP <- all_data_csv %>% 
  select(-c(`Total employment`, `Informal employment`, `Number of respondents`))
###################

looking into creating excel workbook using openxlsx

wb<- createWorkbook("Varun Jakki")

addWorksheet(wb, "Note", 
             header = c("IMPORTANT NOTE", NA, NA),
             footer = c("ALL FOOT RIGHT 2", "ALL FOOT CENTER 2", "ALL FOOT RIGHT 2"))
addWorksheet(wb, "index" )
addWorksheet(wb, "1 rates", header = c("ODD HEAD LEFT", "ODD HEAD CENTER", "ODD HEAD RIGHT"))
addWorksheet(wb, "2 rates")
addWorksheet(wb, "3 rates")

writeData(wb, "Note")
writeData(wb, "1 rates", csv_publish_by_region, startRow = 3, colNames = TRUE, borders = "columns", borderColour = "black")
writeData(wb)
writeData(wb, "2 rates", csv_publish_by_sector,  startRow = 3, colNames = TRUE, borders = "columns", borderColour = "black")

writeData(wb, "3 rates", csv_publish_by_sector_by_region, startRow = 3, colNames = TRUE, borders = "surrounding", borderColour = "black")
saveWorkbook(wb, "test.xlsx", overwrite = TRUE)
openXL(wb)



