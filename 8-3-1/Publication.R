csv_publish_by_sector <- csv_to_publish_prep %>%
  filter(Sector %in% c("Agriculture", "Non-Agriculture") & Country == "United Kingdom")

csv_publish_by_sector_by_region <- csv_to_publish_prep %>%
  filter(Sector %in% c("Agriculture", "Non-Agriculture") & Country != "United Kingdom")

csv_publish_by_region<- csv_to_publish_prep %>%
  filter(!Sector %in% c("Agriculture", "Non-Agriculture"))

csv_NRP <- all_data_csv %>% 
  select(-c(`Total employment`, `Informal employment`, `Number of respondents`))