# first look at 14-1-1 MCS beach litter data
library(tidyr)
library(dplyr)
library(stringr)
# library(tidyxl)
# library(unpivotr)

library(SDGupdater)

indicator <- "14-1-1b" # name of folder for indicator

setwd(paste0("H:/Coding_repos/sdg_data_updates/", indicator))

filename_main_data <- ask_user_for_filename("Input")
filename_MSFD_beaches <- ask_user_for_filename("Input")

original_data <- read.csv(paste0("Input/", filename_main_data)) 
MSFD_beaches <- read.csv("InputMSFD_beaches.csv") 
GBBC_dates <- read.csv("Input/GBBC_dates.csv") %>% 
  mutate(GBBC_date = TRUE)

main_data_clean <- original_data %>% 
  mutate(Beach.Name = as.character(Beach.Name)) %>% 
  mutate(Beach.Name = trimws(str_to_title(Beach.Name), which = "both")) %>% 
  select(-c(X, X.1))

MSFD_beaches_clean <- MSFD_beaches %>% 
  filter(Subject_to_new_contract == FALSE) %>% 
  mutate(Beach_name = as.character(Beach_name)) %>% 
  mutate(Beach_name = str_to_title(Beach_name))

beach_name_mismatches <- MSFD_beaches_clean %>% 
  left_join(main_data_clean, by = c("Beach_name" = "Beach.Name")) %>% 
  filter(is.na(Year))

manual_beach_name_check <- MSFD_beaches_clean %>% 
  left_join(main_data_clean, by = c("Beach_name" = "Beach.Name")) %>% 
  mutate(county_matches = ifelse(as.character(Beach_County) == as.character(Beach.County), TRUE, FALSE)) %>% 
  distinct(Beach_name, Beach_County, Beach.County)

joined_data <- MSFD_beaches_clean %>% 
  select(Beach_name) %>% 
  mutate(MSFD_beach = TRUE) %>% 
  right_join(main_data_clean, by = c("Beach_name" = "Beach.Name")) %>% 
  mutate(MSFD_beach = ifelse(is.na(MSFD_beach), FALSE, MSFD_beach)) %>% 
  left_join(GBBC_dates, by = c("Date.of.Survey" = "Date")) %>% 
  mutate(GBBC_date = ifelse(is.na(GBBC_date), FALSE, GBBC_date))

# ncol(main_data_clean)
# first_count_column <- which(colnames(main_data_clean) == "Plastic...Polystyrene..Yokes..4..or.6.pack.")
# last_count_column <- which(colnames(main_data_clean) == "Faeces..Bagged.dog.faeces")

valid_entries <- joined_data %>% 
  filter(as.character(Time.Survey.Starts) != as.character(Time.Survey.Ends) &
           Number.of.volunteers > 0 &
           Length.Surveyed..metres. > 0 &
           as.character(Date.of.Survey) != as.character(Date.beach.was.last.cleaned))

date_counts <- valid_entries %>%
  group_by(Year, Date.of.Survey) %>% 
  summarise(number_of_litter_picks = n()) %>% 
  filter(number_of_litter_picks > 1 & substr(as.character(Date.of.Survey), 4, 5) == "09")

# beach 2612 on 17/09/2017 I think the counts need to be added together (looks like they just forgot 0.5 of a bag and added it as a whole new entry)


GBBC_multiple_entries <- valid_entries %>% 
  filter(GBBC_date == TRUE) %>% 
  filter(OrganiserID != 0) %>% 
  group_by(BeachID, Date.of.Survey) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)
manual_check_multiple_entries <- GBBC_multiple_entries %>% 
  left_join(valid_entries, by = c("BeachID", "Date.of.Survey"))
# These all look like two groups cleaned different bits of the same beach, so can remain in the dataset,
# though we may need to create a single entry for each one?


  # organiser ID - is this a way of determining GBBC beach cleans?
valid_entries[, first_count_column:last_count_column] <-
  apply(valid_entries[, first_count_column:last_count_column], 2, function(x) as.numeric(as.character(x)))

tidy_data <- valid_entries %>% 
  pivot_longer(cols = starts_with(c("Plastic", "Rubber", "Sanitary", "Medical", "Faeces")),
               names_to = "litter_type",
               values_to = "litter_count")
