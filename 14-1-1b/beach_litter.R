# first look at 14-1-1 MCS beach litter data
library(tidyr)
library(dplyr)
library(stringr)
# library(tidyxl)
# library(unpivotr)

library(SDGupdater)

indicator <- "14-1-1b" # name of folder for indicator

setwd(paste0("H:/Coding_repos/sdg_data_updates/", indicator))

filename_main_data <- "Copy of BeachwatchData1994-29.11.2020_BrianHurley_ONS.csv"
filename_MSFD_beaches <- "MSFD_beaches.csv"

original_data <- read.csv(paste0("Input/", filename_main_data)) 
MSFD_beaches <- read.csv(paste0("Input/", filename_MSFD_beaches)) 
GBBC_dates <- read.csv("Input/GBBC_dates.csv") %>% 
  mutate(GBBC_date = TRUE)

main_data_clean <- original_data %>% 
  mutate(Beach.Name = as.character(Beach.Name)) %>% 
  mutate(Beach.Name = trimws(str_to_title(Beach.Name), which = "both")) %>% 
  select(-c(X, X.1))

MSFD_beaches_clean <- MSFD_beaches %>% 
  # filter(Subject_to_new_contract == FALSE) %>% 
  mutate(Beach_name = as.character(Beach_name)) %>% 
  mutate(Beach_name = str_to_title(Beach_name))

duplicate_row_check <- main_data_clean %>% 
  mutate(duplicate = duplicated(.)) %>% 
  filter(duplicate == TRUE)

# beach_name_mismatches <- MSFD_beaches_clean %>% 
#   left_join(main_data_clean, by = c("Beach_name" = "Beach.Name")) %>% 
#   filter(is.na(Year))
# # no mismatches
# 
# manual_beach_name_check <- MSFD_beaches_clean %>% 
#   left_join(main_data_clean, by = c("Beach_name" = "Beach.Name")) %>% 
#   mutate(county_matches = ifelse(as.character(Beach_County) == as.character(Beach.County), TRUE, FALSE)) %>% 
#   filter(county_matches == FALSE) %>% 
#   distinct(Beach_name, Beach_County, Beach.County, county_matches)
# # all looks fine

joined_data <- MSFD_beaches_clean %>% 
  select(Beach_name) %>% 
  mutate(MSFD_beach = TRUE) %>% 
  right_join(main_data_clean, by = c("Beach_name" = "Beach.Name")) %>% 
  mutate(MSFD_beach = ifelse(is.na(MSFD_beach), FALSE, MSFD_beach)) %>% 
  left_join(GBBC_dates, by = c("Date.of.Survey" = "Date")) %>% 
  mutate(GBBC_date = ifelse(is.na(GBBC_date), FALSE, GBBC_date))

GBBC_date_selection_comparison <- joined_data %>% 
  mutate(GBBC_date_but_not_window = ifelse(GBBC_date == TRUE & Survey.Window != "GBBC", TRUE, FALSE),
         GBBC_window_but_not_date = ifelse(GBBC_date == FALSE & Survey.Window == "GBBC", TRUE, FALSE)) %>% 
  filter(GBBC_date_but_not_window == TRUE | GBBC_window_but_not_date == TRUE)
# selection of GBBC beaches is the same using both approaches


# check best way to identify MSFD beaches
MSFD_beaches_identified_by_window <- main_data_clean %>% 
  filter(substr(Survey.Window, 1, 3) %in% c("Spr", "Sum", "Aut", "Win")) %>% 
  select(Beach.Name, Year, Survey.Window, Time.Survey.Starts, Time.Survey.Ends, Number.of.volunteers, Length.Surveyed..metres.,
         Date.of.Survey, Date.beach.was.last.cleaned) %>% 
  mutate(by_window = TRUE)
MSFD_beaches_identified <- joined_data %>% 
  filter(MSFD_beach == TRUE &
           Survey.Window != "GBBC") %>% 
  select(Beach_name, Year, Survey.Window, Time.Survey.Starts, Time.Survey.Ends, Number.of.volunteers, Length.Surveyed..metres.,
         Date.of.Survey, Date.beach.was.last.cleaned) %>% 
  mutate(from_valid = TRUE)
Comparison <- full_join(MSFD_beaches_identified_by_window, MSFD_beaches_identified, 
                        by = c("Beach.Name" = "Beach_name", "Year", 
                               "Survey.Window", "Time.Survey.Starts", "Time.Survey.Ends", 
                               "Number.of.volunteers", "Length.Surveyed..metres.",
                               "Date.of.Survey", "Date.beach.was.last.cleaned"))
# identifying MSFD beaches just using the survey window doesn't work as this includes non-MSFD beaches


# # below code used for manual check that GBC dates are correct
# date_counts <- valid_entries %>%
#   group_by(Year, Date.of.Survey) %>% 
#   summarise(number_of_litter_picks = n()) %>% 
#   filter(number_of_litter_picks > 1 & substr(as.character(Date.of.Survey), 4, 5) == "09")



## organiser ID - is this a way of determining GBBC beach cleans?
## I think it is probably best to keep all entries, though some have Organiser ID as 0 -
## TO DO: check with MSC why this is
# ncol(main_data_clean)
# first_count_column <- which(colnames(main_data_clean) == "Plastic...Polystyrene..Yokes..4..or.6.pack.")
# last_count_column <- which(colnames(main_data_clean) == "Faeces..Bagged.dog.faeces")
# valid_entries[, first_count_column:last_count_column] <-
#   apply(valid_entries[, first_count_column:last_count_column], 2, function(x) as.numeric(as.character(x)))

#-----------------------------------------------------------------------------------------------
# --- create clean data sets for analysis ----

valid_entries_for_effort_correction <- joined_data %>% 
  filter(as.character(Time.Survey.Starts) != as.character(Time.Survey.Ends) &
           Number.of.volunteers > 0 &
           Length.Surveyed..metres. > 0)

# remove duplicates
#  as.character(Date.of.Survey) != as.character(Date.beach.was.last.cleaned)


# beach 2612 on 17/09/2017 I think the counts need to be added together (looks like they just forgot 0.5 of a bag and added it as a whole new entry)
GBBC_multiple_entries <- valid_entries_for_effort_correction %>% 
  filter(GBBC_date == TRUE) %>% 
  # filter(OrganiserID != 0) %>%
  group_by(BeachID, Date.of.Survey) %>% 
  summarise(count = n()) %>% 
  filter(count > 1) 
manual_check_multiple_entries <- GBBC_multiple_entries %>% 
  left_join(valid_entries, by = c("BeachID", "Date.of.Survey"))
# These all look like two groups cleaned different bits of the same beach, so can remain in the dataset,
# though we may need to create a single entry for each one?




MSFD_data <- filter(valid_entries, MSFD_beach == TRUE & Survey.Window != "GBBC") 

tidy_data <- valid_entries_for_effort_correction %>% 
  pivot_longer(cols = starts_with(c("Plastic", "Rubber", "Sanitary", "Medical", "Faeces")),
               names_to = "litter_type",
               values_to = "litter_count")
