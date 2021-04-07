# Author: Emma Wood
# Date: 02/06/2020
# Purpose: An aid for QA-ing numbers and character strings in csv
# Requirements:  This script is called by run_QA_assistant.R. For further requirements see that file or the readme file.

## read in csv and QA figures (laid out in the same way but not necessarily the same order)
csv <- read_excel(paste("//nsdata2/SDGs/Data Collection and Reporting/Jemalex/QA/", Filepath, sep = ""),
                  sheet = csv_sheet, na = "", guess_max = 5000) %>% 
  # rename value column so it can be distinguished from qa value
  rename(csv_Value = Value)

qa <- read_excel(paste("//nsdata2/SDGs/Data Collection and Reporting/Jemalex/QA/", Filepath, sep = ""),
                 sheet = QA_sheet, na = "", guess_max = 5000) %>% 
  # rename value column so it can be distinguished from csv value
  rename(qa_Value = Value)

columns_for_joining <- intersect(names(csv), names(qa)) # gives list of matching names
csv_cols_not_in_qa <- setdiff(setdiff(names(csv), names(qa)),"csv_Value")  # gives non-matching names
qa_cols_not_in_csv <- setdiff(setdiff(names(qa), names(csv)),"qa_Value") # gives non-matching names

match_rows <- csv %>% 
  full_join(qa, by = columns_for_joining) %>% 
  mutate(in_csv_not_qa = ifelse(is.na(qa_Value) & !is.na(csv_Value), TRUE, FALSE),
         in_qa_not_csv = ifelse(is.na(csv_Value) & !is.na(qa_Value), TRUE, FALSE)) %>% 
  mutate(numbers_match = ifelse(qa_Value == csv_Value | is.na(qa_Value) & is.na(csv_Value), TRUE, FALSE))

# create a dataframe of rows that have been repeated 
repeated_rows <- csv %>% 
  group_by_all() %>% 
  mutate(row_num = row_number()) %>% 
  filter(row_num > 1) %>% 
  select(-row_num)

# all the following checks are printed in the summary
num_rows_with_diff_values <- nrow(match_rows[which(match_rows$numbers_match == FALSE), ])
num_rows_in_csv_not_qa <- nrow(match_rows[which(match_rows$in_csv_not_qa == TRUE), ])
num_rows_in_qa_not_csv <- nrow(match_rows[which(match_rows$in_qa_not_csv == TRUE), ])

cat("\n Summary: \n \n", num_rows_in_csv_not_qa, " row(s) in the csv sheet do not appear in the QA data \n",
    num_rows_in_qa_not_csv, " row(s) in the QA sheet do not appear in the csv data \n",
    "and of rows that have matching names in other columns, values do not match on ", num_rows_with_diff_values, " row(s) \n",
    "To investigate these mismatches please use the file '", qa_csv_output_file, "', which has been created in the QA folder for this indicator \n \n",
    "The following columns were used to join the csv to the QA: \n", paste(columns_for_joining, '\n'),
    sep = "")

if(length(csv_cols_not_in_qa) > 0) {
  cat("\n", "The following columns in the csv are not in the QA: \n", paste(csv_cols_not_in_qa, '\n'))
} else {
  cat("\n", "All columns in the csv are also in the QA \n")
}

if(length(qa_cols_not_in_csv) > 0) {
  cat("\n", "The following columns in the QA are not in the csv: \n", paste(qa_cols_not_in_csv, '\n'))
} else {
  "\n All columns in the QA are also in the csv \n"
}

if(nrow(repeated_rows) > 0 & nrow(repeated_rows) <= 50) {
  cat("\n", "There are", nrow(repeated_rows), "rows that have been repeated in the csv file: \n")
  print(repeated_rows)
} else if(nrow(repeated_rows) > 0 & nrow(repeated_rows) > 50) {
  cat("\n", "There are", nrow(repeated_rows), "rows that have been repeated in the csv file. The first 50 are: \n")
  print(repeated_rows, n = 50)
} else if(nrow(repeated_rows) == 0) {
  cat("\n", "There are no repeated rows in the csv \n")
}

write.csv(match_rows, paste("//nsdata2/SDGs/Data Collection and Reporting/Jemalex/QA/", qa_csv_output_file, sep = ""), row.names = FALSE)


