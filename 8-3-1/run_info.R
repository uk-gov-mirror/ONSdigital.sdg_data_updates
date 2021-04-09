
# Create run info file

setwd(output_directory)

low_counts_text <- ifelse(nrow(disaggregations_with_low_counts_compiled) == 0, 
                          "Respondent counts are all greater than 25 (check passed)", 
                          "WARNING: Some respondent counts for one or more disaggregations are low (<= 25) - see table below")
suppressed_data_text <- ifelse(nrow(suppressed_data_compiled) == 0, 
                               "Respondent counts are all greater than 3 (check passed)", 
                               "WARNING: Some respondent counts for one or more disaggregations are lower than 3 and have been suppressed - see table below")
repeat_respondents_text <- ifelse(all(repeat_checks_compiled$repeated_respondents == 0), 
                                  "There were no repeat respondents (check passed)",
                                  "WARNING: There were one or more repeated respondents in one or more years - see table below")

filepath_list <- paste0(filepath, year_filepaths, collapse = "\n")

run_info <- paste0("8-3-1 update \n\n",
                   "Date and time of run: ", run_date_time, "\n",
                   "File(s) used: ", filepath_list, "\n\n",
                   "Checks: \n",
                   low_counts_text, "\n",
                   suppressed_data_text, "\n",
                   repeat_respondents_text, "\n")

cat(run_info, file = run_info_filename, append = FALSE)
cat("\n Disaggregation combinations where one or more counts are 25 or lower:\n\n", file = run_info_filename, append = TRUE)
write.table(disaggregations_with_low_counts_compiled, 
            file = run_info_filename, append = TRUE, sep='\t\t', 
            row.names = FALSE, col.names = TRUE)
cat("\n Rows where count is lower than 3:\n\n", file = run_info_filename, append = TRUE)
write.table(suppressed_data_compiled, 
            file = run_info_filename, append = TRUE, sep='\t\t', 
            row.names = FALSE, col.names = TRUE)
cat("\n Number of times respondents appear in the data (this is a check for repeated rows):\n\n", file = run_info_filename, append = TRUE)
write.table(repeat_checks_compiled, 
            file = run_info_filename, append = TRUE, sep='\t', 
            row.names = FALSE, col.names = TRUE)
