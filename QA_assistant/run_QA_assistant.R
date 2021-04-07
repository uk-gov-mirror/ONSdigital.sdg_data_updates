# Author: Emma Wood
# Date: 02/06/2020
# Purpose: to QA numbers in csv
# Description:  -This script allows easy matching of numbers downloaded by the QA person to the
#                QA sheet in the in progress indicator file. This is particularly useful when there are many 
#                rows of data and the order is complex.
#               -Using this script makes it easier to pick up spelling errors in the csv as it will
#                only align the QA row with the csv row if the strings match exactly.
#               -It also identifies any duplicate rows in the csv.
#
# Requirements: It requires that the QA data has the same column headings and that string values 
#                (e.g. in 3.b.1 the names of vaccines) match exactly (including capitalisation). 
#                If in the csv, Year is "2017 to 18", the qa must be in the same format not e.g."2017/18" (use find replace on the column in Excel) 
#                Values must be to the same number of decimal places (which should be the maximum available in the source).
#
# Troubleshooting: If you get an error similar to 'Error: Can't join on 'Age' x 'Age' because of incompatible types (logical / character)',
#                  this is probably because the specified column is blank in your qa file. To remedy, either add a dummy row
#                  to the qa sheet or copy over just the data you want to qa onto a new csv sheet (and change the csv_sheet name below)
#
#                 If you get a warning in red that includes something like 'In addition: There were 50 or more warnings (use warnings() to see the first 50)'
#                 this may be because you have something that is not a number in a column that should be read as a number, e.g. there may be a '..' or
#                 '#' in cells where values have been suppressed, or the thousands separator may have been used.
#                 Edit to be a number without the thousands separator or blank in the excel file and try again
#
# Instructions: 1) Edit the file/sheet names below 
#               2) If this is the first time you have run the script and you have not yet installed the required packages
#                  remove the # at the beginning of the first four rows
#               3) Click 'Source' in the top right of this window to run the script


# install.packages(tidyr, dependencies = TRUE)
# install.packages(dplyr, dependencies = TRUE)
# install.packages(readxl, dependencies = TRUE)
# install.packages(stringr, dependencies = TRUE)

rm(list = ls())

# Enter location of file and name of QA sheet (do not include any of the filepath up to the QA folder)
Filepath <- "3.3.1/3.3.1_QA_2020.xlsx" # location and name of the QA xlsx in the QA folder in the following format: "name of folder/name of file.xlsx"
QA_sheet <- "3_3_1_data_for_R_2020" # sheet name of QA data
csv_sheet <- "CSV" # sheet name of CSV - should nearly always be "CSV" (though see note in troubleshooting)
qa_csv_output_file <- "3.3.1/csv_checks_EW_2020.csv" # the name of the spreadsheet that will be output showing checks in the following format: "name of folder/name of file.xlsx"

####################################################################################################################
# Do not edit code below this point
####################################################################################################################

library(tidyr)
library(dplyr)
library(readxl)
library(stringr)

setwd("//nsdata2/SDGs/Data Collection and Reporting/Jemalex/Extraction tools and macros/R") # location of QA_assistant.R
source("QA_assistant.R")
