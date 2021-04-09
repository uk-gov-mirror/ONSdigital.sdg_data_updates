# Code based on R v 4.0.2
# This is an example code for running the prevalence_calc function, generating prevalence of stunting summary tables
# prevalence_calc() takes two arguments: the table with NCMP data and the end year the table refers to (e.g. 2019 for 2018-19).
#
#
# The function needs to be run on each year of data independently (separate csv files per year)
# The data should contain the following variables (columns): 
# AgeinMonths; Height; GenderCode; .......
# Output is a summary table in .csv format for age (<5) and sex (Summary_stunting_all_year.csv)
#
###############################################################################################
#
# The user must set the working directory to match the directory of this script
# The directory must also contain the prevalence_calc.R code and the two csv files relating to WHO growth standards 
# setwd("SET/DIRECTORY/HERE") 
#
###############################################################################################

source("prevalence_calc.R")

# example calls
#
# assuming the data csv is one level up from the current working directory:
data_201819<-read.csv("../ncmp_1819_final_non_disclosive_published.csv")
prevalence_calc(NCMP_data = data_201819, end_year = 2019)

# alternatively, feeding the data directory irectly to the function:
prevalence_calc(NCMP_data = read.csv("../ncmp_1819_final_non_disclosive_published.csv"), end_year = 2019)


# Successful run should generate two csv files one directory up from the current working directory, with a message:
# "CSV outputs generated for year 2019"
  

