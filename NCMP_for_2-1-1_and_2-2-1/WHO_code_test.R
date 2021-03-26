###############################################################################################
#
# The user must set the working directory to match the directory of this R script
# setwd() # uncomment this line to set the directory of the downloded script,, + csv files, keeping the folder with the data one directory above
#
#
#NB: The WHO function needs to be run on each year of data separately - so 1 csv file per year
###############################################################################################

# install the package and load it in your R session:
#install.packages("anthro",dependencies=TRUE, type="win.binary") #only need to do this once
library(anthro) #need to run every time you start a new session, if you want to use the package


# reading the data from 2018-19 download of the zipped file from NHS Digital, after extracting
# assuming the data csv is one level up from the current working directory
NCMP_data <- read.csv("../ncmp_1819_final_non_disclosive_published.csv")


# truncating the age in months variable, so 49.9 is classed as 49
# (that is, similarly to rounding age, you are not 30 until 29th year has passed)
NCMP_data$AgeInMonths <- trunc(NCMP_data$AgeInMonths) 

# retaining only age groups of interest (under 5s, exclusing 5)
NCMP_data <- NCMP_data[NCMP_data$AgeInMonths < 60, ]
if((max(NCMP_data$AgeInMonths) > 59) | (min(NCMP_data$AgeInMonths) < 48)){
  print("ERROR! Check AgeInMonths is in expected range (between 48 and 59 months)")}

# clean up suppressions from dataset (not necessary when working with full data set)
clean_NCMP <- NCMP_data[NCMP_data$Height > 0, ]

# remove rows with suppressed imd and perform check that imd is in expected range
clean_NCMP <- clean_NCMP[clean_NCMP$suppress_imd != "Y", ]
if (any(unique(clean_NCMP$schoolindexofmultipledepriv) %in% c(1:10)) == FALSE){
    print("ERROR! Check that schoolindexofmultipledepriv variable ranges from 1 to 10")
  }

#check how many entries were lost after all the cleaning:
# This is just for testing purposes, raw data should need no cleaning
lost_data <- paste("Cases:", nrow(clean_NCMP)-nrow(NCMP_data), ",", 
                   "Percent:", (nrow(NCMP_data)-nrow(clean_NCMP)) / nrow(NCMP_data) * 100) # as above, in %
lost_data

# regroup imd variable from deciles into quintiles to fit the WHO function requirement
clean_NCMP$school_imd_quintile <- ceiling(clean_NCMP$schoolindexofmultipledepriv / 2)

if (any(unique(clean_NCMP$school_imd_quintile) %in% c(1:5)) == FALSE){
  print("ERROR! Check that school_imd_quintile variable ranges from 1 to 5")
}


#main function, takes around 26 mins to run:

#the wealthq argument is fed the regrouped IMD variable and assumes 1 = most deprived, 5 = least deprived
#typeres argument expects urban/rural breakdown variable, 
# which is not available in the test data but is reported to be available in the raw data as "PupilUrbanRuralIndicator"
# schoolgovernmentofficeregion variable is used here as a test for othergr argument (which may take ethnicity from the raw data)
# reformatting this variable into character, in case it's a factor (to avoid function errors)
clean_NCMP$schoolgovernmentofficeregion <- as.character(clean_NCMP$schoolgovernmentofficeregion)

test_NCMP <- anthro_prevalence(sex = clean_NCMP$GenderCode,
                             age = clean_NCMP$AgeInMonths, 
                             is_age_in_month = TRUE, 
                             weight = clean_NCMP$Weight, 
                             lenhei = clean_NCMP$Height, 
                             measure = "H",
                             wealthq = clean_NCMP$school_imd_quintile,
                             othergr = clean_NCMP$schoolgovernmentofficeregion)

#get relevant info only
rownames(test_NCMP) <- test_NCMP$Group #assigning the first column as rownames (makes is cleaner)
test_NCMP <- test_NCMP[, -1] #removing the duplicate column

#lots of NA rows for the non-used function arguments, so getting rid of those:
test_NCMP <- test_NCMP[rowSums(is.na(test_NCMP)) != ncol(test_NCMP), ]

#renaming the default deprivation variable to match dataset:
first_Q <- which(rownames(test_NCMP)=="Wealth quintile: Q1: Poorest")
rownames(test_NCMP)[first_Q:(first_Q + 4)] <- c("School IMD Q1: most deprived", 
                                                "School IMD Q2",
                                                "School IMD Q3","School IMD Q4",
                                                "School IMD Q5: least deprived")


#these are the columns of interest from the function output (based on the function documentation)
cols_to_keep <- c("HAZ_pop","HA_2_r", "WH2_r", "WH_2_r") 
final_result <- test_NCMP[, cols_to_keep] # keeping only the useful columns
#removing obsolete rows (we only have 1 age group, so deleting repetitions)
final_result <- final_result[!(row.names(final_result) %in% c("All", "Sex: Female","Sex: Male")), ]
#renaming the columns to more meaningful things
colnames(final_result) <- c("SampleSize","HeightforAge_-2SD", "WeightforHeight_+2SD", "WeightforHeight_-2SD")

#final result output. IMD is not available as an interaction with other variables
# sample sizes are very small, but this should not be the case for the unsuppressed data
file_year <- 201819 # CHANGE to the year this data refers to
write.csv(final_result, paste("../Target_2.2_", file_year, ".csv", sep=""))

