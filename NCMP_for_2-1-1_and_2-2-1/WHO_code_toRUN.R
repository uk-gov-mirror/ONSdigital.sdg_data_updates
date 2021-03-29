########################################################################################################
# 
# The code below performs some pre-processing before feeding the data to the WHO function,
# and post-processing before outputting a single csv with the final data for the corresponding year
# NB: The WHO function needs to be run on each year of data separately
#     It is assumed there are no suppressions in the data, 
#     and that non-numerical variables are formatted as characters, not factors
#
# INPUT: .csv file with NCMP data (separate file for each year) with the following variables present:
# [GenderCode]	    Sex of child coded as M and F (or 1 = male, 2 = female)
# [AgeInMonths]	    Age at time of measurement 
# [Ethnicity_desc]	17 ethnic groups, text descriptions e.g.  White - British
# [Height]	        Height in cm 
# [Weight]	        Weight in kg 
# [PupilIndexOfMultipleDeprivationD]	IMD decile of child LSOA (NB: 1 = most deprived)
# [PupilUrbanRuralIndicator]	ONS urban/rural classification based on child LSOA - code
# 
# 
# The output would be the the levels of the variables above as rows, disaggregated on: 
# Sample size (unweighted);	
# Height for Age -2SD from WHO standards = prevalence of stunting;
# Weight for Height +2SD from WHO standrards = prevalence of malnutrition (overweight);
# Weight for Height -2SD from WHO standardss = prevalence of malnutrition (wasting)
# 
# For information on WHO standards: https://www.who.int/tools/child-growth-standards
# WHO R package anthro: https://cran.r-project.org/web/packages/anthro/anthro.pdf
#
# USER INPUT:
#
  file_year <- 201819 # CHANGE to the years this data refers to
  input_filepath <- "D:/change/filepath/ncmp_input_data.csv"
  output_filepath <- "D:/output/filepath"
#
###############################################################################################


# uncomment to install the package and load it in your R session (only need to do this once):
# install.packages("anthro",dependencies=TRUE, type="win.binary") 
library(anthro) #need to run every time you start a new session, if you want to use the package

NCMP_data <- read.csv(input_filepath)

# truncating the age in months variable, so 49.9 is classed as 49
# (that is, similarly to rounding age, you are not 30 until 29th year has passed)
NCMP_data$AgeInMonths <- trunc(NCMP_data$AgeInMonths) 

# retaining only age groups of interest (under 5s, exclusing 5)
NCMP_data <- NCMP_data[NCMP_data$AgeInMonths < 60, ]
if((max(NCMP_data$AgeInMonths) > 59) | (min(NCMP_data$AgeInMonths) < 48)){
  print("ERROR! Check AgeInMonths is in expected range (between 48 and 59 months)")}

# regroup imd variable from deciles into quintiles to fit the WHO function requirement
NCMP_data$pupil_imd_quintile <- ceiling(NCMP_data$PupilIndexOfMultipleDeprivationD / 2)

if(any(unique(NCMP_data$pupil_imd_quintile) %in% c(1:5)) == FALSE){
  print("ERROR! Check that pupil_imd_quintile variable ranges from 1 to 5")
}


# main function, takes around 26 mins to run:

# the wealthq argument is fed the regrouped IMD variable, assuminf 1 = most deprived, 5 = least deprived
# typeres argument expects urban/rural breakdown variable, which was not tested 

SDG_2.2 <- anthro_prevalence(sex = NCMP_data$GenderCode, 
                             age = NCMP_data$AgeInMonths, 
                             is_age_in_month = TRUE, 
                             weight = NCMP_data$Weight, 
                             lenhei = NCMP_data$Height, 
                             measure = "H",
                             wealthq = NCMP_data$pupil_imd_quintile,
                             typeres = NCMP_data$PupilUrbanRuralIndicator,
                             othergr = NCMP_data$Ethnicity_desc)

# get relevant info only
rownames(SDG_2.2)<-SDG_2.2$Group #assigning the first column as rownames (makes is cleaner)
SDG_2.2<-SDG_2.2[,-1] #removing the duplicate column

# Cleaning rows with NAs only from the non-used function arguments
test_NCMP <- SDG_2.2[rowSums(is.na(SDG_2.2)) != ncol(SDG_2.2), ]


# renaming the function's default deprivation variable to match dataset:
first_Q <- which(rownames(SDG_2.2)=="Wealth quintile: Q1: Poorest")
rownames(SDG_2.2)[first_Q:(first_Q + 4)] <- c("School IMD Q1: most deprived", 
                                                "School IMD Q2",
                                                "School IMD Q3","School IMD Q4",
                                                "School IMD Q5: least deprived")


#these are the columns of interest from the function output (based on the function documentation)
cols_to_keep<- c("HAZ_unwpop","HA_2_r", "WH2_r", "WH_2_r") 
final_result<-SDG_2.2[, cols_to_keep] # keeping only the useful columns

#removing obsolete rows (we only have 1 age group, so deleting repetitions)
final_result <- final_result[!(row.names(final_result) %in% c("All", "Sex: Female","Sex: Male")), ]
#renaming the columns to more meaningful concepts
colnames(final_result) <- c("Unweighted sample size", 
                            "Prevalence of stunting", 
                            "Prevalence of malnutrition (overweight)", 
                            "Prevalence of malnutrition (wasting)")
# final result output
write.csv(final_result, paste(output_filepath,"/Target_2.2_", file_year, ".csv", sep=""))

