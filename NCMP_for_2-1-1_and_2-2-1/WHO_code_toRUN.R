###############################################################################################
# 
# The user must set the working directory to match the directory of this R script
#
# setwd() #! uncomment this line to set the directory of the downloded script.
#
# input: .csv file with NCMP data (separate file for each year) with the following variables present:
# [GenderCode]	    Sex of child coded as M and F
# [AgeInMonths]	    Age at time of measurement 
# [Ethnicity_desc]	17 ethnic groups, text descriptions e.g.  White - British
# [Height]	        Height in cm 
# [Weight]	        Weight in kg 
# [PupilIndexOfMultipleDeprivationD]	IMD decile of child LSOA
# [PupilUrbanRuralIndicator]	ONS urban/rural classification based on child LSOA - code
#
# 
# NB: The WHO function needs to be run on each year of data separately
# The code below performs some pre-processing before feeding the data to the WHO function,
# and post-processing before outputting a single csv with the final data for the corresponding year
# The output file is located one directory up from the current working dir.
###############################################################################################


# install the package and load it in your R session (only need to do this once):
# install.packages("anthro",dependencies=TRUE, type="win.binary") 
library(anthro) #need to run every time you start a new session, if you want to use the package

NCMP_data <- read.csv("../NCMP_data.csv") #change file name and location accordingly

# rounding/truncating the age in months variable
NCMP_data$AgeInMonths <- trunc(NCMP_data$AgeInMonths) 

# retaining only age groups of interest (under 5s, exclusing 5)
NCMP_data <- NCMP_data[NCMP_data$AgeInMonths<60, ]


# regroup imd variable from deciles into quintiles (pair in twos)
# overwrite 2s with 1, 4s and 3s with 2, 5s and 6s with 3, 7s and 8s with 4, and 9s and 10s with 5
NCMP_data$PupilIndexOfMultipleDeprivationD <- ifelse(NCMP_data$PupilIndexOfMultipleDeprivationD==1, 1, 
                                                 ifelse(NCMP_data$PupilIndexOfMultipleDeprivationD==2, 1, 
                                                 ifelse(NCMP_data$PupilIndexOfMultipleDeprivationD==3, 2,
                                                 ifelse(NCMP_data$PupilIndexOfMultipleDeprivationD==4, 2,
                                                 ifelse(NCMP_data$PupilIndexOfMultipleDeprivationD==5, 3,
                                                 ifelse(NCMP_data$PupilIndexOfMultipleDeprivationD==6, 3,
                                                 ifelse(NCMP_data$PupilIndexOfMultipleDeprivationD==7, 4,
                                                 ifelse(NCMP_data$PupilIndexOfMultipleDeprivationD==8, 4,
                                                 ifelse(NCMP_data$PupilIndexOfMultipleDeprivationD==9, 5, 
                                                 ifelse(NCMP_data$PupilIndexOfMultipleDeprivationD==10, 5, NA))))))))))

unique(NCMP_data$schoolindexofmultipledepriv) # re-checking the range is as expected (1:5)


#main function, takes around 26 mins to run:

# the wealthq argument is fed the regrouped IMD variable and assumes 1 = most deprived, 5 = least deprived
# typeres argument expects urban/rural breakdown variable, which was not tested 

SDG_2.2<-anthro_prevalence(sex = NCMP_data$GenderCode, 
                             age = NCMP_data$AgeInMonths, 
                             is_age_in_month = T, 
                             weight = NCMP_data$Weight, 
                             lenhei = NCMP_data$Height, 
                             measure = "H",
                             wealthq = NCMP_data$PupilIndexOfMultipleDeprivationD,
                             typeres = NCMP_data$PupilUrbanRuralIndicator,
                             othergr = NCMP_data$Ethnicity_desc)

#get relevant info only
rownames(SDG_2.2)<-SDG_2.2$Group #assigning the first column as rownames (makes is cleaner)
SDG_2.2<-SDG_2.2[,-1] #removing the duplicate column

# Cleaning NAs from the non-used function arguments
SDG_2.2<-na.omit(SDG_2.2) 

#renaming the default deprivation variable to match dataset:
first_Q <- which(rownames(SDG_2.2)=="Wealth quintile: Q1: Poorest")
rownames(SDG_2.2)[first_Q:(first_Q + 4)] <- c("School IMD Q1: most deprived", 
                                                "School IMD Q2",
                                                "School IMD Q3","School IMD Q4",
                                                "School IMD Q5: least deprived")


#these are the columns of interest from the function output (based on the function documentation)
cols_to_keep<- c("HAZ_pop","HA_2_r", "WH2_r", "WH_2_r") 
final_result<-SDG_2.2[, cols_to_keep] # keeping only the useful columns

#removing obsolete rows (we only have 1 age group, so deleting repetitions)
final_result <- final_result[!(row.names(final_result) %in% c("All", "Sex: Female","Sex: Male")), ]
#renaming the columns to more meaningful things
colnames(final_result)<-c("SampleSize","HeightforAge_-2SD", "WeightforHeight_+2SD", "WeightforHeight_-2SD")

# final result output
# file_year <- 201819 # uncomment & CHANGE to the year this data refers to
write.csv(final_result, paste("../Target_2.2_", file_year, ".csv", sep=""))


