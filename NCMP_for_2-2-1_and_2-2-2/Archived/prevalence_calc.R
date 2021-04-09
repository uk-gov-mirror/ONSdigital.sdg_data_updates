# Code based on R v 4.0.2
#
# This is a refactored version of the NCMP.R code, turning it into a single function: prevalence_calc()
# The function generates prevalence of stunting for children under 5, based on WHO standards of height-for-age
# Part of the output is broken down by sex and age in months, including actual counts in addition to prevalence.
# prevalence_calc() takes two arguments: the table with NCMP data and the end year the table refers to (e.g. 2019 for 2018-19).
#
# example run: 
# prevalence_calc(NCMP_data = data_201819, end_year = 2019)
#
# The function will need to be run on each year of data independently (separate csv files per year)
# The data should contain the following variables (columns): 
# AgeinMonths; Height; GenderCode; .......
# Outputs are two summary tables in .csv format, one detailed per month of age (Summary_stunting_AgeSex_year.csv), 
#                                                and one across age (<5) and sex (Summary_stunting_all_year.csv)
#
# (current cleaning removes -ves, but this step should be omitted for the final product)


prevalence_calc <- function(NCMP_data, end_year){
  # error handling and checks
  if(min(NCMP_data$AgeInMonths)<48) stop('Minimum age is less than 4 - check data')


  # rounding the age in months variable (to match WHO tables better)
  NCMP_data$AgeInMonths<-trunc(NCMP_data$AgeInMonths) #this truncates to the integer value, so similar to age in years rounding
  
  # clean up suppressions from dataset (may need to be more sophisticated, here just removing all negatives)
  # this will not be necessary when working with full data set!
  clean_NCMP <- NCMP_data[NCMP_data$Height > 0, ]
  
  # retaining only age groups of interest (under 5s, exclusing 5)
  clean_NCMP <- clean_NCMP[clean_NCMP$AgeInMonths<60, ] 
  # extract the unique ages in months (12 months in total, as we only have age 4)
  ages<- sort(unique(clean_NCMP$AgeInMonths))
  
  # loading the WHO standards (should be in the current working directory)
  who_girls <- read.csv("./height-for-age-(z-scores)-f.csv", header = T)
  who_boys <- read.csv("./height-for-age-(z-scores)-m.csv", header = T)
  
  
  # 1=boys and 2=girls in NCMP data; creating separate tables for each:
  NCMP_girls<-clean_NCMP[clean_NCMP$GenderCode==2, ]
  NCMP_boys<-clean_NCMP[clean_NCMP$GenderCode==1, ]
  # appending the -2SD cut-off from the WHO standards table to the NCMP tables
  NCMP_girls$WHO_SD <- who_girls$min2.SD[match(NCMP_girls$AgeInMonths,who_girls$Months)] 
  NCMP_boys$WHO_SD <- who_boys$min2.SD[match(NCMP_boys$AgeInMonths,who_boys$Months)] 
  
  
  # Total girls and boys under 5:
  prev_under5_girls <- (nrow(NCMP_girls[NCMP_girls$Height < NCMP_girls$WHO_SD, ])/nrow(NCMP_girls))*100
  prev_under5_boys <- (nrow(NCMP_boys[NCMP_boys$Height < NCMP_boys$WHO_SD, ])/nrow(NCMP_boys))*100
  
  # calculating total stunting prevalence across sex (can also be done from the table above):
  # joining girls and boys together (overwriting clean_NCMP to preserve the WHO_SD column)
  clean_NCMP <- rbind(NCMP_boys, NCMP_girls)
  stunting_prev_total <- (nrow(clean_NCMP[clean_NCMP$Height < clean_NCMP$WHO_SD, ])/nrow(clean_NCMP))*100
  
  # putting all summaries in one table
  prevalence_total<- rbind(prev_under5_boys,prev_under5_girls,stunting_prev_total)
  colnames(prevalence_total)<- end_year
  
  # outputting final tables (one directory up from current folder)
  write.csv(prevalence_total,paste("../Summary_stunting_all_",end_year,".csv", sep=""))
  return(paste("CSV outputs generated for year",end_year))
  
}