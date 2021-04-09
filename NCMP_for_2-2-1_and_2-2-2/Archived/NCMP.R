# Code based on R v4.0.2
# basic tests, may need more cleaning up of suppressed values (large negatives)
# current cleaning removes -ves, but perhaps can keep values < 0.005th percentile because they are guaranteed to be < -2SD (?)
# use tidyR and dplyr for efficiency? (maybe not, to avoid issues if using anything expect base R)

###############################################################################################
#
# The user must set the working directory to match the directory of this NCMP.R script
# setwd() # uncomment this line to set the directory of the downloded script,, + csv files, keeping the folder with the data one directory above
#
###############################################################################################

# reading the data from 2018-19 download of the zipped file from NHS Digital, after extracting
# assuming the data csv is one level up from the current working directory
NCMP_data<-read.csv("../ncmp_1819_final_non_disclosive_published.csv")
# some checks
# head(NCMP_data)
# min(NCMP_data$AgeInMonths)/12
# max(NCMP_data$AgeInMonths)/12

# rounding the age in months variable (to match WHO tables better)
NCMP_data$AgeInMonths<-trunc(NCMP_data$AgeInMonths) #this truncates to the integer value, so similar to age in years rounding

# clean up suppressions from dataset (may need to be more sophisticated, here just removing all negatives)
# this will not be necessary when working with full data set!
clean_NCMP <- NCMP_data[NCMP_data$Height > 0, ]
nrow(clean_NCMP)-nrow(NCMP_data) # N supressed height instances altogether

# retaining only age groups of interest (under 5s, exclusing 5)
clean_NCMP <- clean_NCMP[clean_NCMP$AgeInMonths<60, ] 
### extract the unique ages in months (12 months in total, as we only have age 4)
###ages<- sort(unique(clean_NCMP$AgeInMonths))

# loading the WHO standards (should be in the current working directory)
who_girls <- read.csv("./height-for-age-(z-scores)-f.csv", header = T)
who_boys <- read.csv("./height-for-age-(z-scores)-m.csv", header = T)


# 1=boys and 2=girls in NCMP data; creating separate tables for each:
NCMP_girls<-clean_NCMP[clean_NCMP$GenderCode==2, ]
NCMP_boys<-clean_NCMP[clean_NCMP$GenderCode==1, ]
# appending the -2SD cut-off from the WHO standards table to the NCMP tables
NCMP_girls$WHO_SD <- who_girls$min2.SD[match(NCMP_girls$AgeInMonths,who_girls$Months)] 
NCMP_boys$WHO_SD <- who_boys$min2.SD[match(NCMP_boys$AgeInMonths,who_boys$Months)] 


### loop through data using the ages variable variable 
### initiating an empty variables to hold the results
###deviations_girls <- NULL
###prevalence_girls <- NULL
###instances_girls <- NULL
###main loop
###for(i in ages){
###  total_age_temp<-nrow(NCMP_girls[NCMP_girls$AgeInMonths==i, ])
###  deviations_temp<-nrow(NCMP_girls[NCMP_girls$AgeInMonths==i & NCMP_girls$Height < NCMP_girls$WHO_SD, ])
###  prevalence_temp<-(deviations_temp/total_age_temp)*100 
###  deviations_girls<-c(deviations_girls,deviations_temp)
###  prevalence_girls<-c(prevalence_girls,prevalence_temp)
###  instances_girls<-c(instances_girls,total_age_temp)
###}

###formtting final table for girls with detailed results for age
###girls_table<-rbind(instances_girls,deviations_girls,prevalence_girls)
###colnames(girls_table)<-ages
###girls_table 


# global prevalence for under 5s girls (i.e. for 4 year olds)
###prevalence_under5_girls <- (sum(girls_table["deviations_girls", ])/sum(girls_table["instances_girls", ]))*100
###prevalence_under5_girls

 
### loop procedure, but for boys
###deviations_boys <- NULL
###prevalence_boys <- NULL
###instances_boys <- NULL
#main loop
###for(i in ages){
###  total_age_temp<-nrow(NCMP_boys[NCMP_boys$AgeInMonths==i, ])
###  deviations_temp<-nrow(NCMP_boys[NCMP_boys$AgeInMonths==i & NCMP_boys$Height < NCMP_boys$WHO_SD, ])
###  prevalence_temp<-(deviations_temp/total_age_temp)*100 
###  deviations_boys<-c(deviations_boys,deviations_temp)
###  prevalence_boys<-c(prevalence_boys,prevalence_temp)
###  instances_boys<-c(instances_boys,total_age_temp)
###}

###formtting final table for boys with detailed results for age
###boys_table<-rbind(instances_boys,deviations_boys,prevalence_boys)
###colnames(boys_table)<-ages
###boys_table 

### global prevalence for under 5s girls (i.e. for 4 year olds)
###prevalence_under5_boys <- (sum(boys_table["deviations_boys", ])/sum(boys_table["instances_boys", ]))*100
###prevalence_under5_boys

###prevalence_AgeSex <- rbind(girls_table, boys_table)

# Is it normal that there are incrementally more observations as age increases??
# Is it normal there are so few observations of the youngest age group?
#barplot(prevalence_AgeSex["instances_girls",], main="Number of girls per age") 
#barplot(prevalence_AgeSex["instances_boys",], main = "Number of boys per age") 
#barplot(diff(prevalence_AgeSex["instances_girls",]), main="Differences in N with age (girls)") #??

### outputting final table (one directory up from current folder) (no longer needed)
###write.csv(prevalence_AgeSex, "../Summary_stunting_AgeSex.csv")

# Total girls and boys under 5 (without relying on the ages loop):
prev_under5_girls <- (nrow(NCMP_girls[NCMP_girls$Height < NCMP_girls$WHO_SD, ])/nrow(NCMP_girls))*100
prev_under5_boys <- (nrow(NCMP_boys[NCMP_boys$Height < NCMP_boys$WHO_SD, ])/nrow(NCMP_boys))*100

# calculating total stunting prevalence across sex (can also be done from the table above):
# joining girls and boys together (overwriting clean_NCMP to preserve the WHO_SD column)
clean_NCMP <- rbind(NCMP_boys, NCMP_girls)
stunting_prev_total <- (nrow(clean_NCMP[clean_NCMP$Height < clean_NCMP$WHO_SD, ])/nrow(clean_NCMP))*100
stunting_prev_total

# putting all summaries in one table and outputting it
prevalence_total<- rbind(prev_under5_boys,prev_under5_girls,stunting_prevalence_total)
colnames(prevalence_total)<- "2018/19"
write.csv(prevalence_total,"../Summary_stunting_all.csv")

