Author: Emma Wood
Date: 08/01/2021
Purpose: To create csv data for 3-2-2 with the following disaggregations:
- birthweight by mother age
- country of occurrence by sex of baby
- area of residence (region)
        
Updates process: 

1) Save 'Child mortality (death cohort) tables in England and Wales' as an xlsx file in Jemalex/code for updates/3.2.2/Input. 
2) Open main.R from RStudio
3) Enter the filename of the file you just downloaded in the config section of main.R e.g. filename <- "Input/cms2017correctedfeb2020.xlsx"
4) Check all other configs are correct for the file you downloaded (tab names and row number of first row with column headings)
5) Run all disaggregation scripts by clicking the source button in the top right of the script window 
Note: If this is the first time you have run the script you may need to install some packages. The error you get should make it clear what package you dont have.
	unhash the relevant install.packages() rows at the top of the script and repeat step 5.
	Please put the hash(es) back in once packages are installed.
6) csv files will be exported to Output folder.

Please check the data source to see if the best data are being reported, or if there are other series we sohould include

QA process:
...

TROUBLESHOOTING:
If you get this error in red:
Error in file(file, "rt") : cannot open the connection
In addition: Warning message:
In file(file, "rt") :
  cannot open file 'birthweight_by_mum_age_2017.csv': No such file or directory
You have specified a filename that does not exist. The file needs to be a csv and needs to be saved in the location specified by filepath. 
filename must exactly match the name of the file, be in "", and have the .csv suffix included.

If you get an error similar to 
Error in file(file, ifelse(append, "a", "w")) : 
  cannot open the connection
This is probably becaue you already have a previous version of that output file open. Close the file and try again.