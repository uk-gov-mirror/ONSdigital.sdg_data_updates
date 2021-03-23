Author: Emma Wood

Create csv data for 3-2-2 with the following disaggregations:
- birthweight by mother age
- country of occurrence by sex of baby
- area of residence (region)

Input data are stored in a folder named 'Input' (See 'Example Input'). Outputs are saved to a folder named 'Output'. 

        
USER INSTRUCTIONS (SDG Data team): 

1) Save 'Child mortality (death cohort) tables in England and Wales' as an xlsx file in Jemalex/code for updates/3.2.2/Input. 
2) Open update_indicator_main.R from RStudio
3) Run all disaggregation scripts by clicking the source button in the top right of the script window 
Note: If this is the first time you have run the script you may need to install some packages. 
	The error you get should make it clear what package you dont have.
	unhash the relevant install.packages() rows at the top of the script and repeat step 5.
	Please put the hash(es) back in once packages are installed.
4) csv files will be exported to Output folder.

Please check the data source to see if the best data are being reported, or if there are other series we sohould include

QA PROCESS (SDG Data team):
...

TROUBLESHOOTING:
If you get the error:
Error in setwd(paste0("H:/Coding_repos/sdg_data_updates/", indicator)) : 
  cannot change working directory
-The folder name you have given for indicator does not exist. Check you are using '-' not '.' between the numbers.

If you get an error similar to 
Error in file(file, ifelse(append, "a", "w")) : 
  cannot open the connection
-This is probably becaue you already have a version of that output file open. Close the file and try again.

DEVELOPER NOTES:
compile_tables.R: called by update_indicator_main.R

compile_tables.R: asks the user to choose a file from the Input folder, 
	runs the scriots for each table, and compiles them into a single csv, 
	which it saves in the Output folder.
	
config.R: contains configuration data that are likely to change between years e.g tab names.
	These configurations are called by country_of_occurence_by_sex.R, birthweight_by_mum_age.R, and region.R,
	which ask the user to double check the configs. The user is given the option to override the configs.
	If the change is permanent for all years going forward, update the config file.
	
country_of_occurence_by_sex.R, birthweight_by_mum_age.R, and region.R: where the bulk of the work is done.
	These scripts take the data from the input file, mung it into the format we want, and do calculations.
	Each script uses a different tab in the xlsx file. Output from each is saved in the global environment
	in case the user wants to see what data came from where, but they are not individually saved to the Output folder.
