# sdg_data_updates

### General info ###
R code for updating SDG data files. For each indicator data from the source file(s) (usually excel) are read in, calluclations required for the indicator are performed, and data are reshaped to fit the tidy format required to upload data to the SDG data platform.

lead author: Emma Wood
emma.wood@ons.gov.uk

Links to source data can be found in the relevant indicator page on the [SDG data platform](https://sdgdata.gov.uk/) in the Sources tab.


### File structure ###
Use update_indicator_main.R to run all indicator update scripts.

Indicator specific files can be found in each indicator folder (name structure num-num-num)

SDGupdater is a package used by indicator update scripts. Code for installing this is commented out in update_indicator_main.R
