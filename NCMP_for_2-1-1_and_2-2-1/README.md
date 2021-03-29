# NCMP_Rcode

Sustainable Development Goals, UK reporting development.

Collaborative R code to source new data or improve proxy SDG indicators 2.2.1 and 2.2.2
The methodology is specified by WHO, using specific standards on [height-for-age](https://www.who.int/tools/child-growth-standards/standards/length-height-for-age) and [weight-for-height](https://www.who.int/tools/child-growth-standards/standards/weight-for-length-height).

There is a dedicated R package ["anthro"](https://cran.r-project.org/web/packages/anthro/anthro.pdf) developed by WHO, which has a function (anthro_prevalence) to perform calculations and output the data as needed for the indicator reporting. This project develops a script to use the WHO package with the NCMP data. The R code is developed by the ONS, SDG team and delivered to PHE to run on NCMP data. The purpose is to avoid direct access to potentially sensitive data.
The raw data is not accessible, so the code is developed and tested on publicly available suppressed tables from [NHS Digital](https://digital.nhs.uk/data-and-information/publications/statistical/national-child-measurement-programme/2018-19-school-year).

The key steps of the scripts aim to produce headline data for indicators 2.2.1 and 2.2.2, including an additional disaggregation using the Index of multiple deprivation (IMD) variable, ethnicity and residence type (rural/urban). The IMD variable in the NCMP data is recoded from deciles into quintiles. The current code assumes the data doesn't have entries with age less than 4 years old (if it is the case, the code will need to be edited).

The script can be tested on the [NCMP suppressed dataset](https://files.digital.nhs.uk/A2/DBE4D6/National%20Child%20Measurement%20Programme%20-%20England%202018-19%20CSV%20File%20and%20Guidance.zip), which is publicly available from NHS Digital - use WHO_code_test.R. This script includes cleaning up the data to remove any suppressed cells. The main file for delivery to PHE is WHO_code_toRUN.R (no suppression cleaning, assuming the raw data has no suppressions or missing values).

The output from the test code is one .csv file, containing sample size, prevalence Height for Age -2SD (stunting), prevalence	Weight for Height +2SD, and prevalence	Weight for Height -2SD. This is broken down by the total sample, by sex, by IMD quintile (1 = most deprived LSOA), 10 school GORs. This latter disaggregation was used only for testing one of the function arguments.

The output from the main code is one .csv file, containing sample size, prevalence Height for Age -2SD (stunting), prevalence	Weight for Height +2SD (overweight), and prevalence	Weight for Height -2SD (wasting). This is broken down by the total sample, by sex, by IMD quintile (1 = most deprived LSOA), 17 ethnicity groups, and urban/rural residence. The latter two variables are not available in the published data used for testing.

The script needs to be run separately for each year of data.

There are other R scripts and csv files available in this repository kept for record, but they were meant for testing and/or developing code before knowing about the 'anthro' package.
