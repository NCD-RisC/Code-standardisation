# Code-standardisation
Standardising shared functions and scripts in NCD-RisC codes. These include those that are used to:
1. extract, clean and summarise individual level data
2. extract and format summary level data
3. check data before model runs
4. set up model runs
5. postprocess data
6. make figures

## 1. Individual level data processing

### Extraction, merging and subsetting risk factor data
* `Extraction Template.R`  
Template script for extracting new individual-level studies
* `Check extraction.R`  
Check internal consistency of extraction data - run after every individual-level data extraction

### Data cleaning  
#### General cleaning functions `cleaning_functions.R`
* `clean_data`  
Level 1 master function: cleaning a specific variable in the data frame
Convert units of measurement data to standard units and clean the data (calls `convert_unit` and `clean_data_index` functions); returns the cleaned variable as a vector
* `convert_unit`  
Convert units of measurement data to standard units and remove studies with 'EXCLUDE' as units; print out number cleaned by `id_study`
* `clean_data_index`  
Level 2 master function: get the positions to be cleaned for the specified variable
Calls specific functions according to the variable being cleaned for; returns a list of locations to be cleaned (calls the specific cleaning functions below)
* Specific cleaning functions by variable  
`clean_height`, 
`clean_weight`, 
`clean_bmi`, 
`clean_waist`, 
`clean_wth`, 
`clean_hip`, 
`clean_whr`, 
`clean_sex`, 
`clean_age`, 
`clean_continuous`, 
`clean_categorical`,
`clean_preg`  

#### Functions for cleaning survye design variables `cleaning_survey_design.R`
* `clean_svydesign`  
Clean survey design variables: beta version - need to work on testing and commenting
* `svy_check`  
Check survey design variables: legacy function called in `clean_svydesign`
* `clean_single_psu_ssa`  
Cleaning to avoid single PSU errors

#### Multivariate cleaning functions `mahalanobis_detection.R`
* `maha_clean`  
Cleaning a pair of variables using Mahalanobis distance
Example available in `examples\example_maha.R`  

#### Functions specific to certain risk factors
* `clean_multi_chol` - to do  
Accounting for multivariate constraints for cholesterol
* `make_fasting_status` - to do  
Generate fasting status based on `fasting_time` and `is_fasting`

### Summarising data  
* `make_age_groups`  
Create age groups according `age`, `age_min_xxxx_F|M`, `age_max_xxxx_F|M`
* `make_svydesign` - to do  
Generate survey design dummies by `id_study`, according to availability of `psu`, `stratum`, `samplewt_xxxx`

#### For anthropometrics `summarising_functions_anthro.R`  
* `get_bmi_prev`  
Generate dummy (0/1) including missing data for BMI categories for calculating prevalence  
Need to specify the location of `child_adolescent_bmi_cutoffs.csv` to BMI categories for 5-19 year-olds  
* `get_height_prev`  
Generate dummy (0/1) including missing data for height categories for calculating prevalence  
* `bmi_adol`  
Utility function for `get_bmi_prev`: deciding the direction of the comparison (`>` or `<`)  

#### For blood pressure - to do  
`summarising_functions_BP.R`  

#### For glucose/diabetes - to do  
`summarising_functions_glu.R`  

#### For cholesterol - to do  
`summarising_functions_chol.R`  

#### Functions for generating summaries - to do 
* `get_summary` 
summarise data by `id_study`, `sex`, `age`, and potentially `is_urban`
* `get_summary_parallel`  
`get_summary` using parallel computing

### Metadata - to do
* get_number  
return the number of non-NA values in a variable

## 2. Summary level data processing - to do

#### Extraction
* check_sheets  
check the sheet names against a standard list for completeness
* check_age_groups  
check if `age_group` information is consistent across tabs

#### Metadata
Nothing

## 3. Check data - to do
* make_data_source_map  
make data source map (using `map_function`)
* bubble_plot  
to rewrite the original script to avoid manual resizing the bubbles
* data_plot_by_region  
aka 'regional plot'
* data_plot_by_country  
aka 'fit plot without fit'  
* data_plot_by_country_interactive  
interactive 'fit plot without fit'  
* compare_data  
compare different versions of data (typically used in model runs)

## 4. Modelling - to do 

#### Cluster shell scripts
* model_run  
template shell script
* model_run_with_restart  
shell script with auto restart after walltime (to develop)

#### Processing raw outputs
* combine_chains.R

## 5. Postprocessing - to do

Aggregating age-specific results into age-standardised or crude results
by country or by groups of countries,
including for region, super-region, WHO region, WB group, world

### Aggregating results

#### by sex
* asd_by_country
* asd_by_group  
* crude_by_country
* crude_by_group  

#### for both sexes combined
* asd_by_country_bothsex
* asd_by_group_bothsex  
* crude_by_country_bothsex
* crude_by_group_bothsex  

## 6. Making figures
* `0.1 Region Superregion palette.R`
Returns named vector with NCD-RisC standard regional and super-regional colours, and setting factor levels so that regions/super-regions appear in certain orders  

#### World maps `new map function.R`
* `map_function`  
Plot a map with values stored in `colour_val` using either user-defined or built-in colour schemes, with NCD-RisC layout (including dots for small countries) and density  
Example available in `example\example_make_maps.R`
* `get_change_scale`  
Returns a colour scale for change which makes zero white-ish and have symetric shades of colours in both directions  

#### Circular bars - to do

#### Hexagon maps - to do

#### Long bars with country names on two sides - to do