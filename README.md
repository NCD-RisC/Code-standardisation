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
* [extractions_template.R](./R/data-extraction/extractions_template.R)  
Template script for extracting new individual-level studies
* [extractions_check.R](./R/data-extraction/extractions_check.R)  
Check internal consistency of extraction data - run after every individual-level data extraction

### Data cleaning  
#### General cleaning functions [cleaning_functions_data.R](./R/data-handling/cleaning_functions_data.R)
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
* Examples available in [example/example_data_cleaning.R](./example/example_data_cleaning.R)

#### Functions for cleaning survye design variables [cleaning_functions_survey_design.R](./R/data-handling/cleaning_functions_survey_design.R)
* `clean_svydesign`  
Clean survey design variables: beta version - need to work on testing and commenting
* `svy_check`  
Check survey design variables: legacy function called in `clean_svydesign`
* `clean_single_psu_ssa`  
Cleaning to avoid single PSU errors

#### Multivariate cleaning functions [cleaning_functions_mahalanobis.R](./R/data-handling/cleaning_functions_mahalanobis.R)
* `maha_clean`  
Cleaning a pair of variables using Mahalanobis distance
Example available in [example/example_maha.R](./example/example_maha.R)  

#### Functions specific to certain risk factors - to do
* `clean_multi_chol`  
Accounting for multivariate constraints for cholesterol
* `make_fasting_status`  
Generate fasting status based on `fasting_time` and `is_fasting`

### Summarising data  
* `make_age_groups`  
Create age groups according `age`, `age_min_xxxx_F|M`, `age_max_xxxx_F|M`  
Example available in [example/example_make_age_group.R](./example/example_make_age_group.R)
* `make_svydesign` - to do  
Generate survey design dummies by `id_study`, according to availability of `psu`, `stratum`, `samplewt_xxxx`

#### For anthropometrics [summarising_functions_anthro.R](./R/data-handling/summarising_functions_anthro.R)  
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

## 4. Modelling

initial_values
  
* `1_NCD_RisC_diabetes_prevalence_iv_model.R`  
- Global model using arbitrary initial values (sampled from selected distributions and then overdispersed).  
- Sources `1_functions.R`, and is run on the HPC using `1_iv_model_.run`, which sources `1_iv_model_.config` to configure model specifications.  
- Run 10 chains (per sex).  
* `2_combine_MCMC_chains_master.R`  
- Master script for combining the 10 model chains. Inspect for any egregious issues with chains.  
- Sources `2_combine_MCMC_chains.R` and `2_MCMC_convergence_plot_functions.R`.  
- This is run either locally or on Open OnDemand.  
* `3_means_variances_dispersion.R`  
- Script to calculate the multivariate normal distribution to sample parameter initial values from.    
- Means given by the posterior means of parameters.  
- Covariance matrix given by the posterior covariance matrix of all of the parameters, which is then dispersed by a factor of 1.5.  
- A labelled list of parameter indices is also generated here, to then be used when allocating initial values within the 'main run' model code.  
- Run on the HPC using `1_means_variances_dispersion.run`, which sources `1_means_variances_dispersion.config` to configure script specifications.  
* `4_sample_iv.R`  
- Simply samples initial values from the multivariate normal distribution as calculated in `3_means_variances_dispersion.R`.  
- Run 10 in parallel (per sex) on the HPC using `4_sample_iv.run`, which sources `4_sample_iv.config` to configure script specifications.  
- We need a distinctly sampled set of initial values for each of the 10 chains we will run for the 'main model' run.  
  
prevalence_model  
  
* `1_NCD_RisC_diabetes_prevalence_model.R`  
- Global model using sampled initial values (sampled using the workflow as in initial_values).  
- Sources `1_functions.R , and is run on the HPC using `1_model_.run`, which sources `1_model_.config` to configure model specifications.  
- Run 10 chains (per sex).  
* `2_combine_MCMC_chains_master.R`  
- Master script for combining model chains. Convergence must be inspected here. The usual procedure is to combine the first 8, and then the remaining two chains are to be used if any need replacing due to convergence issues.  
- Sources `2_combine_MCMC_chains.R` and `2_MCMC_convergence_plot_functions.R`.  
- This is run either locally or on Open OnDemand.  

  
#### Running on cluster with NCD-RisC container
* NCD-RisC HPC docker image ([DockerHub](https://hub.docker.com/r/ncdrisc/ncdrisc_hpc_docker))
* model_run  - to do  
template shell script  
* model_run_with_restart - to do  
shell script with auto restart after walltime  

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

#### Standard palette and regional order  
* [region_sregion_palette.R](./R/figures/region_sregion_palette.R)
Returns named vector with NCD-RisC standard regional and super-regional colours (`region_col` and `sregion_col`)  
Sets factor levels so that regions and super-regions appear in certain orders (`region_order` and `sergion_order`)  

#### World maps [map function.R](./R/figures/map_function.R)
* `map_function`  
Plot a map with values stored in `colour_val` using either user-defined or built-in colour schemes, with NCD-RisC layout (including dots for small countries) and density  
Example available in [example/example_make_maps.R](./example/example_make_maps.R), including one for a map of number of data sources with categorical colour scheme
* `get_change_scale`  
Returns a colour scale for change which makes zero white-ish and have symetric shades of colours in both directions  

#### Circular bars - to do

#### Hexagon maps - to do

#### Long bars with country names on two sides - to do
