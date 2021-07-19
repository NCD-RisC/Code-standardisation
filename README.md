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
* Extraction_template.R  
Template script for extracting new individual-level studies
* data_consistency_check  
Check internal consistency of merged data

### Data cleaning (`general_cleaning.R`)
* clean_numerical
clean a numerical variable
clean a categorical variable
* clean_categorical
* clean_data  
clean a variable to remove certain locations and print out percentage cleaned by `id_study` (called in `clean_numerical` and `clean_categorical`)
* print_cleaned  
print out percentage cleaned by `id_study` (called in `clean_data`; deprecated)
* clean_svydesign  
clean survey design variables
* clean_single_psu_ssa  
cleaning for single PSU errors
* mahalanobis_cleaning  
cleaning a pair of variables using Mahalanobis distance

#### functions specific to certain risk factors
* clean_multi_chol  
accounting for multivariate constraints for cholesterol
* make_fasting_status  
generate fasting status based on `fasting_time` and `is_fasting`

### Summarising data
* make_age_group  
create age groups according `age`, `age_min_xxxx_F|M`, `age_max_xxxx_F|M`
* make_svydesign  
generate survey design dummies by `id_study`, according to availability of `psu`, `stratum`, `samplewt_xxxx`
* get_prev  
take a set of booleans as input and generate dummy including missing data
* get_summary  
summarise data by `id_study`, `sex`, `age`, and potentially `is_urban`
* get_summary_parallel  
`get_summary` using parallel computing

#### functions specific to certain risk factors
* get_bmi_prev  
generate dummies for BMI in standard categories
* get_bmi_prev_adol  
generate dummies for BMI in standard categories using adolescent cut-offs
* get_height_prev  
generate dummies for height in standard categories
* get_average_bp  
generate average BP from individual measurements

### Metadata
* get_number  
return the number of non-NA values in a variable

## 2. Summary level data processing

#### Extraction
* check_sheets  
check the sheet names against a standard list for completeness
* check_age_groups  
check if `age_group` information is consistent across tabs

#### Metadata
Nothing

## 3. Check data
* make_data_source_map  
make data source map (using `make_map`)
* bubble_plot  
to rewrite the original script to avoid manual resizing the bubbles
* data_plot_by_region  
aka 'regional plot'
* data_plot_by_country  
aka 'fit plot without fit'
* compare_data  
compare different versions of data (typically used in model runs)

## 4. Modelling

#### Cluster shell scripts
* model_run  
template shell script
* model_run_with_restart  
shell script with auto restart after walltime (to develop)

#### Processing raw outputs
* combine_chains.R

## 5. Postprocessing

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

### Utilities
* read_country_list  
Read the list of countries with region information and setting factor levels so that regions/super-regions appear in certain orders

## 6. Making figures
* regional_colour_palette  
returns named vector with NCD-RisC standard regional and super-regional colours
* symetrical_colour_scales  
a colour scale for change which makes zero white-ish and have symetric shades of colours in both directions
* make_map  
mapping function with standard NCD-RisC layout (including boxes for small countries)
* make_map_with_density  
a variation of map with density (by country) displayed

