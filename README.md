# Code-standardisation
 Standardising shared functions in NCD-RisC codes

## Functions that are shared across RFs for data processing

### Individual level

#### Extraction

#### Merging

#### Subsetting

#### Cleaning: by RF
* clean_data
* print_cleaned
* clean_single_psu_ssa

##### anthro only
* get_bmi_prev
* get_height_prev

#### Summarising: by RF
* generate_age_group
* clean_svydesign
* make_svydesign
* get_summary
* get_summary_parallel

#### Metadata: by RF
* find_number

### Summary level

#### Extraction: by RF
* check_sheets
* check_age_groups

#### Metadata: by RF

### Combined data

#### Merging summary: by RF

#### Merging metadata: by RF

## Functions that are shared across RFs for postprocessing and plotting

### Postprocessing

#### Male and female separately
* asd_by_country

* asd_by_group

including for region, super-region, WHO region, WB group, world

* crude_by_country

* crude_by_group

including for region, super-region, WHO region, WB group, world

#### Male and female combined
* asd_by_country_bothsex
* asd_by_group_bothsex
including for region, super-region, WHO region, WB group, world
* crude_by_country_bothsex
* crude_by_group_bothsex
including for region, super-region, WHO region, WB group, world

### Plotting
* make_maps
* 
