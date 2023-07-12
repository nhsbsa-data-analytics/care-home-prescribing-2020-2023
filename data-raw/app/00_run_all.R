source("data-raw/app/data_raw_helpers.R")
source("data-raw/app/geo_data.R")

keep_vars = c(ls(), 'keep_vars')

source("data-raw/app/01_headline_figures_df.R")
source("data-raw/app/02_patients_age_gender_df.R")
source("data-raw/app/03_patients_by_imd_df.R")
source("data-raw/app/04_metrics_by_ch_type_df.R")
source("data-raw/app/05_metrics_age_gender_df.R")
source("data-raw/app/06_metrics_by_geo_and_ch_flag_df.R")
source("data-raw/app/07_geo_ch_flag_drug_df.R")
