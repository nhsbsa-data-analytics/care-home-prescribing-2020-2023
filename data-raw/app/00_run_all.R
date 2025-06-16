Sys.time()
# Load library and generate base geo data
library(tictoc)
source("data-raw/app/data_raw_helpers.R")
source("data-raw/app/geo_data.R")

# Define vars to retain in workflow
keep_vars = c(ls(), 'keep_vars')

# Run all scripts that generate an Rda file
tic(); source("data-raw/app/01_headline_figures_df.R"); toc()                   # 10 mins
tic(); source("data-raw/app/02_patients_age_gender_df.R"); toc()
#tic(); source("data-raw/app/03_patients_by_imd_df.R"); toc()                   NA
tic(); source("data-raw/app/04_metrics_by_ch_type_85_split_df.R"); toc()        # 3  hours
tic(); source("data-raw/app/05_metrics_age_gender_df.R"); toc()                 # 30 mins
tic(); source("data-raw/app/06_metrics_by_geo_and_ch_flag_df.R"); toc()         # 90 mins
tic(); source("data-raw/app/07_ch_flag_drug_df.R"); toc()                       # 30 mins
tic(); source("data-raw/app/08_geo_ch_flag_drug_df.R"); toc()                   # 90 mins
Sys.time()

