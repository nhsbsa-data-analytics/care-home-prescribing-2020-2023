
# Install pacman if necessary
if (!('pacman' %in% rownames(installed.packages()))) {
  install.packages('pacman')
}

# Libraries
pacman::p_load(
  # Global
  'dplyr',
  'dbplyr',
  'DBI',
  'tictoc',
  'parallel',
  'janitor',
  
  # CQC API data 
  'httr',
  'jsonlite',
  
  # AddressBase Packages
  'archive',
  'readr'
)

# Libraries from github
pacman::p_load_gh("nhsbsa-data-analytics/nhsbsaR")
pacman::p_load_gh("nhsbsa-data-analytics/addressMatchR")

# Define start and end dates
start_date = "2021-04-01"
end_date = "2022-03-31"