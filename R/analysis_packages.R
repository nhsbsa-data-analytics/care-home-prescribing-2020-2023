
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
  'readr',
  'lubridate'
)

# Libraries from github
pacman::p_load_gh("nhsbsa-data-analytics/nhsbsaR")
pacman::p_load_gh("nhsbsa-data-analytics/addressMatchR")
