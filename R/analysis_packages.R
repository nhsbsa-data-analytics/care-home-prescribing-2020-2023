
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
  # MMc: had some issues running this, so manually installed each separately...
  # got this warning:
  # Warning in install.packages :
  #   package ‘parallel’ is a base package, and should not be updated
  # 'parallel',
  'janitor',
  'yaml',
  'glue',
  
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
