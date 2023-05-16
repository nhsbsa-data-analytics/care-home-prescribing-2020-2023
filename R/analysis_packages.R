<<<<<<< HEAD

# Install pacman if necessary
if (!('pacman' %in% rownames(installed.packages()))) {
  install.packages('pacman')
}

# Libraries
pacman::p_load(
  # Global
  'dplyr',
  'dbplyr',
  'tidyr',
  'DBI',
  'tictoc',
  'janitor',
  'yaml',
  'highcharter',
  'glue',
  'purrr',
=======
analysis_packages <- function() {
  # Install pacman if necessary
  if (!('pacman' %in% rownames(utils::installed.packages()))) {
    utils::install.packages('pacman')
  }
>>>>>>> 7fad2b0 (Rebase and resolve conflicts from main)
  
  # Libraries
  pacman::p_load(
    # Global
    'dplyr',
    'dbplyr',
    'DBI',
    'tictoc',
    'parallel',
    'janitor',
    'yaml',
    
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
}

