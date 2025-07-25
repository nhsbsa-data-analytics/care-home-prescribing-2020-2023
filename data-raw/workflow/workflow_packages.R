# Install pacman if necessary
if (!('pacman' %in% rownames(utils::installed.packages()))) {
  utils::install.packages('pacman')
}

# Libraries
pacman::p_load(
  # Global
  'dplyr',
  'dbplyr',
  'DBI',
  'tictoc',
  'janitor',
  'yaml',
  'tidyr',
  'purrr',
  'glue',
  'config', 
  'forcats',
  'RSelenium',
  'data.table',
  'usethis',
  'stingr',
  
  # CQC API data 
  'httr',
  'jsonlite',
  
  # AddressBase Packages
  'archive',
  'readr',
  'lubridate',
  
  # Testing
  'assertr',
  
  # Shiny
  'sf',
  'highcharter',
  'geojsonsf',
  'shinyusertracking',
  'golem', 
  'kableExtra', 
  'markdown', 
  'reactable',
  'rsvg'
)

# Libraries from github
pacman::p_load_gh("nhsbsa-data-analytics/nhsbsaR")
pacman::p_load_gh("nhsbsa-data-analytics/addressMatchR")
pacman::p_load_gh("nhsbsa-data-analytics/personMatchR")
pacman::p_load_gh("nhsbsa-data-analytics/assertr.alt") # inline data validation
pacman::p_load_gh("MarkMc1089/netstat") # fixed version of netstat CRAN package
