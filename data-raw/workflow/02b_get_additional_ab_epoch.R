
# Will use a temp dir for downloading data, and further processing
download_dir <- tempdir()

# Any Chrome options that should not be the default need specified here.
extra_caps <- list(
  chromeOptions =
    list(
      prefs = list(
        "download.default_directory" = download_dir
      )
    )
)

# Create the Selenium driver; netstat::free_port finds an open port, preventing
# trying to use a busy one. 

# FIRST TIME RUN OR OUTDATED DRIVERS
# The correct version of chromedriver will be determined automatically. Should
# you get an error, use binman::list_versions("chromedriver") to find the
# available versions. Check your Chrome version with the settings drop down
# Help > About Google Chrome in your browser. If your installed version of Chrome
# is not represented by the chromedriver versions available, you will need to get
# some updated drivers.
# If you are in a restricted access environment (such as DALL AVDs), you can run
# the below code on your unrestricted device, then copy over the folder binman
# creates in your AppData folder.
#
# rD <- RSelenium::rsDriver(
#   browser = "chrome",
#   chromever = NULL,
#   port = netstat::free_port(),
#   verbose = TRUE, # just to see what is going on...
#   check = TRUE
# )
#
# If you still get an error, try deleting the LICENSE file from the folder of
# the chromedriver version you are using. This is a known bug. Can use
# binman::app_dir("chromedriver") to find the folder.
rD <- RSelenium::rsDriver(
  browser = "chrome",
  chromever = getChromeDriverVersion(),
  port = netstat::free_port(),
  verbose = FALSE,
  check = FALSE, # AVDs have restricted access; use TRUE with unrestricted access.
  extraCapabilities = extra_caps
)

# Create reference to the browser opened by Selenium
remDr <- rD$client

# Navigate to the OS download site
remDr$navigate("https://hfs.os.uk/WebInterface/login.html")

# Login
# Save the username as OS_USERNAME in .Renviron; usethis::edit_r_environ()
un_input <- remDr$findElement(using = "id", value = "username")
un_input$clearElement()
un_input$sendKeysToElement(list(Sys.getenv("OS_USERNAME")))

# Save the password as OS_PASSWORD in .Renviron; usethis::edit_r_environ()
pw_input <- remDr$findElement(using = "id", value = "password")
pw_input$clearElement()
pw_input$sendKeysToElement(list(Sys.getenv("OS_PASSWORD")))

# Click Login button
remDr$findElement(using = "id", value = "btnLogin")$
  clickElement()

# Short wait for page to load
Sys.sleep(1)

# Get names of epochs
data_names <- remDr$
  findElements(using = "partial link text", value = "abPlus_csv_") %>%
  map(\(x) x$getElementText() %>% unlist())

# Get checkboxes associated with each epoch
checkboxes <- remDr$findElements(using = "class", value = "columnSelect") %>%
  set_names(data_names)

checkbox <- checkboxes[grepl(epoch_year, names(checkboxes))]

data_folder <- names(checkbox)
checkbox <- checkbox[[1]]

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Load the AB+ schema
abp_col_names <- read_rds("data-raw/workflow/abp_col_names.rds")

# Set working directory to the temp dir
# Click the checkbox and then click Download
# Loop 1: every second ...
#   check if download has completed
# Unzip the downloaded folder
# Delete the downloaded zip
# Set working directory to unzipped epoch folder of zipped csvs
# Check for sub-folders...if any
#   Loop 2: move all files into root folder
# Loop 3: for each zipped csv...
#   unzip the csv
#   delete the zipped csv
# Set epoch and table names
# Loop 4: for each csv...
#   process it with process_csv() - this creates a table in db with all data
# Delete the downloaded data and stop Selenium
# Set working directory back to project directory
# Connect to new table
# Create and tidy SLAs with addressMatchR
# Select columns desired in final epoch table
# Write the final table to db
# Remove the temp table

# Temporarily use the temp download folder as working directory
proj_dir <- setwd(download_dir)

checkbox$clickElement()
remDr$findElement(using = "link text", value = "Download")$
  clickElement()

# Loop 1: short wait then check every second for presence of partial download.
# If not there, we know the download has completed.
Sys.sleep(5)
while(length(list.files(pattern = ".crdownload") > 0)) Sys.sleep(1)

# Extract the archive then immediately delete it
unzip("archive.zip")
unlink("archive.zip")

# Temporarily use the unzipped data folder as working directory
setwd(data_folder)

# Some epochs have sub-folders, e.g. data1, data2.
maybe_dirs <- list.dirs(full.names = FALSE)[-1]
if (length(maybe_dirs) > 0) {
  # Loop 2: Attempt to move all files from each sub-folder. If all successful,
  #   remove sub-folder. Otherwise stop running and error with message.
  maybe_dirs %>% walk(
    \(x) {
      if (all(
        file.copy(list.files(x, full.names = TRUE), getwd(), overwrite = TRUE)
      )) {
        unlink(x, recursive = TRUE)
      } else {
        stop(glue("Files could not be moved from subfolder for {data_folder}"))
      }
    }
  )
}

# Loop 3: One by one unzip each csv and delete the zipped file.
list.files() %>% walk(
  \(x) {
    unzip(x)
    unlink(x)
  }
)

# Construct epoch date
ab_plus_epoch_date <- data_folder %>%
  parse_number() %>%
  dmy() %>%
  format("%Y%m%d")

# Define table name
table_name <- paste0("INT646_ABP_", ab_plus_epoch_date)

# Define temp table names; this is needed as compute does not support overwrite
# and we are splitting the SLA tidying and merge strings steps
table_name_temp <- paste0(table_name, "_TEMP")
table_name_temp2 <- paste0(table_name, "_TEMP2")

# Drop table if it exists already
drop_table_if_exists_db(table_name_temp)

csvs <- list.files()

# Loop 4: Process each csv, appending to a temp table in db.
csvs %>% iwalk(process_csv)

# Delete data folder now we are done with it
unlink(data_folder, recursive = TRUE)

# Close browser and stop Selenium server
remDr$close()
rD[["server"]]$stop()

# Move back to project dir from temp dir
setwd(proj_dir)

# Connect to temp table
ab_plus_db = con %>%
  tbl(from = table_name_temp) %>%
  # SLA creation plus formatting
  addressMatchR::calc_addressbase_plus_dpa_single_line_address() %>%
  addressMatchR::calc_addressbase_plus_geo_single_line_address() %>%
  addressMatchR::tidy_single_line_address(col = DPA_SINGLE_LINE_ADDRESS) %>%
  addressMatchR::tidy_single_line_address(col = GEO_SINGLE_LINE_ADDRESS)

drop_table_if_exists_db(table_name_temp2)

ab_plus_db %>%
  compute(
    name = table_name_temp2,
    temporary = FALSE
  )

ab_plus_db = con %>%
  tbl(from = table_name_temp2) %>%
  nhsbsaR::oracle_merge_strings(
    first_col = "DPA_SINGLE_LINE_ADDRESS",
    second_col = "GEO_SINGLE_LINE_ADDRESS",
    merge_col = "CORE_SINGLE_LINE_ADDRESS"
  ) %>% 
  select(
    UPRN,
    PARENT_UPRN,
    POSTCODE,
    DPA_SINGLE_LINE_ADDRESS,
    GEO_SINGLE_LINE_ADDRESS,
    CORE_SINGLE_LINE_ADDRESS,
    CH_FLAG,
    EPOCH
  ) 

# Drop table if it exists already
drop_table_if_exists_db(table_name)

# Write the table back to the DB with indexes
ab_plus_db %>%
  compute(
    name = table_name,
    temporary = FALSE,
    indexes = c("UPRN", "PARENT_UPRN", "POSTCODE")
  )

# Print that table has been created
print(paste0("This script has created table: ", table_name))

# Drop tables if they already exist
drop_table_if_exists_db(table_name_temp)
drop_table_if_exists_db(table_name_temp2)

# Grant access
c("MIGAR", "ADNSH", "MAMCP") %>% grant_table_access (table_name)

# Disconnect connection to database
DBI::dbDisconnect(con)

# Remove vars specific to script
remove_vars <- setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
