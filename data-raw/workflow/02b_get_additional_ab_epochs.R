
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

# If you get an error, use binman::list_versions("chromedriver") to find latest
# version of driver. Check your Chrome version with the settings drop down 
# Help > About Google Chrome in your browser. Use the closest chromedriver
# version to your Chrome version. If you still get an error, try deleting the
# LICENSE file from the folder of the chromedriver version you are using. This
# is a known bug. Can use binman::app_dir("chromedriver") to find the folder.

# If you are in a restricted access environment (such as DALL AVDs), you will
# need to run this on your local device, then copy over the folder binman
# creates in your AppData folder.
rD <- RSelenium::rsDriver(
  browser = "chrome",
  chromever = "109.0.5414.74",
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
epochs <- remDr$findElements(using = "class", value = "columnSelect") %>%
  set_names(data_names)

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Load the AB+ schema
abp_col_names <- read_rds("data-raw/workflow/abp_col_names.rds")

# Loop 1: for each available epoch...
#   set working directory to the temp dir
#   click next checkbox and then click Download
#   Loop 2: every second ...
#     check if download has completed
#   unzip the downloaded folder
#   delete the downloaded zip
#   check for sub-folders...if any
#   Loop 3: move all files into root folder
#   set working directory to unzipped epoch folder of zipped csvs
#   Loop 4: for each zipped csv...
#     unzip the csv
#     delete the zipped csv
#   Set epoch and table names
#   Loop 5: for each csv...
#     process it with process_csv() - this creates a table in db with all data
#   Connect to new table
#   Create and tidy SLAs with addressMatchR
#   Select columns desired in final epoch table
#   Write the final table to db
#   Delete the downloaded data and remove the temp table

# Loop 1
epochs %>% iwalk(
  \(x, idx) {
    # Temporarily use the temp download folder as working directory
    withr::local_dir(download_dir)

    x$clickElement()
    remDr$findElement(using = "link text", value = "Download")$
      clickElement()

    # Loop 2: short wait then check every second for prescence of partial
    #   download. If not there, we know the download has completed.
    Sys.sleep(5)
    while(length(list.files(pattern = ".crdownload") > 0)) Sys.sleep(1)

    # Extract the archive then immeidately delete it
    unzip("archive.zip")
    unlink("archive.zip")

    # Temporarily use the current epoch folder as working directory
    withr::local_dir(idx)

    # Some epochs have sub-folders, e.g. data1, data2.
    maybe_dirs <- list.dirs(idx, full.names = FALSE)[-1]
    if (length(maybe_dirs) > 0) {
      # Loop 3: Attempt to move all files from each sub-folder. If all successful,
      #   remove sub-folder. Otherwise stop running and error with message.
      maybe_dirs %>% walk(
        \(x) {
          if (all(
            file.copy(list.files(x, full.names = TRUE), getwd(), overwrite = TRUE)
          )) {
            unlink(x, recursive = TRUE)
          } else {
            stop(glue("Files could not be moved from subfolder for {idx}"))
          }
        }
      )
    }

    # Loop 4: One by one unzip each csv and delete the zipped file.
    list.files() %>% walk(
      \(x) {
        unzip(x)
        unlink(x)
      }
    )

    ab_plus_epoch_date <<- idx %>% parse_number() %>% dmy() %>% format("%Y%m%d")

    # Define table name
    table_name <<- paste0("INT646_ABP2_", ab_plus_epoch_date)

    # Define temp table name
    table_name_temp <<- paste0(table_name, "_TEMP")

    # Drop table if it exists already
    drop_table_if_exists_db(table_name_temp)

    csvs <<- list.files()

    # Loop 5: Process each csv, appending to a temp table in db.
    csvs %>% iwalk(process_csv)
    
    # Connect to temp table
    ab_plus_db = con %>%
      tbl(from = table_name_temp) %>%
      # SLA creation plus formatting
      addressMatchR::calc_addressbase_plus_dpa_single_line_address() %>%
      addressMatchR::calc_addressbase_plus_geo_single_line_address() %>%
      addressMatchR::tidy_single_line_address(col = DPA_SINGLE_LINE_ADDRESS) %>%
      addressMatchR::tidy_single_line_address(col = GEO_SINGLE_LINE_ADDRESS) %>% 
      oracle_merge_strings_edit(
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
        temporary = FALSE
      )
    
    # Print that table has been created
    print(paste0("This script has created table: ", table_name))
    
    # Drop table if it exists already
    drop_table_if_exists_db(table_name_temp)
    
    # Grant access
    c("MIGAR", "ADNSH", "MAMCP") %>% walk(
      \(x) {
        DBI::dbExecute(con, paste0("GRANT SELECT ON ", table_name, " TO ", x))
      }
    )
  }
)

# Disconnect connection to database
DBI::dbDisconnect(con)

# Remove vars specific to script
remove_vars <- setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
