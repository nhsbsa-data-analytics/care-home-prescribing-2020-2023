
# 1. Generate companies house account ------------------------------------------

# Create account
browseURL('https://account.companieshouse.gov.uk/user/register?request=ey')

# Request a key
browseURL("https://developer.companieshouse.gov.uk/developer/applications/register ")

# Get key from renviron file
key <- Sys.getenv("CH_API_KEY")


#
# Once you have aquired the key, please create a copy this file and replace the '<>' with the actual key below.
#
# Save the file as companieshouse_key.R at the below location
# "F:\Information Services\DALL\DALL Initiatives CRISP-DM\INT #502 Pharmacy loss reduction POCs\Project Deliverables\Network Analysis\Company House API
#
### --- ###  ### --- ###


mkey <- "-0O7Cia-VzRzeKYeo5TN_X4mtCEhdS520JJUh-fw"