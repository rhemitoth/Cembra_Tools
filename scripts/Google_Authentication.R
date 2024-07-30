## ---------------------------
##
## Script name: Google Authentication
##
## Author: Rhemi Toth
##
## Date Created: 2024-07-16
##
## Email: rhemitoth@g.harvard.edu
##
## ---------------------------
##
## Notes: This script authenticates the user
## to pull data directly from Google Drive into R
##
## ---------------------------

# Google Drive Authentication ---------------------------------------------

# Load packages for working with Google Drive
require(googlesheets4)
require(googledrive)

# Clear previous token cache
if (file.exists("~/.R/gargle/gargle-oauth")) {
  unlink("~/.R/gargle/gargle-oauth", recursive = TRUE)
}

# Set the OAuth scopes
scopes <- c(
  "https://www.googleapis.com/auth/spreadsheets",
  "https://www.googleapis.com/auth/drive"
)

# Authenticate explicitly with scopes
gs4_auth(
  email = TRUE,
  scopes = scopes,
  cache = gargle::gargle_oauth_cache(),
  use_oob = TRUE # Use out-of-band authentication if needed
)

# Also authenticate for googledrive as needed
drive_auth(
  email = TRUE,
  scopes = scopes,
  cache = gargle::gargle_oauth_cache(),
  use_oob = TRUE # Use out-of-band authentication if needed
)
# Verify the scopes for Google Sheets
gs4_scopes <- gs4_token()$credentials$scope
print(gs4_scopes)

# Verify the scopes for Google Drive
drive_scopes <- drive_token()$credentials$scope
print(drive_scopes)

