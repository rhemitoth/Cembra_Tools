## ---------------------------
##
## Script name: Mock Query
##
## Author: Rhemi Toth
##
## Date Created: 2024-07-12
##
## Email: rhemitoth@g.harvard.edu
##
## ---------------------------
##
## Notes: This is an example script that uses functions defined in QueryDatabseFunctions.R
## to pull data from the database.
##   
##
## ---------------------------


# Load Functions ----------------------------------------------------------

source("/Users/rhemitoth/Documents/PhD/Cembra/Cembra_Tools/scripts/Functions/QueryDatabaseFunctions.R")

# Google Authentication ---------------------------------------------------

source("/Users/rhemitoth/Documents/PhD/Cembra/Cembra_Tools/scripts/Google_Authentication.R")

# Set time period ---------------------------------------------------------

start <- "2024-07-17"
end <- "2024-07-18"

# Set root directory ------------------------------------------------------

root <- "/Users/rhemitoth/Library/CloudStorage/GoogleDrive-rhemitoth@g.harvard.edu/Shared drives/2C2T_Cembra/Cembra/Rhemi's /DATA collection/DATA/"

# Get Session IDs Example -------------------------------------------------

session_ids <- get_session_ids(start_date = start, end_date = end)
print(session_ids)

# Get Directory Example ---------------------------------------------------

dir <- get_directory(root_directory = root, session_id = session_ids[1])
print(dir)

# Get Melixa Example ------------------------------------------------------

melixa <- get_melixa(session = session_ids[1], root_directory = root, start_time = start, end_time = end)
head(melixa)

# Get Gallagher Example ---------------------------------------------------

#gallagher <- get_gallagher(session = session_ids[1], root_directory = root, start_time = start, end_time = end)

# Get TrentiNoise Example -------------------------------------------------

TrentiNoise <- get_TrentiNoise(session = session_ids[1], root_directory = root, start_time = start, end_time = end)
head(TrentiNoise)


# Combine Weather Data Example --------------------------------------------

weather_data <- combine_weather(melixa_dat = melixa, TrentiNoise_dat = TrentiNoise)


# Get Thermal Data Example ------------------------------------------------

thermal <- find_thermal(session = session_ids[1], root_directory = root, start_time = start, end_time = end)

# Merge Datasets Example --------------------------------------------------

merged_data <- merge_datasets(weather = weather_data, thermal = thermal)


# Build Request Example ---------------------------------------------------

request <- build_request(start_date = start, end_date = end, root_directory = root)
