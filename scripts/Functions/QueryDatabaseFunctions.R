## ---------------------------
##
## Script name: Query Database Functions
##
## Author: Rhemi Toth
##
## Date Created: 2024-07-12
##
## Email: rhemitoth@g.harvard.edu
##
## ---------------------------
##
## Notes: This script contains functions to pull data from the Cembra Google Drive Database
##   
##
## ---------------------------


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(googlesheets4)
library(lubridate)

# Get Sessions from Start/End Time ----------------------------------------
#
# This function returns the session ids data that were 
# collected between the specified start time and end time are stored
#
# Arguments:
# start_date (datetime object): Start of data period (YYYY-MM-DD)
# end_date (datetime object): End of data period (YYYY-MM-DD)
#
# Returns:
# session_paths (string)

get_session_ids <- function(start_date, end_date){
  
  # Read in and format Week_sessions.gsheet 
  sessions_info <- read_sheet("https://docs.google.com/spreadsheets/d/1udvc_rz1GlOJF2mLwpJN0Tlmtu5IfKq2vNtGdE_Ou00/edit?gid=867151523#gid=867151523")%>%
    select(Week, path, timestamp_start, timestamp_end)
  
  # Filter records between the start and end time
  sessions_info <- sessions_info %>%
    filter(timestamp_start >= start_date,
           timestamp_end <= end_date)
  
  # Get the session ids
  session_ids <- sessions_info$Week
  
  # Return the ids
  return(session_ids)
  
}


# Get Directory -----------------------------------------------------------
#
# This function gets the directory where the data from the session is stored
#
# Arguments:
# root_directory (string): Root directory on your computer that points to the Cembra/Rhemi's/DATA collection/DATA/
# session_id (string): session id (e.g. "week1")

get_directory <- function(root_directory, session_id){
  directory <- paste(root_directory,session_id,sep = "")
  return(directory)
}


# Get Closest Timestamp ---------------------------------------------------
#
# This function matches timestamps from table A to the closest timestamps in table B
#
# Args:
# tsA: timestamps from table A
# tsB: timestamps from table B
#
# Returns:
# Timestamp from table B

find_closest <- function(tsA, tsB) {
  tsB[which.min(abs(tsA - tsB))]
}

# Get Melixa --------------------------------------------------------------

# This function reads in and formats the Melixa data from the Google Drive Database
#
# Arguments: 
# session: session id where the data is stored
# root_directory: Root directory on your computer that points to the Cembra/Rhemi's/DATA collection/DATA/
# start_time (datetime object): Desired data start time
# end_time (datetime object): Desired data end time
#
# Returns:

get_melixa <- function(session,root_directory, start_time, end_time){
  
  # Get the directory where the melixa data is stored using the session id
  directory <- get_directory(root_directory = root_directory, session_id = session)
  
  # Find the melixa data
  files <- list.files(directory)
  numfiles <- length(files)
  for(i in 1:numfiles){
    file <- files[i]
    contains_melixa <- grepl("elixa",file)
    if(contains_melixa == TRUE ){
      melixa_file <- file
      break
    }
  }
  filepath <- paste(directory,"/",melixa_file,sep = "")
  melixa_data_raw <- read_csv2(filepath)
  
  # Clean the data
  melixa_data_cleaned <- melixa_data_raw %>%
    select(weight, temperature,humidity,wind_speed_mean,date)%>%
    mutate(weight = weight/1000 - 17.66,
           temperature = temperature/100)%>%
    mutate(temperature = ifelse(temperature == -1, NA, temperature),
           humidity = ifelse(humidity == 255, NA, humidity),
           wind_speed_mean = ifelse(wind_speed_mean == -1, NA, wind_speed_mean))%>%
    filter(date(date) > start_time,
           date(date) < end_time)

  # Return the result
  return(melixa_data_cleaned)
}


# Get Gallagher -----------------------------------------------------------
#
# This function reads in and formats the Gallagher data from the Google Drive database.
#
# Arguments: 
# session: session id where the data is stored
# root_directory: Root directory on your computer that points to the Cembra/Rhemi's/DATA collection/DATA/
# start_time (datetime object): Desired data start time
# end_time (datetime object): Desired data end time

get_gallagher <- function(session,root_directory, start_time, end_time){
  
  # Get the directory where the melixa data is stored using the session id
  directory <- get_directory(root_directory = root_directory, session_id = session)
  
  # Find the melixa data
  files <- list.files(directory)
  numfiles <- length(files)
  for(i in 1:numfiles){
    file <- files[i]
    contains_gallagher <- grepl("allagher",file)
    if(contains_gallagher == TRUE ){
      gallagher_file <- file
      break
    }
  }
  filepath <- paste(directory,"/",gallagher_file,sep = "")
  gallagher_data_raw <- read_csv(filepath)
  
  # Clean the data
  gallagher_data_clean <- gallagher_data_raw %>%
    select(Date,`Live Weight (kg)`, Species, Age, Sex, True_weight, `Tag Number`)%>%
    mutate(Date = dmy_hm(Date))%>%
    filter(date(Date) > start_time,
           date(Date) < end_time)%>%
    filter(Species == "roe",
           True_weight == "yes")
  
  # Return the result
  return(gallagher_data_clean)
}


# Find thermal data --------------------------------------------------------
#
# This function gets the filenames and timestamps of thermal images in the Google drive database
#
# Arguments:
# session: session id where the data is stored
# root_directory: Root directory on your computer that points to the Cembra/Rhemi's/DATA collection/DATA/
# start_time (datetime object): Desired data start time
# end_time (datetime object): Desired data end time

find_thermal <- function(session,root_directory, start_time, end_time){
  
  # Get the directory where the thermal data is stored using the session id
  directory <- get_directory(root_directory = root_directory, session_id = session)
  directory <- paste(directory,"/","thermal",sep = "")
  
 
  
  # Initialize a dataframe of filenames and timestamps
  files <- list.files(directory)
  numfiles <- length(files)
  thermal_data <- tibble(timestamp = rep(ymd_hms("1900-01-01 00:00:00"),numfiles),
                         filepath = rep("hi",numfiles))
  for(i in 1:numfiles){
    # get the filename
    file <- files[i]
    # get the filepath
    fpath <- paste(directory,file,sep="")
    # get the timestamp from the filename
    file_stringsplit <- strsplit(file,"-")
    date_raw <- file_stringsplit[[1]][2]
    file_double_stringsplit <- strsplit(file_stringsplit[[1]][3],"_")
    time_raw <- file_double_stringsplit[[1]][1]
    # format the timestamp
    year <- substr(date_raw,start = 1, stop = 4)
    month <- substr(date_raw,start=5,stop = 6)
    day <- substr(date_raw,start=7,stop=8)
    hour <- substr(time_raw,start=1,stop=2)
    minutes <- substr(time_raw,start=3,stop=4)
    seconds <- substr(time_raw,start=5,stop=6)
    ts_txt <- paste(year,"-",month,"-",day," ",hour,":",minutes,":",seconds,sep = "") 
    ts <- ymd_hms(ts_txt)
    # Populate the dataframe
    thermal_data$timestamp[i] <- ts
    thermal_data$filepath[i] <- fpath
  }
  # return the result
  return(thermal_data)
}

# Merge Data --------------------------------------------------------------
#
# This function is used to combine the melixa, gallagher, and thermal data into
# one data table. Both the gallagher data and thermal data are matched to the closest 
# melixa record in time. 
#
# Args:
# melixa (dataframe)
# gallagher (dataframe)
# thermal (dataframe)
#
# Returns:
# datatable

merge_datasets <- function(melixa, gallagher, thermal){
  
  # Join the gallagher data to the melixa data
  
  gallagher <- gallagher %>%
    rowwise() %>%
    mutate(closest_timestep = find_closest(Date, melixa$date))
  
  # Join tableA with tableB on the closest timestep
  gallagher_melixa <- gallagher %>%
    left_join(melixa, by = c("closest_timestep" = "date"))
  
  return_gallagher_melixa
  
  
}

# Build Request -----------------------------------------------------------
# 
# This function is used to build a request from the Google Drive database
#
# Args:
# start_date (string): Start date of data ("YYYY-MM-DD")
# end_date (string): End date of data ("YYYY-MM-DD")
#
# Returns:
# Results table

build_request <- function(start_date, end_date){
  
  # Get the session ids that fall between the start and end dates
  
  session_ids <- get_session_ids(start_date, end_date)
  
  # Convert start and end dates to dt objects
  start <- ymd(start_date)
  end <- ymd(end_date)
  
  # Loop through the session ids and pull data from each session folder
  num_sessions <- length(session_ids)
  for(i in 1:num_sessions){
    session <- session_ids[i]
    melixa <- get_melixa(session, start, end)
    gallgher <- get_gallagher(session,start,end)
    thermal <- get_thermal(session,start,end)
    
  }
}
