## ---------------------------
##
## Script name: Thermal Imagery Functions
##
## Author: Rhemi Toth
##
## Date Created: 2024-07-30
##
## Email: rhemitoth@g.harvard.edu
##
## ---------------------------
##
## Notes: This script contains functions that are used to 
# calculate surface temperature from raw FLIR images (tiff format) 
##
## ---------------------------


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(terra)
library(TrenchR)

# Load raster as array ----------------------------------------------------
#
# This function loads rasters into R using the terra library and converts them to an array
#
# Arguments:
# filepath (string): filepath where the raster is saved
#
# Returns:
# a an array

raster2array <- function(filepath){
  
  # Load raster
  r <- rast(filepath)
  
  # Convert to an array
  r_array <- as.array(r)
  
  # Return the result
  return(r_array)
  
}

# Extract Image Datetime --------------------------------------------------
#
# This function is used to extract the time that a thermal image was captured using the filename
#
# Arguments:
# filename (string): Filename where the image is saved (file-yyyymmdd-hhmmss_burstX.tiff)
#
# Returns:
# creation_time (datetime object)

extract_image_datetime <- function(filename){
  
  file <- filename
  
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
  
  # Return the result
  return(ts)
  
}

# Get LAI -----------------------------------------------------------------
#
# This function finds the closest LAI at the time of image capture
#
# Args:
# lai_data (dataframe): filepath where LAI timeseries is stored (CSV is generated using Google Earth Engine)
# dt (datetime): Timestamp of the image capture
#
# Returns:
# lai (numeric): LAI at the time of image capture

get_lai <- function(lai_data, dt){
  
  # Read in the data
  df <- read.csv(lai_data)
  
  # Check that "date" and "LAI" columns exist
  
  if("date" %in% colnames(df) == FALSE){
    print("ERROR: No column called 'date' in LAI dataframe")
    break
  }
  
  if("LAI" %in% colnames(df) == FALSE){
    print("ERROR: No column called 'LAI' in LAI dataframe")
    break
  }
  
  # Covert date column from character to datetime object
  
  if(class(df$date) == "character"){
    df$date <- ymd(df$date)
  }
  
  # Calculate the absolute differences between the time column and target time
  df <- df %>%
    mutate(diff = abs(difftime(date, date(dt), units = "days")))
  
  # Find the row with the minimum difference
  closest_row <- df %>%
    filter(diff == min(diff)) %>%
    select(-diff)  # Remove the difference column if no longer needed
  
  # Get LAI
  lai <- closest_row$LAI
  
  # Return result
  return(lai)

}


# Get Atmospheric Transmissivity ------------------------------------------
#
# This function finds the closest atmospheric transmissivity at the time of image capture
#
# Args:
# at_data (string): Filepath where atmospheric transmissivity timeseries is stored (CSV is generated using Google Earth Engine)
# dt (datetime): Timestamp of the image capture
#
# Returns:
# a_trans (numeric): Atmospheric transmissivity at the time of image capture

get_atmospheric_trans <- function(at_data, dt){
  
  # Read in the data
  df <- read.csv(at_data)
  
  # Check that "date" and "LAI" columns exist
  
  if("time" %in% colnames(df) == FALSE){
    print("ERROR: No column called 'time' in atmospheric transimissivity dataframe")
    break
  }
  
  if("clear_sky_index" %in% colnames(df) == FALSE){
    print("ERROR: No column called 'clear_sky_index' in atmospheric transimissivity dataframe")
    break
  }
  
  # Covert date column from character to datetime object
  
  if(class(df$time) == "character"){
    df$time <- ymd_hms(df$time)
  }
  
  # Calculate the absolute differences between the time column and target time
  df <- df %>%
    mutate(diff = abs(difftime(time, dt, units = "secs")))
  
  # Find the row with the minimum difference
  closest_row <- df %>%
    filter(diff == min(diff)) %>%
    select(-diff)  # Remove the difference column if no longer needed
  
  # Get Atmospheric Transmissivity
  atmospheric_transmissivity <- closest_row$clear_sky_index
  
  # Return result
  return(atmospheric_transmissivity)
  
}


# Get Albedo --------------------------------------------------------------
#
# Finds the closest land surface albedo at the time of image capture.
# 
# Args:
# albedo_data (dataframe): File where albedo timeseries is saved (CSV is generated using Google Earth Engine)
# datetime (datetime): Timestamp of the image capture
# 
# Returns:
# float: albedo at the time of image capture

get_albedo <- function(albedo_data,dt){
  
  # Read in the data
  df <- read.csv(albedo_data)
  
  # Check that "date" and "LAI" columns exist
  
  if("date" %in% colnames(df) == FALSE){
    print("ERROR: No column called 'date' in albedo dataframe")
    break
  }
  
  if("albedo" %in% colnames(df) == FALSE){
    print("ERROR: No column called 'albedo' in albedo dataframe")
    break
  }
  
  # Covert date column from character to datetime object
  
  if(class(df$date) == "character"){
    df$date <- ymd(df$date)
  }
  
  # Calculate the absolute differences between the time column and target time
  df <- df %>%
    mutate(diff = abs(difftime(date, date(dt), units = "days")))
  
  # Find the row with the minimum difference
  closest_row <- df %>%
    filter(diff == min(diff)) %>%
    select(-diff)  # Remove the difference column if no longer needed
  
  # Get Albedo
  albedo <- closest_row$albedo
  
  # Return result
  return(albedo)
  
}

# Get DOY -----------------------------------------------------------------

# This function calculates the day of the year from a timestamp
#
# Arguments:
# timestamp (datetime object): Date and time in the format "YYYY-MM-DD hh:mm:ss"
#
# Returns:
# doy (integer): the day of year

get_doy <- function(timestamp){
  
  # Extract the day of the year
  doy <- yday(timestamp)
  
  # Return the result
  doy <- as.numeric(doy)
  return(doy)
}


# Get hour ----------------------------------------------------------------

# This function calculates the hour of the day from a timestamp
#
# Arguments:
# timestamp (datetime object): Date and time in the format "YYYY-MM-DD hh:mm:ss"
#
# Returns:
# hour (integer): the hour of the day (0 - 24)

get_hour <- function(timestamp){
  
  # Extract the hour
  hour <- hour(timestamp)
  
  # Return the result
  return(hour)
  
}

# Calculate Zenith Angle --------------------------------------------------

# This function calculates the zenith angle of the sun based on the day of the year,
# latitude, and longitude. If the slope is not flat, the function calls update_zenith
# to apply a correction to the zenith angle based on the slope of the ground.
# Corrections for the zenith angle are conducted using equations from
# "Underlying theory and equations of the NicheMapR microclimate model" (Kearney & Porter 2024)

# Arguments
# DOY (integer): Day of the year (1-365)
# LAT (float): Latitude
# LON (float): Longitude
# SLOPE (float): Slope of the ground surface in degrees
# SLOPE_ASPECT (float): Aspect of the slope in degrees
# HEMISHPHERE (text): Hemisphere of the location ("north" or "south")

get_zenith <- function(datetime,LAT,LON,SLOPE,SLOPE_ASPECT,HEMISPHERE){
  
  # Get the day of the year from the datetime
  DOY <- get_doy(datetime)
  
  # Get the hour of the day from the datetime
  HOUR <- get_hour(datetime)
  
  # Get the closest hour of the day from the datetime
  zenith <- zenith_angle(doy = DOY, lat = LAT, lon = LON, hour = HOUR)
  
  # Convert inputs to radians
  
  zenith <- degrees_to_radians(zenith)
  SLOPE <- degrees_to_radians(SLOPE)
  LAT <- degrees_to_radians(LAT)
  LON <- degrees_to_radians(LON)
  SLOPE_ASPECT <- degrees_to_radians(SLOPE_ASPECT)
  
  # Get ecpliptic longitude of earths orbit
  e <- 0.01675 # eccentricity of Earth's orbit
  w <- 2*pi/365
  ecliptic_longitude <- w * (DOY - 80) + 2 * e * (sin(w*DOY)-sin(w*80))
  
  # Get the solar declination angle
  solar_declination <- asin(0.39784993*sin(ecliptic_longitude))
  
  # Apply correction if location is not on flat ground
  
  if (SLOPE != 0){
    if(HEMISPHERE == "north"){
      a <- sin(LAT)
      b <- sin(90*pi/180-zenith)
      c <- sin(solar_declination)
      d <- cos(LAT)
      e <- cos(90*pi/180-zenith)
      sun_azimuth <- acos((a*b-c)/(d*e))
    }
    else{
      a <- sin(solar_declination)
      b <- sin(LAT)
      c <- sin(90*pi/180-zenith)
      d <- cos(LAT)
      e <- cos(90*pi/180-z)
      sun_azimuth <- acos((a-b*c)/(d*e))
    }
    
    a <- cos(zenith)
    b <- cos(SLOPE)
    c <- sin(zenith)
    d <- sin(SLOPE)
    e <- cos(sun_azimuth-SLOPE_ASPECT)
    
    zenith_updated <- acos(a*b+c*d*e)
    
  }
  else{
    zenith_updated <- zenith
  }
  
  res <- data.frame(
    ZENITH_ORIG = zenith,
    ZENITH_UPDATED = zenith_updated,
    SUN_AZIMUTH = sun_azimuth
  )
  
  return(res)
  print(res)
  
}


# Calculate Solar Radiation -----------------------------------------------

# This function calculate the total solar radiation reaching a point on the Earth's surface, accounting for atmospheric transitivity, but not
# shading from vegetation (to do this you need to apply a correction using Beer's law).
# The function calls get_zenith() to compute zenith angles.
#
# Args:
# dt (string): Date and time in the format "YYYY-MM-DD hh:mm:ss"
# lat (float): Latitude in degrees
# lon (float): Longitude in degrees
# slope (float): slope of the ground surface in degrees
# slope_aspect (float): aspect of the ground slope in degrees
# hemisphere (text): hemisphere where solar radiation is being calculate ("north" or "south")
# ATMOSPHERIC_TRANSMISSIVITY (float): ratio of the global solar radiation measured at the surface to the total solar radiation at the top of the atmosphere. (0-1)
# ELEV (float): Elevation where solar radiation is being calculated in meters
# ALBEDO (float): albedo of the ground surface (0-1)
#
# Returns:
# solrad (float): Solar radiation reaching the ground surface in W/m2

get_solar_radiation <- function(dt,
                                lat,
                                lon,
                                slope,
                                slope_aspect,
                                hemisphere,
                                ATMOSPHERIC_TRANSMISSIVITY,
                                ELEV,
                                ALBEDO,
                                lai){
  # calculate the zenith
  zenith_df <- get_zenith(datetime = dt, LAT = lat, LON = lon, SLOPE = slope, SLOPE_ASPECT =  slope_aspect, HEMISPHERE = "north")
  zenith <- zenith_df$ZENITH_UPDATED[1]
  
  # calculate solar radiation
  
  doy <- get_doy(dt)
  
  solrad <- solar_radiation(doy = doy,
                            psi = zenith,
                            tau = ATMOSPHERIC_TRANSMISSIVITY,
                            elev = ELEV,
                            rho = ALBEDO)
  
  solrad <- sum(solrad) # summing the direct, diffuse, and reflected radiation
  
  # Apply Beer's law correction to account for attenuation by vegetation\
  k <- 0.5 # extinction coefficient (Harrow and Sommer 2002, Remote Sensing of Environment)
  solrad <- solrad * exp(-0.5 * lai)
  
  # returning the result
  return(solrad)
  
}


# Estimate Ground Surface Temperature -------------------------------------
#
# This function is used to estimate ground surface temperature based on an empirical relationship
# between ground surface temperature and air temperature identified by Bartlett et al. 2006. A decade of ground-air temperature tracking at emigrant pass observatory, Utah. Journal of Climate. 19: 3722-3731.
# The code for this function was adapted from the Tground function in the Thermimage R package. 
#
# Args:
# Ta (float): air temperature (Celcius)
# SE (float): Incident solar radiaiton (W per m2)
#
# Returns:
# t_ground (float): ground surface temperature (Celcius)

get_t_ground <- function(Ta,SE) {
  
  # Compute Ground Temperature
  t_ground<-0.0121*SE+Ta
  
  # Return the result
  return(t_ground)
}


# Get Ground Emissivity ---------------------------------------------------
#
# Finds the closest land surface emissivity  at the time of image capture.
# 
# Args:
# lse_data (dataframe): File where land surface emissivity timeseries is saved (CSV is generated using Google Earth Engine)
# dt (datetime): Timestamp of the image capture
# 
# Returns:
# float: LSE at the time of image capture

get_lse <- function(lse_data, dt){
  # Read in the data
  df <- read.csv(lse_data)
  
  # Check that "date" and "LAI" columns exist
  
  if("date" %in% colnames(df) == FALSE){
    print("ERROR: No column called 'date' in LSE dataframe")
    break
  }
  
  if("LSE" %in% colnames(df) == FALSE){
    print("ERROR: No column called 'LSE' in LSE dataframe")
    break
  }
  
  # Covert date column from character to datetime object
  
  if(class(df$date) == "character"){
    df$date <- ymd(df$date)
  }
  
  # Calculate the absolute differences between the time column and target time
  df <- df %>%
    mutate(diff = abs(difftime(date, date(dt), units = "days")))
  
  # Find the row with the minimum difference
  closest_row <- df %>%
    filter(diff == min(diff)) %>%
    select(-diff)  # Remove the difference column if no longer needed
  
  # Get Atmospheric Transmissivity
  lse <- closest_row$LSE
  
  # Return result
  return(lse)
}



# Get Upward Longwave Radiation -------------------------------------------
#
# This function estimates upward longwave radiation emitted by the ground based on ground surface temperature and
# ground emissivty using a relationship derived by Blaxter, 1986. Energy metabolism in animals and man. Cambridge University Press, Cambridge, UK, 340 pp.
# The code for this function was adapted from the Lu function in the Thermimage R package. 
#
# Arguments:
# Tg (float) = ground surface temperature (Celcius)
# lse (float) = Land surface emissivity (0 - 1)
#
# Returns:
# lw_upward (float): Upward facing ground radiation (W/m2)

get_lw_upward <- function(Tg, lse){
  
  # Convert ground temperature from celcius to Kelvin
  GT <- Tg + 273.15
  
  # Estimate upward longwave radiation
  lw_upward <- lse*5.67e-08*(GT)^4
  
  # Return the result
  return(lw_upward)
}


# Get Cloud Cover Fraction ------------------------------------------------
#
# This function finds the cloud cover fraction to the time of image capture
#
# Args:
# cloud_cover_data (string): Filepath where cloud cover timeseries is stored (CSV is generated using Google Earth Engine)
# dt (datetime): Timestamp of the image capture
#
# Returns:
# cloud_cover (numeric): Cloud cover fraction at the time of image capture

get_cloud_cover <- function(cloud_cover_data, dt){
  
  # Read in the data
  df <- read.csv(cloud_cover_data)
  
  # Check that "date" and "LAI" columns exist
  
  if("timestamp" %in% colnames(df) == FALSE){
    print("ERROR: No column called 'timestamp' in cloud cover dataframe")
    break
  }
  
  if("cloud_fraction" %in% colnames(df) == FALSE){
    print("ERROR: No column called 'cloud_fraction' in cloud cover dataframe")
    break
  }
  
  # Covert date column from character to datetime object
  
  if(class(df$timestamp) == "character"){
    df$timestamp <- ymd_hms(df$timestamp)
  }
  
  # Calculate the absolute differences between the time column and target time
  df <- df %>%
    mutate(diff = abs(difftime(timestamp, dt, units = "secs")))
  
  # Find the row with the minimum difference
  closest_row <- df %>%
    filter(diff == min(diff)) %>%
    select(-diff)  # Remove the difference column if no longer needed
  
  # Get Atmospheric Transmissivity
  cloud_cover <- closest_row$cloud_fraction
  
  # Return result
  return(cloud_cover)
  
}

# Get Downward Longwave Radiation -----------------------------------------
#
# This function estimates downward incoming longwave radiation based on air temperature, humidity, and fractional cloud cover
# using a relationship derived from Konzelmann et al. 1994.
# The code for this function was adapted from the Ld function in the Thermimage R package. 
#
# Arguments:
# Ta (float) = ground surface temperature (Celcius)
# RH (float) = Relative humidity (0 - 1)
# n (float) = Fractional cloud cover (0 - 1)
# Returns:
# lw_downward (float): Downward facing incoming longwave radiation (W/m2)

get_lw_downard <- function(Ta, RH, n) {
  
  # Convert air temperature from celcius to kelvin
  AT <- Ta + 273.15
  
  # Estimate water vapor pressure of air
  WVPs<-611*exp(17.27*(AT-273.15)/(AT-36))  # Pascals
  WVP<-RH*WVPs # saturated vapour pressure at AT
  
  # Emissivity of clear sky
  ecs<-0.23 + 0.443*(WVP/AT)^(1/8) # emissivity clear sky
  
  # Emissivity of clouds
  ecl<-0.976 
  
  # Total emissivity of the sky
  etotal<-ecs*(1-n^2) + ecl*n^2
  
  # Calculate downward longwave radiation
  Ld<-etotal*5.67e-08*AT^4
  
  # Return result
  return(Ld)
}



