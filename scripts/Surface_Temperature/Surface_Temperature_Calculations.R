## ---------------------------
##
## Script name: Surface Temperature Calculations
##
## Author: Rhemi Toth
##
## Date Created: 2024-07-30
##
## Email: rhemitoth@g.harvard.edu
##
## ---------------------------
##
## Notes: This script demonstrates a workflow for calculating surface temperature form raw FLIR images
##   
##
## ---------------------------


# Load Thermal Imagery Functions ------------------------------------------

source("/Users/rhemitoth/Documents/PhD/Cembra/Cembra_Tools/scripts/Functions/Thermal_Imagery_Functions.R")

#  Load Database Query Functions ------------------------------------------

source("/Users/rhemitoth/Documents/PhD/Cembra/Cembra_Tools/scripts/Functions/QueryDatabaseFunctions.R")

# Google Authentication ---------------------------------------------------

source("/Users/rhemitoth/Documents/PhD/Cembra/Cembra_Tools/scripts/Google_Authentication.R")


# Specify Time Period and Location Parameters -----------------------------

start <- "2024-07-17" # start of time period
end <- "2024-07-18" # end of time period
latitude <- 46.130961999999997 # latitude where solar radiation will be calculated (degrees)
longitude <- 11.190009999999999 # longitude where solar radiation will be calculated (degrees)
SLOPE <- 1.017573714256287 # slope of the ground at the location where solar radiation will be calculated (degrees). You can calculate this using your method of choice (ArcGIS, QGIS, terra package etc.)
aspect <- 92.496437072753906 # aspect of the slope at the location where solar radiation will be calculated (degrees). You can calculate this using your method of choice (ArcGIS, QGIS, terra package etc.)
elev <- 900.943969726562614 # elevation at the location where solar radiation will be calculated. You can calculate this using your method of choice (ArcGIS, QGIS, terra package etc.)
HEMISPHERE <- "north" # hemisphere of the earth at the location where solar radiation will be calculated

# Step 2: Query Database --------------------------------------------------

root <- "/Users/rhemitoth/Library/CloudStorage/GoogleDrive-rhemitoth@g.harvard.edu/Shared drives/2C2T_Cembra/Cembra/Rhemi's /DATA collection/DATA/"
database_request <- build_request(start_date = start, end_date = end, root_directory = root)

# Step 3: Specify location of albedo, atmospheric transmissivity, LAI, and LSE timeseries data on computer --------

lai_data <- "/Users/rhemitoth/Library/CloudStorage/GoogleDrive-rhemitoth@g.harvard.edu/My Drive/Cembra/Cembra_GEE/LAI_TimeSeries.csv"
atmospheric_transmissivity_data <- "/Users/rhemitoth/Library/CloudStorage/GoogleDrive-rhemitoth@g.harvard.edu/My Drive/Cembra/Cembra_GEE/ClearSkyIndexTimeSeries.csv"
albedo_data <- "/Users/rhemitoth/Library/CloudStorage/GoogleDrive-rhemitoth@g.harvard.edu/My Drive/Cembra/Cembra_GEE/albedo_time_series.csv"
lse_data <- "/Users/rhemitoth/Library/CloudStorage/GoogleDrive-rhemitoth@g.harvard.edu/My Drive/Cembra/Cembra_GEE/LSE_TimeSeries.csv"
cloud_cover_data <- "/Users/rhemitoth/Library/CloudStorage/GoogleDrive-rhemitoth@g.harvard.edu/My Drive/Cembra/Cembra_GEE/CloudCover_TimeSeries_Sentinel5P_NRTI_Hourly.csv"

# Step 4: Perform the Calculations ----------------------------------------

num_images <- nrow(database_request)

for(i in 1:num_images){
  
  # Get the image filepath
  fpath <- database_request$filepath[i]
  
  # Get the image filename
  fname <- database_request$filename[i]
  
  # Load the image as an array
  img_array <- raster2array(fpath)
  
  # Extract the time that the image was captured
  img_datetime <- extract_image_datetime(fname)
  
  # Get the air temperature at the time of image capture
  temp <- database_request$temperature[i]
  
  # Get the humiditiy at the time of image capture
  hum <- database_request$humidity[i]
  
  # Get the albedo at the time of image capture
  albedo <- get_albedo(albedo_data = albedo_data,
                       dt = img_datetime)
  
  # Get the atmospheric transmissivity at the time of image capture
  a_trans <- get_atmospheric_trans(at_data = atmospheric_transmissivity_data,
                                   dt = img_datetime)
  
  # Get the land surface emissivity at the time of image capture
  lse <- get_lse(lse_data = lse_data,
                 dt = img_datetime)
  
  # Get the LAI at the time of image capture
  lai <- get_lai(lai_data = lai_data,
                 dt = img_datetime)
  
  # Estimate solar radiation at the time of image capture
  solrad <- get_solar_radiation(dt = img_datetime,
                                lat = latitude,
                                lon = longitude,
                                slope = SLOPE,
                                slope_aspect = aspect,
                                hemisphere = HEMISPHERE,
                                ATMOSPHERIC_TRANSMISSIVITY = a_trans,
                                ELEV = elev,
                                ALBEDO = albedo,
                                lai = lai)
  
  # Estimate ground surface temperature
  ground_temp <- get_t_ground(Ta = temp, SE = solrad)
  
  # Estimate cloud cover fraction
  cloud_cover <- get_cloud_cover(cloud_cover_data = cloud_cover_data,
                                 dt = img_datetime)
  
  # Estimate longwave radiation
  upward_lw <- get_lw_upward(Tg = ground_temp, lse = lse) # Upward facing longwave radiation from the ground
  downward_lw <- get_lw_downard(Ta = temp, RH = hum, n = cloud_cover) # Downward facing longwave radiation from the sky
  lw_total <- upward_lw + downward_lw

  
}
