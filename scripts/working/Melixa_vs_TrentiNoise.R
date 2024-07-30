## ---------------------------
##
## Script name: TrentiNoise vs Melixa
##
## Author: Rhemi Toth
##
## Date Created: 2024-07-30
##
## Email: rhemitoth@g.harvard.edu
##
## ---------------------------
##
## Notes: This script generates plots to compare the TrentiNoise weather station with the Melixa
##   
##
## ---------------------------


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggpubr)

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

# Get Directory Example ---------------------------------------------------

dir <- get_directory(root_directory = root, session_id = session_ids[1])

# Get Melixa Example ------------------------------------------------------

melixa <- get_melixa(session = session_ids[1], root_directory = root, start_time = start, end_time = end)

# Get TrentiNoise Example -------------------------------------------------

TrentiNoise <- get_TrentiNoise(session = session_ids[1], root_directory = root, start_time = start, end_time = end)


# Prep data for plotting --------------------------------------------------

# Prep TrentiNoise

TrentiNoise_filt <- TrentiNoise %>%
  select(temperature,humidity,wind_speed,date)%>%
  mutate(dataset = "TrentiNoise")

TrentiNoise_5MinAvg <- TrentiNoise_filt %>%
  mutate(floor = floor_date(date, "5 min"))%>%
           group_by(floor)%>%
  summarize(temperature = mean(temperature),
            humidity = mean(humidity),
            wind_speed = mean(wind_speed))%>%
  mutate(dataset = "TrentiNoise 5 Min Avg",
         date = floor)%>%
    select(-c(floor))

# Prep Melixa Data

melixa_filt <- melixa %>%
  mutate(dataset = "Melixa")%>%
  select(-c(weight))%>%
  rename(wind_speed = "wind_speed_mean")

merged_data <- rbind(TrentiNoise_filt, melixa_filt, TrentiNoise_5MinAvg)%>%
  filter(date > ymd_hms("2024-07-17 10:50:00"),
         date < ymd_hms("2024-07-17 13:50:00"))

# Temperature Plot
temp <- ggplot(merged_data, aes(x = date, y = temperature, color = dataset))+
                 geom_line()

# Humidity Plot
hum <- ggplot(merged_data, aes(x = date, y = humidity, color = dataset))+
  geom_line()

# Wind Speed Plot
ws <- ggplot(merged_data, aes(x = date, y = wind_speed, color = dataset))+
  geom_line()

# Comparison Plot
comparison_plot <- ggarrange(temp,hum,ws, common.legend = TRUE)

# Save result
ggsave(filename = "/Users/rhemitoth/Documents/PhD/Cembra/Cembra_Tools/plots/Melixa_vs_TrentiNoise.jpg",
       height = 5,
       width = 7,
       units = "in")

  
