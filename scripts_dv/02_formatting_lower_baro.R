##==============================================================================
## Project: QuEST
## This script is to format baro data for DV lower sites. Baro data is from DW4069 Weather Station
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

##################
#### Packages ####
##################
library(googledrive)
library(ggplot2)
library(dplyr)

####################################
## Clear folders that we will use ##
####################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "merged", full.names = TRUE)
file.remove(files)

##############################################
#### Load PT depth data from Google drive ####
##############################################
(depth <- drive_get("https://docs.google.com/spreadsheets/d/1JZ1q_3HWNe253P9kkgNtJVNcDb5tnRQfaTh0VFiUr8Y/edit?gid=0#gid=0"))
3

# download the file as a csv file
drive_download(as_id(depth$id), path = "googledrive/DV_baro_lower.csv", type = "csv", overwrite = T)

# fetch the file
air_lower <- read.csv("googledrive/DV_baro_lower.csv")

# rename columns and convert types
air_lower <- air_lower %>%
  # rename columns
  dplyr::rename(
    LEVEL.mb = Station_Pressure_mb,
    TEMPERATURE.C = Temp_C,
    DateTime = Date_Time
  ) %>%
  mutate(
    # change date format
    DateTime = as.POSIXct(DateTime, format = "%m/%d/%Y %H:%M:%S"),
    # change pressure units from mb to m
    LEVEL.m = (LEVEL.mb * 0.0101972)
    )

# 1 mbar = 0.0101972 m

##############################
#### Save formatted file  ####
##############################
write.csv(air_lower, "data/air_lower.csv")

drive_folder_id <- "1SeGx6MUt6icUFum4Yu-kUHHqZSbN4AQU"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/air_lower.csv",
  path = as_id(drive_folder_id)
)

