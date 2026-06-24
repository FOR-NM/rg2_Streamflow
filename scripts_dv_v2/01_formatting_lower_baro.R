##==============================================================================
## Project: QuEST
## This script is to format baro data for DV lower sites. Baro data is part from DW4069 Weather Station the other from baro logger
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

##################
#### Packages ####
##################
library(googledrive)
library(ggplot2)
library(dplyr)
library(lubridate) 

####################################
## Clear folders that we will use ##
####################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

##############################################
#### Load PT depth data from Google drive ####
##############################################
#### weather station data ####
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
    # change pressure units from mb to kpa
    LEVEL.kPa = (LEVEL.mb * 0.1)
    )
# 1 mbar = 0.1kPa

# change date format
air_lower$DateTime <- as.POSIXct(air_lower$DateTime, format = "%m/%d/%Y %H:%M:%S")
air_lower$Date <- as.POSIXct(air_lower$DateTime, format = "%Y-%m-%d")
air_lower$Time <- hms::as_hms(air_lower$DateTime)

# round time to nearest 15 #
# create extra columns so you don't erase original time               
air_lower$DateTimeNotRounded <- air_lower$DateTime
# transform to datetime format
air_lower$DateTimeNotRounded <- as.POSIXct(air_lower$DateTimeNotRounded,format = "%Y-%m-%d %H:%M:%S")

# round DateTime to the nearest 15-minute interval 
air_lower$DateTime <- round_date(air_lower$DateTime, unit="15 mins")
air_lower$TimeOnly <- round_date(air_lower$TimeOnly, unit="15 mins")

# remove duplicates
air_lower <- air_lower[duplicated(air_lower$DateTime), ]
air_lower <- air_lower[duplicated(air_lower$DateTime), ]

air_lower$Time <- format(air_lower$DateTime, "%H:%M:%S") # extracts time as string

# plot
ggplot(data = air_lower, aes(x = DateTime, y = LEVEL.kPa)) +
  geom_line() + ggtitle("Baro logger data - lower sites")

Date1 <- as.Date("2025-03-01", "%Y-%m-%d")
Date2 <- as.Date("2025-03-15", "%Y-%m-%d")
subdf <- air_lower[air_lower$DateTime < Date2 & air_lower$DateTime > Date1,]
# sheet says we got to the site at 12:00:00
ggplot(data=subdf, aes(DateTime,LEVEL.kPa)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-07 12:15:00"), linetype="dashed", color="red") 

#### baro logger data ####
# this is the raw baro folder
pt_air <- googledrive::as_id("https://drive.google.com/drive/folders/1XRKIeb4eKFngCjs2owqfjO0xzOOkZHQ6")
# list all CSV files in the folder
pt_csvs_air <- googledrive::drive_ls(path = pt_air, type = "csv")
# call the specific file you want
googledrive::drive_download(file = pt_csvs_air$id[pt_csvs_air$name=="2212515_2026_03_09_DVO_Barologger.csv"], 
                            path = "googledrive/2212515_2026_03_09_DVO_Barologger.csv",
                            overwrite = T)
DVO_error <- read.csv("googledrive/2212515_2026_03_09_DVO_Barologger.csv", skip = 10)
# rename columns and convert types
DVO_error <- DVO_error %>%
  mutate(
    # make date into date format
    Date = as.Date(Date, format = "%m/%d/%Y"),
    # combine Date and Time columns into a new DateTime column
    DateTime = paste(Date, Time, sep = " "),
    # convert the DateTime column to POSIXct
    DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %I:%M:%S %p")
  )

# make time the same format in both files
DVO_error$Time <- format(DVO_error$DateTime, "%H:%M:%S") # extracts time as string
DVO_error$LEVEL.kPa <- DVO_error$LEVEL
# plot
ggplot(data = DVO_error, aes(x = DateTime, y = LEVEL.kPa)) +
  geom_line() + ggtitle("Baro logger data - lower sites")

Date1 <- as.Date("2024-09-01", "%Y-%m-%d")
Date2 <- as.Date("2024-09-15", "%Y-%m-%d")
subdf <- DVO_error[DVO_error$DateTime < Date2 & DVO_error$DateTime > Date1,]
# sheet says we got to the site at 12:00:00
ggplot(data=subdf, aes(DateTime,LEVEL.kPa)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-07 12:15:00"), linetype="dashed", color="red") 


##############################
#### Save formatted file  ####
##############################
# directory
drive_folder_id <- "1_KUXHWDuAbO3Z6EV_RKBZL9IyWeeaj-X"

# write Weather Station file
write.csv(air_lower, "data/DVO_WeatherStation.csv")

# upload file to the specified Google Drive folder
drive_put(
  media = "data/DVO_WeatherStation.csv",
  path = as_id(drive_folder_id)
)

# write baro logger file
write.csv(DVO_error, "data/DVO_logger.csv")

# upload file to the specified Google Drive folder
drive_put(
  media = "data/DVO_logger.csv",
  path = as_id(drive_folder_id)
)

