##==============================================================================
## Project: QuEST
## This script is to calculate barologger offset for Dog Valley air_upper - DVNWT5
##==============================================================================

##################
#### Packages ####
##################
library(dplyr)
library(ggplot2)
library(lubridate)

####################################
## Clear folders that we will use ##
#################################### 
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data", full.names = TRUE)
file.remove(files)

#########################
#### Get air pt data ####
#########################
# this is the merged days baro folder
pt_air <- googledrive::as_id("https://drive.google.com/drive/folders/1SeGx6MUt6icUFum4Yu-kUHHqZSbN4AQU")
# list all CSV files in the folder
pt_csvs_air <- googledrive::drive_ls(path = pt_air, type = "csv")
3
# call the specific file you want
googledrive::drive_download(file = pt_csvs_air$id[pt_csvs_air$name=="air_upper.csv"], 
                            path = "googledrive/air_upper.csv",
                            overwrite = T)

air_upper <- read.csv("googledrive/air_upper.csv")

# changing some column names
air_upper <- air_upper %>%
  dplyr::rename(Date.air = Date,
                Time.air = Time,
                Level_air.m = LEVEL)

################################
#### Format DateTime column ####
################################
# check datetime format
air_upper$DateTime <- paste(air_upper$Date.air, air_upper$Time.air, sep = " ")
# convert the DateTime column to POSIXct
air_upper$DateTime <- as.POSIXct(air_upper$DateTime, format = "%Y-%m-%d %I:%M:%S %p")

#################################################
#### Find offset, when did the change happen ####
################################################# 
ggplot(air_upper, aes(x = DateTime, y = Level_air.m)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-09-05"), linetype="dashed", color="red") + # "sensor moved"
  geom_vline(xintercept = as.POSIXct("2024-11-15"), linetype="dashed", color="red") + # sensor change
  labs(title = "LEVEL", x = "Date", y = "Water Level (m)")

############################
#### Look at it closely ####
############################
# take the subset of the data
# Sept 6th - 12th
Date1 <- as.Date("2024-09-04", "%Y-%m-%d")
Date2 <- as.Date("2024-09-15", "%Y-%m-%d")
subdf <- air_upper[air_upper$DateTime < Date2 & air_upper$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Level_air.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-08-06 08:15:00"), linetype="dashed", color="red") 

# Nov 17th - 20th
Date1 <- as.Date("2024-11-17", "%Y-%m-%d")
Date2 <- as.Date("2024-11-20", "%Y-%m-%d")
subdf <- air_upper[air_upper$DateTime < Date2 & air_upper$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Level_air.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-09-13 15:50:00"), linetype="dashed", color="red")

######################################################################
#### Remove times where PT was out of the water and error section ####
######################################################################
Date1 <- as.Date("2024-11-17", "%Y-%m-%d")
Date2 <- as.Date("2024-11-20", "%Y-%m-%d")
air_upper$Level_air.m[air_upper$DateTime >= Date1 & air_upper$DateTime <= Date2] <- NA
Date1 <- as.Date("2024-09-06", "%Y-%m-%d")
Date2 <- as.Date("2024-09-13", "%Y-%m-%d")
air_upper$Level_air.m[air_upper$DateTime >= Date1 & air_upper$DateTime <= Date2] <- NA

# plot after cleaning
ggplot(air_upper, aes(x = DateTime, y = Level_air.m)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-05"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2024-10-24"), linetype="dashed", color="red") +
  labs(title = "Level_air.m", x = "Date", y = "Water Level (m)")

###################
#### Save file ####
###################
write.csv(air_upper, "data/DVNWT5.csv")

# this is the "merged days baro" folder
drive_folder_id <- "1SeGx6MUt6icUFum4Yu-kUHHqZSbN4AQU"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/DVNWT5.csv",
  path = as_id(drive_folder_id)
)
