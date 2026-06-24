##==============================================================================
## Project: QuEST
## This script is to clean predicted discharge files for DV
##
## WARNING (pipeline v2): the offset and depth-conversion scripts upstream
## now add two columns that didn't exist before (Final_Corrected_Lvl in 05,
## Final_Water_Depth_m in 05b), so every discharge_DV<SITE>.csv has 2 extra
## columns compared to the old pipeline. The positional column drops below
## (e.g. DVMS1[,-c(5,6,8,9:12,16)]) are index-based, not name-based, so those
## indices have likely shifted. Re-check column positions with str()/names()
## on a fresh file before trusting these drops — don't assume the old indices
## still point at the same columns.
##==============================================================================

##################
#### Packages ####
##################
library(googledrive) 
library(ggplot2)
library(dplyr)
library(lubridate) 
library(xts) # for time series
library(zoo) # for rollmean function

####################################
## Clear folders that we will use ##
####################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "merged", full.names = TRUE)
file.remove(files)

#####################
#### Import data ####
#####################
# this is the predicted folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1FQCOmazrRF6MN9VGKHvlUq_WrM7LZC26")

# list all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3
## call all the files in the salt slugs folder ##
# create empty list to store data frames
pt_list <- list()

# loop over each file in the `pt_csvs` data frame
for (i in seq_along(pt_csvs$id)) {
  # define the local file path
  local_path <- file.path("googledrive", pt_csvs$name[i])
  
  # download the file
  googledrive::drive_download(
    file = pt_csvs$id[i],
    path = local_path,
    overwrite = T
  )
  # read the CSV file and add it to the list
  pt_list[[pt_csvs$name[i]]] <- read.csv(local_path)
}

# look at it
DVMS1 <- pt_list[["discharge_DVMS1.csv"]]
DVWT3 <- pt_list[["discharge_DVWT3.csv"]]

############################
#### Format date column #### 
############################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # combine Date and Time columns into a new DateTime column
  df$DateTime <- paste(df$Date, df$TimeOnly, sep = " ")
  
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
  # update the data frame in the list
  pt_list[[i]] <- df
  
  # make date into date fomat
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  # update the data frame in the list
  pt_list[[i]] <- df
}

# look at it
DVMS1 <- pt_list[["discharge_DVMS1.csv"]]
DVWT3 <- pt_list[["discharge_DVWT3.csv"]]

##################
#### Clean up #### 
##################
# columns that I don't want
drops <- c("X.1", "X.2", "X.3", "X", "Date.x","Date.y", "Date.air", "LEVEL", "TEMPERATURE","TEMPERATURE.x", "TEMPERATURE.y", "TEMPERATURE.C",
           "Actual_Water_Depth_m", "Actual_Water_Depth_cm.y", "Actual_Water_Depth_m.x",  "Actual_Water_Depth_cm.x",  "Actual_Water_Depth_m.y",
           "ms.x", "ms.y", "medianSPC.x", "Q", "medianSPC.y", "Level_air.m", "final_SPC", "final_index", "SPC_difference", "background",
           "time_arrived_if_applicable", "stage_depth_m", "mean_stream_width_m","sensor_deployment_time", "salt", "salt.x", "salt.y",
           "Direct_Observation.x", "Direct_Observation", "Direct_Observation.y", "Flow_Status", "Flow_Status.x","Flow_Status.y",
           "Time.y", "Time.x", "Time.air", "Predicted_Discharge_Linear_m3s", "Residual_Linear", "LEVEL.kPa", "Baro_positive",
           "slug_flag", "slug_notes", "slug_notes.x", "slug_notes.y", "slug_flag.y", "slug_flag.x", "Time.x.1", "Time.y.1",
           "Sea_Level_Pressure_mb", "Altimeter_Setting_mb", "DateTimeNotRounded")

# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # remove columns 
  df <- df[ , !(names(df) %in% drops)]
  # update the data frame in the list
  pt_list[[i]] <- df
}

# look at it
DVMS1 <- pt_list[["discharge_DVMS1.csv"]]
DVMS5 <- pt_list[["discharge_DVMS5.csv"]]
DVNWT4 <- pt_list[["discharge_DVNWT4.csv"]]
DVNWT5 <- pt_list[["discharge_DVNWT5.csv"]]
DVWT3 <- pt_list[["discharge_DVWT3.csv"]]

######################################
#### Change discharge column name ####
######################################
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  
  # rename columns
  df <- df %>%
    dplyr::rename(Discharge_m3s = Predicted_Discharge_Log_m3s) %>%
    mutate(Discharge_m3s = as.numeric(Discharge_m3s))
  
  pt_list[[i]] <-  df
}

# look at it
DVMS1 <- pt_list[["discharge_DVMS1.csv"]]
DVMS5 <- pt_list[["discharge_DVMS5.csv"]]
DVNWT4 <- pt_list[["discharge_DVNWT4.csv"]]
DVNWT5 <- pt_list[["discharge_DVNWT5.csv"]]
DVWT3 <- pt_list[["discharge_DVWT3.csv"]]

# clean up
DVMS1 <- DVMS1[,-c(5,6,8,9:12,16)]
DVMS5 <- DVMS5[,-c(5,6,8:13,17)]
DVNWT4 <- DVNWT4[,-c(5,6,8:12,16)]
DVNWT5 <- DVNWT5[,-c(5,6,8:14,18)]
DVWT3 <- DVWT3[,-c(5,6,8,9:12,16)]

# return to list
pt_list <- list(
  DVMS1 = DVMS1,
  DVMS5 = DVMS5,
  DVNWT4 = DVNWT4,
  DVNWT5 = DVNWT5,
  DVWT3 = DVWT3
)

#####################################
#### Split data into water years ####
#####################################
# Function to filter for Water Year 2024
filter_wy24 <- function(df) {
  df %>% filter(DateTime >= "2023-10-01" & DateTime <= "2024-09-30")
}

# Function to filter for Water Year 2025
filter_wy25 <- function(df) {
  df %>% filter(DateTime >= "2024-10-01" & DateTime <= "2025-09-30")
}

# Create the two lists
pt_list_wy24 <- lapply(pt_list, filter_wy24)
pt_list_wy25 <- lapply(pt_list, filter_wy25)

#######################################
#### Save merged PT files to Drive ####
#######################################
# loop through each data frame in the list
for (i in seq_along(pt_list_wy24)) {
  # Access the current data frame
  df <- pt_list_wy24[[i]]
  
  # save new data frame
  write.csv(df, paste0("data/", names(pt_list_wy24)[i]), row.names=FALSE, quote=FALSE)
  
  # define the local folder path and the target folder ID in Google Drive
  file <- paste0("data/", names(pt_list_wy24)[i])
  # this is the "smooth" folder
  drive_folder_id <- "1HYPLQ3rG80L1X2SK0ysu_I0RO-wlcxUN"
  
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}

###############################
#### Do just one at a time ####
###############################
# # plot
# ggplot(data = DVMS1, aes(x = DateTime, y = Predicted_Discharge_Log_m3s)) + 
#   geom_line() + ggtitle(paste(pt_csvs$name[i])) 
# 
# # 1. Convert data frame to an xts object
# discharge_xts20 <- xts(DVMS1$Predicted_Discharge_Log_m3s, order.by = DVMS1$DateTime)
# 
# # 2. Apply the 1-hour moving average (4 observations for 15-min data)
# # k = 4 for a 1-hour window with 15-minute data 
# # align = "right" means the result is aligned with the end of the window.
# # na.pad = TRUE will keep the original length and fill leading NAs.
# smoothed_discharge_xts <- rollmean(discharge_xts20, k = 4, align = "right", na.pad = TRUE)
# 
# # 3. Add the smoothed data back to your original data frame
# DVMS1$Smoothed_Discharge_Log_m3s <- as.numeric(smoothed_discharge_xts)
# 
# # plot smoothed discharge 
# ggplot(data = DVMS1, aes(x = DateTime, y = Smoothed_Discharge_Log_m3s)) +
#   geom_line() + ggtitle("DVMS1 compensated level data")
# 
