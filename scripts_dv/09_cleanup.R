##==============================================================================
## Project: QuEST
## This script is to clean predicted discharge files for DV
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
           "ms.x", "ms.y", "medianSPC.x", "Q", "medianSPC.y", "Level_air.m", "final_SPC", "final_index", "SPC_difference",
           "time_arrived_if_applicable", "stage_depth_m", "mean_stream_width_m","sensor_deployment_time", "salt", 
           "Direct_Observation.x", "Direct_Observation", "Direct_Observation.y", "Flow_Status", "Flow_Status.x","Flow_Status.y",
           "Time.y", "Time.x", "Time.air", "Predicted_Discharge_Linear_m3s", "Residual_Linear", "LEVEL.kPa")

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

#####################
#### Plot curves ####
#####################
# # loop through each data frame in the list
# for (i in seq_along(pt_list)) {
#   # access the current data frame
#   df <- pt_list[[i]]
#   # Plot
#   p <- ggplot(data = df, aes(x = DateTime, y = Predicted_Discharge_Log_m3s)) + 
#     geom_line() + ggtitle(paste(pt_csvs$name[i])) 
#   # save the plot as a PNG file
#   ggsave(paste0("pt_figs/", pt_csvs$name[i], ".png"), plot = p)
#   # display the plot in the plot panel
#   print(p)
# }

###########################################
#### one hour rolling window smoothing ####
###########################################
# filter daylight savings empty dates
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # remove NA dates 
  df <- df %>%
    filter(!is.na(DateTime))
  # update the data frame in the list
  pt_list[[i]] <- df
}

# 1. Convert data frame to an xts object
xts_list <- list()

for (i in seq_along(pt_list)) {
  df <- pt_list[[i]]
  # convert to xts object
  xts_list[[i]] <- tryCatch({
    xts(df$Predicted_Discharge_Log_m3s, order.by = df$DateTime)
  })
}
# keep original file names
names(xts_list) <- names(pt_list)

# 2. Apply the 1-hour moving average (4 observations for 15-min data)
# k = 4 for a 1-hour window with 15-minute data 
# align = "right" means the result is aligned with the end of the window.
# na.pad = TRUE will keep the original length and fill leading NAs.

# make new list
smoothed_xts_list <- list()

for (i in seq_along(xts_list)) {
  # access the current data frame
  df <- xts_list[[i]]
  # smooth
  smoothed_xts_list[[i]] <- rollmean(df, k = 4, align = "right", na.pad = TRUE)
  # update the data frame in the list
  xts_list[[i]] <- df
}

# preserve the names from the original xts_list for easier identification
names(smoothed_xts_list) <- names(xts_list)

# 3. Add the smoothed data back to your original data frame
# make new list
smooth_df <- list()

# Loop through the original data frames and the smoothed xts objects
for (i in seq_along(pt_list)) {
  # Get the original data frame
  df_original <- pt_list[[i]]
  
  # Get the corresponding smoothed xts object
  smoothed_xts_obj <- smoothed_xts_list[[i]]
  # Add the smoothed data as a new column to the original data frame
  df_original$Smooth_Discharge_Log_m3s <- as.numeric(smoothed_xts_obj)
  # Store the modified data frame in the new list
  smooth_df[[i]] <- df_original
}

# preserve the names from the original xts_list for easier identification
names(smooth_df) <- names(smoothed_xts_list)

############################
#### Plot smooth curves ####
############################
# loop through each data frame in the list
for (i in seq_along(smooth_df)) {
  # access the current data frame
  df <- smooth_df[[i]]
  # plot
  p <- ggplot(data = df, aes(x = DateTime, y = Smooth_Discharge_Log_m3s)) + 
    geom_line() + ggtitle(paste(pt_csvs$name[i])) 
  # save the plot as a PNG file
  # ggsave(paste0("pt_figs/", pt_csvs$name[i], "smooth.png"), plot = p)
  # display the plot in the plot panel
  print(p)
}

#######################################
#### Save merged PT files to Drive ####
#######################################
# loop through each data frame in the list
for (i in seq_along(smooth_df)) {
  # Access the current data frame
  df <- smooth_df[[i]]
  
  # save new data frame
  write.csv(df, paste0("data/", names(smooth_df)[i]), row.names=FALSE, quote=FALSE)
  
  # define the local folder path and the target folder ID in Google Drive
  file <- paste0("data/", names(smooth_df)[i])
  # this is the "smooth" folder
  drive_folder_id <- "1RRtQMmBfUKfZ6qCYopo_cTZrxdJKs-fh"
  
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
