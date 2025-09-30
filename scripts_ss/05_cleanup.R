##==============================================================================
## Project: QuEST
## This script is to clean predicted discharge files for South Sandy
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
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1tkYDtcrI_wgbb16zufLJizcjfMzePtkw")

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
SSM01 <- pt_list[["discharge_SSM01.csv"]]
SST13 <- pt_list[["discharge_SST13.csv"]]

############################
#### Format date column #### 
############################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # combine Date and Time columns into a new DateTime column
  df$DateTime <- paste(df$Date.x, df$Time.x, sep = " ")
  
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %I:%M:%S %p")
  # update the data frame in the list
  pt_list[[i]] <- df
  
  # make date into date fomat
  df$Date.x <- as.Date(df$Date.x, format = "%Y-%m-%d")
  
  # make date into date fomat
  df$Predicted_Discharge_Log.m3s <- as.numeric(df$Predicted_Discharge_Log)
  
  # update the data frame in the list
  pt_list[[i]] <- df
}

# look at it
SSM01 <- pt_list[["discharge_SSM01.csv"]]
SST13 <- pt_list[["discharge_SST13.csv"]]

##################
#### Clean up #### 
##################
# columns that I don't want
drops <- c("X.1", "X.2", "X", "Date.y","Date.air", "Date", "Time.y", "Time", "ms.x", "LEVEL.kPa", "LEVEL.m", "Level_air.m", "ms.x", "ms.y", "Level.air.kPa1", 
           "Pres.abs.kPa", "TEMPERATURE.x", "TEMPERATURE.air", "Measurement.Time", "Measurement.Type", "PT.SN..Solonist.",
           "Depth.above.PT..cm....7", "Depth.above.PT..cm....13", "Flow.Conditions", "PT.Downloaded.", "Time.PT.Deployed", "Width..m.",
           "Comments", "Q..L.s.")

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
SSM01 <- pt_list[["discharge_SSM01.csv"]]
SST13 <- pt_list[["discharge_SST13.csv"]]
SSM20 <- pt_list[["discharge_SSM20.csv"]]
SST05 <- pt_list[["discharge_SST05.csv"]]
SST06 <- pt_list[["discharge_SST06.csv"]]
SST07 <- pt_list[["discharge_SST07.csv"]]
SST08 <- pt_list[["discharge_SST08.csv"]]
SST09 <- pt_list[["discharge_SST09.csv"]]

#####################
#### Plot curves ####
#####################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = Predicted_Discharge_Log.m3s)) + 
    geom_line() + ggtitle(paste(pt_csvs$name[i])) 
  # save the plot as a PNG file
  #ggsave(paste0("pt_figs/", pt_csvs$name[i], ".png"), plot = p)
  # display the plot in the plot panel
  print(p)
}

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
    xts(df$Predicted_Discharge_Log.m3s, order.by = df$DateTime)
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
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  
  # save new data frame
  write.csv(df, paste0("data/", names(pt_list)[i]), row.names=FALSE, quote=FALSE)
  
  # define the local folder path and the target folder ID in Google Drive
  file <- paste0("data/", names(pt_list)[i])
  # this is the "smooth" folder
  drive_folder_id <- "1e3I99uqgBETgbxOeju2Ot-QhQAfb6GGq"
  
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}

###############################
#### Do just one at a time ####
###############################
# plot
ggplot(data = USF20, aes(x = DateTime, y = Predicted_Discharge_Log_m3s)) + 
  geom_line() + ggtitle(paste(pt_csvs$name[i])) 

# 1. Convert data frame to an xts object
discharge_xts20 <- xts(USF20$Predicted_Discharge_Log_m3s, order.by = USF20$DateTime)

# 2. Apply the 1-hour moving average (4 observations for 15-min data)
# k = 4 for a 1-hour window with 15-minute data 
# align = "right" means the result is aligned with the end of the window.
# na.pad = TRUE will keep the original length and fill leading NAs.
smoothed_discharge_xts <- rollmean(discharge_xts20, k = 4, align = "right", na.pad = TRUE)

# 3. Add the smoothed data back to your original data frame
USF20$Smoothed_Discharge_Log_m3s <- as.numeric(smoothed_discharge_xts)

# plot smoothed discharge 
ggplot(data = USF20, aes(x = DateTime, y = Smoothed_Discharge_Log_m3s)) +
  geom_line() + ggtitle("USF20 compensated level data")

# View the head of your updated data frame to see the new smoothed column
head(USF20)
