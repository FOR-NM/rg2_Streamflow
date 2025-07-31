##==============================================================================
## Project: QuEST
## This script is to calculate daily average discharge for NM
##==============================================================================

##################
#### Packages ####
##################
library(dplyr)
library(lubridate)

####################################
## Clear folders that we will use ##
####################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "merged", full.names = TRUE)
file.remove(files)

####################
#### Import data ####
#####################
# this is the smooth folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1y2bMWCS48cROq_BO5HkaNWmFIxdJUON0")

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

############################
#### Format date column #### 
############################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # combine Date and Time columns into a new DateTime column
  df$DateTime <- paste(df$Date, df$Time, sep = " ")
  
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %I:%M:%S %p")
  # update the data frame in the list
  pt_list[[i]] <- df
  
  # make date into date fomat
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  # update the data frame in the list
  pt_list[[i]] <- df
}

# look at it
USF21 <- pt_list[["discharge_USF21.csv"]]
USF20 <- pt_list[["discharge_USF20.csv"]]
USF03 <- pt_list[["discharge_USF03.csv"]]

##################################
#### Calculate daily averages #### 
##################################
# create empty list to store new data frames
da_list <- list()

# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  
  df <- df %>%
    mutate(date = floor_date(Date)) %>%
    group_by(Date) %>%
    summarize(smooth_dailyaverage.m3s = mean(Smooth_Discharge_Log_m3s),
              discharge_dailyaverage.m3s = mean(Predicted_Discharge_Log_m3s),
              DataID = DataID)
  
  # update the data frame in the list
  da_list[[i]] <- df
}
# keep original file names
names(da_list) <- names(pt_list)

# look at it
USF21 <- da_list[["discharge_USF21.csv"]]
USF20 <- da_list[["discharge_USF20.csv"]]
USF03 <- da_list[["discharge_USF03.csv"]]

#######################################
#### Save merged PT files to Drive ####
#######################################
# loop through each data frame in the list
for (i in seq_along(da_list)) {
  # Access the current data frame
  df <- da_list[[i]]
  
  # save new data frame
  write.csv(df, paste0("data/", names(da_list)[i]), row.names=FALSE, quote=FALSE)
  
  # define the local folder path and the target folder ID in Google Drive
  file <- paste0("data/", names(da_list)[i])
  # this is the "daily averages" folder
  drive_folder_id <- "17zLOfcBqiw1-b7WoG-INL9DOeQ-VTGBq"
  
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}
            