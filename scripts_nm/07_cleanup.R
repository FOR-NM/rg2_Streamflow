##==============================================================================
## Project: QuEST
## This script is to clean predicted discharge files for NM
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
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1fPDNinUQ3pCFFQXJ1dtLGbqEawyTmPUx")

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

# check the contents of the list
str(pt_list)

# look at it
USF21 <- pt_list[["discharge_USF21.csv"]]
USF20 <- pt_list[["discharge_USF20.csv"]]

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

##################
#### Clean up #### 
##################
# columns that I don't want
drops <- c("X.1", "X.2", "Date.x", "ms", "LEVEL", "X", "Date.air", "ms.x", "ms.y", "Level.air.kPa1", "TEMPERATURE", "TEMPERATURE.x", 
           "TEMPERATURE.y", "Predicted_air1", "Predicted_air1_local", "Residual_local", "Date.air_new", "Date.y", 
           "background", "salt", "medianSPC.x", "Q", "final_SPC", "final_index", "SPC_difference", "Time24h", "temp_at_start_of_salt_slug_c", 
           "reach", "pt", "pt_depth_measurement_point_description", "relevant_notes")

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
USF03 <- pt_list[["discharge_USF03.csv"]]
USF20 <- pt_list[["discharge_USF20.csv"]]
USF21 <- pt_list[["discharge_USF21.csv"]]
USF19 <- pt_list[["discharge_USF19.csv"]]

###################################################
#### Separate data in lower vs upper vs middle ####
###################################################
# # list of site names
# uppersites <- c("USF21", "USF13", "USF14", "USF16", "USF19")
# middlesites <- c("USF09", "USF10", "USF11")
# lowersites <- c("USF03", "USF04", "USF05", "USF07", "USF20")
# 
# # create an empty lists to store the files
# upper_list <- list()
# middle_list <- list()
# lower_list <- list()
# 
# # separate sites in lists
# upper_list <- append(upper_list, c(pt_list["discharge_USF21.csv"], pt_list["discharge_USF13.csv"], pt_list["discharge_USF14.csv"], pt_list["discharge_USF16.csv"], pt_list["discharge_USF19.csv"]))
# middle_list <- append(middle_list, c(pt_list["discharge_USF09.csv"], pt_list["discharge_USF10.csv"], pt_list["discharge_USF11.csv"]))
# lower_list <- append(lower_list, c(pt_list["discharge_USF03.csv"], pt_list["discharge_USF04.csv"], pt_list["discharge_USF05.csv"], pt_list["discharge_USF20.csv"]))

#####################
#### Plot curves ####
#####################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = Predicted_Discharge_Log_m3s)) + 
    geom_line() + ggtitle(paste(pt_csvs$name[i])) 
  # save the plot as a PNG file
  ggsave(paste0("pt_figs/", pt_csvs$name[i], ".png"), plot = p)
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
  # this is the "depth" folder
  drive_folder_id <- "1y2bMWCS48cROq_BO5HkaNWmFIxdJUON0"
  
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
