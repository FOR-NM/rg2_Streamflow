##==============================================================================
## Project: QuEST
## This script is to format baro data for DV upper sites. 
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
# this is the "inuse baro logger" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1XRKIeb4eKFngCjs2owqfjO0xzOOkZHQ6")
# list all CSV files in the folder
pt_files <- googledrive::drive_ls(path = pt)
3

# create an empty list to store the cleaned data frames
pt_list <- lapply(seq_along(pt_files$name), function(i) {
  googledrive::drive_download(
    file = pt_files$id[i],
    path = paste0("googledrive/", pt_files$name[i]),
    overwrite = TRUE
  )
  
  # read the CSV file, skipping the first 10 rows (header is on row 11)
  read.csv(paste0("googledrive/", pt_files$name[i]), skip = 10, header = TRUE)
})
# assign names to the list elements based on the file names
names(pt_list) <- pt_files$name

# remove lower site
pt_list = pt_list[-c(2)]

# rename columns and convert types
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  
  # make date into date format
  df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
  # combine Date and Time columns into a new DateTime column
  df$DateTime <- paste(df$Date, df$Time, sep = " ")
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %I:%M:%S %p")
  # calculate pressure units in meters from kPa
  df$LEVEL.m <- df$LEVEL * 0.101972
  
  # update the data frame in the list
  pt_list[[i]] <- df
}

# 1 kPa = 0.101972 m

#########################################################
#### Rounding the time to nearest 15 for level files ####
#########################################################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  
  # round
  df$DateTime <- round_date(df$DateTime, unit="15 mins")
  # remove duplicates
  df <- df[!duplicated(df$DateTime), ]
  
  # update the data frame in the list
  pt_list[[i]] <- df
}

## make a column for the new rounded time
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # time only
  df <- df %>%
    mutate(Time = format(DateTime, "%H:%M:%S"))
  
  # update the data frame in the list
  pt_list[[i]] <- df
}

#####################
#### Plot curves ####
#####################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = LEVEL.m)) + 
    geom_line() + ggtitle(paste(pt_files$name[i])) 
  # save the plot as a PNG file
  #ggsave(paste0("pt_figs/", pt_csvs$name[i], ".png"), plot = p)
  # display the plot in the plot panel
  print(p)
}

##############################
#### Save formatted file  ####
##############################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  
  # save new data frame
  write.csv(df, paste0("data/", names(pt_list)[i]), row.names=FALSE, quote=FALSE)
  
  # define the local folder path and the target folder ID in Google Drive
  file <- paste0("data/", names(pt_list)[i])
  # this is the "inuse_barologger" folder
  drive_folder_id <- "1_KUXHWDuAbO3Z6EV_RKBZL9IyWeeaj-X"
  
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}
