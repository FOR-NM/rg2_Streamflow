##==============================================================================
## Project: QuEST
## Script to format Air Pressure Transducers and upload them back to Drive
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================
##############
## Packages ##
##############
library(googledrive)
library(tidyverse)

########################################
#### Clear folders that we will use ####
########################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

#####################
#### Import Data ####
#####################
# set up Google Drive folder
# this is the "raw air" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1jhw-miI7fPaP8947HCbJ1idnIl6g1A8c")

# list and filter CSV files with "pt" in their names
pt_files <- googledrive::drive_ls(path = pt, type = "csv")
3

# create an empty list to store the cleaned data frames
pt_list <- lapply(seq_along(pt_files$name), function(i) {
  googledrive::drive_download(
    file = pt_files$id[i],
    path = paste0("googledrive/", pt_files$name[i]),
    overwrite = TRUE
  )
  
  # read the CSV file, skipping the first 11 rows (header is on row 12)
  read.csv(paste0("googledrive/", pt_files$name[i]), skip = 10, header = TRUE)
})

# assign names to the list elements based on the file names
names(pt_list) <- pt_files$name

# check the contents of the list
str(pt_list)

############################
#### Format date column ####
############################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  
  # make date into date fomat
  df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
  # update the data frame in the list
  pt_list[[i]] <- df
}

##########################################################
#### Combine and format Date and Time into one column #### 
##########################################################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # Access the current data frame
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

# check the contents of the list and make sure there are no NAs
str(pt_list)

#########################################
#### Save edited files back to Drive ####
#########################################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  
  # save new data frame
  write.csv(df, paste0("googledrive/", pt_files$name[i]), row.names=FALSE, quote=FALSE)
  
  # define the local folder path and the target folder ID in Google Drive
  file <- paste0("googledrive/", pt_files$name[i])
  # this is the "in use" folder
  drive_folder_id <- "1i7G-q7FV0_bszqeCdJ6Otz8b9xhpU1cx"
  
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}
