##==============================================================================
## Project: QuEST
## Script to format all the hobo to a cleaner state and upload them back to Drive
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

##############
## Packages ##
##############
library(googledrive) 
library(tidyverse)
library(dplyr)

###################################
## Clear folders that we will use ##
###################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

#####################
#### Import Data ####
#####################
# set up Google Drive folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1i7G-q7FV0_bszqeCdJ6Otz8b9xhpU1cx")

# list and filter CSV files with "pt" in their names
pt_files <- googledrive::drive_ls(path = pt, type = "csv")
pt_files <- pt_files[grepl("hobo", pt_files$name), ]

# create an empty list to store the cleaned data frames
pt_list <- lapply(seq_along(pt_files$name), function(i) {
  googledrive::drive_download(
    file = pt_files$id[i],
    path = paste0("googledrive/", pt_files$name[i]),
    overwrite = TRUE
  )
  
  # read the CSV file, skipping the first 13 rows (header is on row 14)
  read.csv(paste0("googledrive/", pt_files$name[i]), skip = 1, header = TRUE)
})

# assign names to the list elements based on the file names
names(pt_list) <- pt_files$name

# check the contents of the list
str(pt_list)

###################################
#### Format names to match PT ####
###################################
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  
  # rename columns
  df <- df %>%
    dplyr::rename(DateTime = Date.Time..GMT.06.00)
  
  # rename columns to standard names based on prefixes
  df <- df %>%
    rename_with(~ "TEMPERATURE", .cols = starts_with("Temp")) %>%
    rename_with(~ "LEVEL", .cols = starts_with("Abs.Pres"))
  # save back to the list
  pt_list[[i]] <- df
}

############################
#### Format date column ####
############################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%m/%d/%y %H:%M:%S")
  # update the data frame in the list
  pt_list[[i]] <- df
  
}
#########################################
#### Save edited files back to Drive ####
#########################################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  
  # save new data frame
  write.csv(df, paste0("googledrive/", pt_files$name[i]), row.names=FALSE, quote=FALSE)
  
  # define the local folder path and the target folder ID in Google Drive
  file <- paste0("googledrive/", pt_files$name[i])
  # this is the in use folder
  drive_folder_id <- "1i7G-q7FV0_bszqeCdJ6Otz8b9xhpU1cx"
  
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}
