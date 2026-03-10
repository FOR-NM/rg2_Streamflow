##==============================================================================
## Project: QuEST
## Script to format all the Level Loggers from DV to a cleaner state and upload them back to Drive 
##==============================================================================
##############
## Packages ##
##############
library(googledrive)

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
# this is the "raw level logger" folder
# pt <- googledrive::as_id("https://drive.google.com/drive/folders/1r0xE5ybzFTP3nKyecwSNj2S1oC2Ci6PZ")

# this is the "raw baro logger" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1XRKIeb4eKFngCjs2owqfjO0xzOOkZHQ6")

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
  
  # FOR LEVEL FILES read the CSV file, skipping the first 11 rows (header is on row 12) #### FOR BARO FILES IT IS 10
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
  # this is the "in use level logger" folder for BARO FILES: 1_KUXHWDuAbO3Z6EV_RKBZL9IyWeeaj-X
  # this is the "in use level logger" folder for LEVEL FILES: 1B1xRjXUzU1Q3baLGwN5NvCk5axHG-ylq
  drive_folder_id <- "1_KUXHWDuAbO3Z6EV_RKBZL9IyWeeaj-X"
  
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}

