##==============================================================================
## Project: QuEST
## Script to format all the Pressure Transducer (Level Logger) to a cleaner state and upload them back to Drive for BR sites
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

##############
## Packages ##
##############
library(googledrive) 
library(tidyverse)

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
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1E2jjd84l36SsY3jXqHiI-fybS3415Ug8")

# list and filter CSV files with "pt" in their names
pt_files <- googledrive::drive_ls(path = pt, type = "csv")

# create an empty list to store the cleaned data frames
pt_list <- lapply(seq_along(pt_files$name), function(i) {
  googledrive::drive_download(
    file = pt_files$id[i],
    path = paste0("googledrive/", pt_files$name[i]),
    overwrite = TRUE
  )
  
  # read the CSV file, skipping the first 11 rows (header is on row 12)
  read.csv(paste0("googledrive/", pt_files$name[i]), skip = 11, header = TRUE)
})

# assign names to the list elements based on the file names
names(pt_list) <- pt_files$name

BRMQ1 <- pt_list[["2024-11-17_BRMQ1_PT20240830-20241011.csv"]]

############################
#### Format date column ####
############################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  
  # make date into date fomat
  df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
  # update the data frame in the list
  pt_list[[i]] <- df
}

BRMQ1 <- pt_list[["2024-11-17_BRMQ1_PT20240830-20241011.csv"]]

##########################################################
#### Combine and format Date and Time into one column #### 
##########################################################
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
}

BRMQ1 <- pt_list[["2024-11-17_BRMQ1_PT20240830-20241011.csv"]]

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
  drive_folder_id <- "1q_HNENGb8tyHmWTA-TP5sOM8APgxKRxI"
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}

#### if you have to edit a single file ####

# # read the CSV file
# SST05 <- read.csv("10-04-2024_SST05_PTS_SN2192883.csv")
# 
# # combine Date and Time columns into a new DateTime column
# SST05$DateTime <- paste(SST05$Date, SST05$Time, sep = " ")
# 
# # convert the DateTime column to POSIXct
# SST05$DateTime <- as.POSIXct(SST05$DateTime, format = "%m/%d/%y %I:%M:%S %p")
# 
# # check the structure of the data frame to confirm the DateTime column
# str(SST05)
# 
# # remove the unwanted X column
# SST05$X <- NULL  # alternatively, you can use SST05 <- SST05[, -which(names(SST05) == "X")]
# 
# # save the data frame to a CSV file
# write.csv(SST05, file = "SST05_cleaned.csv", row.names = FALSE, quote = FALSE)

