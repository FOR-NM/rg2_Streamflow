##==============================================================================
## Project: QuEST
## Script to format all the Pressure Transducer (Level Logger) to a cleaner state and upload them back to Drive for SS sites
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
# List and delete all files in the folder

files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

#####################
#### Import Data ####
#####################

# Set up Google Drive folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/194hsX_kF8xMs-9Uzq_yyaxoh_ywh6HMm")

# List and filter CSV files with "pt" in their names
pt_files <- googledrive::drive_ls(path = pt, type = "csv")
pt_files <- pt_files[!grepl("baro", pt_files$name), ]

# Create an empty list to store the cleaned data frames
pt_list <- lapply(seq_along(pt_files$name), function(i) {
  googledrive::drive_download(
    file = pt_files$id[i],
    path = paste0("googledrive/", pt_files$name[i]),
    overwrite = TRUE
  )
  
  # Read the CSV file, skipping the first 11 rows (header is on row 12)
  read.csv(paste0("googledrive/", pt_files$name[i]), skip = 11, header = TRUE)
})

# Assign names to the list elements based on the file names
names(pt_list) <- pt_files$name

# Check the contents of the list
str(pt_list)

############################
#### Format date column ####
############################
# Loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  
  # Make date into date format
  df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
  # Update the data frame in the list
  pt_list[[i]] <- df
}

##########################################################
#### Combine and format Date and Time into one column #### 
##########################################################
# Loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  # Combine Date and Time columns into a new DateTime column
  df$DateTime <- paste(df$Date, df$Time, sep = " ")
  
  # Convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %I:%M:%S %p")
  # Update the data frame in the list
  pt_list[[i]] <- df
  
  # Make date into date fomat
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  # Update the data frame in the list
  pt_list[[i]] <- df
}

# Check the contents of the list and make sure there are no NAs
str(pt_list)

#########################################
#### Save edited files back to Drive ####
#########################################
# Loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  
  # Save new data frame
  write.csv(df, paste0("googledrive/", pt_files$name[i]), row.names=FALSE, quote=FALSE)
  
  # Define the local folder path and the target folder ID in Google Drive
  file <- paste0("googledrive/", pt_files$name[i])
  # this is the in use folder
  drive_folder_id <- "194hsX_kF8xMs-9Uzq_yyaxoh_ywh6HMm"
  
  # Upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}


#### Single excel that I had to edit on it's own ####

# Read the CSV file
SST05 <- read.csv("googledrive/2024-12-16_SSM20_PTS_SN2191067_baro.csv", skip = 10)

# Convert the Date column to POSIXct
SST05$Date <- as.Date(SST05$Date, format = "%m/%d/%Y")

# Combine Date and Time columns into a new DateTime column
SST05$DateTime <- paste(SST05$Date, SST05$Time, sep = " ")

# Convert the DateTime column to POSIXct
SST05$DateTime <- as.POSIXct(SST05$DateTime, format = "%Y-%m-%d %I:%M:%S %p")

# Check the structure of the data frame to confirm the DateTime column
str(SST05)

# Save the data frame to a CSV file
write.csv(SST05, file = "2024-12-16_SSM20_PTS_SN2191067_baro.csv", row.names = FALSE, quote = FALSE)

