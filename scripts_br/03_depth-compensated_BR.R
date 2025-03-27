##==============================================================================
## Project: QuEST
## This script is to merge discharge data with pt data for Brush Creek
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

##################
#### Packages ####
##################
library(googledrive)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tools)

####################################
## Clear folders that we will use ##
####################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data", full.names = TRUE)
file.remove(files)

###############################################
#### Load discharge data from Google drive ####
###############################################
# this is the general BR discharge synoptic folder
Q <- googledrive::as_id("https://drive.google.com/drive/folders/1E3DklQjXkkJGxTyZJTVDkPGABJIQOiTh")
# list all CSV files in the folder
q <- googledrive::drive_ls(path = Q)
3
# choose the specific file by name
q  <- q  %>% filter(name == "discharge_BR_full.csv")

# download the most recent CSV file
drive_download(as_id(q$id), path = "data/discharge_BR_full.csv", overwrite = TRUE)

# fetch the file
discharge <- read.csv("data/discharge_BR_full.csv")

######################################
#### Format Date and Time columns ####
######################################
# combine Date and Time columns into a new DateTime column
discharge$DateTime <- paste(discharge$Date, discharge$Time, sep = " ")

# convert the DateTime column to POSIXct
discharge$DateTime <- as.POSIXct(discharge$DateTime, format = "%Y-%m-%d %H:%M:%S")

###########################
#### Rounding the time ####
###########################
# create extra columns so you don't erase original time               
discharge$DateTimeNotRounded <- discharge$DateTime

# transform to datetime format
discharge$DateTime <- as.POSIXct(discharge$DateTime,format = "%Y-%m-%d %H:%M:%S")
discharge$DateTimeNotRounded <- as.POSIXct(discharge$DateTimeNotRounded,format = "%Y-%m-%d %H:%M:%S")

# round DateTime to the nearest 15-minute interval 
discharge$DateTime <- round_date(discharge$DateTime, unit="10 mins")

# check if it worked!
str(discharge)

####################################################
#### Load PT compensated data from Google drive ####
####################################################
# this is the compensated folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1Ix6n94e2pkKTyojz4SDQKe5MV7WbkH0C")

# list all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")

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

###################################
#### Add DataID column to csvs ####
###################################
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  
  # extract the file name without extension
  site_id <- file_path_sans_ext(pt_csvs$name[[i]])
  # add the DataID column
  df <- df %>%
    mutate(DataID = site_id)
  
  # save the modified data frame back to the list
  pt_list[[i]] <- df
}

###################################
#### Change to DateTime format ####
###################################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  
  # combine Date and Time columns into a new DateTime column
  df$DateTime <- paste(df$Date.x, df$Time.x, sep = " ")
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %I:%M:%S %p")
  
  # update the data frame in the list
  pt_list[[i]] <- df
}

#######################################
#### Combine depth info to PT data ####
#######################################
# check unique DataID values
unique(discharge$DataID)
unique(pt_list[[1]]$DataID)

# check example DateTime values
head(discharge$DateTime)
head(pt_list[[1]]$DateTime)

# merge discharge_depth with each data frame in csvs list
depth_merged <- lapply(pt_list, function(df) {
  # Perform the merge by DataID and DateTime
  merged_df <- merge(df, discharge, by = c("DataID", "DateTime"), all.x = TRUE)
  return(merged_df)
})

# count non-NA values in the 'pt' column for each data frame
non_na_counts <- sapply(depth_merged, function(df) {
  if ("pt" %in% colnames(df)) {
    sum(!is.na(df$pt))
  } else {
    NA  # If 'pt' column is missing, return NA
  }
})

# print the counts
print(non_na_counts)

BRM01 <- depth_merged[["BRM01.csv"]]
BRMQ1 <- depth_merged[["BRMQ1.csv"]]
BRAA1 <- depth_merged[["BRAA1.csv"]]
BRM02 <- depth_merged[["BRM02.csv"]]
BRM03 <- depth_merged[["BRM03.csv"]]
BRD01 <- depth_merged[["BRD01.csv"]]
BRM05 <- depth_merged[["BRM05.csv"]]
BRF01 <- depth_merged[["BRF01.csv"]]
BRM07 <- depth_merged[["BRM07.csv"]]
BRA01 <- depth_merged[["BRA01.csv"]]

#######################################
#### Save merged PT files to Drive ####
#######################################
# loop through each data frame in the list
for (i in seq_along(depth_merged)) {
  # Access the current data frame
  df <- depth_merged[[i]]
  
  # save new data frame
  write.csv(df, paste0("data/", names(depth_merged)[i]), row.names=FALSE, quote=FALSE)
  
  # define the local folder path and the target folder ID in Google Drive
  file <- paste0("data/", names(depth_merged)[i])
  # this is the "depth" folder
  drive_folder_id <- "1n17b_9yf5DCO_h6uPya5vBPz2dh13L3v"
  
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}