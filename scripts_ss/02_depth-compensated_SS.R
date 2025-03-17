##==============================================================================
## Project: QuEST
## This script is to merge discharge data with pt data for SS
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

files <- list.files(path = "merged", full.names = TRUE)
file.remove(files)

###############################################
#### Load discharge data from Google drive ####
###############################################
(disch <- drive_get("https://docs.google.com/spreadsheets/d/1JVDwzSoHetQGHhYPoeoTOHzlmRrcWNPs-i5b8t2U754/edit?gid=893974061#gid=893974061"))
3

# download the file as a xlsx file
drive_download(as_id(disch$id), path = "googledrive/discharge.xlsx", type = "xlsx", overwrite = T)

# fetch the file
discharge <- readxl::read_xlsx("googledrive/discharge.xlsx", sheet = "Master Q", skip = 1)

# Rename column to match all other data
discharge <- discharge %>% 
  rename( "DataID" = "Site" ) 

######################################
#### Format Date and Time columns ####
######################################
# fix Time column 
discharge$Time <- format(as.POSIXct(discharge$`Measurement Time`, format = "%Y-%m-%d %H:%M:%S"), format = "%H:%M:%S")

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
discharge$DateTime <- round_date(discharge$DateTime, unit="15 mins")

# check if it worked!
str(discharge)

####################################################
#### Load PT compensated data from Google drive ####
####################################################
# this is the compensated folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1SAtC_CJd6KC2yWtJB_VdTebMBDSjy-Pk")

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
  # Read the CSV file and add it to the list
  pt_list[[pt_csvs$name[i]]] <- read.csv(local_path)
}

###################################
#### Add DataID column to csvs ####
###################################
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  
  # Extract the file name
  filename <- tools::file_path_sans_ext(pt_csvs$name[[i]])
  
  # Extract the site identifier (text between the first two underscores)
  site_id <- stringr::str_extract(filename, "(?<=_)[A-Za-z0-9]+(?=_PTS_)")
  
  # Add the DataID column
  df <- df %>%
    dplyr::mutate(DataID = site_id)
  
  # Save the modified data frame back to the list
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

#################################
#### Rounding time for SST07 ####
#################################
# rounding the time up or down to the nearest consistent interval 
# example: 10:04 gets converted to 10:05 for this we use the lubridate package

pt_list[["2024-12-16_SST07_PTS_SN2192880.csv"]]$DateTimeNotRounded <- pt_list[["2024-12-16_SST07_PTS_SN2192880.csv"]]$DateTime

pt_list[["2024-12-16_SST07_PTS_SN2192880.csv"]]$DateTime <- round_date(pt_list[["2024-12-16_SST07_PTS_SN2192880.csv"]]$DateTime, unit="15 mins")

str(pt_list)

# round DateTime to the nearest 15-minute interval across all files in pt_list
for (i in seq_along(pt_list)) {
  pt_list[[i]]$DateTime <- round_date(pt_list[[i]]$DateTime, unit = "15 mins")
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

# convert DateTime in pt_list
pt_list <- lapply(pt_list, function(df) {
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
  df
})

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

SSM01 <- depth_merged[["2024-12-16_SSM01_PTS_SN2192882.csv"]]
SSM20 <- depth_merged[["2024-12-16_SSM20_PTS_SN2192885.csv"]]
SST13 <- depth_merged[["2024-12-16_SST13_PTS_SN2192886.csv"]]
SST03 <- depth_merged[["2024-12-16_SST03_PTS_SN2192627.csv"]] # MOVED
SST04 <- depth_merged[["2024-12-16_SST04_PTS_SN2192624.csv"]] # MOVED
SST05 <- depth_merged[["2024-12-16_SST05_PTS_SN2192883.csv"]] # MOVED
SST06 <- depth_merged[["2024-12-17_SST06_PTS_SN2186356.csv"]]
SST07 <- depth_merged[["2024-12-16_SST07_PTS_SN2192880.csv"]] # MOVED?
SST08 <- depth_merged[["2024-12-16_SST08_PTS_SN2192632.csv"]]
SST09 <- depth_merged[["2024-12-16_SST09_PTS_SN2192621.csv"]]

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
  drive_folder_id <- "11vn2jsiB7YEsrhjI5_NnOSTA579NMtK4"
  
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}
