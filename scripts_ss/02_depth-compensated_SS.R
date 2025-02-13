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
# List and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "merged", full.names = TRUE)
file.remove(files)

##############################################
#### Load PT depth data from Google drive ####
##############################################

(depth <- drive_get("https://docs.google.com/spreadsheets/d/1JVDwzSoHetQGHhYPoeoTOHzlmRrcWNPs-i5b8t2U754/edit?gid=584354365#gid=584354365"))
3

# Download the file as a xlsx file
drive_download(as_id(depth$id), path = "googledrive/discharge.xlsx", type = "xlsx", overwrite = T)

# Fetch the file
discharge <- readxl::read_xlsx("googledrive/discharge.xlsx", sheet = "Master Q", skip = 1)

# Rename column to match all other data
discharge <- discharge %>% 
  rename( "DataID" = "Site" ) 

######################################
#### Format Date and Time columns ####
######################################

# Fix Time column 
discharge$Time <- format(as.POSIXct(discharge$`Measurement Time`, format = "%Y-%m-%d %H:%M:%S"), format = "%H:%M:%S")

# Combine Date and Time columns into a new DateTime column
discharge$DateTime <- paste(discharge$Date, discharge$Time, sep = " ")

# Convert the DateTime column to POSIXct
discharge$DateTime <- as.POSIXct(discharge$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "MST")

###########################
#### Rounding the time ####
###########################

# Create extra columns so you don't errase original time               
discharge$DateTimeNotRounded <- discharge$DateTime

# Transform to datetime format
discharge$DateTime <- as.POSIXct(discharge$DateTime,format = "%Y-%m-%d %H:%M:%S")
discharge$DateTimeNotRounded <- as.POSIXct(discharge$DateTimeNotRounded,format = "%Y-%m-%d %H:%M:%S")

# Round DateTime to the nearest 15-minute interval 
discharge$DateTime <- round_date(discharge$DateTime, unit="15 mins")

# Check if it worked!
str(discharge)

####################################################
#### Load PT compensated data from Google drive ####
####################################################

# This is the compensated folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1SAtC_CJd6KC2yWtJB_VdTebMBDSjy-Pk")

# List all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")

## Call all the files in the salt slugs folder ##
# Create empty list to store data frames
pt_list <- list()

# Loop over each file in the `pt_csvs` data frame
for (i in seq_along(pt_csvs$id)) {
  # Define the local file path
  local_path <- file.path("googledrive", pt_csvs$name[i])
  
  # Download the file
  googledrive::drive_download(
    file = pt_csvs$id[i],
    path = local_path,
    overwrite = T
  )
  # Read the CSV file and add it to the list
  pt_list[[pt_csvs$name[i]]] <- read.csv(local_path)
}

# Check the contents of the list
str(pt_list)

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

# Check the contents of the list
str(pt_list)

###################################
#### Change to DateTime format ####
###################################

# Loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  
  # Convert the DateTime column to POSIXct
  df$Date.x <- as.Date(df$Date.x, format = "%Y-%m-%d")
  # Update the data frame in the list
  pt_list[[i]] <- df
}

# Check the contents of the list and make sure there are no NAs
str(pt_list)

#######################################
#### Combine depth info to PT data ####
#######################################
# Check unique DataID values
unique(discharge$DataID)
unique(pt_list[[1]]$DataID)

# Check example DateTime values
head(discharge$DateTime)
head(pt_list[[1]]$DateTime)

# Convert DateTime in pt_list
pt_list <- lapply(pt_list, function(df) {
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "MST")
  df
})

# Merge discharge_depth with each data frame in csvs list
depth_merged <- lapply(pt_list, function(df) {
  # Perform the merge by DataID and DateTime
  merged_df <- merge(df, discharge, by = c("DataID", "DateTime"), all.x = TRUE)
  return(merged_df)
})

# Check the structure of the merged list
str(depth_merged)

# Count non-NA values in the 'pt' column for each data frame
non_na_counts <- sapply(depth_merged, function(df) {
  if ("pt" %in% colnames(df)) {
    sum(!is.na(df$pt))
  } else {
    NA  # If 'pt' column is missing, return NA
  }
})

# Print the counts
print(non_na_counts)

SSM01 <- depth_merged[["09-16-2024_SSM01_PTS_SN2192882.csv"]]
SSM20 <- depth_merged[["09-16-2024_SSM20_PTS_SN2192885.csv"]]
SST03 <- depth_merged[["09-16-2024_SST03_PTS_SN2192627.csv"]]
SST04 <- depth_merged[["09-16-2024_SST04_PTS_SN2192624.csv"]]
SST05 <- depth_merged[["10-04-2024_SST05_PTS_SN2192883..csv"]]
SST07 <- depth_merged[["09-16-2024_SST07_PTS_SN2192880.csv"]]
SST08 <- depth_merged[["09-16-2024_SST08_PTS_SN2192632.csv"]]
SST09 <- depth_merged[["09-16-2024_SST09_PTS_SN2192621.csv"]]
SST13 <- depth_merged[["09-16-2024_SST13_PTS_SN2192886.csv"]]
SST06 <- depth_merged[["09-17-2024_SST06_PTS_SN2186356.csv"]]

#######################################
#### Save merged PT files to Drive ####
#######################################
# Loop through each data frame in the list
for (i in seq_along(depth_merged)) {
  # Access the current data frame
  df <- depth_merged[[i]]
  
  # Save new data frame
  write.csv(df, paste0("data/", names(depth_merged)[i]), row.names=FALSE, quote=FALSE)
  
  # Define the local folder path and the target folder ID in Google Drive
  file <- paste0("data/", names(depth_merged)[i])
  # this is the "depth" folder
  drive_folder_id <- "11vn2jsiB7YEsrhjI5_NnOSTA579NMtK4"
  
  # Upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}


