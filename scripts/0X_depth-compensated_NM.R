##==============================================================================
## Project: QuEST
## This script is to 
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

##################
#### Packages ####
##################
library(googledrive) 
library(ggplot2)
library(dplyr)

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

(Q <- drive_get("https://docs.google.com/spreadsheets/d/1rIWYWFUoF6UtzTcvN-WpIw2Cw4u9NjI_HuhThoDOvv4/edit?gid=0#gid=0"))
3

# Download the file as a csv file
drive_download(as_id(Q$id), path = "googledrive/salt.csv", type = "csv", overwrite = T)

# Fetch the file
salt <- read.csv("googledrive/salt.csv")

# Clean the column names (this removes spaces, special characters, etc.)
salt <- salt %>%
  janitor::clean_names()

# Rename columns and convert types
salt <- salt %>%
  # Rename columns
  dplyr::rename(
    DataID = site,
    Date = date,
    Time24h = time_24h_rounded_to_nearest_15_min,
    reach = reach_length_m,
    salt = salt_added_g,
    pt = is_there_a_pt_at_this_site,
    pt_depth_cm = pt_depth_cm_at_the_time_of_the_discharge_measurement_numbers_only
  ) %>%
  # Convert
  mutate(
    pt_depth_cm = as.numeric(as.character(pt_depth_cm)),
    # Change date format
    Date = as.Date(Date, format = "%m/%d/%Y"),
  )

colnames(salt)

# Remove rows that I don't want
salt <- salt[ , -c(1:3, 8, 9, 12:14, 18)]

# Replace empties with NA
salt["relevant_notes"][salt["relevant_notes"] == ''] <- NA
salt["flag_notes"][salt["flag_notes"] == ''] <- NA


#### Filter to only sites with PT ####
PT_sites <- salt %>% filter(pt == "Yes" )

########################################################
#### Combine and format Date and Time in one column ####
########################################################

# Combine Date and Time columns into a new DateTime column
PT_sites$DateTime <- paste(PT_sites$Date, PT_sites$Time24h, sep = " ")

# Convert the DateTime column to POSIXct
PT_sites$DateTime <- as.POSIXct(PT_sites$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "MST")

####################################################
#### Load PT compensated data from Google drive ####
####################################################

# This is the compensated folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1VsT7hirl5OHIGhrrc3b7dxPpSqr1wNC0")

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
  
  # Extract the file name without the .csv extension
  data_id <- tools::file_path_sans_ext(pt_csvs$name[[i]])
  
  # Add the DataID column
  df <- df %>%
    dplyr::mutate(DataID = data_id)
  
  # Save the modified data frame back to the list
  pt_list[[i]] <- df
}

# Check the contents of the list
str(pt_list)

#######################################
#### Combine depth info to PT data ####
#######################################
# Check unique DataID values
unique(PT_sites$DataID)
unique(pt_list[[1]]$DataID)

# Check example DateTime values
head(PT_sites$DateTime)
head(pt_list[[1]]$DateTime)

# Convert DateTime in pt_list
pt_list <- lapply(pt_list, function(df) {
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "MST")
  df
})

# Merge PT_sites with each data frame in csvs list
depth_merged <- lapply(pt_list, function(df) {
  # Perform the merge by DataID and DateTime
  merged_df <- merge(df, PT_sites, by = c("DataID", "DateTime"), all.x = TRUE)
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
  drive_folder_id <- "1EswIfUWCK6bsdcs-ZrAMGW1oYKs4B0Eh"
  
  # Upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}


