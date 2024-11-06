##==============================================================================
## Project: QuEST
## This script is to merge PT and barometric (air pressure) data for SS sites
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

##################
#### Packages ####
##################

library(googledrive)
library(ggplot2)
library(lubridate)

####################################
## Clear folders that we will use ##
####################################
# List and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

#################################
#### Import & Visualize Data ####
#################################
#### Load data from Google drive ####
# This is the inuse folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/194hsX_kF8xMs-9Uzq_yyaxoh_ywh6HMm")

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

#####################
#### Plot curves ####
#####################
#just for fun

# Visualize
# Loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = LEVEL)) + 
    geom_point() + ggtitle(paste(pt_csvs$name[i])) 
  # Display the plot in the plot panel
  print(p)
}

####################################################
#### Prep data for merging PT with Air pressure ####
####################################################
# All sites will be merged with the one air data that South Sandy has

# Load the baro data
air_data <- read.csv("googledrive/09-16-2024_SSM20_PTSbaro_SN2191067.csv")

################################
#### Format DateTime column ####
################################

# Loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  
  # Convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
  # Convert the Time column to POSIXct (assuming it's a character time without date)
  
  df$Time <- as.POSIXct(df$Time, format = "%I:%M:%S %p")
  
  # Format the Time column to a unified time format (e.g., "HH:MM:SS")
  df$Time <- format(df$Time, format = "%H:%M:%S")
  # Update the data frame in the list
  pt_list[[i]] <- df
}

# Check the contents of the list and make sure there are no NAs
str(pt_list)

# Now check datetime format for air data
air_data$DateTime <- as.POSIXct(air_data$DateTime)

###########################
#### Rounding the time ####
###########################
# Rounding the time up or down to the nearest consistent interval 
# example: 10:04 gets converted to 10:05 for this we use the lubridate package
# 

pt_list[["09-16-2024_SST07_PTS_SN2192880.csv"]]$DateTimeNotRounded <- pt_list[["09-16-2024_SST07_PTS_SN2192880.csv"]]$DateTime

pt_list[["09-16-2024_SST07_PTS_SN2192880.csv"]]$DateTime <- round_date(pt_list[["09-16-2024_SST07_PTS_SN2192880.csv"]]$DateTime, unit="15 mins")

str(pt_list)


# Round DateTime to the nearest 15-minute interval across all files in pt_list
for (i in seq_along(pt_list)) {
  pt_list[[i]]$DateTime <- round_date(pt_list[[i]]$DateTime, unit = "15 mins")
}

######################################
#### Merging PT with Air pressure ####
######################################

# Create a list to store merged results
merged_list <- list()

# Loop through each site file in pt_list
for (i in names(pt_list)) {
  # Merge each site data with air_data on the DateTime column
  merged_list[[i]] <- merge(pt_list[[i]], air_data, by = "DateTime", all.x = TRUE)
}

# Check the contents of the list
str(merged_list)

##################################
#### Format some column names ####
##################################
for (i in seq_along(merged_list)) {
  # Access the current data frame
  df <- merged_list[[i]]
  
  # Rename columns
  df <- df %>%
    dplyr::rename(Pres.abs.kPa = LEVEL.y,
                  Water.level.m_sensor = LEVEL.x,
                  TEMPERATURE.air = TEMPERATURE.y)
  merged_list[[i]] <-  df
}

# Check the contents of the list
str(merged_list)

####################################
#### Save merged slugs to Drive ####
####################################
# Loop through each data frame in the list
for (i in seq_along(merged_list)) {
  # Access the current data frame
  df <- merged_list[[i]]
  
  # Save new data frame
  write.csv(df, paste0("merged/", names(merged_list)[i]), row.names=FALSE, quote=FALSE)
  
  # Define the local folder path and the target folder ID in Google Drive
  file <- paste0("merged/", names(merged_list)[i])
  # this is the in use folder
  drive_folder_id <- "1SAtC_CJd6KC2yWtJB_VdTebMBDSjy-Pk"
  
  # Upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}
