##==============================================================================
## Project: QuEST
## This script is to merge PT and barometric (air pressure) data for the South Sandy sites
## press Command+Option+O to collapse all sections and get an overview of the workflow
##==============================================================================

##################
#### Packages ####
##################
library(googledrive)
library(ggplot2)
library(lubridate)
library(dplyr)

####################################
## Clear folders that we will use ##
####################################
# List and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "merged", full.names = TRUE)
file.remove(files)

#################################
#### Import & Visualize Data ####
#################################
#### Load data from Google drive ####
# This is the "inuse" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/194hsX_kF8xMs-9Uzq_yyaxoh_ywh6HMm")

# List all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3
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

#### Remove baro file from pt list ####
# remove second item in this case, check position of baro file
pt_list = pt_list[-1]

# Load the baro data separately
# All sites will be merged with the one air data that South Sandy has
air_data <- read.csv("googledrive/2024-12-16_SSM20_PTS_SN2191067_baro.csv")

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
air_data$DateTime <- as.POSIXct(air_data$DateTime, format = "%Y-%m-%d %H:%M:%S")
# Convert baro DateTime column to POSIXct
air_data$DateTime <- as.POSIXct(air_data$DateTime, format = "%Y/%m/%d %H:%M:%S")

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

#####################################
#### Change all units to meters  ####
#####################################

## Air pressure in kPa to m
#1 kPa = 0.101972 m
air_data <- air_data %>%
  mutate(Level_air.m = (.[[4]] * 0.101972))

## water level is in kPa, change to m
# Loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  
  #kPa to m
  df <- df %>%
    mutate(LELVEL.m = (.[[4]] * 0.101972))
  # Update the data frame in the list
  pt_list[[i]] <- df
}

# Check the contents of the list and make sure there are no NAs
str(pt_list)

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
                  LEVEL.kPa = LEVEL.x,
                  TEMPERATURE.air = TEMPERATURE.y)
  merged_list[[i]] <-  df
}

# Check the contents of the list
str(merged_list)

########################################
#### Manual Barometric Compensation ####
########################################
## Once the units for each column are the same, subtract the barometric column from the Levelogger data 
# to get the true net water level recorded by the Levelogger.

#Create empty compensated list
compensated_list <- list()

for (i in names(merged_list)) {
  # Access the current data frame
  df <- merged_list[[i]]
  
  # Compensate
  df <- df %>%
  mutate(Baro_Cor_Lvl.m = (.[[7]] - .[[13]]))
  
  compensated_list[[i]] <-  df
}

#########################################
#### Save compensated files to Drive ####
#########################################
# Loop through each data frame in the list
for (i in seq_along(compensated_list)) {
  # Access the current data frame
  df <- compensated_list[[i]]
  
  # Save new data frame
  write.csv(df, paste0("data/", names(compensated_list)[i]), row.names=FALSE, quote=FALSE)
  
  # Define the local folder path and the target folder ID in Google Drive
  file <- paste0("data/", names(compensated_list)[i])
  # this is the "compensated" folder
  drive_folder_id <- "1SAtC_CJd6KC2yWtJB_VdTebMBDSjy-Pk"
  
  # Upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}

