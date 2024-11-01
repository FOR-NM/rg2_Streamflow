##==============================================================================
## Project: QuEST
## This script is to merge PT and barometric (air pressure) data for SS sites
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

##################
#### Packages ####
##################

library(googledrive) 

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
pt <- googledrive::as_id("https://drive.google.com/drive/folders/14x-d5gB7V9Y78gOR4L1p_56wNZ4WBO2F")

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

# List of folder names
uppersites <- c("USF21", "USF13", "USF14", "USF16", "USF19")

# Create an empty list to store the files
upper_list <- list()

# Loop through each site name in the list
for (site in uppersites) {
  # Construct the file path and use a pattern to match files with the specific site
  folder_path <- "googledrive/"
  pattern <- paste0("^[0-9-]+_", site, "_WaterLevel")
  
  # Find the file matching the pattern (assuming only one match per site)
  files <- list.files(folder_path, pattern = pattern, full.names = TRUE)
  
  # Read the first matching file for each site
  upper_list[[site]] <- read.csv(files[1])
}

# Load the Air2 data
air_data <- read.csv("googledrive/2024-10-29_Air2.csv")

################################
#### Format DateTime column ####
################################

# Loop through each data frame in the list
for (i in seq_along(upper_list)) {
  # Access the current data frame
  df <- upper_list[[i]]
  
  # Convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
  # Update the data frame in the list
  upper_list[[i]] <- df
}

# Check the contents of the list and make sure there are no NAs
str(upper_list)

# Now check datetime format for air data
air_data$DateTime <- as.POSIXct(air_data$DateTime)

######################################
#### Merging PT with Air pressure ####
######################################

# Create a list to store merged results
merged_list <- list()

# Loop through each site file in upper_list
for (site in names(upper_list)) {
  # Merge each site data with air_data on the DateTime column
  merged_list[[site]] <- merge(upper_list[[site]], air_data, by = "DateTime", all.x = TRUE)
}

# Check the contents of the list
str(merged_list)

###################################
#### Format names to match YSI ####
###################################
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
  drive_folder_id <- "1orEmVMeuqL1oNwaJ3I9ONJQJ93mt7Tms"
  
  # Upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}