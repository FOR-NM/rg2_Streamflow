##==============================================================================
## Project: QuEST
## This script is to merge PT and barometric (air pressure) data for the New Hampshire sites
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

#################################
#### Import & Visualize Data ####
#################################
#### Load data from Google drive ####
# Set up Google Drive folder
# This is the "raw" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1EXpGBTsTHikhCdfb1UOIpFl0rmjAvT57")

# List and filter CSV files with "pt" in their names
pt_files <- googledrive::drive_ls(path = pt, type = "csv")
pt_files <- pt_files[!grepl("hobo", pt_files$name), ]

# Create an empty list to store the cleaned data frames
pt_list <- lapply(seq_along(pt_files$name), function(i) {
  googledrive::drive_download(
    file = pt_files$id[i],
    path = paste0("googledrive/", pt_files$name[i]),
    overwrite = TRUE
  )
  
  # Read the CSV file, skipping the first 11 rows (header is on row 12)
  read.csv(paste0("googledrive/", pt_files$name[i]), header = TRUE)
})

# Assign names to the list elements based on the file names
names(pt_list) <- pt_files$name

# Check the contents of the list
str(pt_list)

#############################
#### Format date columns ####
#############################
# For date column
# Loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  
  # Make date into date fomat
  df$Date <- as.Date(df$Date, format = "%m/%d/%y")
  # Update the data frame in the list
  pt_list[[i]] <- df
}

# For DateTime column
# Loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  
  # Make date into date fomat
  df$DateTime <- as.POSIXct(df$DateTime, format = "%d/%m/%Y %H:%M")
  # Update the data frame in the list
  pt_list[[i]] <- df
}

# Check the contents of the list and make sure there are no NAs
str(pt_list)

#####################
#### Plot curves ####
#####################
#just for fun
# Loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = Water.level.m_sensor)) + 
    geom_point() + ggtitle(paste(pt_files$name[i])) 
  # Display the plot in the plot panel
  print(p)
}

####################################################
#### Prep data for merging PT with Air pressure ####
####################################################

# List of site names
uppersites <- c("USF21", "USF13", "USF14", "USF16", "USF19")
middlesites <- c("USF09", "USF10", "USF11")
lowersites <- c("USF03", "USF04", "USF05", "USF07", "USF20")

# Create an empty lists to store the files
upper_list <- list()
middle_list <- list()
lower_list <- list()

## UPPER
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

# ## MIDDLE
# # Loop through each site name in the list
# for (site in middlesites) {
#   # Construct the file path and use a pattern to match files with the specific site
#   folder_path <- "googledrive/"
#   pattern <- paste0("^[0-9-]+_", site, "_WaterLevel")
#   
#   # Find the file matching the pattern (assuming only one match per site)
#   files <- list.files(folder_path, pattern = pattern, full.names = TRUE)
#   
#   # Read the first matching file for each site
#   upper_list[[site]] <- read.csv(files[1])
# }
# 
## LOWER

# Loop through each site name in the list
for (site in lowersites) {
  # Construct the file path and use a pattern to match files with the specific site
  folder_path <- "googledrive/"
  pattern <- paste0("^[0-9-]+_", site, "_WaterLevel")

  # Find the file matching the pattern (assuming only one match per site)
  files <- list.files(folder_path, pattern = pattern, full.names = TRUE)

  # Read the first matching file for each site
  lower_list[[site]] <- read.csv(files[1])
}

# Load the Air2 data for the upper sites
air_upper <- read.csv("googledrive/2024-10-29_Air2.csv")
air_middle
air_lower <- read.csv("googledrive/2024-10-29_Air2.csv") # For now :(

################################
#### Format DateTime column ####
################################

## Upper
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
air_upper$DateTime <- as.POSIXct(air_upper$DateTime, format = "%Y-%m-%d %H:%M:%S")

## Lower
# Loop through each data frame in the list
for (i in seq_along(lower_list)) {
  # Access the current data frame
  df <- lower_list[[i]]
  
  # Convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
  # Update the data frame in the list
  lower_list[[i]] <- df
}

# Check the contents of the list and make sure there are no NAs
str(lower_list)

# Now check datetime format for air data
air_lower$DateTime <- as.POSIXct(air_lower$DateTime, format = "%Y-%m-%d %H:%M:%S")

#####################################
#### Change all units to meters  ####
#####################################

### Upper site
## Air pressure in kpa to m
air_upper <- air_upper %>%
  mutate(Level_air.m = (.[[4]] * 0.101972))

#1 kPa = 0.101972 m in common barometric units to water column equivalent conversions

## water level is in cm, change to m
# Loop through each data frame in the list
for (i in seq_along(upper_list)) {
  # Access the current data frame
  df <- upper_list[[i]]
  
  #cm to m
  df <- df %>%
    mutate(LELVEL.m = (.[[4]] * 0.01))
  # Update the data frame in the list
  upper_list[[i]] <- df
}

# Check the contents of the list and make sure there are no NAs
str(upper_list)

### Lower site
## Air pressure in kpa to m
air_lower <- air_lower %>%
  mutate(Level_air.m = (.[[4]] * 0.101972))

#1 kPa = 0.101972 m in common barometric units to water column equivalent conversions

## water level is in cm, change to m
# Loop through each data frame in the list
for (i in seq_along(lower_list)) {
  # Access the current data frame
  df <- lower_list[[i]]
  
  #cm to m
  df <- df %>%
    mutate(LELVEL.m = (.[[4]] * 0.01))
  # Update the data frame in the list
  lower_list[[i]] <- df
}

# Check the contents of the list and make sure there are no NAs
str(lower_list)
  
######################################
#### Merging PT with Air pressure ####
######################################

## Upper
# Create a list to store merged results
merged_upper <- list()

# Loop through each site file in upper_list
for (site in names(upper_list)) {
  # Merge each site data with air_upper on the DateTime column
  merged_upper[[site]] <- merge(upper_list[[site]], air_upper, by = "DateTime", all.x = TRUE)
}

# Check the contents of the list
str(merged_upper)

## Lower
# Create a list to store merged results
merged_lower <- list()

# Loop through each site file in upper_list
for (site in names(lower_list)) {
  # Merge each site data with air_upper on the DateTime column
  merged_lower[[site]] <- merge(lower_list[[site]], air_lower, by = "DateTime", all.x = TRUE)
}

# Check the contents of the list
str(merged_lower)

##################################
#### Format some column names ####
##################################

## Upper
for (i in seq_along(merged_upper)) {
  # Access the current data frame
  df <- merged_upper[[i]]
  
  # Rename columns
  df <- df %>%
    dplyr::rename(Pres.abs.kPa = LEVEL.y,
                  LEVEL.cm = LEVEL.x,
                  TEMPERATURE.air = TEMPERATURE.y)
  merged_upper[[i]] <-  df
  }

# Check the contents of the list
str(merged_upper)

## Lower
for (i in seq_along(merged_lower)) {
  # Access the current data frame
  df <- merged_lower[[i]]
  
  # Rename columns
  df <- df %>%
    dplyr::rename(Pres.abs.kPa = LEVEL.y,
                  LEVEL.cm = LEVEL.x,
                  TEMPERATURE.air = TEMPERATURE.y)
  merged_lower[[i]] <-  df
}

# Check the contents of the list
str(merged_lower)

########################################
#### Manual Barometric Compensation ####
########################################

## Once the units for each column are the same, subtract the barometric column from the Levelogger data 
# to get the true net water level recorded by the Levelogger.

## Upper
#Create empty compensated list
compensated_upper <- list()

for (i in names(merged_upper)) {
  # Access the current data frame
  df <- merged_upper[[i]]
  
  # Compensate
  df <- df %>%
    mutate(Baro_Cor_Lvl = (.[[7]] - .[[13]]))
  
  compensated_upper[[i]] <-  df
}

## Lower
#Create empty compensated list
compensated_lower <- list()

for (i in names(merged_lower)) {
  # Access the current data frame
  df <- merged_lower[[i]]
  
  # Compensate
  df <- df %>%
    mutate(Baro_Cor_Lvl = (.[[7]] - .[[13]]))
  
  compensated_lower[[i]] <-  df
}

isna <- is.na(compensated_lower$USF20)

####################################################
#### Save merged and compensated slugs to Drive ####
####################################################
## Upper
# Loop through each data frame in the list
for (i in seq_along(compensated_list)) {
  # Access the current data frame
  df <- compensated_list[[i]]
  
  # Save new data frame
  write.csv(df, paste0("data/", names(compensated_list)[i], ".csv"), row.names=FALSE, quote=FALSE)
  
  # Define the local folder path and the target folder ID in Google Drive
  file <- paste0("data/", names(compensated_list)[i], ".csv")
  # this is the "compensated" folder
  drive_folder_id <- "1VsT7hirl5OHIGhrrc3b7dxPpSqr1wNC0"
  
  # Upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}

## Lower
# Loop through each data frame in the list
for (i in seq_along(compensated_lower)) {
  # Access the current data frame
  df <- compensated_lower[[i]]
  
  # Save new data frame
  write.csv(df, paste0("data/", names(compensated_lower)[i], ".csv"), row.names=FALSE, quote=FALSE)
  
  # Define the local folder path and the target folder ID in Google Drive
  file <- paste0("data/", names(compensated_lower)[i], ".csv")
  # this is the "compensated" folder
  drive_folder_id <- "1VsT7hirl5OHIGhrrc3b7dxPpSqr1wNC0"
  
  # Upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}
