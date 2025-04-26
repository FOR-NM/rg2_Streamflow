##==============================================================================
## Project: QuEST
## This script is to merge PT and barometric (air pressure) data for the New Mexico sites
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
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "merged", full.names = TRUE)
file.remove(files)

#################################
#### Import & Visualize Data ####
#################################
#### load data from Google drive ####
# This is the inuse folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1i7G-q7FV0_bszqeCdJ6Otz8b9xhpU1cx")

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

# check the contents of the list
str(pt_list)

#### remove baro files from pt list ####
# remove 7th item in this case, check position of baro file
pt_list = pt_list[-c(7, 13)]

# look at it
USF21 <- pt_list[["2024-10-29_USF21_WaterLevel.csv"]]
USF20 <- pt_list[["2024-10-24_USF20_WaterLevel.csv"]]

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
  
  # make date into date fomat
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  # update the data frame in the list
  pt_list[[i]] <- df
}

# look at it
USF21 <- pt_list[["2024-10-29_USF21_WaterLevel.csv"]]
USF20 <- pt_list[["2024-10-24_USF20_WaterLevel.csv"]]

#####################
#### Plot curves ####
#####################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = LEVEL)) + 
    geom_point() + ggtitle(paste(pt_csvs$name[i])) 
  # display the plot in the plot panel
  print(p)
}

####################################################
#### Prep data for merging PT with Air pressure ####
####################################################
# list of site names
uppersites <- c("USF21", "USF13", "USF14", "USF16", "USF19")
middlesites <- c("USF09", "USF10", "USF11")
lowersites <- c("USF03", "USF04", "USF05", "USF07", "USF20")

# create an empty lists to store the files
upper_list <- list()
middle_list <- list()
lower_list <- list()

## UPPER
# loop through each site name in the list
for (site in uppersites) {
  # construct the file path and use a pattern to match files with the specific site
  folder_path <- "googledrive/"
  pattern <- paste0("^[0-9-]+_", site, "_WaterLevel")
  
  # find the file matching the pattern (assuming only one match per site)
  files <- list.files(folder_path, pattern = pattern, full.names = TRUE)
  
  # read the first matching file for each site
  upper_list[[site]] <- read.csv(files[1])
}

# ## MIDDLE
# # loop through each site name in the list
# for (site in middlesites) {
#   # construct the file path and use a pattern to match files with the specific site
#   folder_path <- "googledrive/"
#   pattern <- paste0("^[0-9-]+_", site, "_WaterLevel")
#   
#   # find the file matching the pattern (assuming only one match per site)
#   files <- list.files(folder_path, pattern = pattern, full.names = TRUE)
#   
#   # read the first matching file for each site
#   upper_list[[site]] <- read.csv(files[1])
# }
# 

## LOWER
# loop through each site name in the list
for (site in lowersites) {
  # construct the file path and use a pattern to match files with the specific site
  folder_path <- "googledrive/"
  pattern <- paste0("^[0-9-]+_", site, "_WaterLevel")

  # find the file matching the pattern (assuming only one match per site)
  files <- list.files(folder_path, pattern = pattern, full.names = TRUE)

  # read the first matching file for each site
  lower_list[[site]] <- read.csv(files[1])
}

# load the Air2 data for the upper sites
air_upper <- read.csv("googledrive/2024-10-29_Air2.csv")
air_middle
air_lower <- read.csv("googledrive/2025-04-03_Air3.csv") # for now :(

################################
#### Format DateTime column ####
################################
## upper
# loop through each data frame in the list
for (i in seq_along(upper_list)) {
  # access the current data frame
  df <- upper_list[[i]]
  # combine Date and Time columns into a new DateTime column
  df$DateTime <- paste(df$Date, df$Time, sep = " ")
  
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %I:%M:%S %p")
  # update the data frame in the list
  upper_list[[i]] <- df
}

# check the contents of the list and make sure there are no NAs
str(upper_list)

# now check datetime format for upper air data
air_upper$DateTime <- paste(air_upper$Date, air_upper$Time, sep = " ")
# convert the DateTime column to POSIXct
air_upper$DateTime <- as.POSIXct(air_upper$DateTime, format = "%Y-%m-%d %I:%M:%S %p")

## lower
# loop through each data frame in the list
for (i in seq_along(lower_list)) {
  # access the current data frame
  df <- lower_list[[i]]
  # combine Date and Time columns into a new DateTime column
  df$DateTime <- paste(df$Date, df$Time, sep = " ")
  
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %I:%M:%S %p")
  # update the data frame in the list
  lower_list[[i]] <- df
}

# check the contents of the list and make sure there are no NAs
str(lower_list)

# now check datetime format for lower air data
air_lower$DateTime <- paste(air_lower$Date, air_lower$Time, sep = " ")
# convert the DateTime column to POSIXct
air_lower$DateTime <- as.POSIXct(air_lower$DateTime, format = "%Y-%m-%d %I:%M:%S %p")

#####################################
#### Change all units to meters  ####
#####################################
### upper site
## air pressure in kpa to m
air_upper <- air_upper %>%
  mutate(Level_air.m = (.[[4]] * 0.101972))

#1 kPa = 0.101972 m in common barometric units to water column equivalent conversions

## water level is in cm, change to m
# loop through each data frame in the list
for (i in seq_along(upper_list)) {
  # access the current data frame
  df <- upper_list[[i]]
  
  #cm to m
  df <- df %>%
    mutate(LELVEL.m = (.[[4]] * 0.01))
  # update the data frame in the list
  upper_list[[i]] <- df
}

# check the contents of the list and make sure there are no NAs
str(upper_list)

# look at it
USF21 <- upper_list[["USF21"]]

### lower site
## air pressure in kpa to m
air_lower <- air_lower %>%
  mutate(Level_air.m = (.[[4]] * 0.101972))

#1 kPa = 0.101972 m in common barometric units to water column equivalent conversions

## water level is in cm, change to m
# loop through each data frame in the list
for (i in seq_along(lower_list)) {
  # access the current data frame
  df <- lower_list[[i]]
  
  #cm to m
  df <- df %>%
    mutate(LELVEL.m = (.[[4]] * 0.01))
  # update the data frame in the list
  lower_list[[i]] <- df
}

# check the contents of the list and make sure there are no NAs
str(lower_list)
  
######################################
#### Merging PT with Air pressure ####
######################################
## upper
# create a list to store merged results
merged_upper <- list()

# loop through each site file in upper_list
for (site in names(upper_list)) {
  # Merge each site data with air_upper on the DateTime column
  merged_upper[[site]] <- merge(upper_list[[site]], air_upper, by = "DateTime", all.x = TRUE)
}

# check the contents of the list
str(merged_upper)

## lower
# create a list to store merged results
merged_lower <- list()

# loop through each site file in upper_list
for (site in names(lower_list)) {
  # Merge each site data with air_upper on the DateTime column
  merged_lower[[site]] <- merge(lower_list[[site]], air_lower, by = "DateTime", all.x = TRUE)
}

# check the contents of the list
str(merged_lower)

# look at it
USF21 <- merged_upper[["USF21"]]

##################################
#### Format some column names ####
##################################
## upper
for (i in seq_along(merged_upper)) {
  # access the current data frame
  df <- merged_upper[[i]]
  
  # rename columns
  df <- df %>%
    dplyr::rename(Pres.abs.kPa = LEVEL.y,
                  LEVEL.cm = LEVEL.x,
                  TEMPERATURE.air = TEMPERATURE.y)
  merged_upper[[i]] <-  df
  }

# check the contents of the list
str(merged_upper)

## lower
for (i in seq_along(merged_lower)) {
  # access the current data frame
  df <- merged_lower[[i]]
  
  # rename columns
  df <- df %>%
    dplyr::rename(Pres.abs.kPa = LEVEL.y,
                  LEVEL.cm = LEVEL.x,
                  TEMPERATURE.air = TEMPERATURE.y)
  merged_lower[[i]] <-  df
}

# check the contents of the list
str(merged_lower)

########################################
#### Manual Barometric Compensation ####
########################################
## once the units for each column are the same, subtract the barometric column from the Levelogger data 
# to get the true net water level recorded by the Levelogger.

## upper
# create empty compensated list
compensated_upper <- list()

for (i in names(merged_upper)) {
  # access the current data frame
  df <- merged_upper[[i]]
  
  # compensate
  df <- df %>%
    mutate(Baro_Cor_Lvl = (.[[7]] - .[[13]]))
  
  compensated_upper[[i]] <-  df
}

## lower
# create empty compensated list
compensated_lower <- list()

for (i in names(merged_lower)) {
  # access the current data frame
  df <- merged_lower[[i]]
  
  # compensate
  df <- df %>%
    mutate(Baro_Cor_Lvl = (.[[7]] - .[[13]]))
  
  compensated_lower[[i]] <-  df
}

isna <- is.na(compensated_lower$USF20)

####################################################
#### Save merged and compensated slugs to Drive ####
####################################################
## upper
# loop through each data frame in the list
for (i in seq_along(compensated_upper)) {
  # access the current data frame
  df <- compensated_upper[[i]]
  
  # save new data frame
  write.csv(df, paste0("data/", names(compensated_upper)[i], ".csv"), row.names=FALSE, quote=FALSE)
  
  # define the local folder path and the target folder ID in Google Drive
  file <- paste0("data/", names(compensated_upper)[i], ".csv")
  # this is the "compensated" folder
  drive_folder_id <- "1VsT7hirl5OHIGhrrc3b7dxPpSqr1wNC0"
  
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}

## lower
# loop through each data frame in the list
for (i in seq_along(compensated_lower)) {
  # access the current data frame
  df <- compensated_lower[[i]]
  
  # save new data frame
  write.csv(df, paste0("data/", names(compensated_lower)[i], ".csv"), row.names=FALSE, quote=FALSE)
  
  # define the local folder path and the target folder ID in Google Drive
  file <- paste0("data/", names(compensated_lower)[i], ".csv")
  # this is the "compensated" folder
  drive_folder_id <- "1VsT7hirl5OHIGhrrc3b7dxPpSqr1wNC0"
  
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}
