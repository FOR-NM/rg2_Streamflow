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
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data", full.names = TRUE)
file.remove(files)

#################################
#### Import & Visualize Data ####
#################################
#### load data from Google drive ####
# This is the "combined by site" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1ra2bNFXMRU8uckYuAq4-09tdlGq2nDmx")

# list all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3
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

# load the baro data separately
# all sites will be merged with the one air data that South Sandy has
air_data <- read.csv("googledrive/baro.csv")

################################
#### Format DateTime column ####
################################
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
}

# now check datetime format for air data
air_data$DateTime <- paste(air_data$Date, air_data$Time, sep = " ")
air_data$DateTime <- as.POSIXct(air_data$DateTime, format = "%Y-%m-%d %I:%M:%S %p")

###########################
#### Rounding the time ####
###########################
# rounding the time up or down to the nearest consistent interval 
# example: 10:04 gets converted to 10:05 for this we use the lubridate package

pt_list[["SST07.csv"]]$DateTimeNotRounded <- pt_list[["SST07.csv"]]$DateTime
pt_list[["SST07.csv"]]$DateTime <- round_date(pt_list[["SST07.csv"]]$DateTime, unit="15 mins")

str(pt_list)

# round DateTime to the nearest 15-minute interval across all files in pt_list
for (i in seq_along(pt_list)) {
  pt_list[[i]]$DateTime <- round_date(pt_list[[i]]$DateTime, unit = "15 mins")
}

#####################################
#### Change all units to meters  ####
#####################################
## air pressure in kPa to m
#1 kPa = 0.101972 m
air_data <- air_data %>%
  mutate(Level_air.m = (.[[4]] * 0.101972))

## water level is in kPa, change to m
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  
  #kPa to m
  df <- df %>%
    mutate(LEVEL.m = (LEVEL * 0.101972))
  # Update the data frame in the list
  pt_list[[i]] <- df
}

# check the contents of the list and make sure there are no NAs
str(pt_list)

#####################
#### Plot curves ####
#####################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # plot
  p <- ggplot(data = df, aes(x = DateTime, y = LEVEL.m)) + 
    geom_line() + ggtitle(paste(pt_csvs$name[i])) 
  # display the plot in the plot panel
  print(p)
  # save the plot as a PNG file
  ggsave(paste0("pt_figs/", pt_csvs$name[i], ".png"), plot = p)
}

#### remove baro file from pt list ####
# remove second item in this case, check position of baro file
pt_list = pt_list[-1]

######################################
#### Merging PT with Air pressure ####
######################################
# create a list to store merged results
merged_list <- list()

# loop through each site file in pt_list
for (i in names(pt_list)) {
  # Merge each site data with air_data on the DateTime column
  merged_list[[i]] <- merge(pt_list[[i]], air_data, by = "DateTime", all.x = TRUE)
}

# check the contents of the list
str(merged_list)

##################################
#### Format some column names ####
##################################
for (i in seq_along(merged_list)) {
  # access the current data frame
  df <- merged_list[[i]]
  
  # rename columns
  df <- df %>%
    dplyr::rename(Pres.abs.kPa = LEVEL.y,
                  LEVEL.kPa = LEVEL.x,
                  TEMPERATURE.air = TEMPERATURE.y)
  merged_list[[i]] <-  df
}

# check the contents of the list
str(merged_list)

########################################
#### Manual Barometric Compensation ####
########################################
## Once the units for each column are the same, subtract the barometric column from the Levelogger data 
# to get the true net water level recorded by the Levelogger.

# create empty compensated list
compensated_list <- list()

for (i in names(merged_list)) {
  # access the current data frame
  df <- merged_list[[i]]
  
  # compensate
  df <- df %>%
  mutate(Baro_Cor_Lvl.m = (.[["LEVEL.kPa"]] - .[["Pres.abs.kPa"]]))
  
  compensated_list[[i]] <-  df
}

#########################################
#### Save compensated files to Drive ####
#########################################
# loop through each data frame in the list
for (i in seq_along(compensated_list)) {
  # access the current data frame
  df <- compensated_list[[i]]
  
  # save new data frame
  write.csv(df, paste0("data/", names(compensated_list)[i]), row.names=FALSE, quote=FALSE)
  
  # define the local folder path and the target folder ID in Google Drive
  file <- paste0("data/", names(compensated_list)[i])
  # this is the "compensated" folder
  drive_folder_id <- "1SAtC_CJd6KC2yWtJB_VdTebMBDSjy-Pk"
  
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}
