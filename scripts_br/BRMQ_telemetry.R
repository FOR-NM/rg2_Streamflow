##==============================================================================
## Project: QuEST
##
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

files <- list.files(path = "data", full.names = TRUE)
file.remove(files)

###############################
#### Import telemetry Data ####
###############################
#### Load data from Google drive ####
# this is the "telemetry data" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1Ny6cXbjzX2GvZwgnGxiIh-KqkKbR6Q7g")

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

# format datetime
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$Time, format = "%Y-%m-%d %H:%M:%S")
  # update the data frame in the list
  pt_list[[i]] <- df
}

#####################################
#### Combine telemetry only data ####
#####################################
# bind rows of all data frames for the site
combined <- bind_rows(pt_list) %>%
  arrange(DateTime) %>%  # chronological order if 'DateTime' exists
  distinct(DateTime, .keep_all = TRUE) # remove duplicates

combined$DateTime <- combined$Time

###################################
#### Import non-telemetry Data ####
###################################
#### Load data from Google drive ####
# this is the "depth" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1n17b_9yf5DCO_h6uPya5vBPz2dh13L3v")

# List all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3

#BRMQ1
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="BRMQ1.csv"], 
                            path = "googledrive/BRMQ1.csv",
                            overwrite = T)
# load file
BRMQ1 <- read.csv("googledrive/BRMQ1.csv")

# combine Date and Time columns into a new DateTime column
BRMQ1$DateTime <- paste(BRMQ1$Date.x, BRMQ1$Time.x, sep = " ")

# convert the DateTime column to POSIXct
BRMQ1$DateTime <- as.POSIXct(BRMQ1$DateTime, format = "%Y-%m-%d %H:%M:%S")

######################
#### Combine data ####
######################
# bind rows of all data frames for the site
BRMQ1_combined <- bind_rows(pt_list) %>%
  arrange(DateTime) %>%  # chronological order if 'DateTime' exists
  distinct(DateTime, .keep_all = TRUE) # remove duplicates