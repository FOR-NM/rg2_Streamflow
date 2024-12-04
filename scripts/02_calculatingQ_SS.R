##==============================================================================
## Project: QuEST
## This script is to merge PT and barometric (air pressure) data for SS sites
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

files <- list.files(path = "data", full.names = TRUE)
file.remove(files)

#################################
#### Import & Visualize Data ####
#################################
#### Load data from Google drive ####
# This is the "compensated" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1SAtC_CJd6KC2yWtJB_VdTebMBDSjy-Pk")

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

######################################################################################################
#### Select between two different rating curves based on whether it was a high or low measurement ####
######################################################################################################

#Create empty discharge list
rating_list <- list()

for (i in names(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  
  # Rename columns
  df <- df %>%
    mutate(Prov_Flow = if_else(
      .[[12]] < 0.165, 
      55.528*.[[12]]^4.545, 
      3.5837*.[[12]]^2-0.7562*.[[12]]+0.0425)
    )

  rating_list[[i]] <-  df
}

#############################
#### Calculate discharge ####
#############################

#Create empty discharge list
discharge_list <- list()

for (i in names(pt_list)) {
  # Access the current data frame
  df <- rating_list[[i]]
  
  # Rename columns
  df <- df %>%
    mutate(Q.mm = .[[13]]/(D$7*1000000))*1000*60*5)
  
  discharge_list[[i]] <-  df
}

