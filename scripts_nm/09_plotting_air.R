##==============================================================================
## Project: QuEST
## This script is to plot predicted discharge for NM
##==============================================================================

##################
#### Packages ####
##################
library(ggplot2)
library(dplyr)
library(purrr)  # for map functions
library(dataRetrieval) # download USGS discharge data

####################################
## Clear folders that we will use ##
####################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data", full.names = TRUE)
file.remove(files)
files <- list.files(path = "pt_figs", full.names = TRUE)
file.remove(files)

####################
#### Import data ####
#####################
# this is the smooth folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1BsASDFjFci_7mndSKj6T5uXKxW1UZoaP")

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

# check the contents of the list
str(pt_list)

############################
#### Format date column #### 
############################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # make date into date fomat
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
  # update the data frame in the list
  pt_list[[i]] <- df
}

# look at it
Air1 <- pt_list[["AIR1_complete.csv"]]
Air2 <- pt_list[["AIR2.csv"]]
Air3 <- pt_list[["AIR3.csv"]]

####################################################
#### Plot discharge data together for all sites #### 
####################################################
# plot the data
ggplot(Air1, aes(x = DateTime, y = TEMPERATURE)) +
  geom_line() +
  labs(title = "Air 1",
    x = "Date",
    y = "Temperature (C)",
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# plot the data
ggplot(Air2, aes(x = DateTime, y = TEMPERATURE)) +
  geom_line() +
  labs(title = "Air 2",
    x = "Date",
    y = "Temperature (C)",
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# plot the data
ggplot(Air3, aes(x = DateTime, y = TEMPERATURE)) +
  geom_line() +
  labs(title = "Air 3",
    x = "Date",
    y = "Temperature (C)",
  ) +
  theme_minimal() +
  theme(legend.position = "right")

###########################
#### Plotting together ####
###########################
# add an ID column to each dataframe
Air1$Site <- "Air 1"
Air2$Site <- "Air 2"
Air3$Site <- "Air 3"

# combine into one dataframe
Air_all <- bind_rows(Air1, Air2, Air3)

# plot in 3 panels
ggplot(Air_all, aes(x = DateTime, y = TEMPERATURE)) +
  geom_line() +
  facet_wrap(~Site, ncol = 1) +   # ncol = 1 makes vertical panels
  labs(
    title = "Air Temperature Time Series",
    x = "Date",
    y = "Temperature (°C)"
  ) +
  scale_x_datetime(date_breaks = "2 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
