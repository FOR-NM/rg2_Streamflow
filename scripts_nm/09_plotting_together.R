##==============================================================================
## Project: QuEST
## TO DO: Adapt this script for FOR-NM 

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
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1y2bMWCS48cROq_BO5HkaNWmFIxdJUON0")

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
USF21 <- pt_list[["discharge_USF21.csv"]]
USF20 <- pt_list[["discharge_USF20.csv"]]
USF03 <- pt_list[["discharge_USF03.csv"]]
USF05 <- pt_list[["discharge_USF05.csv"]]

####################################################
#### Plot discharge data together for all sites ####
####################################################
# combine all data frames into one
combined_df <- pt_list %>%
  bind_rows()  # Automatically adds a row for each df, keeping column names

# plot the data
ggplot(combined_df, aes(x = DateTime, y = Smooth_Discharge_Log_m3s, color = DataID)) +
  geom_line() +
  labs(
    x = "Date",
    y = "Discharge (m³/s)",
    color = "Site"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Filter out large discharge sites
combined_df_filtered <- combined_df %>%
  filter(!DataID %in% c("USF03", "USF05", "USF20"))

# plot the data
ggplot(combined_df_filtered, aes(x = DateTime, y = Smooth_Discharge_Log_m3s, color = DataID)) +
  geom_line() +
  labs(
    x = "Date",
    y = "Discharge (m³/s)",
    color = "Site"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

##################################
#### Pull USGS discharge data ####
##################################
siteNo <- "08315480" #USF12 site code
pCode <- "00060" # discharge code
start.date <- "2024-05-07"
end.date <- "2025-10-18"

USGS <- readNWISuv(siteNumbers = siteNo,
                   parameterCd = pCode,
                   startDate = start.date,
                   endDate = end.date)

USGS <- renameNWISColumns(USGS)
# parameter_units: ft3/s

# rename DateTime
USGS$DateTime <- USGS$dateTime

### convert ft3/s to m3/s ###
USGS <- USGS %>%
  mutate(Flow_Inst.m = (Flow_Inst * 0.02832 ))

# plot
ts <- ggplot(data = USGS,
             aes(dateTime, Flow_Inst.m)) +
  geom_line()
ts

############################
#### Plot USGS with USF ####
############################
# merge discharge with scan data
merged_df <- merge(USGS, USF05, by = c("DateTime"), all.x = TRUE)

# plot discharge for all sites
ggplot(data = merged_df, aes(x = DateTime)) +
  # map color inside aes() to create a legend
  geom_line(aes(y = Flow_Inst.m, color = "USF12")) +
  geom_line(aes(y = Smooth_Discharge_Log_m3s, color = "USF05")) +
  labs(x = "DateTime", y = "Discharge (m³/s)", color = "Scan sites") +  # add legend titles
  labs(title = "USF05 with USGS gauge") +
  theme_minimal()

#################################
#### Plotting daily averages ####
#################################
# this is the smooth folder
da <- googledrive::as_id("https://drive.google.com/drive/folders/17zLOfcBqiw1-b7WoG-INL9DOeQ-VTGBq")

# list all CSV files in the folder
da_csvs <- googledrive::drive_ls(path = da, type = "csv")
3
## call all the files in the salt slugs folder ##
# create empty list to store data frames
da_list <- list()

# loop over each file in the `pt_csvs` data frame
for (i in seq_along(da_csvs$id)) {
  # define the local file path
  local_path <- file.path("googledrive", da_csvs$name[i])
  
  # download the file
  googledrive::drive_download(
    file = da_csvs$id[i],
    path = local_path,
    overwrite = T
  )
  # read the CSV file and add it to the list
  da_list[[da_csvs$name[i]]] <- read.csv(local_path)
}

# check the contents of the list
str(da_list)

############################
#### Format date column ####
############################
# loop through each data frame in the list
for (i in seq_along(da_list)) {
  # access the current data frame
  df <- da_list[[i]]
  
  # convert the DateTime column to POSIXct
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  
  # update the data frame in the list
  da_list[[i]] <- df
}

# look at it
USF21 <- da_list[["discharge_USF21.csv"]]
USF20 <- da_list[["discharge_USF20.csv"]]
USF03 <- da_list[["discharge_USF03.csv"]]

####################################################
#### Plot discharge data together for all sites ####
####################################################
# combine all data frames into one
combined_df <- da_list %>%
  bind_rows()  # Automatically adds a row for each df, keeping column names

# plot the data
ggplot(combined_df, aes(x = Date, y = smooth_dailyaverage.m3s, color = DataID)) +
  geom_line() +
  labs(
    x = "Date",
    y = "Discharge (m³/s)",
    color = "Site"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Filter out large discharge sites
combined_df_filtered <- combined_df %>%
  filter(!DataID %in% c("USF03", "USF05", "USF20"))

# plot the data
ggplot(combined_df_filtered, aes(x = Date, y = smooth_dailyaverage.m3s, color = DataID)) +
  geom_line() +
  labs(
    x = "Date",
    y = "Discharge (m³/s)",
    color = "Site"
  ) +
  theme_minimal() +
  theme(legend.position = "right")