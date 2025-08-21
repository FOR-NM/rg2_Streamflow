##==============================================================================
## Project: QuEST
## This script is to merge discharge data from salt slugs with pt data
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

##################
#### Packages ####
##################
library(googledrive)
library(ggplot2)
library(dplyr)
library(lubridate) 
library(hms)

####################################
## Clear folders that we will use ##
####################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data", full.names = TRUE)
file.remove(files)

########################################################
#### Load PT depth and times data from Google drive ####
########################################################
(depth <- drive_get("https://docs.google.com/spreadsheets/d/1f4iH0JrE9bNU3SSsXhK-gk3yzFa_k70TaJQ1BOPd3mk/edit?gid=1002055380#gid=1002055380"))
3
# download the file as a csv file
drive_download(as_id(depth$id), path = "googledrive/salt.csv", type = "csv", overwrite = T)

# fetch the file
salt <- read.csv("googledrive/salt.csv", skip = 1)

# clean the column names (this removes spaces, special characters, etc.)
salt <- salt %>%
  janitor::clean_names()

# rename columns and convert types
salt <- salt %>%
  # rename columns
  dplyr::rename(
    DataID = site,
    Date = date,
    Time = time_of_injection,
    background = background_spc_m_s_cm,
    salt = amount_of_salt_injected_g,
    Actual_Water_Depth_m = actual_depth_m
  ) 

# remove rows that I don't want
salt <- salt[ , -c(4, 8, 9, 12, 14:16)]

# replace empties with NA
salt["slug_flag"][salt["slug_flag"] == ''] <- NA
salt["slug_notes"][salt["slug_notes"] == ''] <- NA

# convert the Date column to Date
salt$Date <- as.POSIXct(salt$Date, format = "%m/%d/%Y")

# combine Date and Time columns into a new DateTime column
salt$DateTime <- paste(salt$Date, salt$Time, sep = " ")
# convert the DateTime column to POSIXct
salt$DateTime <- as.POSIXct(salt$DateTime, format = "%Y-%m-%d %I:%M:%S %p")

#######################################
#### Load Q data from Google drive ####
#######################################
discharge <- googledrive::as_id("https://drive.google.com/drive/folders/1zAYT2ZXfCbIuoxzR7-CpZPw19Wk91pIH")

# list all CSV files in the folder
discharge_csv <- googledrive::drive_ls(path = discharge, type = "csv")
3

# call the specific file you want (most recent one)
googledrive::drive_download(file = discharge_csv$id[discharge_csv$name=="Q_DV.csv"], 
                            path = "googledrive/Q_DV.csv",
                            overwrite = T)

# load it into R
Q = read.csv("googledrive/Q_DV.csv")

# convert the Date column to Date
Q$Date <- as.Date(Q$Date, format = "%Y-%m-%d")

# remove duplicate rows
Q <- Q[ , -c(3, 4, 7, 8)]

colnames(Q)

#########################################
#### Load flow status of the streams ####
#########################################
(st <- drive_get("https://docs.google.com/spreadsheets/d/1ig_BVOc9yp33Gkn-nLVaEA3rKy1TdgNnd69zbz4KOsQ/edit?gid=0#gid=0"))
3
# download the file as a csv file
drive_download(as_id(st$id), path = "googledrive/flowstatus.csv", type = "csv", overwrite = T)

# fetch the file
flowstatus <- read.csv("googledrive/flowstatus.csv")

# rename columns and convert types
flowstatus <- flowstatus %>%
  # rename columns
  dplyr::rename(
    DataID = SiteID,
    Time = YSI_StartTime,
    slug_flag = Flow_Status
  )

# convert the Date column to Date
flowstatus$Date <- as.POSIXct(flowstatus$Date, format = "%m/%d/%Y")

# combine Date and Time columns into a new DateTime column
flowstatus$DateTime <- paste(flowstatus$Date, flowstatus$Time, sep = " ")
# convert the DateTime column to POSIXct
flowstatus$DateTime <- as.POSIXct(flowstatus$DateTime, format = "%Y-%m-%d %I:%M:%S %p")
# round DateTime to the nearest 15-minute interval 
flowstatus$DateTime <- round_date(flowstatus$DateTime, unit="15 mins")

# remove rows and columns that I don't want
flowstatus <- flowstatus[ , -c(6:17)]
flowstatus <- flowstatus[ -c(321:360),]

# retain only dry moments
flowstatus <- flowstatus %>%
  filter(slug_flag == 'Dry')

flowstatus <- flowstatus %>%
  mutate(Q = 0)

## reload to add actual water depth data ##
actual_water_depth <- read.csv("googledrive/flowstatus.csv")

# rename columns and convert types
actual_water_depth <- actual_water_depth %>%
  # rename columns
  dplyr::rename(
    DataID = SiteID,
    Time = YSI_StartTime
  ) 

# convert the Date column to Date
actual_water_depth$Date <- as.POSIXct(actual_water_depth$Date, format = "%m/%d/%Y")
# combine Date and Time columns into a new DateTime column
actual_water_depth$DateTime <- paste(actual_water_depth$Date, actual_water_depth$Time, sep = " ")
# convert the DateTime column to POSIXct
actual_water_depth$DateTime <- as.POSIXct(actual_water_depth$DateTime, format = "%Y-%m-%d %I:%M:%S %p")
# round DateTime to the nearest 15-minute interval 
actual_water_depth$DateTime <- round_date(actual_water_depth$DateTime, unit="15 mins")

# retain only observed moments
actual_water_depth <- actual_water_depth %>%
  filter(Direct_Observation == 'yes')

# remove rows and columns that I don't want
actual_water_depth <- actual_water_depth[ , -c(6:14, 16, 17)]
actual_water_depth <- actual_water_depth[ -c(321:360),]

# change cm to m
actual_water_depth$Actual_Water_Depth_cm <- as.numeric(actual_water_depth$Actual_Water_Depth_cm)
actual_water_depth <- actual_water_depth %>%
  mutate(Actual_Water_Depth_m = Actual_Water_Depth_cm/100)

########################################
#### Merge depth and discharge data ####
########################################
discharge_depth <- merge(Q, salt, by = c("DataID", "Date"), all.x = TRUE)
# round DateTime to the nearest 15-minute interval 
discharge_depth$DateTime <- round_date(discharge_depth$DateTime, unit="15 mins")

# add flow status data
discharge_depth <- bind_rows(discharge_depth, flowstatus)

# add actual water depth data to table
discharge_depth <- merge(discharge_depth, actual_water_depth, by = c("DataID", "Date", "DateTime", "Actual_Water_Depth_m"), all.x = TRUE, all.y = TRUE)

####################################################
#### Load PT compensated data from Google drive ####
####################################################
# this is the compensated folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/15HqdL5fw1BDTt_X6xWsm__GTJcZCIy8w")

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

pt_list_backup <- pt_list

# check the contents of the list
str(pt_list)

# look at it
DVSB1 <- pt_list[["DVSB1.csv"]]
DVSB2 <- pt_list[["DVSB2.csv"]]

###################################
#### Add DataID column to csvs ####
###################################
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  
  # extract the file name without the .csv extension
  data_id <- tools::file_path_sans_ext(pt_csvs$name[[i]])
  
  # add the DataID column
  df <- df %>%
    dplyr::mutate(DataID = data_id)
  
  # save the modified data frame back to the list
  pt_list[[i]] <- df
}

# check individual data frame
DVSB1 <- pt_list[["DVSB1"]]
DVSB2 <- pt_list[["DVSB2"]]

# remove upper sites
pt_list = pt_list[-c(3:10)]

###################################
#### Change to DateTime format ####
###################################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  
  # make date into date format
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  
  # DateTime at midnight is missing 00:00:00 time in lower air df, so filling in that time using grep
  df$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",df$DateTime)] <- paste(
    df$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",df$DateTime)],"00:00:00")
  
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
  
  # update the data frame in the list
  pt_list[[i]] <- df
}

# check individual data frame
DVSB1 <- pt_list[["DVSB1.csv"]]
DVSB2 <- pt_list[["DVSB2.csv"]]

#######################################
#### Combine depth info to PT data ####
#######################################
# check unique DataID values
unique(discharge_depth$DataID)
unique(pt_list[[1]]$DataID)

# check example DateTime values
head(discharge_depth$DateTime)
head(pt_list[[1]]$DateTime)

# merge discharge_depth with each data frame in csvs list
depth_merged <- lapply(pt_list, function(df) {
  # Perform the merge by DataID and DateTime
  merged_df <- merge(df, discharge_depth, by = c("DataID", "DateTime"), all.x = TRUE)
  return(merged_df)
})

# check the structure of the merged list
str(depth_merged)

# count non-NA values in the 'pt' column for each data frame
non_na_counts <- sapply(depth_merged, function(df) {
  if ("pt" %in% colnames(df)) {
    sum(!is.na(df$pt))
  } else {
    NA  # If 'pt' column is missing, return NA
  }
})

# print the counts
print(non_na_counts)

# offline
# DVSB1_depth <- merge(DVSB1, discharge_depth, by = c("DateTime"), all.y = T)

DVSB1 <- depth_merged[["DVSB1.csv"]]
DVSB2 <- depth_merged[["DVSB2.csv"]]

Date1 <- as.Date("2025-04-01", "%Y-%m-%d")
Date2 <- as.Date("2025-04-30", "%Y-%m-%d")
subdf <- DVSB1[DVSB1$DateTime < Date2 & DVSB1$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,LEVEL.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-18 14:00:00"), linetype="dashed", color="red")

#######################################
#### Save merged PT files to Drive ####
#######################################
# loop through each data frame in the list
for (i in seq_along(depth_merged)) {
  # Access the current data frame
  df <- depth_merged[[i]]
  
  # save new data frame
  write.csv(df, paste0("data/", names(depth_merged)[i]), row.names=FALSE, quote=FALSE)
  
  # define the local folder path and the target folder ID in Google Drive
  file <- paste0("data/", names(depth_merged)[i])
  # this is the "depth" folder
  drive_folder_id <- "1uyQmmLawojBw-yN2sjsCbTMDPRbvqyz4"
  
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}

