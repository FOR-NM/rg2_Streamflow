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
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1zAYT2ZXfCbIuoxzR7-CpZPw19Wk91pIH")
# list all CSV files in the folder
discharge_files <- googledrive::drive_ls(path = pt)
3

# create an empty list to store the cleaned data frames
q_list <- lapply(seq_along(discharge_files$name), function(i) {
  googledrive::drive_download(
    file = discharge_files$id[i],
    path = paste0("googledrive/", discharge_files$name[i]),
    overwrite = TRUE
  )
  
  # read the CSV file, skipping the first 11 rows (header is on row 12)
  read.csv(paste0("googledrive/", discharge_files$name[i]), header = TRUE)
})

# assign names to the list elements based on the file names
names(q_list) <- discharge_files$name

# format date and time columns
for (i in seq_along(q_list)) {
  # access the current data frame
  df <- q_list[[i]]
  
  # make date into date format
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  # update the data frame in the list
  q_list[[i]] <- df
}

######################
#### Combine data ####
######################
# Bind and filter for unique site-date combinations
Q_DV <- q_list %>%
  bind_rows() %>%
  # Correct way to specify multiple columns in distinct
  distinct(DataID, Date, .keep_all = TRUE)

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
discharge_depth <- merge(Q_DV, salt, by = c("DataID", "Date"), all.x = TRUE)
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

#########################
pt_list_backup <- pt_list 
#########################

# look at it
DVMS1 <- pt_list[["DVMS1.csv"]]
DVWT3 <- pt_list[["DVWT3.csv"]]
DVMS5 <- pt_list_backup[["DVMS5.csv"]]
DVNWT3 <- pt_list_backup[["DVNWT3.csv"]]

###################################
#### Add DataID column to csvs ####
###################################
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  
  # extract the file name without the .csv extension
  DataID <- tools::file_path_sans_ext(pt_csvs$name[[i]])
  
  # add the DataID column
  df <- df %>%
    dplyr::mutate(DataID = DataID)
  
  # save the modified data frame back to the list
  pt_list[[i]] <- df
}

# check individual data frame
DVMS5 <- pt_list[["DVMS5.csv"]]
DVNWT3 <- pt_list[["DVNWT3.csv"]]

# remove upper sites
pt_list = pt_list[-c(1, 2)]

###################################
#### Change to DateTime format ####
###################################
# add missing midnight time
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
DVMS1 <- pt_list[["DVMS1.csv"]]
DVWT3 <- pt_list[["DVWT3.csv"]]
DVMS5 <- pt_list[["DVMS5.csv"]]
DVNWT3 <- pt_list[["DVNWT3.csv"]]

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
#DVSB1_depth <- merge(DVSB1, discharge_depth, by = c("DateTime"), all.y = T)

DVMS1 <- depth_merged[["DVMS1.csv"]]
DVWT3 <- depth_merged[["DVWT3.csv"]]
DVMS5 <- depth_merged[["DVMS5.csv"]]
DVNWT3 <- depth_merged[["DVNWT3.csv"]]
DVET <- depth_merged[["DVET.csv"]]
DVNWT4 <- depth_merged[["DVNWT4.csv"]]

#write.csv(DVNWT3, "data/DVNWT3.csv")
#write.csv(DVMS5, "data/DVMS5.csv")
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
 
