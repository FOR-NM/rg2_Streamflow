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

files <- list.files(path = "merged", full.names = TRUE)
file.remove(files)

########################################################
#### Load PT depth and times data from Google drive ####
########################################################
(depth <- drive_get("https://docs.google.com/spreadsheets/d/1f4iH0JrE9bNU3SSsXhK-gk3yzFa_k70TaJQ1BOPd3mk/edit?gid=1002055380#gid=1002055380"))
3

# download the file as a csv file
drive_download(as_id(depth$id), path = "googledrive/salt.csv", type = "csv", overwrite = T)

# fetch the file
salt <- read.csv("data/salt.csv", skip = 1)

# rename columns and convert types
salt <- salt %>%
  # rename columns
  dplyr::rename(
    DataID = Site,
    Time = Time.Arrived..if.applicable.,
    salt = Amount.of.Salt.Injected..g.,
  ) %>%
  # change date format
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

colnames(salt)

# remove rows that I don't want
salt <- salt[ , -c(4, 8, 9, 12, 14:16, 19:32)]

# replace empties with NA
salt["Slug.flag"][salt["Slug.flag"] == ''] <- NA
salt["Slug.notes"][salt["Slug.notes"] == ''] <- NA

########################################################
#### Combine and format Date and Time in one column ####
########################################################
# combine Date and Time columns into a new DateTime column
salt$DateTime <- paste(salt$Date, salt$Time, sep = " ")

# convert the DateTime column to POSIXct
salt$DateTime <- as.POSIXct(salt$DateTime, format = "%Y-%m-%d %H:%M:%S")

#######################################
#### Load Q data from Google drive ####
#######################################
discharge <- googledrive::as_id("https://drive.google.com/drive/folders/1UkRaYRBePgY9XU90_3DvURNGGEWbCew0")

# list all CSV files in the folder
discharge_csv <- googledrive::drive_ls(path = discharge, type = "csv")
3

# call the specific file you want (most recent one)
googledrive::drive_download(file = discharge_csv$id[discharge_csv$name=="Q.csv"], 
                            path = "googledrive/Q.csv",
                            overwrite = T)

# load it into R
Q = read.csv("data/DVMS5_Q.csv")

# convert the Date column to Date
Q$Date <- as.Date(Q$Date, format = "%Y-%m-%d")

# remove duplicate rows
Q <- Q[ , -c(3, 7, 8)]

colnames(Q)

########################################
#### Merge depth and discharge data ####
########################################
discharge_depth <- merge(Q, salt, by = c("DataID", "Date"), all.x = TRUE)

# round DateTime to the nearest 15-minute interval 
discharge_depth$DateTime <- round_date(discharge_depth$DateTime, unit="15 mins")

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
pt_list <- pt_list_backup 
#########################


pt_csvs <- c("DVMS5")

pt_list[["DVMS5.csv"]] <- DVMS5
pt_list[["DVNWT3.csv"]] <- DVNWT3

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
  data_id <- tools::file_path_sans_ext(pt_list$name[[i]])
  
  # add the DataID column
  df <- df %>%
    dplyr::mutate(DataID = data_id)
  
  # save the modified data frame back to the list
  pt_list[[i]] <- df
}

# # offline 
# # extract the file name without the .csv extension
# for (i in seq_along(pt_list)) {
#   # access the current data frame
#   df <- pt_list[[i]]
#   
#   # extract the file name without the .csv extension
#   DataID <- tools::file_path_sans_ext(pt_csvs[[i]])
#   
#   # add the DataID column
#   df <- df %>%
#     dplyr::mutate(DataID = data_id)
#   
#   # save the modified data frame back to the list
#   pt_list[[i]] <- df
# }

# check the contents of the list
str(pt_list)

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
  
  df$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",df$DateTime)] <- paste(
    df$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",df$DateTime)],"00:00:00")
  
  # update the data frame in the list
  pt_list[[i]] <- df
}

# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
  # update the data frame in the list
  pt_list[[i]] <- df
}

# # loop through each data frame in the list
# for (i in seq_along(pt_list)) {
#   # access the current data frame
#   df <- pt_list[[i]]
#   
#   # round
#   df$DateTime <- round_date(df$DateTime, unit="15 mins")
#   # remove duplicates
#   df <- df[!duplicated(df$DateTime), ]
#   
#   # update the data frame in the list
#   pt_list[[i]] <- df
# }

## make a column for the new rounded time
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # time only
  df$TimeOnly <- format(df$DateTime, "%H:%M:%S") # extracts time as string
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
#DVSB1_depth <- merge(DVSB1, discharge_depth, by = c("DateTime"), all.y = T)

DVMS1 <- depth_merged[["DVMS1.csv"]]
DVWT3 <- depth_merged[["DVWT3.csv"]]
DVMS5 <- depth_merged[["DVMS5.csv"]]
DVNWT3 <- depth_merged[["DVNWT3.csv"]]

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
