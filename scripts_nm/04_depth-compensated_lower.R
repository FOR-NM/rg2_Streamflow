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

####################################
## Clear folders that we will use ##
####################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

##############################################
#### Load PT depth data from Google drive ####
##############################################
(depth <- drive_get("https://docs.google.com/spreadsheets/d/1rIWYWFUoF6UtzTcvN-WpIw2Cw4u9NjI_HuhThoDOvv4/edit?gid=0#gid=0"))
3

# download the file as a csv file
drive_download(as_id(depth$id), path = "googledrive/salt.csv", type = "csv", overwrite = T)

# fetch the file
salt <- read.csv("googledrive/salt.csv")

# clean the column names (this removes spaces, special characters, etc.)
salt <- salt %>%
  janitor::clean_names()

# rename columns and convert types
salt <- salt %>%
  # rename columns
  dplyr::rename(
    DataID = site,
    Date = date,
    Time24h = time_24h_rounded_to_nearest_15_min,
    reach = reach_length_m,
    salt = salt_added_g,
    pt = is_there_a_pt_at_this_site,
    pt_depth_cm = pt_depth_cm_at_the_time_of_the_discharge_measurement_numbers_only
  ) %>%
  # convert
  mutate(
    pt_depth_cm = as.numeric(as.character(pt_depth_cm)),
    # change date format
    Date = as.Date(Date, format = "%m/%d/%Y"),
  )

colnames(salt)

# remove rows that I don't want
salt <- salt[ , -c(1:3, 8, 9, 12:14, 18)]

# replace empties with NA
salt["relevant_notes"][salt["relevant_notes"] == ''] <- NA
salt["flag_notes"][salt["flag_notes"] == ''] <- NA

#### filter to only sites with PT ####
PT_sites <- salt
# PT_sites <- salt %>% filter(pt == "Yes" )

########################################################
#### Combine and format Date and Time in one column ####
########################################################
# combine Date and Time columns into a new DateTime column
PT_sites$DateTime <- paste(PT_sites$Date, PT_sites$Time24h, sep = " ")

# convert the DateTime column to POSIXct
PT_sites$DateTime <- as.POSIXct(PT_sites$DateTime, format = "%Y-%m-%d %H:%M:%S")

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
Q = read.csv("googledrive/Q.csv")

# convert the Date column to Date
Q$Date <- as.Date(Q$Date, format = "%Y-%m-%d")

# remove duplicate rows
Q <- Q[ , -c(3, 7, 8)]

colnames(Q)

########################################
#### Merge depth and discharge data ####
########################################
discharge_depth <- merge(Q, PT_sites, by = c("DataID", "Date"), all.x = TRUE)

####################################################
#### Load PT compensated data from Google drive ####
####################################################
# this is the compensated folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1VsT7hirl5OHIGhrrc3b7dxPpSqr1wNC0")

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

# check the contents of the list
str(pt_list)

# check individual data frame
USF20 <- pt_list[["USF20.csv"]]
USF07 <- pt_list[["USF07.csv"]]

# remove upper sites
pt_list = pt_list[-c(6:10)]

###################################
#### Change to DateTime format ####
###################################
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

# check the contents of the list and make sure there are no NAs
str(pt_list)

#################################################
#### Rounding the time for some of the sites ####
#################################################
USF03 <- pt_list[["USF03.csv"]]
USF04 <- pt_list[["USF04.csv"]]
USF05 <- pt_list[["USF05.csv"]]
USF07 <- pt_list[["USF07.csv"]]
USF20 <- pt_list[["USF20.csv"]]

# transform to datetime format
USF03$DateTime <- as.POSIXct(USF03$DateTime,format = "%Y-%m-%d %H:%M:%S")
USF04$DateTime <- as.POSIXct(USF04$DateTime,format = "%Y-%m-%d %H:%M:%S")
USF05$DateTime <- as.POSIXct(USF05$DateTime,format = "%Y-%m-%d %H:%M:%S")
USF07$DateTime <- as.POSIXct(USF07$DateTime,format = "%Y-%m-%d %H:%M:%S")

# round DateTime to the nearest 15-minute interval 
USF07$DateTime <- floor_date(USF07$DateTime, unit="minute")

USF03$DateTime <- round_date(USF03$DateTime, unit="15 mins")
USF04$DateTime <- round_date(USF04$DateTime, unit="15 mins")
USF05$DateTime <- round_date(USF05$DateTime, unit="15 mins")
USF07$DateTime <- round_date(USF07$DateTime, unit="15 mins")

# return to list
pt_list$USF03.csv <- USF03
pt_list$USF04.csv <- USF04
pt_list$USF05.csv <- USF05
pt_list$USF07.csv <- USF07

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

USF03 <- depth_merged[["USF03.csv"]]
USF04 <- depth_merged[["USF04.csv"]]
USF05 <- depth_merged[["USF05.csv"]]
USF07 <- depth_merged[["USF07.csv"]]
USF20 <- depth_merged[["USF20.csv"]]

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
  drive_folder_id <- "1EswIfUWCK6bsdcs-ZrAMGW1oYKs4B0Eh"
  
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}

