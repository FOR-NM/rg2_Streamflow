##==============================================================================
## Project: QuEST
## Script to format all the Pressure Transducer (Level Logger) to a cleaner state and upload them back to Drive for SS sites
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

##############
## Packages ##
##############
library(googledrive) 
library(tidyverse)

###################################
## Clear folders that we will use ##
###################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

#####################
#### Import Data ####
#####################
# set up Google Drive folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/194hsX_kF8xMs-9Uzq_yyaxoh_ywh6HMm")

# list and filter CSV files with "pt" in their names
pt_files <- googledrive::drive_ls(path = pt, type = "csv")
3
# pt_files <- pt_files[!grepl("baro", pt_files$name), ]

# create an empty list to store the cleaned data frames
pt_list <- lapply(seq_along(pt_files$name), function(i) {
  googledrive::drive_download(
    file = pt_files$id[i],
    path = paste0("googledrive/", pt_files$name[i]),
    overwrite = TRUE
  )
  
  # read the CSV file, skipping the first 11 rows (header is on row 12)
  read.csv(paste0("googledrive/", pt_files$name[i]), skip = 11 , header = TRUE)
})

# assign names to the list elements based on the file names
names(pt_list) <- pt_files$name

############################
#### Format date column ####
############################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  
  # make date into date format
  df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
  # update the data frame in the list
  pt_list[[i]] <- df
}

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
  
  # update the data frame in the list
  pt_list[[i]] <- df
}

####################################
#### Combine data for each site ####
####################################
# site names
site_names <- c("SSM01", "SSM20", "SST13", "SST03", "SST04", "SST05", "SST06", "SST07", "SST08", "SST09", "baro")
# group files in `pt_list` by matching `site_names` in file names
pt_list_by_site <- lapply(site_names, function(site) {
  # names(pt_list) gives the names of all files in pt_list
  site_files <- names(pt_list)[grepl(site, names(pt_list))] 
  # grep checks if the current site (e.g., DVSB1) appears in each file name in pt_list 
  # this returns a logical vector (TRUE for matches, FALSE otherwise).
  pt_list[site_files] # select only the files for this site
  # the [ ] indexing selects only the file names where the match is TRUE.
})

# name the list by site
names(pt_list_by_site) <- site_names

# combine data for each site
combined_by_site <- lapply(pt_list_by_site, function(site_data_list) {
  # bind rows of all data frames for the site
  bind_rows(site_data_list) %>%
    arrange(DateTime) %>%  # ensure chronological order if 'DateTime' exists
    distinct(DateTime, .keep_all = TRUE) # remove duplicates
})

###########################################
#### Save combined files back to Drive ####
###########################################
# write files to local data folder
lapply(names(combined_by_site), function(site) {
  # define file path
  file <- paste0("data/", site, ".csv")
  # save each data frame
  write.csv(combined_by_site[[site]], file, row.names = FALSE, quote = FALSE)
  # this is the "merged_days" folder
  drive_folder_id <- "1ra2bNFXMRU8uckYuAq4-09tdlGq2nDmx"
  # upload the file to Google Drive
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
})

# #### single excel that I had to edit on it's own ####
# # read the CSV file
# SST05 <- read.csv("googledrive/2024-12-16_SSM20_PTS_SN2191067_baro.csv", skip = 10)
# 
# # convert the Date column to POSIXct
# SST05$Date <- as.Date(SST05$Date, format = "%m/%d/%Y")
# 
# # combine Date and Time columns into a new DateTime column
# SST05$DateTime <- paste(SST05$Date, SST05$Time, sep = " ")
# 
# # convert the DateTime column to POSIXct
# SST05$DateTime <- as.POSIXct(SST05$DateTime, format = "%Y-%m-%d %I:%M:%S %p")
# 
# # check the structure of the data frame to confirm the DateTime column
# str(SST05)
# 
# # save the data frame to a CSV file
# write.csv(SST05, file = "2024-12-16_SSM20_PTS_SN2191067_baro.csv", row.names = FALSE, quote = FALSE)

