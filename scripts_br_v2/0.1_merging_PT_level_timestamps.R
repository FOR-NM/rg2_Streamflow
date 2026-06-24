##==============================================================================
## Project: QuEST
## Script to merge same site PT sites in one file (using timestamp) for Brush Creek watershed
## We are also going to try to tackle the daylight savings time changes 
##==============================================================================

library(readxl) #to read excel 
library(googledrive)
library(dplyr)

########################################
#### Clear folders that we will use ####
########################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

#####################
#### Import data ####
#####################
#### list and download all files in the folder ####
# this is the "02_inuse" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1q_HNENGb8tyHmWTA-TP5sOM8APgxKRxI")
# list all CSV files in the folder
pt_files <- googledrive::drive_ls(path = pt)
3

# create an empty list to store the cleaned data frames
pt_list <- lapply(seq_along(pt_files$name), function(i) {
  googledrive::drive_download(
    file = pt_files$id[i],
    path = paste0("googledrive/", pt_files$name[i]),
    overwrite = TRUE
  )
  
  # read the CSV file, skipping the first 11 rows (header is on row 12)
  read.csv(paste0("googledrive/", pt_files$name[i]), header = TRUE)
})

# assign names to the list elements based on the file names
names(pt_list) <- pt_files$name

BRMQ1_1 <- pt_list[["2024-12-13_BRMQ1_2190536_PTdownload.csv"]]
BRMQ1_2 <- pt_list[["2024-11-17_BRMQ1_PT20240830-20241011.csv"]]

####################################
#### Combine data for each site ####
####################################
# site names
site_names <- c("BRM01","BRM02","BRM03","BRM05","BRM06","BRM07","BRA01","BRAA1","BRD01","BRF01","BRMQ1","BRMQ4")

# group files in `pt_list` by matching `site_names` in file names
pt_list_by_site <- lapply(site_names, function(site) {
  # names(pt_list) gives the names of all files in pt_list
  site_files <- names(pt_list)[grepl(site, names(pt_list))] 
  # grep checks if the current site (e.g., BRM01) appears in each file name in pt_list 
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

################################
#### Format DateTime column ####
################################
# loop through each data frame in the list
for (i in seq_along(combined_by_site)) {
  # access the current data frame
  df <- combined_by_site[[i]]
  # combine Date and Time columns into a new DateTime column
  df$DateTime <- paste(df$Date, df$Time, sep = " ")
  
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
  # update the data frame in the list
  combined_by_site[[i]] <- df
}

BRMQ1 <- combined_by_site[["BRMQ1"]]
BRM01 <- combined_by_site[["BRM01"]]

## round DateTime for BRMQ1 ##
BRMQ1$DateTime <- round_date(BRMQ1$DateTime, unit="15 mins")
# convert the Time column to POSIXct
BRMQ1$Temp_time <- as.POSIXct(BRMQ1$Time, format = "%H:%M:%S")
# round Time
BRMQ1$Temp_time <- round_date(BRMQ1$Temp_time, unit="15 mins")
# extract only the time part (HH:MM:SS) from the rounded Time
BRMQ1$Time <- format(BRMQ1$Temp_time, format = "%H:%M:%S")

##############################
#### Save combined files  ####
##############################
# write files to local data folder
lapply(names(combined_by_site), function(site) {
  # define file path
  file <- paste0("data/", site, ".csv")
  # save each data frame
  write.csv(combined_by_site[[site]], file, row.names = FALSE, quote = FALSE)
  # this is the "merged_days" folder
  drive_folder_id <- "1SbXzLapTIa_dt02JVba4PcsbQaJeFZtD"
  # upload the file to Google Drive
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
})

# Save BRMQ1
write.csv(BRMQ1, "data/BRMQ1.csv")
# 
drive_folder_id <- "1SbXzLapTIa_dt02JVba4PcsbQaJeFZtD"
# upload files
drive_put(
  media = "data/BRMQ1.csv",
  path = as_id(drive_folder_id)
)
