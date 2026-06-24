##==============================================================================
## Project: QuEST
## Script to merge same site PT sites in one file (using timestamp) for Santa Fe watershed
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

##########################
#### Import scan data ####
##########################
#### list and download all files in the folder ####
# this is the "inuse level logger" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1B1xRjXUzU1Q3baLGwN5NvCk5axHG-ylq")
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

DVMS5 <- pt_list[["2192629_2024_10_18_DVMS5_Levelogger.csv"]]
DVNWT5 <- pt_list[["2139884_2024_10_18_DVNWT5_Levelogger.csv"]]

# fix one file's column names
DVNWT5 <- DVNWT5 %>%
  rename(LEVEL = X,
         TEMPERATURE = X.1)

# return to list
pt_list$'2139884_2024_10_18_DVNWT5_Levelogger.csv' <- DVNWT5

################################
#### Format DateTime column ####
################################
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

## round seconds to the 00 interval 
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # floor seconds
  df$DateTime <- floor_date(df$DateTime, unit="minute")

  # update the data frame in the list
  pt_list[[i]] <- df
}

# round time to the nearest 15 min
# some files recorded every 5 minutes others every 10 but baro files are every 15?
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # round time
  df$DateTime <- round_date(df$DateTime, unit="15 mins")
  # remove duplicates
  df <- df[!duplicated(df$DateTime), ]
  
  # update the data frame in the list
  pt_list[[i]] <- df
}

## make a column for the new rounded time
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # time only
  df <- df %>%
    mutate(TimeOnly = format(DateTime, "%H:%M:%S"))
  
  # update the data frame in the list
  pt_list[[i]] <- df
}

# omit NAs
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # is na
  df <- df[!is.na(df$DateTime),]
  # update the data frame in the list
  pt_list[[i]] <- df
}

DVNWT3 <- pt_list[["2192631_2024_11_19_DVWT3_Levelogger.csv"]]

####################################
#### Combine data for each site ####
####################################
# site names
site_names <- c("DVWT3","DVWT1","DVSB2","DVSB1","DVNWT5","DVNWT4","DVNWT3","DVMS5","DVMS1","DVET")

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

DVWT3 <- combined_by_site[["DVWT3"]]

Date1 <- as.Date("2025-04-01", "%Y-%m-%d")
Date2 <- as.Date("2025-04-30", "%Y-%m-%d")
subdf <- DVWT3[DVWT3$DateTime < Date2 & DVWT3$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,LEVEL)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-18 14:00:00"), linetype="dashed", color="red")

for (i in seq_along(combined_by_site)) {
  df <- combined_by_site[[i]]
  df$LEVEL.kPa <- df$LEVEL / 0.101972
  combined_by_site[[i]] <- df
}

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
  drive_folder_id <- "12OxuAC-i4_QB08ip6CI4W-OGt2vbg1eP"
  # upload the file to Google Drive
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
})
