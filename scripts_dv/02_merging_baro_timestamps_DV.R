##==============================================================================
## Project: QuEST
## Script to merge same site PT sites in one file (using timestamp) for Santa Fe watershed
##==============================================================================

library(googledrive)
library(dplyr)

########################################
#### Clear folders that we will use ####
########################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

##########################
#### Import baro data ####
##########################
#### list and download all files in the folder ####
# this is the "inuse baro logger" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1_KUXHWDuAbO3Z6EV_RKBZL9IyWeeaj-X")
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

################################
#### Format DateTime column ####
################################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # convert the Date and Time columns to Date and Time
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  
  # combine Date and Time columns into a new DateTime column
  df$DateTime <- paste(df$Date, df$Time, sep = " ")
  
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
  
  # update the data frame in the list
  pt_list[[i]] <- df
}

####################################
#### Combine data for each site ####
####################################
# site names
site_names <- c("DVNWT5","DVO")

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

#### plot ####
ggplot(data = combined_by_site[["DVNWT5"]], aes(x = DateTime, y = LEVEL.m)) + 
  geom_line()
ggplot(data = combined_by_site[["DVO"]], aes(x = DateTime, y = LEVEL.m)) + 
  geom_line()

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
  drive_folder_id <- "1SeGx6MUt6icUFum4Yu-kUHHqZSbN4AQU"
  # upload the file to Google Drive
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
})
