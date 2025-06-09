##==============================================================================
## Project: QuEST
## Script to merge pressure files in one (using timestamp) for Fayetteville station
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

##############################
#### Import pressure data ####
##############################
#### list and download all files in the folder ####
# this is the "pressure_Fayetteville" folder
fayetteville <- googledrive::as_id("https://drive.google.com/drive/folders/1FgFNGzv0Rh5t62V8SRFdwK6sd_ktwcRD")
# list all CSV files in the folder
p_files <- googledrive::drive_ls(path = fayetteville)
3

# create an empty list to store the cleaned data frames
p_list <- lapply(seq_along(p_files$name), function(i) {
  googledrive::drive_download(
    file = p_files$id[i],
    path = paste0("googledrive/", p_files$name[i]),
    overwrite = TRUE
  )
  
  # read the CSV file, skipping the first 11 rows (header is on row 12)
  read.csv(paste0("googledrive/", p_files$name[i]), header = TRUE)
})

# assign names to the list elements based on the file names
names(p_list) <- p_files$name

############################
#### Format date column ####
############################
# loop through each data frame in the list
for (i in seq_along(p_list)) {
  # Access the current data frame
  df <- p_list[[i]]
  
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$time, format = "%Y-%m-%d %H:%M:%S")
  # update the data frame in the list
  p_list[[i]] <- df
}

######################
#### Combine data ####
######################
pressure <- bind_rows(p_list) %>%
  arrange(DateTime) %>%  # 
  distinct(DateTime, .keep_all = TRUE) # remove duplicates

#####################################
#### Change all units to meters  ####
#####################################
## air pressure in hPa (hectopa) to m
pressure <- pressure %>%
  mutate(pres_kPa = (pres / 10))

pressure <- pressure %>%
  mutate(pres_m = (pres_kPa * 0.101972))

# 1 hPA = 100 pascals
# 1 kPa = 0.101972 m in common barometric units to water column equivalent conversions

##############################
#### Save combined files  ####
##############################
# write files to local data folder
write.csv(pressure, 'fayetteville/fayetteville_pressure.csv')

# this is the Fayetteville folder
drive_folder_id <- "1FgFNGzv0Rh5t62V8SRFdwK6sd_ktwcRD"

# upload file to the specified Google Drive folder
drive_put(
  media = "fayetteville/fayetteville_pressure.csv",
  path = as_id(drive_folder_id)
)

