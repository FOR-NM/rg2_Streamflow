##==============================================================================
## Project: QuEST
## Script to merge pressure files in one (using time stamp) for Fayetteville and Springdale stations
## Also going to correct baro data from sea level to local atmospheric pressure
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
files <- list.files(path = "data", full.names = TRUE)
file.remove(files)

##############################
#### Import pressure data ####
##############################
#### list and download all files in the folder ####
# this is the "pressure_Fayetteville" folder
fayetteville <- googledrive::as_id("https://drive.google.com/drive/folders/1FgFNGzv0Rh5t62V8SRFdwK6sd_ktwcRD")
springdale <- googledrive::as_id("https://drive.google.com/drive/folders/1BX6ZZosa3LxPbyAWSng9xfwjXEFWUfLG")

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

########################################################################
#### Correct baro data from sea level to local atmospheric pressure ####
########################################################################
## BP readings MUST be in mm Hg
## air pressure in hPa (hectopa) to mm Hg
pressure <- pressure %>%
  mutate(pres_mmHg = (pres * 0.75006375541921))
# 1 hPa = 0.75006375541921 mmHg

# True BP = [Corrected BP] – [2.5 * (Local Altitude in ft above sea level/100)]. Note that Inches of Hg x 25.4 = mm Hg].
# Fayetteville's elevation is 436 m = 1430.45 ft ELEVATION HAS TO BE IN FEET

pressure <- pressure %>%
  mutate(TrueBP_mmHg = (pres_mmHg - (2.5 * 1430.45/100)))
 
#####################################
#### Change all units to meters  ####
#####################################
## air pressure is in mmHg now, change it to m
pressure <- pressure %>%
  mutate(pres_m = (TrueBP_mmHg * 0.0136))
#1 mm Hg = 0.0136 meter of head

##############################
#### Save combined files  ####
##############################
# write files to local data folder
write.csv(pressure, 'air_br/fayetteville_pressure.csv')

# this is the Fayetteville folder
drive_folder_id <- "1FgFNGzv0Rh5t62V8SRFdwK6sd_ktwcRD"
# this is the Springdale folder
drive_folder_id <- "1BX6ZZosa3LxPbyAWSng9xfwjXEFWUfLG"

# upload file to the specified Google Drive folder
drive_put(
  media = 'air_br/fayetteville_pressure.csv',
  path = as_id(drive_folder_id)
)

