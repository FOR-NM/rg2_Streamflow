##==============================================================================
## Project: QuEST
## Script to merge dilution gauging and velocity method discharge calculations into one for Brush Creek watershed
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

###############################
#### Import discharge data ####
###############################
### get salt slug discharge ###
# this is the "most recent Q" folder for salt slugs
saltslugQ <- googledrive::as_id("https://drive.google.com/drive/folders/1fkh_o6Dj0INLiQrTM-ti1P8YG-yBnzvX")
# list all CSV files in the folder
ssq <- googledrive::drive_ls(path = saltslugQ)
3

# choose the specific file by name
ssq <- ssq %>% filter(name == "Q_BR.csv")

# download the most recent CSV file
drive_download(as_id(ssq$id), path = "data/Q_BR.csv", overwrite = TRUE)

# fetch the file
SSQ <- read.csv("data/Q_BR.csv")

### get velocity discharge ####
velocityQ <- drive_get("https://docs.google.com/spreadsheets/d/1a09kFxDMyko_1BcsWNKd7H3-0jQKPEmvAKzAC4P9leg/edit?gid=0#gid=0")
3

# download the file as a xlsx file
drive_download(as_id(velocityQ$id), path = "googledrive/BR_FLO.xlsx", type = "xlsx", overwrite = T)
# fetch the file
VQ <- readxl::read_xlsx("googledrive/BR_FLO.xlsx")

### remove some columns ###
SSQ <- SSQ[,-c(1, 9:12, 14:29)]

### rename some columns ###
# rename column to match all other data
SSQ <- SSQ %>% 
  rename("Q_L_per_s" = "Q",
         "Time" = "Time24h.x",
         "flag" = "flag.x",)

VQ <- VQ %>% 
  rename( "DataID" = "Site")

#### remove badly flagged curves ####
SSQ <- SSQ %>%
  filter(flag != "BD")

#################################
#### Format DateTime columns ####
#################################
# for VQ
# extract Time (HH:MM:SS) as a string
VQ$Time <- format(VQ$Time, "%H:%M:%S")
# copy and paste date and time into DateTime column
VQ$DateTime <- paste(VQ$Date, VQ$Time, sep = " ")
# convert the DateTime column to POSIXct
VQ$DateTime <- as.POSIXct(VQ$DateTime, format = "%Y-%m-%d %H:%M:%S")

# convert the injection DateTime column to POSIXct
SSQ$Date <- as.Date(SSQ$Date, format = "%Y-%m-%d")

# convert the DateTime column to POSIXct
SSQ$DateTime <- as.POSIXct(SSQ$DateTime, format = "%Y-%m-%d %H:%M:%S")

######################
#### Combine data ####
######################
discharge <- bind_rows(SSQ, VQ)

#############################
#### Save combined files ####
#############################
write.csv(discharge, "data/discharge_BR_full.csv")

drive_folder_id <- "1E3DklQjXkkJGxTyZJTVDkPGABJIQOiTh"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/discharge_BR_full.csv",
  path = as_id(drive_folder_id)
)
