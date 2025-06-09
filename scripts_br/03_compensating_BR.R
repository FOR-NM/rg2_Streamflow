##==============================================================================
## Project: QuEST
## This script is to merge PT and barometric data for the Brush Creek sites
## press Command+Option+O to collapse all sections and get an overview of the workflow
##==============================================================================

##################
#### Packages ####
##################
library(googledrive)
library(ggplot2)
library(lubridate)
library(dplyr)

####################################
## Clear folders that we will use ##
####################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data", full.names = TRUE)
file.remove(files)

#################################
#### Import & Visualize Data ####
#################################
#### load data from Google drive ####
# this is the "merged_days" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1SbXzLapTIa_dt02JVba4PcsbQaJeFZtD")

# list all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3

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

BRMQ1 <- pt_list[["BRMQ1.csv"]]

#####################
#### Plot curves ####
#####################
# visualize
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # plot
  p <- ggplot(data = df, aes(x = DateTime, y = LEVEL)) + 
    geom_line() + ggtitle(paste(pt_csvs$name[i])) 
  # display the plot in the plot panel
  print(p)
  
  # save the plot as a PNG file
  ggsave(paste0("pt_figs/", pt_csvs$name[i], "raw.png"), plot = p)
}

#######################################
#### Load the baro data separately ####
#######################################
# for now, we are using Fayetteville weather service's data for compensation
# from this page: https://meteostat.net/en/place/us/fayetteville-ar?s=KFYV0&t=2025-05-21/2025-05-28
# I downloaded and merged station pressure in a separate script. 
# See 01_pressure_Fayetteville_API.R and 02_merging_timestamps_pressure_Fayetteville.R

# this is the Fayetteville folder
Fayetteville <- googledrive::as_id("https://drive.google.com/drive/folders/1FgFNGzv0Rh5t62V8SRFdwK6sd_ktwcRD")
# list all CSV files in the folder
ssq <- googledrive::drive_ls(path = Fayetteville)

# choose the specific file by name
fy_pressure <- ssq %>% filter(name == "fayetteville_pressure.csv")
# download it
drive_download(as_id(fy_pressure$id), path = "data/pressure_fv.csv", overwrite = TRUE)
# fetch the file
pressure <- read.csv("data/pressure_fv.csv")

#### convert the DateTime column to POSIXct ####
pressure$DateTime <- as.POSIXct(pressure$time, format = "%Y-%m-%d %H:%M:%S")

######################################
#### Merging PT with Air pressure ####
######################################
# create a list to store merged results
merged_list <- list()

# loop through each site file in pt_list
for (i in names(pt_list)) {
  # merge each site data with air_data on the DateTime column
  merged_list[[i]] <- merge(pt_list[[i]], pressure, by = "DateTime", all.x = TRUE)
}

BRMQ1 <- merged_list[["BRMQ1.csv"]]

##################################
#### Format some column names ####
##################################
for (i in seq_along(merged_list)) {
  # access the current data frame
  df <- merged_list[[i]]
  
  # rename columns
  df <- df %>%
    dplyr::rename(LEVEL.m = LEVEL,
                  TEMPERATURE.C = TEMPERATURE)
  merged_list[[i]] <-  df
}

########################################
#### Manual Barometric Compensation ####
########################################
## the compensation is done by subtracting the barometric column from the Levelogger data 
# to get the true net water level recorded by the Levelogger, make sure the units for each are the same (in m)
# create empty compensated list 
compensated_list <- list()

for (i in names(merged_list)) {
  # access the current data frame
  df <- merged_list[[i]]
  # compensate
  df <- df %>%
    mutate(Baro_Cor_Lvl.m = (.[["LEVEL.m"]] - .[["pres_m"]]))
  
  compensated_list[[i]] <-  df
}

BRMQ1 <- compensated_list[["BRMQ1.csv"]]

#########################################
#### Save compensated files to Drive ####
#########################################
# loop through each data frame in the list
for (i in seq_along(compensated_list)) {
  # access the current data frame
  df <- compensated_list[[i]]
  
  # save new data frame
  write.csv(df, paste0("data/", names(compensated_list)[i]), row.names=FALSE, quote=FALSE)
  
  # define the local folder path and the target folder ID in Google Drive
  file <- paste0("data/", names(compensated_list)[i])
  # this is the "compensated" folder
  drive_folder_id <- "1Ix6n94e2pkKTyojz4SDQKe5MV7WbkH0C"
  
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}

