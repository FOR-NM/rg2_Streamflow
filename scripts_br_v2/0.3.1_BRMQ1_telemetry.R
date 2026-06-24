##==============================================================================
## Project: QuEST
##
##==============================================================================

##################
#### Packages ####
##################
library(googledrive)
library(ggplot2)
library(lubridate)
library(dplyr)
library(hms)

####################################
## Clear folders that we will use ##
####################################
# List and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data", full.names = TRUE)
file.remove(files)

###############################
#### Import telemetry Data ####
###############################
#### Load data from Google drive ####
# this is the "telemetry data" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1Ny6cXbjzX2GvZwgnGxiIh-KqkKbR6Q7g")

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

# format datetime
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$Time, format = "%Y-%m-%d %H:%M:%S")
  # update the data frame in the list
  pt_list[[i]] <- df
}

#####################################
#### Combine telemetry only data ####
#####################################
# bind rows of all data frames for the site
combined <- bind_rows(pt_list) %>%
  arrange(DateTime) %>%  # chronological order if 'DateTime' exists
  distinct(DateTime, .keep_all = TRUE) # remove duplicates

combined$DateTime <- combined$Time
combined$DateTime <- as.POSIXct(combined$Time, format = "%Y-%m-%d %H:%M:%S")

########################################
#### Plot pressure compensated data ####
########################################
# remove older data
combined <- combined[-c(1:8319),]

# remove negative outlier looking values 
combined <- combined %>% 
  filter(Level.ft. > 0)
  
ggplot(data = combined, aes(x = DateTime, y = Level.ft.)) +
  geom_vline(xintercept = as.POSIXct("2025-03-14 12:00:00"), linetype="dashed", color="red") +
  geom_line() + ggtitle("BRMQ1 telemetry data")

###################################
#### Import non-telemetry Data ####
###################################
#### Load data from Google drive ####
# this is the "compensated" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1Ix6n94e2pkKTyojz4SDQKe5MV7WbkH0C")

# List all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")

#BRMQ1
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="BRMQ1_no.csv"], 
                            path = "googledrive/BRMQ1_no.csv",
                            overwrite = T)
# load file
BRMQ1 <- read.csv("googledrive/BRMQ1_no.csv")

# combine Date and Time columns into a new DateTime column
BRMQ1$DateTime <- paste(BRMQ1$Date, BRMQ1$Time, sep = " ")
# convert the DateTime column to POSIXct
BRMQ1$DateTime <- as.POSIXct(BRMQ1$DateTime, format = "%Y-%m-%d %H:%M:%S")

BRMQ1_clean <- BRMQ1 %>%
  filter(
    time <= "2025-03-14 08:00:00" |
      time >= "2025-06-04 10:00:00")

#########################
#### Convert ft to m ####
#########################
# Telemetry data is in ft, logger data is in m. transform!
combined <- combined %>%
  mutate(Baro_Cor_Lvl.m = Level.ft. / 3.281)

################################
#### Combine and clean data ####
################################
BRMQ_combined <- bind_rows(BRMQ1_clean, combined)

# convert the DateTime column to POSIXct
BRMQ_combined$DateTime <- as.POSIXct(BRMQ_combined$DateTime, format = "%Y-%m-%d %H:%M:%S")

# fill in Date column 
BRMQ_combined$Date <- as.Date(BRMQ_combined$DateTime, format = "%Y-%m-%d %H:%M:%S")
# Fix Time column 
BRMQ_combined$Time <- as_hms(BRMQ_combined$DateTime)

# remove empty date data
BRMQ_combined <- BRMQ_combined %>%
  filter(!is.na(DateTime))

# remove unnecessary columns
# BRMQ_combined <- BRMQ_combined[,-c(7)]

##############
#### Plot ####
##############
ggplot(data = BRMQ1_clean, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_vline(xintercept = as.POSIXct("2025-03-14 12:00:00"), linetype="dashed", color="red") +
  geom_line() + ggtitle("BRMQ1 logger data")

ggplot(data = BRMQ_combined, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_vline(xintercept = as.POSIXct("2025-03-14 12:00:00"), linetype="dashed", color="red") +
  geom_line() + ggtitle("BRMQ1 full stitched data")

###################
#### Save file ####
###################
write.csv(BRMQ_combined, "data/BRMQ1.csv")

# this is the "compensated" folder 
drive_folder_id <- "1Ix6n94e2pkKTyojz4SDQKe5MV7WbkH0C"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/BRMQ1.csv",
  path = as_id(drive_folder_id)
)

