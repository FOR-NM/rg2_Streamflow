##==============================================================================
## Project: QuEST
## This script is to calculate PT offset for Dog Valley DVET site
## press Command+Option+O to collapse all sections and get an overview of the workflow
##
## NOTE (pipeline v2): this script is still WIP and never reaches a write.csv/
## drive_put save step, so there's no finished "offset_DVET.csv" for a
## depth-conversion step to consume yet. Once this script is finished and
## saves a Final_Corrected_Lvl column to the "offset" Drive folder, add
## 05b_pressure_to_depth_DVET.R following the same pattern used for the
## other DV sites in this folder.
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

#################################
#### Import & Visualize Data ####
#################################
#### load data from Google drive ####
# this is the "depth" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1uyQmmLawojBw-yN2sjsCbTMDPRbvqyz4")

# list all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3

#DVET
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="DVET.csv"], 
                            path = "googledrive/DVET.csv",
                            overwrite = T)
# load file
DVET <- read.csv("googledrive/DVET.csv")

# convert Date column to Date type if not already
# combine Date and Time columns into a new DateTime column
DVET$DateTime <- paste(DVET$Date.x, DVET$TimeOnly, sep = " ")
# convert the DateTime column to POSIXct
DVET$DateTime <- as.POSIXct(DVET$DateTime, format = "%Y-%m-%d %H:%M:%S")

# filter out rows with missing stage or discharge
rating_data <- DVET %>%
  filter(!is.na(Baro_Cor_Lvl.m), !is.na(Q))
# level data
level_data <- DVET %>% 
  filter(!is.na(Baro_Cor_Lvl.m), !is.na(Actual_Water_Depth_m))

DVET$Q..L.s. <- as.numeric(DVET$Q)
rating_data$Q..L.s. <- as.numeric(rating_data$Q)

# make compensated backup 
DVET$Baro_backup <- DVET$Baro_Cor_Lvl.m

########################################
#### Plot pressure compensated data ####
########################################
ggplot(data = DVET, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() + ggtitle("DVET compensated level data")

ggplot(data = DVET, aes(x = DateTime, y = LEVEL.m)) +
  geom_line() + ggtitle("DVET level data")

