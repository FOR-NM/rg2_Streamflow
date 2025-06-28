##==============================================================================
## Project: QuEST
## This script is to calculate PT offsed for Brush Creek BRM01 site
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
# List and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data", full.names = TRUE)
file.remove(files)

#################################
#### Import & Visualize Data ####
#################################
#### Load data from Google drive ####
# this is the "depth" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1n17b_9yf5DCO_h6uPya5vBPz2dh13L3v")

# list all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3

#BRM01
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="BRM01.csv"], 
                            path = "googledrive/BRM01.csv",
                            overwrite = T)
# load file
BRM01 <- read.csv("googledrive/BRM01.csv")

# combine Date and Time columns into a new DateTime column
BRM01$DateTime <- paste(BRM01$Date.x, BRM01$Time.x, sep = " ")

# convert the DateTime column to POSIXct
BRM01$DateTime <- as.POSIXct(BRM01$DateTime, format = "%Y-%m-%d %H:%M:%S")

# filter out rows with missing stage or discharge
rating_data <- BRM01 %>% 
  filter(!is.na(Baro_Cor_Lvl.m), !is.na(Q_L_per_s))

# check the structure of the cleaned data
head(rating_data)

########################################
#### Plot pressure compensated data ####
########################################
# filter out rows with missing Baro NAs
BRM01_baro <- BRM01 %>% 
  filter(!is.na(Baro_Cor_adjusted.m))

ggplot(data = BRM01_baro, aes(x = DateTime, y = Baro_Cor_adjusted.m)) +
  geom_line() + ggtitle("BRM01 compensated level data")

ggplot(data = BRM01_baro, aes(x = DateTime, y = LEVEL.m)) +
  geom_line() + ggtitle("BRM01 level data in m")

ggplot(data = BRM01_baro, aes(x = DateTime, y = pres.psi)) +
  geom_line() + ggtitle("BRM01 level data in m")

##################################
#### Plot Stage vs. Discharge ####
##################################
ggplot(rating_data, aes(x = Baro_Cor_Lvl.m, y = Q_L_per_s)) +
  geom_point(color = "blue") +
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL.m)", y = "Discharge (Q)") +
  theme_minimal()

# discharge from L/s to m3/s
rating_data <- rating_data %>%
  mutate(Q.m3s = Q_L_per_s/1000)

ggplot(rating_data, aes(x = Baro_Cor_Lvl.m, y = Q.m3s)) +
  geom_point(color = "blue") +
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

#################################################
#### Find offset, when did the change happen ####
#################################################
ggplot(BRM01, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-11 11:00:00"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2024-11-08 07:21:00"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2024-11-12 11:08:00"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2024-12-12 08:40:00"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2025-01-23 09:08:00"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2025-02-28 07:52:00"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2025-04-17 08:30:00"), linetype="dashed", color="red") +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")

###########################################################################
#### Find the average Baro_Cor_Lvl TWO HOURS before and after the move ####
###########################################################################
# define the move time
move_time1 <- as.POSIXct("2024-07-30 14:57:00")  
move_time2 <- as.POSIXct("2024-07-30 15:20:00")  

# compute mean water level in the two hours before the move
before_move <- BRM01 %>%
  filter(DateTime >= (move_time1 - hours(2)) & DateTime < move_time1) %>%
  summarize(mean_before = mean(Baro_Cor_Lvl, na.rm = TRUE))

# compute mean water level in the two hours after the move
after_move <- BRM01 %>%
  filter(DateTime >= move_time2 & DateTime < (move_time2 + hours(2))) %>%
  summarize(mean_after = mean(Baro_Cor_Lvl, na.rm = TRUE))

# compute offset
offset2 <- after_move$mean_after - before_move$mean_before
print(offset2)

#################################################
####  Apply the Correction for the two hours ####
#################################################
BRM01_offset <- BRM01 %>%
  mutate(Baro_Cor_offset2 = if_else(DateTime >= "2024-07-30 14:57:00", Baro_Cor_Lvl - offset2, Baro_Cor_Lvl))

###############################
####  Plot with Correction ####
###############################
ggplot(BRM01_offset, aes(x = DateTime, y = Baro_Cor_offset2)) +
  geom_line() +
  #geom_vline(xintercept = as.POSIXct("2024-07-30 15:10:00"), linetype="dashed", color="red") +
  labs(title = "Corrected Baro_Cor Over Time", x = "Date", y = "Water Level (m)")

###################################################
#### Plot Stage vs. Discharge after correction ####
###################################################
# filter out rows with missing stage or discharge
rating_data_offset <- BRM01_offset %>% 
  filter(!is.na(Baro_Cor_Lvl), !is.na(Q))

ggplot(rating_data_offset, aes(x = Baro_Cor_offset2, y = Q)) +
  geom_point(color = "blue") +
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL.m)", y = "Discharge (Q)") +
  theme_minimal()

# plot with date info
ggplot(rating_data_offset, aes(x = Baro_Cor_offset2, y = Q)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m³/s)") +
  theme_minimal()

###################
#### Save file ####
###################

write.csv(BRM01_offset, "data/offset_BRM01.csv")

drive_folder_id <- "1EswIfUWCK6bsdcs-ZrAMGW1oYKs4B0Eh"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/offset_BRM01.csv",
  path = as_id(drive_folder_id)
)