##==============================================================================
## Project: QuEST
## This script is to calculate PT offsed for Santa Fe USF20 site
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
# This is the "depth" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1EswIfUWCK6bsdcs-ZrAMGW1oYKs4B0Eh")

# List all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3

#USF20
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="USF20.csv"], 
                            path = "googledrive/USF20.csv",
                            overwrite = T)
# Load file
USF20 <- read.csv("googledrive/USF20.csv")

# Convert Date column to Date type if not already
USF20$Date <- as.Date(USF20$Date)
USF20$DateTime <- as.POSIXct(USF20$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "MST")
head(USF20)

# Filter out rows with missing stage or discharge
rating_data <- USF20 %>% 
  filter(!is.na(Baro_Cor_Lvl), !is.na(Q))

# Check the structure of the cleaned data
head(rating_data)

##################################
#### Plot Stage vs. Discharge ####
##################################

ggplot(rating_data, aes(x = Baro_Cor_Lvl, y = Q)) +
  geom_point(color = "blue") +
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL.m)", y = "Discharge (Q)") +
  theme_minimal()

# discharge from L/s to m3/s
rating_data <- rating_data %>%
  mutate(Q.m3s = Q/1000)

ggplot(rating_data, aes(x = Baro_Cor_Lvl, y = Q.m3s)) +
  geom_point(color = "blue") +
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

# pt depth from cm to m
rating_data <- rating_data %>%
  mutate(pt_depth_m = pt_depth_cm/100)

# Plot discharge vs manual stage measurement
ggplot(rating_data, aes(x = pt_depth_m, y = Q.m3s)) +
  geom_point(color = "blue") +
  labs(title = "Manual Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

#################################################
#### Find offset, when did the change happen ####
#################################################

ggplot(USF20, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  #geom_vline(xintercept = as.POSIXct("2024-07-30"), linetype="dashed", color="red") +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")

#################################################################
#### Find the average Baro_Cor_Lvl before and after the move ####
#################################################################
## Here we calculate the offset!!
# Compute mean water level before and after the move
before_move <- USF20 %>%
  filter(DateTime < "2024-07-30") %>%
  summarize(mean_before = mean(Baro_Cor_Lvl, na.rm = TRUE))

after_move <- USF20 %>%
  filter(DateTime >= "2024-07-31") %>%
  summarize(mean_after = mean(Baro_Cor_Lvl, na.rm = TRUE))

# Compute offset
offset <- after_move$mean_after - before_move$mean_before
print(offset)

###############################
####  Apply the Correction ####
###############################

USF20 <- USF20 %>%
  mutate(Baro_Cor_offset = if_else(DateTime >= "2024-07-30 17:30:00", Baro_Cor_Lvl - offset, Baro_Cor_Lvl))

###############################
####  Plot with Correction ####
###############################

ggplot(USF20, aes(x = DateTime, y = Baro_Cor_offset)) +
  geom_line() +
  #geom_vline(xintercept = as.POSIXct("2024-07-30"), linetype="dashed", color="red") +
  labs(title = "Corrected Baro_Cor Over Time", x = "Date", y = "Water Level (m)")

###################
#### Save file ####
###################

write.csv(USF20, "data/offset_USF20.csv")

drive_folder_id <- "1EswIfUWCK6bsdcs-ZrAMGW1oYKs4B0Eh"

# Upload file to the specified Google Drive folder
drive_put(
  media = "data/offset_USF20.csv",
  path = as_id(drive_folder_id)
)

