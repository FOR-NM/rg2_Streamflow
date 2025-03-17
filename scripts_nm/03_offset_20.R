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
# this is the "depth" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1EswIfUWCK6bsdcs-ZrAMGW1oYKs4B0Eh")

# List all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3

#USF20
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="USF20.csv"], 
                            path = "googledrive/USF20.csv",
                            overwrite = T)
# load file
USF20 <- read.csv("googledrive/USF20.csv")

# combine Date and Time columns into a new DateTime column
USF20$DateTime <- paste(USF20$Date.x, USF20$Time.x, sep = " ")

# convert the DateTime column to POSIXct
USF20$DateTime <- as.POSIXct(USF20$DateTime, format = "%Y-%m-%d %I:%M:%S %p")

# filter out rows with missing stage or discharge
rating_data <- USF20 %>% 
  filter(!is.na(Baro_Cor_Lvl), !is.na(Q))

# check the structure of the cleaned data
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
  geom_vline(xintercept = as.POSIXct("2024-07-30 15:15:00"), linetype="dashed", color="red") +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")

# #################################################################
# #### Find the average Baro_Cor_Lvl before and after the move ####
# #################################################################
# ## Here we calculate the offset!!
# # Compute mean water level before and after the move
# before_move <- USF20 %>%
#   filter(DateTime < "2024-07-30") %>%
#   summarize(mean_before = mean(Baro_Cor_Lvl, na.rm = TRUE))
# 
# after_move <- USF20 %>%
#   filter(DateTime >= "2024-07-31") %>%
#   summarize(mean_after = mean(Baro_Cor_Lvl, na.rm = TRUE))
# 
# # Compute offset
# offset <- after_move$mean_after - before_move$mean_before
# print(offset)
# 
# ###############################
# ####  Apply the Correction ####
# ###############################
# 
# USF20 <- USF20 %>%
#   mutate(Baro_Cor_offset = if_else(DateTime >= "2024-07-30 15:15:00", Baro_Cor_Lvl - offset, Baro_Cor_Lvl))
# 
# ###############################
# ####  Plot with Correction ####
# ###############################
# 
# ggplot(USF20, aes(x = DateTime, y = Baro_Cor_offset)) +
#   geom_line() +
#   #geom_vline(xintercept = as.POSIXct("2024-07-30"), linetype="dashed", color="red") +
#   labs(title = "Corrected Baro_Cor Over Time", x = "Date", y = "Water Level (m)")

###########################################################################
#### Find the average Baro_Cor_Lvl TWO HOURS before and after the move ####
###########################################################################
# define the move time
move_time <- as.POSIXct("2024-07-30 15:30:00")  # Adjust time as needed

# compute mean water level in the two hours before the move
before_move <- USF20 %>%
  filter(DateTime >= (move_time - hours(2)) & DateTime < move_time) %>%
  summarize(mean_before = mean(Baro_Cor_Lvl, na.rm = TRUE))

# compute mean water level in the two hours after the move
after_move <- USF20 %>%
  filter(DateTime >= move_time & DateTime < (move_time + hours(2))) %>%
  summarize(mean_after = mean(Baro_Cor_Lvl, na.rm = TRUE))

# compute offset
offset2 <- after_move$mean_after - before_move$mean_before
print(offset2)

#################################################
####  Apply the Correction for the two hours ####
#################################################
USF20_offset <- USF20 %>%
  mutate(Baro_Cor_offset2 = if_else(DateTime >= "2024-07-30 15:30:00", Baro_Cor_Lvl - offset2, Baro_Cor_Lvl))

###############################
####  Plot with Correction ####
###############################
ggplot(USF20_offset, aes(x = DateTime, y = Baro_Cor_offset2)) +
  geom_line() +
  #geom_vline(xintercept = as.POSIXct("2024-07-30 15:30:00"), linetype="dashed", color="red") +
  labs(title = "Corrected Baro_Cor Over Time", x = "Date", y = "Water Level (m)")

###################################################
#### Plot Stage vs. Discharge after correction ####
###################################################
# filter out rows with missing stage or discharge
rating_data_offset <- USF20_offset %>% 
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

write.csv(USF20_offset, "data/offset_USF20.csv")

drive_folder_id <- "1EswIfUWCK6bsdcs-ZrAMGW1oYKs4B0Eh"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/offset_USF20.csv",
  path = as_id(drive_folder_id)
)

