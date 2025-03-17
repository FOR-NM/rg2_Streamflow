##==============================================================================
## Project: QuEST
## This script is to calculate PT offset for South Sandy SST13 site
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
# this is the "depth" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/11vn2jsiB7YEsrhjI5_NnOSTA579NMtK4")

# list all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3

#SST13
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="2024-12-16_SST13_PTS_SN2192886.csv"], 
                            path = "googledrive/2024-12-16_SST13_PTS_SN2192886.csv",
                            overwrite = T)
# load file
SST13 <- read.csv("googledrive/2024-12-16_SST13_PTS_SN2192886.csv")

# convert Date column to Date type if not already
SST13$Date <- as.Date(SST13$Date)
SST13$DateTime <- as.POSIXct(SST13$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "MST")
head(SST13)

# filter out rows with missing stage or discharge
rating_data <- SST13 %>% 
  filter(!is.na(Baro_Cor_Lvl.m), !is.na(Q..L.s.))

# check the structure of the cleaned data
head(rating_data)

########################################
#### Plot pressure compensated data ####
########################################
# filter out the one row with negative baro lvl value 
SST13 <- SST13 %>% 
  filter(Baro_Cor_Lvl.m >= 0)

ggplot(data = SST13, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() + ggtitle("SST13 compensated level data")

##################################
#### Plot Stage vs. Discharge ####
##################################
# discharge from L/s to m3/s
rating_data <- rating_data %>%
  mutate(Q.m3s = Q..L.s./1000)

ggplot(rating_data, aes(x = Baro_Cor_Lvl.m, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # adds date labels above points
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

# pt depth from cm to m
rating_data <- rating_data %>%
  mutate(pt_depth_m = Depth.above.PT..cm....7/100)

# plot discharge vs manual stage measurement
ggplot(rating_data, aes(x = pt_depth_m, y = Q.m3s)) +
  geom_point(color = "blue") +
  labs(title = "Manual Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # adds date labels above points
  theme_minimal()

#################################################
#### Find offset, when did the change happen ####
#################################################
ggplot(SST13, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-08-13"), linetype="dashed", color="red") +
  labs(title = "Baro_Cor_Lvl.m", x = "Date", y = "Water Level (m)")

###########################################################################
#### Find the average Baro_Cor_Lvl TWO HOURS before and after the move ####
###########################################################################
# define the move time
move_time <- as.POSIXct("2024-08-13 09:20:00")  # Adjust time as needed

# compute mean water level in the two hours before the move
before_move <- SST13 %>%
  filter(DateTime >= (move_time - hours(2)) & DateTime < move_time) %>%
  summarize(mean_before = mean(Baro_Cor_Lvl.m, na.rm = TRUE))

# compute mean water level in the two hours after the move
after_move <- SST13 %>%
  filter(DateTime >= move_time & DateTime < (move_time + hours(2))) %>%
  summarize(mean_after = mean(Baro_Cor_Lvl.m, na.rm = TRUE))

# compute offset
offset2 <- after_move$mean_after - before_move$mean_before
print(offset2)

################################################
#### Apply the Correction for the two hours ####
################################################
SST13 <- SST13 %>%
  mutate(Baro_Cor_offset2 = if_else(DateTime >= "2024-08-13 09:20:00", Baro_Cor_Lvl.m - offset2, Baro_Cor_Lvl.m))

###############################
####  Plot with Correction ####
###############################
ggplot(SST13, aes(x = DateTime, y = Baro_Cor_offset2)) +
  geom_line() +
  #geom_vline(xintercept = as.POSIXct("2024-08-13 09:20:00"), linetype="dashed", color="red") +
  labs(title = "Corrected Baro_Cor Over Time", x = "Date", y = "Water Level (m)")

###################
#### Save file ####
###################
write.csv(SST13, "data/offset_SST13.csv")

drive_folder_id <- "11vn2jsiB7YEsrhjI5_NnOSTA579NMtK4"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/offset_SST13.csv",
  path = as_id(drive_folder_id)
)

