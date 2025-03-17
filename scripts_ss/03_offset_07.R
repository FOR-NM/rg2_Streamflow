##==============================================================================
## Project: QuEST
## This script is to calculate PT offset for South Sandy SST07 site
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

#SST07
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="2024-12-16_SST07_PTS_SN2192880.csv"], 
                            path = "googledrive/2024-12-16_SST07_PTS_SN2192880.csv",
                            overwrite = T)
# load file
SST07 <- read.csv("googledrive/2024-12-16_SST07_PTS_SN2192880.csv")

# remove rows from before deployment
SST07 <- SST07[-c(1:121), ]

#convert Date column to Date type if not already
SST07$DateTime <- paste(SST07$Date.x, SST07$Time.y, sep = " ")
# convert the DateTime column to POSIXct
SST07$DateTime <- as.POSIXct(SST07$DateTime, format = "%Y-%m-%d %I:%M:%S %p")
head(SST07)

# filter out rows with missing stage or discharge
rating_data <- SST07 %>% 
  filter(!is.na(Baro_Cor_Lvl.m), !is.na(Q..L.s.))

# check the structure of the cleaned data
head(rating_data)

########################################
#### Plot pressure compensated data ####
########################################
ggplot(data = SST07, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() + ggtitle("SST07 compensated level data")

##################################
#### Plot Stage vs. Discharge ####
##################################
rating_data$Q..L.s. <- as.numeric(rating_data$Q..L.s.)
# discharge from L/s to m3/s
rating_data <- rating_data %>%
  mutate(Q.m3s = Q..L.s./1000)

# plot with date info
ggplot(rating_data, aes(x = Baro_Cor_Lvl.m, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # adds date labels above points
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m³/s)") +
  theme_minimal()

# pt depth from cm to m
rating_data <- rating_data %>%
  mutate(pt_depth_m = Depth.above.PT..cm....7/100)

# plot discharge vs manual stage measurement
ggplot(rating_data, aes(x = pt_depth_m, y = Q.m3s)) +
  geom_point(color = "blue") +
  labs(title = "Manual Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

#################################################
#### Find offset, when did the change happen ####
#################################################
ggplot(SST07, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-09-16 14:24:26"), linetype="dashed", color="red") +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")

###########################################################################
#### Find the average Baro_Cor_Lvl TWO HOURS before and after the move ####
###########################################################################
# define the move time
move_time <- as.POSIXct("2024-09-16 14:24:26")  # Adjust time as needed

# compute mean water level in the two hours before the move
before_move <- SST07 %>%
  filter(DateTime >= (move_time - hours(10)) & DateTime < move_time) %>%
  summarize(mean_before = mean(Baro_Cor_Lvl.m, na.rm = TRUE))

# compute mean water level in the two hours after the move
after_move <- SST07 %>%
  filter(DateTime >= move_time & DateTime < (move_time + hours(10))) %>%
  summarize(mean_after = mean(Baro_Cor_Lvl.m, na.rm = TRUE))

# compute offset
offset <- after_move$mean_after - before_move$mean_before
print(offset)

#################################################
####  Apply the Correction for the two hours ####
#################################################
SST07 <- SST07 %>%
  mutate(Baro_Cor_offset = if_else(DateTime >= "2024-09-16 14:34:26", Baro_Cor_Lvl.m - offset, Baro_Cor_Lvl.m))

###############################
####  Plot with Correction ####
###############################
ggplot(SST07, aes(x = DateTime, y = Baro_Cor_offset)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-09-16 14:24:26"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2024-12-16 13:20:00"), linetype="dashed", color="red") +
  labs(title = "Corrected Baro_Cor Over Time", x = "Date", y = "Water Level (m)")

ggplot(SST07, aes(x = DateTime)) +
  geom_line(aes(y = Baro_Cor_Lvl.m), color = "blue") + 
  geom_line(aes(y = Baro_Cor_offset), color = "red") +
  geom_vline(xintercept = move_time, linetype = "dashed", color = "black") +
  labs(title = "Comparison of Original and Adjusted Water Level", y = "Water Level (m)")

# discharge from L/s to m3/s in whole data set
SST07$Q..L.s. <- as.numeric(SST07$Q..L.s.)
SST07 <- SST07 %>%
  mutate(Q.m3s = Q..L.s./1000)

# filter out rows with missing stage or discharge
new_rating_data <- SST07 %>% 
  filter(!is.na(Baro_Cor_offset), !is.na(Q.m3s))

# plot rating curve without correction
ggplot(new_rating_data, aes(x = Baro_Cor_Lvl.m, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Stage vs. Discharge (No Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q L/s)") +
  theme_minimal()

# plot rating curve with correction
ggplot(new_rating_data, aes(x = Baro_Cor_offset, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Stage vs. Discharge (First Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q m³/s)") +
  theme_minimal()

###################
#### Save file ####
###################
write.csv(SST07, "data/offset_SST07.csv")

drive_folder_id <- "11vn2jsiB7YEsrhjI5_NnOSTA579NMtK4"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/offset_SST07.csv",
  path = as_id(drive_folder_id)
)
