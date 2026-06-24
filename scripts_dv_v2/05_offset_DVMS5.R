##==============================================================================
## Project: QuEST
## This script is to calculate PT offset for Dog Valley DVMS5
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
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1uyQmmLawojBw-yN2sjsCbTMDPRbvqyz4")

# list all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3

#DVMS5
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="DVMS5.csv"], 
                            path = "googledrive/DVMS5.csv",
                            overwrite = T)
# load file
DVMS5 <- read.csv("googledrive/DVMS5.csv")

# convert Date column to Date type if not already
# combine Date and Time columns into a new DateTime column
DVMS5$DateTime <- paste(DVMS5$Date.x, DVMS5$TimeOnly, sep = " ")
# convert the DateTime column to POSIXct
DVMS5$DateTime <- as.POSIXct(DVMS5$DateTime, format = "%Y-%m-%d %H:%M:%S")

# filter out rows with missing stage or discharge
rating_data <- DVMS5 %>% 
  filter(!is.na(Baro_Cor_Lvl.m), !is.na(Q))

DVMS5$Q..L.s. <- as.numeric(DVMS5$Q)
rating_data$Q..L.s. <- as.numeric(rating_data$Q)

# level data
level_data <- DVMS5 %>% 
  filter(!is.na(Baro_Cor_Lvl.m), !is.na(Actual_Water_Depth_m))

########################################
#### Plot pressure compensated data ####
########################################
ggplot(data = DVMS5, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() + ggtitle("DVMS5 compensated level data")

ggplot(data = DVMS5, aes(x = DateTime, y = LEVEL.m)) +
  geom_line() + ggtitle("DVMS5 level data")

##################################
#### Plot Stage vs. Discharge ####
##################################
# discharge from L/s to m3/s
rating_data <- rating_data %>%
  mutate(Q.m3s = Q..L.s./1000)

# plot with date info
ggplot(rating_data, aes(x = Baro_Cor_Lvl.m, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Baro-corrected level vs. Discharge", x = "Baro-corrected level (m)", y = "Discharge (Q m³/s)") +
  theme_minimal()

########################################
#### Plot Water Level vs. Discharge ####
########################################
# discharge from L/s to m3/s
level_data <- level_data %>%
  mutate(Q.m3s = Q/1000)

ggplot(level_data, aes(x = Actual_Water_Depth_m, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Actual water depth at sensor vs Discharge", x = "Water depth  (m)", y = "Discharge (Q m³/s)") +
  theme_minimal()

ggplot(level_data, aes(x = Baro_Cor_Lvl.m, y = Actual_Water_Depth_m)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Baro-corrected level vs. Actual water depth at sensor", x = "Baro-corrected level (m)", y = "Water depth  (m)") +
  theme_minimal()

#################################################
#### Find offset, when did the change happen ####
#################################################
ggplot(DVMS5, aes(x = DateTime, y = LEVEL.m)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-18"), linetype="dashed", color="red") + # Pressure Transducer was moved/lifted up by 17 cm as the sensor was too close to the sediment. 
  geom_vline(xintercept = as.POSIXct("2025-05-16"), linetype="dashed", color="red") + # Downloaded HOBO data
  geom_vline(xintercept = as.POSIXct("2025-05-19"), linetype="dashed", color="red") + # Downloaded HOBO data
  labs(title = "LEVEL", x = "Date", y = "Water Level (m)")

ggplot(DVMS5, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-18"), linetype="dashed", color="red") + # Pressure Transducer was moved/lifted up by 17 cm as the sensor was too close to the sediment. 
  geom_vline(xintercept = as.POSIXct("2025-05-16"), linetype="dashed", color="red") + # Downloaded HOBO data
  geom_vline(xintercept = as.POSIXct("2025-05-19"), linetype="dashed", color="red") + # Downloaded HOBO data
  labs(title = "LEVEL", x = "Date", y = "Water Level (m)")

############################
#### Look at it closely ####
############################
#take the subset of the data for when PT was moved
Date1 <- as.Date("2024-10-17", "%Y-%m-%d")
Date2 <- as.Date("2024-10-20", "%Y-%m-%d")
subdf <- DVMS5[DVMS5$DateTime < Date2 & DVMS5$DateTime > Date1,]

# sheet says we got to the site 10/18/2024	9:05:00 AM
# Level logger maintenance
ggplot(data=subdf, aes(DateTime,LEVEL.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-18 09:15:00"), linetype="dashed", color="red") 
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-18 09:15:00"), linetype="dashed", color="red") 

# sheet says we got to the site 11/19/2024 but no data there
Date1 <- as.Date("2024-11-13", "%Y-%m-%d")
Date2 <- as.Date("2024-11-24", "%Y-%m-%d")
subdf <- DVMS5[DVMS5$DateTime < Date2 & DVMS5$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-11-17 00:00:00"), linetype="dashed", color="red")

# sheet says we got to the site 04/11/2025 1:46:00 PM
Date1 <- as.Date("2025-04-10", "%Y-%m-%d")
Date2 <- as.Date("2025-04-13", "%Y-%m-%d")
subdf <- DVMS5[DVMS5$DateTime < Date2 & DVMS5$DateTime > Date1,]
# Levelogger has been shifted two holes up. 2 holes = 15 cm
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-11 13:45:00"), linetype="dashed", color="red")

# Level logger moved down 9.4 cm
Date1 <- as.Date("2025-06-03", "%Y-%m-%d")
Date2 <- as.Date("2025-06-04", "%Y-%m-%d")
subdf <- DVMS5[DVMS5$DateTime < Date2 & DVMS5$DateTime > Date1,]
# sheet says we got to the site at 11:05
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-03 10:30:00"), linetype="dashed", color="red")

# Something is going on around Aug 2024 but no notes about it
Date1 <- as.Date("2024-08-03", "%Y-%m-%d")
Date2 <- as.Date("2024-08-10", "%Y-%m-%d")
subdf <- DVMS5[DVMS5$DateTime < Date2 & DVMS5$DateTime > Date1,]
# sheet says we got to the site at 11:05
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-08-06 10:30:00"), linetype="dashed", color="red")

######################################################################
#### Remove times where PT was out of the water and error section ####
######################################################################
# now NA the time when the PT was out of water 
time1 <- as.POSIXct("2025-04-11 13:45:00")
time2 <- as.POSIXct("2024-10-18 09:15:00")
time3 <- as.POSIXct("2025-04-11 13:30:00")
time4 <- as.POSIXct("2024-08-06 10:30:00")
time5 <- as.POSIXct("2025-06-03 10:15:00")

DVMS5 <- DVMS5 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time1, NA, Baro_Cor_Lvl.m))
DVMS5 <- DVMS5 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time2, NA, Baro_Cor_Lvl.m))
DVMS5 <- DVMS5 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time3, NA, Baro_Cor_Lvl.m))
DVMS5 <- DVMS5 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time4, NA, Baro_Cor_Lvl.m))
DVMS5 <- DVMS5 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time5, NA, Baro_Cor_Lvl.m))

# plot after cleaning
ggplot(DVMS5, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() +
  labs(title = "Baro_Cor_Lvl.m", x = "Date", y = "Water Level (m)")

###########################################################################
#### Find the average Baro_Cor_Lvl.m TWO HOURS before and after the move ####
###########################################################################
# first move correction (2025-04-11 13:45:00)
move_time1 <- as.POSIXct("2025-04-11 13:45:00")

before_move1 <- DVMS5 %>%
  filter(DateTime >= (move_time1 - hours(2)) & DateTime < move_time1) %>%
  summarize(mean_before1 = mean(Baro_Cor_Lvl.m, na.rm = TRUE))

after_move1 <- DVMS5 %>%
  filter(DateTime >= move_time1 & DateTime < (move_time1 + hours(2))) %>%
  summarize(mean_after1 = mean(Baro_Cor_Lvl.m, na.rm = TRUE))

offset1 <-  after_move1$mean_after1 - before_move1$mean_before1
print(paste("Offset 1:", offset1))

# apply the first correction
DVMS5 <- DVMS5 %>%
  mutate(Baro_Cor_offset1 = if_else(DateTime >= move_time1, Baro_Cor_Lvl.m - offset1, Baro_Cor_Lvl.m))

# second move correction 
move_time2 <- as.POSIXct("2025-06-03 10:15:00")

before_move2 <- DVMS5 %>%
  filter(DateTime >= (move_time2 - hours(2)) & DateTime < move_time2) %>%
  summarize(mean_before2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1

after_move2 <- DVMS5 %>%
  filter(DateTime >= move_time2 & DateTime < (move_time2 + hours(2))) %>%
  summarize(mean_after2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1

offset2 <- after_move2$mean_after2 - before_move2$mean_before2
print(paste("Offset 2:", offset2))

# apply the second correction
DVMS5 <- DVMS5 %>%
  mutate(Baro_Cor_offset2 = if_else(DateTime >= move_time2, Baro_Cor_offset1 - offset2, Baro_Cor_offset1))

# known offset correction (2024-11-19, 2 cm)
move_time5 <- as.POSIXct("2024-11-19 13:06:00")
offset5 <- -0.02  # m offset from field notes

# apply the third correction
DVMS5 <- DVMS5 %>%
  mutate(Baro_Cor_offset3 = if_else(DateTime >= move_time5,
                                    Baro_Cor_offset2 - offset5,
                                    Baro_Cor_offset2))

##############################
#### Plot with Correction ####
##############################
ggplot(DVMS5, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (No Correction)", x = "Date", y = "Water Level (m)")

ggplot(DVMS5, aes(x = DateTime, y = Baro_Cor_offset1)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (First Correction)", x = "Date", y = "Water Level (m)")

ggplot(DVMS5, aes(x = DateTime, y = Baro_Cor_offset2)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (Second Correction)", x = "Date", y = "Water Level (m)")

ggplot(DVMS5, aes(x = DateTime, y = Baro_Cor_offset3)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (third Correction)", x = "Date", y = "Water Level (m)")

# discharge from L/s to m3/s in whole data set
DVMS5$Q..L.s. <- as.numeric(DVMS5$Q..L.s.)
DVMS5 <- DVMS5 %>%
  mutate(Q.m3s = Q..L.s./1000)

# filter out rows with missing stage or discharge
new_rating_data <- DVMS5 %>% 
  filter(!is.na(Baro_Cor_offset1), !is.na(Q.m3s))
new_level_data <- DVMS5 %>% 
  filter(!is.na(Baro_Cor_offset1), !is.na(Actual_Water_Depth_m))

ggplot(new_rating_data, aes(x = Baro_Cor_Lvl.m, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Stage vs. Discharge (No Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

ggplot(new_rating_data, aes(x = Baro_Cor_offset1, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Stage vs. Discharge (First Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

ggplot(new_rating_data, aes(x = Baro_Cor_offset2, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Stage vs. Discharge (Second Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

ggplot(new_rating_data, aes(x = Baro_Cor_offset3, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Stage vs. Discharge (Third Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

####################################
#### Plot level with correction ####
####################################
ggplot(new_level_data, aes(x = Baro_Cor_offset3, y = Actual_Water_Depth_m)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Baro-corrected level vs. Actual water depth at sensor (Third Correction)", x = "Baro-corrected level (m)", y = "Water depth  (m)") +
  theme_minimal()

########################
#### Plot close ups ####
########################
#take the subset of the data for April when PT was moved
Date1 <- as.Date("2025-04-10", "%Y-%m-%d")
Date2 <- as.Date("2025-04-12", "%Y-%m-%d")
subdf <- DVMS5[DVMS5$DateTime < Date2 & DVMS5$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-11 13:15:00"), linetype="dashed", color="red") 
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset1)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-11 13:15:00"), linetype="dashed", color="red") 

#take the subset of the data for October when PT was moved
Date1 <- as.Date("2024-10-17", "%Y-%m-%d")
Date2 <- as.Date("2024-10-19", "%Y-%m-%d")
subdf <- DVMS5[DVMS5$DateTime < Date2 & DVMS5$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-18 09:15:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset2)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-18 09:15:00"), linetype="dashed", color="red")

#take the subset of the data for November when PT was moved
Date1 <- as.Date("2024-11-16", "%Y-%m-%d")
Date2 <- as.Date("2024-11-22", "%Y-%m-%d")
subdf <- DVMS5[DVMS5$DateTime < Date2 & DVMS5$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-05-23 10:15:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset5)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-05-23 10:15:00"), linetype="dashed", color="red")

#take the subset of the data for June when PT was moved
Date1 <- as.Date("2025-06-01", "%Y-%m-%d")
Date2 <- as.Date("2025-06-06", "%Y-%m-%d")
subdf <- DVMS5[DVMS5$DateTime < Date2 & DVMS5$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-05-23 10:15:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset5)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-05-23 10:15:00"), linetype="dashed", color="red")

#take the subset of the data for August when PT was moved
Date1 <- as.Date("2024-08-05", "%Y-%m-%d")
Date2 <- as.Date("2024-08-08", "%Y-%m-%d")
subdf <- DVMS5[DVMS5$DateTime < Date2 & DVMS5$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-05-23 10:15:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset3)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-05-23 10:15:00"), linetype="dashed", color="red")


##########################################################
#### Standardize final corrected column name ####
##########################################################
# alias the fully offset-corrected stage to a consistent name so downstream
# steps (depth conversion, rating curve) don't need to know how many
# site-specific corrections were applied
DVMS5 <- DVMS5 %>%
  mutate(Final_Corrected_Lvl = Baro_Cor_offset3)

###################
#### Save file ####
###################
write.csv(DVMS5, "data/offset_DVMS5.csv")

# this is the "offset" folder
drive_folder_id <- "136WGq6adaNROjaJN2YL63yJExKikM81A"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/offset_DVMS5.csv",
  path = as_id(drive_folder_id)
)

