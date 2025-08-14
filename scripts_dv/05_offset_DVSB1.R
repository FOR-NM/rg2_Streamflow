##==============================================================================
## Project: QuEST
## This script is to calculate PT offset for Dog Valley DVSB1 site
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

#DVSB1
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="DVSB1.csv"], 
                            path = "googledrive/DVSB1.csv",
                            overwrite = T)
# load file
DVSB1 <- read.csv("googledrive/DVSB1.csv")

# convert Date column to Date type if not already
# combine Date and Time columns into a new DateTime column
DVSB1$DateTime <- paste(DVSB1$Date.air, DVSB1$Time.air, sep = " ")
# convert the DateTime column to POSIXct
DVSB1$DateTime <- as.POSIXct(DVSB1$DateTime, format = "%Y-%m-%d %H:%M:%S")

# filter out rows with missing stage or discharge
rating_data <- DVSB1 %>% 
  filter(!is.na(Baro_Cor_Lvl), !is.na(Q))

DVSB1$Q..L.s. <- as.numeric(DVSB1$Q)
rating_data$Q..L.s. <- as.numeric(rating_data$Q)

########################################
#### Plot pressure compensated data ####
########################################
ggplot(data = DVSB1, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() + ggtitle("DVSB1 compensated level data")

ggplot(data = DVSB1, aes(x = DateTime, y = LEVEL.m)) +
  geom_line() + ggtitle("DVSB1 level data")

##################################
#### Plot Stage vs. Discharge ####
##################################
# discharge from L/s to m3/s
rating_data <- rating_data %>%
  mutate(Q.m3s = Q..L.s./1000)

# plot with date info
ggplot(rating_data, aes(x = Baro_Cor_Lvl, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Baro-corrected level vs. Discharge", x = "Baro-corrected level (m)", y = "Discharge (Q m³/s)") +
  theme_minimal()

########################################
#### Plot Water Level vs. Discharge ####
########################################
# discharge from L/s to m3/s
rating_data <- rating_data %>%
  mutate(Q.m3s = Q..L.s./1000)

# plot with date info
ggplot(rating_data, aes(x = Baro_Cor_Lvl, y = actual_depth_m)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Baro-corrected level vs. Actual water depth at sensor", x = "Baro-corrected level (m)", y = "Water depth  (m)") +
  theme_minimal()

# plot with date info
ggplot(rating_data, aes(x = actual_depth_m, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Baro-corrected level vs. Actual water depth at sensor", x = "Baro-corrected level (m)", y = "Water depth  (m)") +
  theme_minimal()

#################################################
#### Find offset, when did the change happen ####
################################################# 
ggplot(DVSB1, aes(x = DateTime, y = LEVEL.m)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-07"), linetype="dashed", color="red") + # Pressure Transducer was moved/lifted up by 17 cm as the sensor was too close to the sediment. 
  geom_vline(xintercept = as.POSIXct("2025-04-18"), linetype="dashed", color="red") + # Sensor almost out of water, may or may not be above water, moved sensor 9.5 cm down
  labs(title = "Raw LEVEL", x = "Date", y = "Water Level (m)")

ggplot(DVSB1, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-07"), linetype="dashed", color="red") + # Pressure Transducer was moved/lifted up by 17 cm as the sensor was too close to the sediment. 
  geom_vline(xintercept = as.POSIXct("2025-04-18"), linetype="dashed", color="red") + # Sensor almost out of water, may or may not be above water, moved sensor 9.5 cm down
  labs(title = "Baro compensated level", x = "Date", y = "Level (m)")

############################
#### Look at it closely ####
############################
#take the subset of the data for when PT was moved
Date1 <- as.Date("2025-02-07", "%Y-%m-%d")
Date2 <- as.Date("2025-02-08", "%Y-%m-%d")
subdf <- DVSB1[DVSB1$DateTime < Date2 & DVSB1$DateTime > Date1,]
# sheet says we got to the site at 12:00:00
ggplot(data=subdf, aes(DateTime,LEVEL.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-07 12:15:00"), linetype="dashed", color="red") 
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-07 11:45:00"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2025-02-07 16:45:00"), linetype="dashed", color="red") 

Date1 <- as.Date("2025-02-01", "%Y-%m-%d")
Date2 <- as.Date("2025-02-28", "%Y-%m-%d")
subdf <- DVSB1[DVSB1$DateTime < Date2 & DVSB1$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Level_air.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-07 11:45:00"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2025-02-07 16:45:00"), linetype="dashed", color="red") 

Date1 <- as.Date("2025-04-01", "%Y-%m-%d")
Date2 <- as.Date("2025-04-30", "%Y-%m-%d")
subdf <- DVSB1[DVSB1$DateTime < Date2 & DVSB1$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,LEVEL.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-18 14:00:00"), linetype="dashed", color="red")

Date1 <- as.Date("2025-05-15", "%Y-%m-%d")
Date2 <- as.Date("2025-05-17", "%Y-%m-%d")
subdf <- DVSB1[DVSB1$DateTime < Date2 & DVSB1$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,LEVEL.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-05-16 08:30:00"), linetype="dashed", color="red")

Date1 <- as.Date("2025-05-18", "%Y-%m-%d")
Date2 <- as.Date("2025-05-20", "%Y-%m-%d")
subdf <- DVSB1[DVSB1$DateTime < Date2 & DVSB1$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,LEVEL.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-05-19 09:00:00"), linetype="dashed", color="red")

######################################################################
#### Remove times where PT was out of the water and error section ####
######################################################################
# now NA the time when the PT was out of water 
time1 <- as.POSIXct("2025-02-07 11:45:00")
time2 <- as.POSIXct("2025-02-07 12:00:00")
time3 <- as.POSIXct("2025-02-07 12:00:00")
time4 <- as.POSIXct("2025-05-21 00:00:00")

DVSB1 <- DVSB1 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time1, NA, Baro_Cor_Lvl))
DVSB1 <- DVSB1 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time2, NA, Baro_Cor_Lvl))
DVSB1 <- DVSB1 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time3, NA, Baro_Cor_Lvl))

# plot after cleaning
ggplot(DVSB1, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-07"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2024-04-18"), linetype="dashed", color="red") +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")

###########################################################################
#### Find the average Baro_Cor_Lvl TWO HOURS before and after the move ####
###########################################################################
# first move correction (2025-02-07 12:15:00)
move_time1 <- as.POSIXct("2025-02-07 12:15:00")

before_move1 <- DVSB1 %>%
  filter(DateTime >= (move_time1 - hours(2)) & DateTime < move_time1) %>%
  summarize(mean_before1 = mean(Baro_Cor_Lvl, na.rm = TRUE))

after_move1 <- DVSB1 %>%
  filter(DateTime >= move_time1 & DateTime < (move_time1 + hours(2))) %>%
  summarize(mean_after1 = mean(Baro_Cor_Lvl, na.rm = TRUE))

offset1 <-  after_move1$mean_after1 - before_move1$mean_before1
print(paste("Offset 1:", offset1))

# apply the first correction
DVSB1 <- DVSB1 %>%
  mutate(Baro_Cor_offset1 = if_else(DateTime >= move_time1, Baro_Cor_Lvl - offset1, Baro_Cor_Lvl))

# second move correction (2025-04-18 14:00:00)
move_time2 <- as.POSIXct("2025-04-18 14:00:00")

before_move2 <- DVSB1 %>%
  filter(DateTime >= (move_time2 - hours(2)) & DateTime < move_time2) %>%
  summarize(mean_before2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1

after_move2 <- DVSB1 %>%
  filter(DateTime >= move_time2 & DateTime < (move_time2 + hours(2))) %>%
  summarize(mean_after2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1

offset2 <- after_move2$mean_after2 - before_move2$mean_before2
print(paste("Offset 2:", offset2))

# apply the second correction
DVSB1 <- DVSB1 %>%
  mutate(Baro_Cor_offset2 = if_else(DateTime >= move_time2, Baro_Cor_offset1 - offset2, Baro_Cor_offset1))

# third move correction (2025-05-23 10:15:00)
move_time3 <- as.POSIXct("2025-02-07 16:45:00")

before_move3 <- DVSB1 %>%
  filter(DateTime >= (move_time3 - hours(2)) & DateTime < move_time3) %>%
  summarize(mean_before3 = mean(Baro_Cor_offset2, na.rm = TRUE)) # Use Baro_Cor_offset1

after_move3 <- DVSB1 %>%
  filter(DateTime >= move_time3 & DateTime < (move_time3 + hours(2))) %>%
  summarize(mean_after3 = mean(Baro_Cor_offset2, na.rm = TRUE)) # Use Baro_Cor_offset1

offset3 <- after_move3$mean_after3 - before_move3$mean_before3
print(paste("Offset 3:", offset3))

# apply the second correction
DVSB1 <- DVSB1 %>%
  mutate(Baro_Cor_offset3 = if_else(DateTime >= move_time3, Baro_Cor_offset2 - offset3, Baro_Cor_offset2))

##############################
#### Plot with Correction ####
##############################
ggplot(DVSB1, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (No Correction)", x = "Date", y = "Water Level (m)")

ggplot(DVSB1, aes(x = DateTime, y = Baro_Cor_offset1)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (First Correction)", x = "Date", y = "Water Level (m)")

ggplot(DVSB1, aes(x = DateTime, y = Baro_Cor_offset2)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (Second Correction)", x = "Date", y = "Water Level (m)")

# discharge from L/s to m3/s in whole data set
DVSB1$Q..L.s. <- as.numeric(DVSB1$Q..L.s.)
DVSB1 <- DVSB1 %>%
  mutate(Q.m3s = Q..L.s./1000)

# filter out rows with missing stage or discharge
new_rating_data <- DVSB1 %>% 
  filter(!is.na(Baro_Cor_offset1), !is.na(Q.m3s))

ggplot(new_rating_data, aes(x = Baro_Cor_Lvl, y = Q.m3s)) +
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
ggplot(new_rating_data, aes(x = Baro_Cor_offset3, y = actual_depth_m)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Stage vs. Discharge (Third Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

########################
#### Plot close ups ####
########################
DVSB1_feb <- DVSB1%>%
  filter(month(DateTime) == 2)

# plot after cleaning 
ggplot(DVSB1_feb, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")
ggplot(DVSB1_feb, aes(x = DateTime, y = Baro_Cor_offset1)) +
  geom_line() +
  labs(title = "Baro_Cor_offset1", x = "Date", y = "Water Level (m)")

#take the subset of the data for February when PT was moved
Date1 <- as.Date("2025-02-07", "%Y-%m-%d")
Date2 <- as.Date("2025-02-08", "%Y-%m-%d")
subdf <- DVSB1[DVSB1$DateTime < Date2 & DVSB1$DateTime > Date1,]

ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-07 12:00:00"), linetype="dashed", color="red") 
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset1)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-07 11:45:00"), linetype="dashed", color="red") 

#take the subset of the data for April when PT was moved
Date1 <- as.Date("2025-04-17", "%Y-%m-%d")
Date2 <- as.Date("2025-04-19", "%Y-%m-%d")
subdf <- DVSB1[DVSB1$DateTime < Date2 & DVSB1$DateTime > Date1,]

ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-18 14:45:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset2)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-18 14:45:00"), linetype="dashed", color="red")

#take the subset of the data for May when PT was moved
Date1 <- as.Date("2025-02-07", "%Y-%m-%d")
Date2 <- as.Date("2025-02-09", "%Y-%m-%d")
subdf <- DVSB1[DVSB1$DateTime < Date2 & DVSB1$DateTime > Date1,]

ggplot(data=subdf, aes(DateTime,Baro_Cor_offset2)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-07 10:15:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset3)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-07 10:15:00"), linetype="dashed", color="red")

###################
#### Save file ####
###################
write.csv(DVSB1, "data/offset_DVSB1.csv")

# this is the "offset" folder
drive_folder_id <- "136WGq6adaNROjaJN2YL63yJExKikM81A"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/offset_DVSB1.csv",
  path = as_id(drive_folder_id)
)
