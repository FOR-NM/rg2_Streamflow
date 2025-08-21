##==============================================================================
## Project: QuEST
## This script is to calculate PT offset for Dog Valley DVNWT5 site
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

#################################
#### Import & Visualize Data ####
#################################
#### load data from Google drive ####
# this is the "depth" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1uyQmmLawojBw-yN2sjsCbTMDPRbvqyz4")

# list all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3

#DVNWT5
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="DVNWT5.csv"], 
                            path = "googledrive/DVNWT5.csv",
                            overwrite = T)
# load file
DVNWT5 <- read.csv("googledrive/DVNWT5.csv")

# convert Date column to Date type if not already
# combine Date and Time columns into a new DateTime column
DVNWT5$DateTime <- paste(DVNWT5$Date.x, DVNWT5$TimeOnly, sep = " ")
# convert the DateTime column to POSIXct
DVNWT5$DateTime <- as.POSIXct(DVNWT5$DateTime, format = "%Y-%m-%d %H:%M:%S")

# filter out rows with missing stage or discharge
rating_data <- DVNWT5 %>% 
  filter(!is.na(Baro_Cor_Lvl), !is.na(Q))

DVNWT5$Q..L.s. <- as.numeric(DVNWT5$Q)
rating_data$Q..L.s. <- as.numeric(rating_data$Q)

# make compensated backup 
DVNWT5$Baro_backup <- DVNWT5$Baro_Cor_Lvl

# level data
level_data <- DVNWT5 %>% 
  filter(!is.na(Baro_Cor_Lvl), !is.na(Actual_Water_Depth_m))

########################################
#### Plot pressure compensated data ####
########################################
ggplot(data = DVNWT5, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() + ggtitle("DVNWT5 compensated level data")

ggplot(data = DVNWT5, aes(x = DateTime, y = LEVEL.m)) +
  geom_line() + ggtitle("DVNWT5 level data")

########################
#### Plot Discharge ####
########################
# discharge from L/s to m3/s
rating_data <- rating_data %>%
  mutate(Q.m3s = Q..L.s./1000)

# plot with date info
ggplot(rating_data, aes(x = Baro_Cor_Lvl, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Baro-corrected level vs. Discharge", x = "Baro-corrected level (m)", y = "Discharge (Q m³/s)") +
  theme_minimal()

##########################
#### Plot Water Level ####
##########################
ggplot(level_data, aes(x = Actual_Water_Depth_m, y = Q..L.s.)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Actual water depth at sensor vs Discharge", x = "Water depth  (m)", y = "Discharge (Q L/s)") +
  theme_minimal()

ggplot(level_data, aes(x = Baro_Cor_Lvl, y = Actual_Water_Depth_m)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Baro-corrected level vs. Actual water depth at sensor", x = "Baro-corrected level (m)", y = "Water depth  (m)") +
  theme_minimal()

#################################################
#### Find offset, when did the change happen ####
################################################# 
ggplot(DVNWT5, aes(x = DateTime, y = LEVEL.m)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-11-19"), linetype="dashed", color="blue") + # "sensor moved"
  geom_vline(xintercept = as.POSIXct("2025-04-18"), linetype="dashed", color="red") + # sensor change
  geom_vline(xintercept = as.POSIXct("2024-10-18"), linetype="dashed", color="red") + # sensor broke
  labs(title = "LEVEL", x = "Date", y = "Water Level (m)")

ggplot(DVNWT5, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-11-19"), linetype="dashed", color="blue") + # "sensor moved"
  geom_vline(xintercept = as.POSIXct("2025-04-18"), linetype="dashed", color="red") + # sensor change
  geom_vline(xintercept = as.POSIXct("2024-10-18"), linetype="dashed", color="red") + # sensor broke
  labs(title = "Baro compensated", x = "Date", y = "Compensated Water Level (m)")

############################
#### Look at it closely ####
############################
# Level logger changed out due to error message when trying to download data, need to send out for repairs, new levelogger deployed at 12:21 PM
# Logger returned 
Date1 <- as.Date("2025-04-16", "%Y-%m-%d")
Date2 <- as.Date("2025-04-20", "%Y-%m-%d")
subdf <- DVNWT5[DVNWT5$DateTime < Date2 & DVNWT5$DateTime > Date1,]
#start of data
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-18 12:15:00"), linetype="dashed", color="red") 

Date1 <- as.Date("2025-04-01", "%Y-%m-%d")
Date2 <- as.Date("2025-04-30", "%Y-%m-%d")
subdf <- DVNWT5[DVNWT5$DateTime < Date2 & DVNWT5$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-2518 12:00:00"), linetype="dashed", color="red")

# Level logger moved down 7.5 cm. Logger back in water at 10:20
Date1 <- as.Date("2025-04-25", "%Y-%m-%d")
Date2 <- as.Date("2025-04-26", "%Y-%m-%d")
subdf <- DVNWT5[DVNWT5$DateTime < Date2 & DVNWT5$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-25 12:00:00"), linetype="dashed", color="red")

# Synoptic Samples, downloaded D.O., HOBO, S::can. barologger, and Levelogger data. Salt Slug. 
Date1 <- as.Date("2025-06-04", "%Y-%m-%d")
Date2 <- as.Date("2025-06-05", "%Y-%m-%d")
subdf <- DVNWT5[DVNWT5$DateTime < Date2 & DVNWT5$DateTime > Date1,]
# spike? adusted?
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-04 11:00:00"), linetype="dashed", color="red")

# Downloaded HOBO, S::can, and Levelogger. Salt Slug. 
Date1 <- as.Date("2025-06-09", "%Y-%m-%d")
Date2 <- as.Date("2025-06-11", "%Y-%m-%d")
subdf <- DVNWT5[DVNWT5$DateTime < Date2 & DVNWT5$DateTime > Date1,]
# spike? adusted?
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-10 11:00:00"), linetype="dashed", color="red")

#this is when it broke?
Date1 <- as.Date("2024-10-17", "%Y-%m-%d")
Date2 <- as.Date("2024-10-21", "%Y-%m-%d")
subdf <- DVNWT5[DVNWT5$DateTime < Date2 & DVNWT5$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-18 08:00:00"), linetype="dashed", color="red")

# S::can situated deeper in the water, Salt Slug. Downloaded barologger, Levelogger, and D.O. sensor.
Date1 <- as.Date("2025-07-07", "%Y-%m-%d")
Date2 <- as.Date("2025-07-09", "%Y-%m-%d")
subdf <- DVNWT5[DVNWT5$DateTime < Date2 & DVNWT5$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-07-08 11:00:00"), linetype="dashed", color="red")

# Seems to be an adjustment but no notes. Site was visited that day at that time
Date1 <- as.Date("2025-05-21", "%Y-%m-%d")
Date2 <- as.Date("2025-05-22", "%Y-%m-%d")
subdf <- DVNWT5[DVNWT5$DateTime < Date2 & DVNWT5$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-05-21 12:30:00"), linetype="dashed", color="red")

######################################################################
#### Remove times where PT was out of the water and error section ####
######################################################################
# now NA the time when the PT was out of water 
time1 <- as.POSIXct("2025-04-18 12:00:00")
time2 <- as.POSIXct("2025-04-25 11:45:00")
time3 <- as.POSIXct("2025-04-25 11:30:00")
time4 <- as.POSIXct("2025-06-04 11:45:00")
time5 <- as.POSIXct("2025-06-04 11:30:00")
time6 <- as.POSIXct("2025-06-04 11:15:00")
time7 <- as.POSIXct("2025-04-18 12:15:00") 

DVNWT5 <- DVNWT5 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time1, NA, Baro_Cor_Lvl))
DVNWT5 <- DVNWT5 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time2, NA, Baro_Cor_Lvl))
DVNWT5 <- DVNWT5 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time3, NA, Baro_Cor_Lvl))
DVNWT5 <- DVNWT5 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time4, NA, Baro_Cor_Lvl))
DVNWT5 <- DVNWT5 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time5, NA, Baro_Cor_Lvl))
DVNWT5 <- DVNWT5 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time6, NA, Baro_Cor_Lvl))
DVNWT5 <- DVNWT5 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time7, NA, Baro_Cor_Lvl))

# remove first couple data points in start of data
Date1 <- as.Date("2024-07-30", "%Y-%m-%d")
Date2 <- as.Date("2024-08-02", "%Y-%m-%d")
DVNWT5$Baro_Cor_Lvl[DVNWT5$DateTime >= Date1 & DVNWT5$DateTime <= Date2] <- NA

# plot after cleaning
ggplot(DVNWT5, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-05"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2024-10-24"), linetype="dashed", color="red") +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")

###########################################################################
#### Find the average Baro_Cor_Lvl TWO HOURS before and after the move ####
###########################################################################
# first move correction
move_time1 <- as.POSIXct("2024-10-18 09:00:00")
move_time2 <- as.POSIXct("2025-04-18 12:30:00")

before_move1 <- DVNWT5 %>%
  filter(DateTime >= (move_time1 - hours(2)) & DateTime < move_time1) %>%
  summarize(mean_before1 = mean(Baro_Cor_Lvl, na.rm = TRUE))

after_move1 <- DVNWT5 %>%
  filter(DateTime >= move_time2 & DateTime < (move_time2 + hours(2))) %>%
  summarize(mean_after1 = mean(Baro_Cor_Lvl, na.rm = TRUE))

offset1 <-  after_move1$mean_after1 - before_move1$mean_before1
print(paste("Offset 1:", offset1))

# apply the first correction
DVNWT5 <- DVNWT5 %>%
  mutate(Baro_Cor_offset1 = if_else(DateTime >= move_time1, Baro_Cor_Lvl - offset1, Baro_Cor_Lvl))

# second move correction
move_time2 <- as.POSIXct("2025-06-04 11:00:00")

before_move2 <- DVNWT5 %>%
  filter(DateTime >= (move_time2 - hours(2)) & DateTime < move_time2) %>%
  summarize(mean_before2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1

after_move2 <- DVNWT5 %>%
  filter(DateTime >= move_time2 & DateTime < (move_time2 + hours(2))) %>%
  summarize(mean_after2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1

offset2 <- after_move2$mean_after2 - before_move2$mean_before2
print(paste("Offset 2:", offset2))

# apply the second correction
DVNWT5 <- DVNWT5 %>%
  mutate(Baro_Cor_offset2 = if_else(DateTime >= move_time2, Baro_Cor_offset1 - offset2, Baro_Cor_offset1))

# third move correction 
move_time3 <- as.POSIXct("2025-04-25 12:00:00")

before_move3 <- DVNWT5 %>%
  filter(DateTime >= (move_time3 - hours(2)) & DateTime < move_time3) %>%
  summarize(mean_before3 = mean(Baro_Cor_offset2, na.rm = TRUE)) # Use Baro_Cor_offset2

after_move3 <- DVNWT5 %>%
  filter(DateTime >= move_time3 & DateTime < (move_time3 + hours(2))) %>%
  summarize(mean_after3 = mean(Baro_Cor_offset2, na.rm = TRUE)) # Use Baro_Cor_offset2

offset3 <- after_move3$mean_after3 - before_move3$mean_before3
print(paste("Offset 3:", offset3))

# apply the second correction
DVNWT5 <- DVNWT5 %>%
  mutate(Baro_Cor_offset3 = if_else(DateTime >= move_time3, Baro_Cor_offset2 - offset3, Baro_Cor_offset2))

# fourth move correction 
move_time4 <- as.POSIXct("2025-06-10 11:00:00")

before_move4 <- DVNWT5 %>%
  filter(DateTime >= (move_time4 - hours(2)) & DateTime < move_time4) %>%
  summarize(mean_before4 = mean(Baro_Cor_offset3, na.rm = TRUE)) # Use Baro_Cor_offset3

after_move4 <- DVNWT5 %>%
  filter(DateTime >= move_time4 & DateTime < (move_time4 + hours(2))) %>%
  summarize(mean_after4 = mean(Baro_Cor_offset3, na.rm = TRUE)) # Use Baro_Cor_offset3

offset4 <- after_move4$mean_after4 - before_move4$mean_before4
print(paste("Offset 4:", offset4))

# apply the second correction
DVNWT5 <- DVNWT5 %>%
  mutate(Baro_Cor_offset4 = if_else(DateTime >= move_time4, Baro_Cor_offset3 - offset4, Baro_Cor_offset3))

# fifth move correction 
move_time5 <- as.POSIXct("2025-07-08 11:00:00")

before_move5 <- DVNWT5 %>%
  filter(DateTime >= (move_time5 - hours(2)) & DateTime < move_time5) %>%
  summarize(mean_before5 = mean(Baro_Cor_offset4, na.rm = TRUE)) # Use Baro_Cor_offset4

after_move5 <- DVNWT5 %>%
  filter(DateTime >= move_time5 & DateTime < (move_time5 + hours(2))) %>%
  summarize(mean_after5 = mean(Baro_Cor_offset4, na.rm = TRUE)) # Use Baro_Cor_offset4

offset5 <- after_move5$mean_after5 - before_move5$mean_before5
print(paste("Offset 5:", offset5))

# apply the second correction
DVNWT5 <- DVNWT5 %>%
  mutate(Baro_Cor_offset5 = if_else(DateTime >= move_time5, Baro_Cor_offset4 - offset5, Baro_Cor_offset4))

# sixth move correction 
move_time6 <- as.POSIXct("2025-05-21 12:45:00")

before_move6 <- DVNWT5 %>%
  filter(DateTime >= (move_time6 - hours(2)) & DateTime < move_time6) %>%
  summarize(mean_before6 = mean(Baro_Cor_offset5, na.rm = TRUE)) # Use Baro_Cor_offset5

after_move6 <- DVNWT5 %>%
  filter(DateTime >= move_time6 & DateTime < (move_time6 + hours(2))) %>%
  summarize(mean_after6 = mean(Baro_Cor_offset5, na.rm = TRUE)) # Use Baro_Cor_offset5

offset6 <- after_move6$mean_after6 - before_move6$mean_before6
print(paste("Offset 6:", offset6))

# apply the second correction
DVNWT5 <- DVNWT5 %>%
  mutate(Baro_Cor_offset6 = if_else(DateTime >= move_time6, Baro_Cor_offset5 - offset6, Baro_Cor_offset5))

##############################
#### Plot with Correction ####
##############################
ggplot(DVNWT5, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (No Correction)", x = "Date", y = "Water Level (m)")

ggplot(DVNWT5, aes(x = DateTime, y = Baro_Cor_offset1)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (First Correction)", x = "Date", y = "Water Level (m)")

ggplot(DVNWT5, aes(x = DateTime, y = Baro_Cor_offset2)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (Second Correction)", x = "Date", y = "Water Level (m)")

ggplot(DVNWT5, aes(x = DateTime, y = Baro_Cor_offset3)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (Third Correction)", x = "Date", y = "Water Level (m)")

ggplot(DVNWT5, aes(x = DateTime, y = Baro_Cor_offset4)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (Fourth Correction)", x = "Date", y = "Water Level (m)")

ggplot(DVNWT5, aes(x = DateTime, y = Baro_Cor_offset5)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (Fourth Correction)", x = "Date", y = "Water Level (m)")

ggplot(DVNWT5, aes(x = DateTime, y = Baro_Cor_offset6)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (Fourth Correction)", x = "Date", y = "Water Level (m)")

# discharge from L/s to m3/s in whole data set
DVNWT5$Q..L.s. <- as.numeric(DVNWT5$Q..L.s.)
DVNWT5 <- DVNWT5 %>%
  mutate(Q.m3s = Q..L.s./1000)

# filter out rows with missing stage or discharge
new_rating_data <- DVNWT5 %>% 
  filter(!is.na(Baro_Cor_offset1), !is.na(Q.m3s))
new_level_data <- DVNWT5 %>% 
  filter(!is.na(Baro_Cor_offset1), !is.na(Actual_Water_Depth_m))

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

ggplot(new_rating_data, aes(x = Baro_Cor_offset4, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Stage vs. Discharge (Fourth Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

ggplot(new_rating_data, aes(x = Baro_Cor_offset5, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Stage vs. Discharge (Fourth Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

ggplot(new_rating_data, aes(x = Baro_Cor_offset6, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Stage vs. Discharge (Fourth Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

####################################
#### Plot level with correction ####
####################################
ggplot(new_level_data, aes(x = Baro_Cor_offset6, y = Actual_Water_Depth_m)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Baro-corrected level vs. Actual water depth at sensor (Sixth Correction)", x = "Baro-corrected level (m)", y = "Water depth  (m)") +
  theme_minimal()

########################
#### Plot close ups ####
########################
#take the subset of the data for April when PT was moved
Date1 <- as.Date("2025-04-24", "%Y-%m-%d")
Date2 <- as.Date("2025-04-28", "%Y-%m-%d")
subdf <- DVNWT5[DVNWT5$DateTime < Date2 & DVNWT5$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-09-26 12:00:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset3)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-09-26 12:00:00"), linetype="dashed", color="red")

#take the subset of the data for June when PT was moved
Date1 <- as.Date("2025-06-03", "%Y-%m-%d")
Date2 <- as.Date("2025-06-06", "%Y-%m-%d")
subdf <- DVNWT5[DVNWT5$DateTime < Date2 & DVNWT5$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-06-04 12:00:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset3)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-06-04 12:00:00"), linetype="dashed", color="red")

#take the subset of the data for June when PT was moved
Date1 <- as.Date("2025-06-10", "%Y-%m-%d")
Date2 <- as.Date("2025-06-11", "%Y-%m-%d")
subdf <- DVNWT5[DVNWT5$DateTime < Date2 & DVNWT5$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-10 11:00:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset5)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-10 11:00:00"), linetype="dashed", color="red")

#take the subset of the data for July when PT was moved
Date1 <- as.Date("2025-07-07", "%Y-%m-%d")
Date2 <- as.Date("2025-07-09", "%Y-%m-%d")
subdf <- DVNWT5[DVNWT5$DateTime < Date2 & DVNWT5$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-07-08 11:00:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset5)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-07-08 11:00:00"), linetype="dashed", color="red")

#take the subset of the data for February when PT was moved
Date1 <- as.Date("2025-05-19", "%Y-%m-%d")
Date2 <- as.Date("2025-05-23", "%Y-%m-%d")
subdf <- DVNWT5[DVNWT5$DateTime < Date2 & DVNWT5$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-05-21 12:00:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset6)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-05-21 12:00:00"), linetype="dashed", color="red")

###################
#### Save file ####
###################
write.csv(DVNWT5, "data/offset_DVNWT5.csv")

# this is the "offset" folder
drive_folder_id <- "136WGq6adaNROjaJN2YL63yJExKikM81A"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/offset_DVNWT5.csv",
  path = as_id(drive_folder_id)
)

