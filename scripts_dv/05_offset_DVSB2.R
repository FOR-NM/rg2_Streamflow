##==============================================================================
## Project: QuEST
## This script is to calculate PT offset for Dog Valley DVSB2 site
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

#DVSB2
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="DVSB2.csv"], 
                            path = "googledrive/DVSB2.csv",
                            overwrite = T)
# load file
DVSB2 <- read.csv("googledrive/DVSB2.csv")

# convert Date column to Date type if not already
# combine Date and Time columns into a new DateTime column
DVSB2$DateTime <- paste(DVSB2$Date.air, DVSB2$Time.air, sep = " ")
# convert the DateTime column to POSIXct
DVSB2$DateTime <- as.POSIXct(DVSB2$DateTime, format = "%Y-%m-%d %H:%M:%S")

# filter out rows with missing stage or discharge
rating_data <- DVSB2 %>% 
  filter(!is.na(Baro_Cor_Lvl), !is.na(Q))

DVSB2$Q..L.s. <- as.numeric(DVSB2$Q)
rating_data$Q..L.s. <- as.numeric(rating_data$Q)

# make compensated backup 
DVSB2$Baro_backup <- DVSB2$Baro_Cor_Lvl

########################################
#### Plot pressure compensated data ####
########################################
ggplot(data = DVSB2, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() + ggtitle("DVSB2 compensated level data")

ggplot(data = DVSB2, aes(x = DateTime, y = LEVEL.m)) +
  geom_line() + ggtitle("DVSB2 level data")

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
ggplot(rating_data, aes(x = Baro_Cor_Lvl, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Baro-corrected level vs. Actual water depth at sensor", x = "Baro-corrected level (m)", y = "Water depth  (m)") +
  theme_minimal()

ggplot(rating_data, aes(x = actual_depth_m, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Baro-corrected level vs. Actual water depth at sensor", x = "Baro-corrected level (m)", y = "Water depth  (m)") +
  theme_minimal()

#################################################
#### Find offset, when did the change happen ####
################################################# 
ggplot(DVSB2, aes(x = DateTime, y = LEVEL.m)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-11-19"), linetype="dashed", color="red") + # cut ziptie loop
  geom_vline(xintercept = as.POSIXct("2025-02-28"), linetype="dashed", color="red") + # Levelogger burried upon arrival. Adjusted up by 9.0 cm. Levelogger redeployed at 11:30:00 AM.
  geom_vline(xintercept = as.POSIXct("2025-06-03"), linetype="dashed", color="red") + # Level logger moved down 1.0 cm using the fence post pounder
  labs(title = "LEVEL", x = "Date", y = "Water Level (m)")

ggplot(DVSB2, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-11-19"), linetype="dashed", color="red") + # cut ziptie loop
  geom_vline(xintercept = as.POSIXct("2025-02-28"), linetype="dashed", color="red") + # Levelogger burried upon arrival. Adjusted up by 9.0 cm. Levelogger redeployed at 11:30:00 AM.
  geom_vline(xintercept = as.POSIXct("2025-06-03"), linetype="dashed", color="red") + # Level logger moved down 1.0 cm using the fence post pounder
  labs(title = "Baro compensated", x = "Date", y = "Compensated Water Level (m)")

############################
#### Look at it closely ####
############################
# these are the moments when the level logger was adjusted or moved 
#take the subset of the data
# Cut zip tie loop
Date1 <- as.Date("2024-11-19", "%Y-%m-%d")
Date2 <- as.Date("2024-11-20", "%Y-%m-%d")
subdf <- DVSB2[DVSB2$DateTime < Date2 & DVSB2$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-11-19 16:15:00"), linetype="dashed", color="red") 
ggplot(data=subdf, aes(DateTime,LEVEL.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-11-19 16:15:00"), linetype="dashed", color="red") 
ggplot(data=subdf, aes(DateTime,Level_air.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-11-19 16:15:00"), linetype="dashed", color="red") 

# Levelogger burried upon arrival. Adjusted up by 9.0 cm. Levelogger redeployed at 11:30:00 AM
Date1 <- as.Date("2025-02-28", "%Y-%m-%d")
Date2 <- as.Date("2025-03-01", "%Y-%m-%d")
subdf <- DVSB2[DVSB2$DateTime < Date2 & DVSB2$DateTime > Date1,]
# adjustment in February
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-28 11:30:00"), linetype="dashed", color="red")

# Level logger moved down 1.0 cm using the fence post pounder
Date1 <- as.Date("2025-06-03", "%Y-%m-%d")
Date2 <- as.Date("2025-06-04", "%Y-%m-%d")
subdf <- DVSB2[DVSB2$DateTime < Date2 & DVSB2$DateTime > Date1,]
# spike?
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-03 09:45:00"), linetype="dashed", color="red")

# Level logger moved down 3.5 cm using the fence post pounder
Date1 <- as.Date("2025-06-06", "%Y-%m-%d")
Date2 <- as.Date("2025-07-02", "%Y-%m-%d")
subdf <- DVSB2[DVSB2$DateTime < Date2 & DVSB2$DateTime > Date1,]
# Level logger moved down 7.5 cm. Logger back in water at 10:20
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-25 12:00:00"), linetype="dashed", color="red")

# 
Date1 <- as.Date("2024-09-01", "%Y-%m-%d")
Date2 <- as.Date("2024-09-20", "%Y-%m-%d")
subdf <- DVSB2[DVSB2$DateTime < Date2 & DVSB2$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-09-06 12:00:00"), linetype="dashed", color="red")
subdf <- DVSB2[DVSB2$DateTime < Date2 & DVSB2$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,LEVEL.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-25 12:00:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Level_air.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-25 12:00:00"), linetype="dashed", color="red")

######################################################################
#### Remove times where PT was out of the water and error section ####
######################################################################
# now NA the time when the PT was out of water 
time1 <- as.POSIXct("2025-06-03 09:45:00")
# time2 <- as.POSIXct("2025-04-25 11:45:00")
# time3 <- as.POSIXct("2025-04-25 11:30:00")
# time4 <- as.POSIXct("2025-06-04 11:45:00")
# time5 <- as.POSIXct("2025-06-04 11:30:00")
# time6 <- as.POSIXct("2025-06-04 11:15:00")

DVSB2 <- DVSB2 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time1, NA, Baro_Cor_Lvl))
# DVSB2 <- DVSB2 %>%
#   mutate(Baro_Cor_Lvl = ifelse(DateTime == time2, NA, Baro_Cor_Lvl))
# DVSB2 <- DVSB2 %>%
#   mutate(Baro_Cor_Lvl = ifelse(DateTime == time3, NA, Baro_Cor_Lvl))
# DVSB2 <- DVSB2 %>%
#   mutate(Baro_Cor_Lvl = ifelse(DateTime == time4, NA, Baro_Cor_Lvl))
# DVSB2 <- DVSB2 %>%
#   mutate(Baro_Cor_Lvl = ifelse(DateTime == time5, NA, Baro_Cor_Lvl))
# DVSB2 <- DVSB2 %>%
#   mutate(Baro_Cor_Lvl = ifelse(DateTime == time6, NA, Baro_Cor_Lvl))

# remove little error section in June
Date1 <- as.Date("2025-06-05", "%Y-%m-%d")
Date2 <- as.Date("2025-07-02", "%Y-%m-%d")
DVSB2$Baro_Cor_Lvl[DVSB2$DateTime >= Date1 & DVSB2$DateTime <= Date2] <- NA
# remove first couple data points in start of data
# Date1 <- as.Date("2024-07-30", "%Y-%m-%d")
# Date2 <- as.Date("2024-08-02", "%Y-%m-%d")
# DVSB2$Baro_Cor_Lvl[DVSB2$DateTime >= Date1 & DVSB2$DateTime <= Date2] <- NA

# plot after cleaning
ggplot(DVSB2, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-05"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2024-10-24"), linetype="dashed", color="red") +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")

###########################################################################
#### Find the average Baro_Cor_Lvl TWO HOURS before and after the move ####
###########################################################################
# first move correction
move_time1 <- as.POSIXct("2025-02-28 11:30:00")

before_move1 <- DVSB2 %>%
  filter(DateTime >= (move_time1 - hours(2)) & DateTime < move_time1) %>%
  summarize(mean_before1 = mean(Baro_Cor_Lvl, na.rm = TRUE))

after_move1 <- DVSB2 %>%
  filter(DateTime >= move_time1 & DateTime < (move_time1 + hours(2))) %>%
  summarize(mean_after1 = mean(Baro_Cor_Lvl, na.rm = TRUE))

offset1 <-  after_move1$mean_after1 - before_move1$mean_before1
print(paste("Offset 1:", offset1))

# apply the first correction
DVSB2 <- DVSB2 %>%
  mutate(Baro_Cor_offset1 = if_else(DateTime >= move_time1, Baro_Cor_Lvl - offset1, Baro_Cor_Lvl))

# second move correction
move_time2 <- as.POSIXct("2025-06-03 10:00:00")

before_move2 <- DVSB2 %>%
  filter(DateTime >= (move_time2 - hours(2)) & DateTime < move_time2) %>%
  summarize(mean_before2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1

after_move2 <- DVSB2 %>%
  filter(DateTime >= move_time2 & DateTime < (move_time2 + hours(2))) %>%
  summarize(mean_after2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1

offset2 <- after_move2$mean_after2 - before_move2$mean_before2
print(paste("Offset 2:", offset2))

# apply the second correction
DVSB2 <- DVSB2 %>%
  mutate(Baro_Cor_offset2 = if_else(DateTime >= move_time2, Baro_Cor_offset1 - offset2, Baro_Cor_offset1))

# third move correction
move_time3 <- as.POSIXct("2024-11-19 16:15:00")

before_move3 <- DVSB2 %>%
  filter(DateTime >= (move_time3 - hours(2)) & DateTime < move_time3) %>%
  summarize(mean_before3 = mean(Baro_Cor_offset2, na.rm = TRUE)) # Use Baro_Cor_offset2

after_move3 <- DVSB2 %>%
  filter(DateTime >= move_time3 & DateTime < (move_time3 + hours(2))) %>%
  summarize(mean_after3 = mean(Baro_Cor_offset2, na.rm = TRUE)) # Use Baro_Cor_offset2

offset3 <- after_move3$mean_after3 - before_move3$mean_before3
print(paste("Offset 3:", offset3))

# apply the second correction
DVSB2 <- DVSB2 %>%
  mutate(Baro_Cor_offset3 = if_else(DateTime >= move_time3, Baro_Cor_offset2 - offset3, Baro_Cor_offset2))

# # fourth move correction 
# move_time4 <- as.POSIXct("2025-05-21 12:45:00")
# 
# before_move4 <- DVSB2 %>%
#   filter(DateTime >= (move_time4 - hours(2)) & DateTime < move_time4) %>%
#   summarize(mean_before4 = mean(Baro_Cor_offset3, na.rm = TRUE)) # Use Baro_Cor_offset3
# 
# after_move4 <- DVSB2 %>%
#   filter(DateTime >= move_time4 & DateTime < (move_time4 + hours(2))) %>%
#   summarize(mean_after4 = mean(Baro_Cor_offset3, na.rm = TRUE)) # Use Baro_Cor_offset3
# 
# offset4 <- after_move4$mean_after4 - before_move4$mean_before4
# print(paste("Offset 4:", offset4))
# 
# # apply the second correction
# DVSB2 <- DVSB2 %>%
#   mutate(Baro_Cor_offset4 = if_else(DateTime >= move_time4, Baro_Cor_offset3 - offset4, Baro_Cor_offset3))

##############################
#### Plot with Correction ####
##############################
ggplot(DVSB2, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (No Correction)", x = "Date", y = "Water Level (m)")

ggplot(DVSB2, aes(x = DateTime, y = Baro_Cor_offset1)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (First Correction)", x = "Date", y = "Water Level (m)")

ggplot(DVSB2, aes(x = DateTime, y = Baro_Cor_offset2)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (Second Correction)", x = "Date", y = "Water Level (m)")

ggplot(DVSB2, aes(x = DateTime, y = Baro_Cor_offset3)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (Third Correction)", x = "Date", y = "Water Level (m)")

# discharge from L/s to m3/s in whole data set
DVSB2$Q..L.s. <- as.numeric(DVSB2$Q..L.s.)
DVSB2 <- DVSB2 %>%
  mutate(Q.m3s = Q..L.s./1000)

# filter out rows with missing stage or discharge
new_rating_data <- DVSB2 %>% 
  filter(!is.na(Baro_Cor_offset2), !is.na(Q.m3s))

ggplot(new_rating_data, aes(x = Baro_Cor_Lvl, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Stage vs. Discharge (No Correction)", x = "Baro corrected level (m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

ggplot(new_rating_data, aes(x = Baro_Cor_offset1, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Stage vs. Discharge (First Correction)", x = "Baro corrected level (m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

ggplot(new_rating_data, aes(x = Baro_Cor_offset2, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Stage vs. Discharge (Second Correction)", x = "Baro corrected level (m)", y = "Discharge (Q m3/s)") +
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
  labs(title = "Stage vs. depth (Third Correction)", x = "Baro corrected level (m)", y = "Actual depth (m)") +
  theme_minimal()

########################
#### Plot close ups ####
########################
# these are the moments when the level logger was adjusted or moved 
#take the subset of the data
# Cut zip tie loop
Date1 <- as.Date("2024-11-19", "%Y-%m-%d")
Date2 <- as.Date("2024-11-20", "%Y-%m-%d")
subdf <- DVSB2[DVSB2$DateTime < Date2 & DVSB2$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-11-19 16:15:00"), linetype="dashed", color="red") 
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset3)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-11-19 16:15:00"), linetype="dashed", color="red") 

# Levelogger burried upon arrival. Adjusted up by 9.0 cm. Levelogger redeployed at 11:30:00 AM
Date1 <- as.Date("2025-02-25", "%Y-%m-%d")
Date2 <- as.Date("2025-03-05", "%Y-%m-%d")
subdf <- DVSB2[DVSB2$DateTime < Date2 & DVSB2$DateTime > Date1,]
# adjustment in February
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-28 11:30:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset3)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-28 11:30:00"), linetype="dashed", color="red")

# Level logger moved down 1.0 cm using the fence post pounder
Date1 <- as.Date("2025-06-03", "%Y-%m-%d")
Date2 <- as.Date("2025-06-04", "%Y-%m-%d")
subdf <- DVSB2[DVSB2$DateTime < Date2 & DVSB2$DateTime > Date1,]
# spike?
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-03 09:00:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset3)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-03 09:00:00"), linetype="dashed", color="red")

###################
#### Save file ####
###################
write.csv(DVSB2, "data/offset_DVSB2.csv")

# this is the "offset" folder
drive_folder_id <- "136WGq6adaNROjaJN2YL63yJExKikM81A"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/offset_DVSB2.csv",
  path = as_id(drive_folder_id)
)

Date1 <- as.Date("2025-03-01", "%Y-%m-%d")
Date2 <- as.Date("2025-03-15", "%Y-%m-%d")
subdf <- DVSB2[DVSB2$DateTime < Date2 & DVSB2$DateTime > Date1,]
# sheet says we got to the site at 12:00:00
ggplot(data=subdf, aes(DateTime,LEVEL.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-07 12:15:00"), linetype="dashed", color="red") 
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-07 11:45:00"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2025-02-07 16:45:00"), linetype="dashed", color="red") 
ggplot(data=subdf, aes(DateTime,Level_air.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-07 11:45:00"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2025-02-07 16:45:00"), linetype="dashed", color="red") 
