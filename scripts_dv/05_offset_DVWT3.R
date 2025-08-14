##==============================================================================
## Project: QuEST
## This script is to calculate PT offset for Dog Valley DVWT3 site
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

#DVWT3
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="DVWT3.csv"], 
                            path = "googledrive/DVWT3.csv",
                            overwrite = T)
# load file
DVWT3 <- read.csv("googledrive/DVWT3.csv")

# convert Date column to Date type if not already
# combine Date and Time columns into a new DateTime column
DVWT3$DateTime <- paste(DVWT3$Date.x, DVWT3$TimeOnly, sep = " ")
# convert the DateTime column to POSIXct
DVWT3$DateTime <- as.POSIXct(DVWT3$DateTime, format = "%Y-%m-%d %H:%M:%S")

# filter out rows with missing stage or discharge
rating_data <- DVWT3 %>% 
  filter(!is.na(Baro_Cor_Lvl), !is.na(Q))

DVWT3$Q..L.s. <- as.numeric(DVWT3$Q)
rating_data$Q..L.s. <- as.numeric(rating_data$Q)

# make compensated backup 
DVWT3$Baro_backup <- DVWT3$Baro_Cor_Lvl

########################################
#### Plot pressure compensated data ####
########################################
ggplot(data = DVWT3, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() + ggtitle("DVWT3 compensated level data")

ggplot(data = DVWT3, aes(x = DateTime, y = LEVEL.m)) +
  geom_line() + ggtitle("DVWT3 level data")

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
ggplot(rating_data, aes(x = actual_depth_m, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Baro-corrected level vs. Actual water depth at sensor", x = "Baro-corrected level (m)", y = "Water depth  (m)") +
  theme_minimal()

#################################################
#### Find offset, when did the change happen ####
################################################# 
ggplot(DVWT3, aes(x = DateTime, y = LEVEL.m)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-11-19"), linetype="dashed", color="red") + # cut ziptie loop
  geom_vline(xintercept = as.POSIXct("2025-02-28"), linetype="dashed", color="red") + # Levelogger burried upon arrival. Adjusted up by 9.0 cm. Levelogger redeployed at 11:30:00 AM.
  geom_vline(xintercept = as.POSIXct("2025-06-03"), linetype="dashed", color="red") + # Level logger moved down 1.0 cm using the fence post pounder
  labs(title = "LEVEL", x = "Date", y = "Water Level (m)")

ggplot(DVWT3, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-11-19"), linetype="dashed", color="red") + # cut ziptie loop
  geom_vline(xintercept = as.POSIXct("2025-02-28"), linetype="dashed", color="red") + # Levelogger burried upon arrival. Adjusted up by 9.0 cm. Levelogger redeployed at 11:30:00 AM.
  geom_vline(xintercept = as.POSIXct("2025-06-03"), linetype="dashed", color="red") + # Level logger moved down 1.0 cm using the fence post pounder
  labs(title = "Baro compensated", x = "Date", y = "Compensated Water Level (m)")

############################
#### Look at it closely ####
############################
#take the subset of the data
Date1 <- as.Date("024-11-19", "%Y-%m-%d")
Date2 <- as.Date("024-11-20", "%Y-%m-%d")
subdf <- DVWT3[DVWT3$DateTime < Date2 & DVWT3$DateTime > Date1,]
#start of data
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-08-06 08:15:00"), linetype="dashed", color="red") 

Date1 <- as.Date("2025-02-28", "%Y-%m-%d")
Date2 <- as.Date("2025-03-01", "%Y-%m-%d")
subdf <- DVWT3[DVWT3$DateTime < Date2 & DVWT3$DateTime > Date1,]
# adjustment in February
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-28 11:30:00"), linetype="dashed", color="red")

Date1 <- as.Date("2025-06-03", "%Y-%m-%d")
Date2 <- as.Date("2025-06-04", "%Y-%m-%d")
subdf <- DVWT3[DVWT3$DateTime < Date2 & DVWT3$DateTime > Date1,]
# spike?
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-03 09:45:00"), linetype="dashed", color="red")

Date1 <- as.Date("2025-06-06", "%Y-%m-%d")
Date2 <- as.Date("2025-07-02", "%Y-%m-%d")
subdf <- DVWT3[DVWT3$DateTime < Date2 & DVWT3$DateTime > Date1,]
# Level logger moved down 7.5 cm. Logger back in water at 10:20
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-25 12:00:00"), linetype="dashed", color="red")

######################################################################
#### Remove times where PT was out of the water and error section ####
######################################################################
# now NA the time when the PT was out of water 
# time1 <- as.POSIXct("2025-04-18 12:00:00")
# time2 <- as.POSIXct("2025-04-25 11:45:00")
# time3 <- as.POSIXct("2025-04-25 11:30:00")
# time4 <- as.POSIXct("2025-06-04 11:45:00")
# time5 <- as.POSIXct("2025-06-04 11:30:00")
# time6 <- as.POSIXct("2025-06-04 11:15:00")
# 
# DVWT3 <- DVWT3 %>%
#   mutate(Baro_Cor_Lvl = ifelse(DateTime == time1, NA, Baro_Cor_Lvl))
# DVWT3 <- DVWT3 %>%
#   mutate(Baro_Cor_Lvl = ifelse(DateTime == time2, NA, Baro_Cor_Lvl))
# DVWT3 <- DVWT3 %>%
#   mutate(Baro_Cor_Lvl = ifelse(DateTime == time3, NA, Baro_Cor_Lvl))
# DVWT3 <- DVWT3 %>%
#   mutate(Baro_Cor_Lvl = ifelse(DateTime == time4, NA, Baro_Cor_Lvl))
# DVWT3 <- DVWT3 %>%
#   mutate(Baro_Cor_Lvl = ifelse(DateTime == time5, NA, Baro_Cor_Lvl))
# DVWT3 <- DVWT3 %>%
#   mutate(Baro_Cor_Lvl = ifelse(DateTime == time6, NA, Baro_Cor_Lvl))

# remove little error section in June
Date1 <- as.Date("2025-06-05", "%Y-%m-%d")
Date2 <- as.Date("2025-07-02", "%Y-%m-%d")
DVWT3$Baro_Cor_Lvl[DVWT3$DateTime >= Date1 & DVWT3$DateTime <= Date2] <- NA
# remove first couple data points in start of data
# Date1 <- as.Date("2024-07-30", "%Y-%m-%d")
# Date2 <- as.Date("2024-08-02", "%Y-%m-%d")
# DVWT3$Baro_Cor_Lvl[DVWT3$DateTime >= Date1 & DVWT3$DateTime <= Date2] <- NA

# plot after cleaning
ggplot(DVWT3, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-05"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2024-10-24"), linetype="dashed", color="red") +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")

###########################################################################
#### Find the average Baro_Cor_Lvl TWO HOURS before and after the move ####
###########################################################################
# first move correction
move_time1 <- as.POSIXct("2025-02-28 11:30:00")

before_move1 <- DVWT3 %>%
  filter(DateTime >= (move_time1 - hours(2)) & DateTime < move_time1) %>%
  summarize(mean_before1 = mean(Baro_Cor_Lvl, na.rm = TRUE))

after_move1 <- DVWT3 %>%
  filter(DateTime >= move_time1 & DateTime < (move_time1 + hours(2))) %>%
  summarize(mean_after1 = mean(Baro_Cor_Lvl, na.rm = TRUE))

offset1 <-  after_move1$mean_after1 - before_move1$mean_before1
print(paste("Offset 1:", offset1))

# apply the first correction
DVWT3 <- DVWT3 %>%
  mutate(Baro_Cor_offset1 = if_else(DateTime >= move_time1, Baro_Cor_Lvl - offset1, Baro_Cor_Lvl))

# second move correction
move_time2 <- as.POSIXct("2025-06-03 09:45:00")

before_move2 <- DVWT3 %>%
  filter(DateTime >= (move_time2 - hours(2)) & DateTime < move_time2) %>%
  summarize(mean_before2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1

after_move2 <- DVWT3 %>%
  filter(DateTime >= move_time2 & DateTime < (move_time2 + hours(2))) %>%
  summarize(mean_after2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1

offset2 <- after_move2$mean_after2 - before_move2$mean_before2
print(paste("Offset 2:", offset2))

# apply the second correction
DVWT3 <- DVWT3 %>%
  mutate(Baro_Cor_offset2 = if_else(DateTime >= move_time2, Baro_Cor_offset1 - offset2, Baro_Cor_offset1))

# # third move correction 
# move_time3 <- as.POSIXct("2025-06-03 09:45:00")
# 
# before_move3 <- DVWT3 %>%
#   filter(DateTime >= (move_time3 - hours(2)) & DateTime < move_time3) %>%
#   summarize(mean_before3 = mean(Baro_Cor_offset2, na.rm = TRUE)) # Use Baro_Cor_offset2
# 
# after_move3 <- DVWT3 %>%
#   filter(DateTime >= move_time3 & DateTime < (move_time3 + hours(2))) %>%
#   summarize(mean_after3 = mean(Baro_Cor_offset2, na.rm = TRUE)) # Use Baro_Cor_offset2
# 
# offset3 <- after_move3$mean_after3 - before_move3$mean_before3
# print(paste("Offset 3:", offset3))
# 
# # apply the second correction
# DVWT3 <- DVWT3 %>%
#   mutate(Baro_Cor_offset3 = if_else(DateTime >= move_time3, Baro_Cor_offset2 - offset3, Baro_Cor_offset2))
# 
# # fourth move correction 
# move_time4 <- as.POSIXct("2025-05-21 12:45:00")
# 
# before_move4 <- DVWT3 %>%
#   filter(DateTime >= (move_time4 - hours(2)) & DateTime < move_time4) %>%
#   summarize(mean_before4 = mean(Baro_Cor_offset3, na.rm = TRUE)) # Use Baro_Cor_offset3
# 
# after_move4 <- DVWT3 %>%
#   filter(DateTime >= move_time4 & DateTime < (move_time4 + hours(2))) %>%
#   summarize(mean_after4 = mean(Baro_Cor_offset3, na.rm = TRUE)) # Use Baro_Cor_offset3
# 
# offset4 <- after_move4$mean_after4 - before_move4$mean_before4
# print(paste("Offset 4:", offset4))
# 
# # apply the second correction
# DVWT3 <- DVWT3 %>%
#   mutate(Baro_Cor_offset4 = if_else(DateTime >= move_time4, Baro_Cor_offset3 - offset4, Baro_Cor_offset3))

##############################
#### Plot with Correction ####
##############################
ggplot(DVWT3, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (No Correction)", x = "Date", y = "Water Level (m)")

ggplot(DVWT3, aes(x = DateTime, y = Baro_Cor_offset1)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (First Correction)", x = "Date", y = "Water Level (m)")

ggplot(DVWT3, aes(x = DateTime, y = Baro_Cor_offset2)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (Second Correction)", x = "Date", y = "Water Level (m)")

# ggplot(DVWT3, aes(x = DateTime, y = Baro_Cor_offset3)) +
#   geom_line() +
#   labs(title = "Corrected Baro_Cor Over Time (Third Correction)", x = "Date", y = "Water Level (m)")
# 
# ggplot(DVWT3, aes(x = DateTime, y = Baro_Cor_offset4)) +
#   geom_line() +
#   labs(title = "Corrected Baro_Cor Over Time (Fourth Correction)", x = "Date", y = "Water Level (m)")

# discharge from L/s to m3/s in whole data set
DVWT3$Q..L.s. <- as.numeric(DVWT3$Q..L.s.)
DVWT3 <- DVWT3 %>%
  mutate(Q.m3s = Q..L.s./1000)

# filter out rows with missing stage or discharge
new_rating_data <- DVWT3 %>% 
  filter(!is.na(Baro_Cor_offset2), !is.na(Q.m3s))

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

# ggplot(new_rating_data, aes(x = Baro_Cor_offset3, y = Q.m3s)) +
#   geom_point(color = "blue") +
#   geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
#   labs(title = "Stage vs. Discharge (Third Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
#   theme_minimal()
# 
# ggplot(new_rating_data, aes(x = Baro_Cor_offset4, y = Q.m3s)) +
#   geom_point(color = "blue") +
#   geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
#   labs(title = "Stage vs. Discharge (Fourth Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
#   theme_minimal()

########################
#### Plot close ups ####
########################
DVWT3_Feb <- DVWT3%>%
  filter(month(DateTime) == 2)

# plot after cleaning 
ggplot(DVWT3_Feb, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")
ggplot(DVWT3_Feb, aes(x = DateTime, y = Baro_Cor_offset2)) +
  geom_line() +
  labs(title = "Baro_Cor_offset2", x = "Date", y = "Water Level (m)")

#take the subset of the data for February when PT was moved
Date1 <- as.Date("2025-06-01", "%Y-%m-%d")
Date2 <- as.Date("2025-06-30", "%Y-%m-%d")
subdf <- DVWT3[DVWT3$DateTime < Date2 & DVWT3$DateTime > Date1,]

ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-09-26 12:00:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset2)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-09-26 12:00:00"), linetype="dashed", color="red")

###################
#### Save file ####
###################
write.csv(DVWT3, "data/offset_DVWT3.csv")

# this is the "offset" folder
drive_folder_id <- "136WGq6adaNROjaJN2YL63yJExKikM81A"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/offset_DVWT3.csv",
  path = as_id(drive_folder_id)
)