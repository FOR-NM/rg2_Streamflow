##==============================================================================
## Project: QuEST
## This script is to calculate PT offset for Dog Valley DVNWT4 site
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

#DVNWT4
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="DVNWT4.csv"], 
                            path = "googledrive/DVNWT4.csv",
                            overwrite = T)
# load file
DVNWT4 <- read.csv("googledrive/DVNWT4.csv")

# convert Date column to Date type if not already
# combine Date and Time columns into a new DateTime column
DVNWT4$DateTime <- paste(DVNWT4$Date.x, DVNWT4$TimeOnly, sep = " ")
# convert the DateTime column to POSIXct
DVNWT4$DateTime <- as.POSIXct(DVNWT4$DateTime, format = "%Y-%m-%d %H:%M:%S")

# filter out rows with missing stage or discharge
rating_data <- DVNWT4 %>% 
  filter(!is.na(Baro_Cor_Lvl.m), !is.na(Q))

DVNWT4$Q..L.s. <- as.numeric(DVNWT4$Q)
rating_data$Q..L.s. <- as.numeric(rating_data$Q)

# level data
level_data <- DVNWT4 %>% 
  filter(!is.na(Baro_Cor_Lvl.m), !is.na(Actual_Water_Depth_m))

# make compensated backup 
DVNWT4$Baro_backup <- DVNWT4$Baro_Cor_Lvl.m

########################################
#### Plot pressure compensated data ####
########################################
ggplot(data = DVNWT4, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() + ggtitle("DVNWT4 compensated level data")

ggplot(data = DVNWT4, aes(x = DateTime, y = LEVEL.m)) +
  geom_line() + ggtitle("DVNWT4 level data")

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
ggplot(level_data, aes(x = Actual_Water_Depth_m, y = Q)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Actual water depth at sensor vs Discharge", x = "Water depth  (m)", y = "Discharge") +
  theme_minimal()

ggplot(level_data, aes(x = Baro_Cor_Lvl.m, y = Actual_Water_Depth_m)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Baro-corrected level vs. Actual water depth at sensor", x = "Baro-corrected level (m)", y = "Water depth  (m)") +
  theme_minimal()

############################
#### Look at it closely ####
############################
# Site was dry?
Date1 <- as.Date("2025-01-20", "%Y-%m-%d")
Date2 <- as.Date("2025-02-20", "%Y-%m-%d")
subdf <- DVNWT4[DVNWT4$DateTime < Date2 & DVNWT4$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-01 18:15:00"), linetype="dashed", color="red")

# Synoptic Samples, downloaded Levelogger, HOBO, and D.O. sensor. Salt Slug.
Date1 <- as.Date("2025-06-04", "%Y-%m-%d")
Date2 <- as.Date("2025-06-06", "%Y-%m-%d")
subdf <- DVNWT4[DVNWT4$DateTime < Date2 & DVNWT4$DateTime > Date1,]
# spike?
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-04 09:15:00"), linetype="dashed", color="red")

# Site is flowing, levelogger was not buried. Levelogger removed at 9:48 AM. 
Date1 <- as.Date("2025-04-18", "%Y-%m-%d")
Date2 <- as.Date("2025-04-20", "%Y-%m-%d")
subdf <- DVNWT4[DVNWT4$DateTime < Date2 & DVNWT4$DateTime > Date1,]
# spike?
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-18 08:45:00"), linetype="dashed", color="red")

# Logger moved but this is where a ching of the baro data is missing so adjusment is not possible?
Date1 <- as.Date("2024-11-16", "%Y-%m-%d")
Date2 <- as.Date("2024-11-22", "%Y-%m-%d")
subdf <- DVNWT4[DVNWT4$DateTime < Date2 & DVNWT4$DateTime > Date1,]
# spike?
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-18 08:45:00"), linetype="dashed", color="red")

# Fixed precipitation guage (glass jar fell off rubber stopper). Downloaded L.L. and D.O. sensors. 
Date1 <- as.Date("2025-07-06", "%Y-%m-%d")
Date2 <- as.Date("2025-07-09", "%Y-%m-%d")
subdf <- DVNWT4[DVNWT4$DateTime < Date2 & DVNWT4$DateTime > Date1,]
# spike?
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-18 08:45:00"), linetype="dashed", color="red")

######################################################################
#### Remove times where PT was out of the water and error section ####
######################################################################
# now NA the time when the PT was out of water 
time1 <- as.POSIXct("2025-02-01 00:00:00")
time2 <- as.POSIXct("2025-06-04 09:00:00")
time3 <- as.POSIXct("2025-04-18 08:45:00")
time4 <- as.POSIXct("2025-04-18 09:00:00")
# time5 <- as.POSIXct("2025-06-03 13:15:00")
# time6 <- as.POSIXct("2024-11-14 11:00:00")

DVNWT4 <- DVNWT4 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time1, NA, Baro_Cor_Lvl.m))
DVNWT4 <- DVNWT4 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time2, NA, Baro_Cor_Lvl.m))
DVNWT4 <- DVNWT4 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time3, NA, Baro_Cor_Lvl.m))
DVNWT4 <- DVNWT4 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time4, NA, Baro_Cor_Lvl.m))
# DVNWT4 <- DVNWT4 %>%
#   mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time5, NA, Baro_Cor_Lvl.m))
# DVNWT4 <- DVNWT4 %>%
#   mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time6, NA, Baro_Cor_Lvl.m))

# plot after cleaning
ggplot(DVNWT4, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-05"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2024-10-24"), linetype="dashed", color="red") +
  labs(title = "Baro_Cor_Lvl.m", x = "Date", y = "Water Level (m)")

###########################################################################
#### Find the average Baro_Cor_Lvl.m TWO HOURS before and after the move ####
###########################################################################
# # first move correction (2025-04-18 08:45:00)
# move_time1 <- as.POSIXct("2025-04-18 08:30:00")
# 
# before_move1 <- DVNWT4 %>%
#   filter(DateTime >= (move_time1 - hours(2)) & DateTime < move_time1) %>%
#   summarize(mean_before1 = mean(Baro_Cor_Lvl.m, na.rm = TRUE))
# 
# after_move1 <- DVNWT4 %>%
#   filter(DateTime >= move_time1 & DateTime < (move_time1 + hours(2))) %>%
#   summarize(mean_after1 = mean(Baro_Cor_Lvl.m, na.rm = TRUE))
# 
# offset1 <-  after_move1$mean_after1 - before_move1$mean_before1
# print(paste("Offset 1:", offset1))
# 
# # apply the first correction
# DVNWT4 <- DVNWT4 %>%
#   mutate(Baro_Cor_offset1 = if_else(DateTime >= move_time1, Baro_Cor_Lvl.m - offset1, Baro_Cor_Lvl.m))
# 
# # second move correction (2025-06-04 09:00:00)
# move_time2 <- as.POSIXct("2025-06-04 09:15:00")
# 
# before_move2 <- DVNWT4 %>%
#   filter(DateTime >= (move_time2 - hours(2)) & DateTime < move_time2) %>%
#   summarize(mean_before2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1
# 
# after_move2 <- DVNWT4 %>%
#   filter(DateTime >= move_time2 & DateTime < (move_time2 + hours(2))) %>%
#   summarize(mean_after2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1
# 
# offset2 <- after_move2$mean_after2 - before_move2$mean_before2
# print(paste("Offset 2:", offset2))
# 
# # apply the second correction
# DVNWT4 <- DVNWT4 %>%
#   mutate(Baro_Cor_offset2 = if_else(DateTime >= move_time2, Baro_Cor_offset1 - offset2, Baro_Cor_offset1))

# # known offset correction (2024-11-19, 4 cm)
# move_time3 <- as.POSIXct("2024-11-19 10:57:00")
# offset3 <- -0.04  # m offset from field notes
# 
# # apply the third correction
# DVNWT4 <- DVNWT4 %>%
#   mutate(Baro_Cor_offset3 = if_else(DateTime >= move_time3,
#                                     Baro_Cor_offset2 - offset3,
#                                     Baro_Cor_offset2))

### move correction to set everything at cero
# Subtracting 0.05 moves the entire line down
DVNWT4 <- DVNWT4 %>%
  mutate(Final_Corrected_Lvl = Baro_Cor_Lvl.m - 0.035)

##############################
#### Plot with Correction ####
##############################
ggplot(DVNWT4, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (No Correction)", x = "Date", y = "Water Level (m)")

ggplot(DVNWT4, aes(x = DateTime, y = Final_Corrected_Lvl)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time", x = "Date", y = "Water Level (m)")

# discharge from L/s to m3/s in whole data set
DVNWT4$Q..L.s. <- as.numeric(DVNWT4$Q..L.s.)
DVNWT4 <- DVNWT4 %>%
  mutate(Q.m3s = Q..L.s./1000)

# filter out rows with missing stage or discharge
new_rating_data <- DVNWT4 %>% 
  filter(!is.na(Final_Corrected_Lvl), !is.na(Q.m3s))
new_level_data <- DVNWT4 %>% 
  filter(!is.na(Final_Corrected_Lvl), !is.na(Actual_Water_Depth_m))

ggplot(new_rating_data, aes(x = Final_Corrected_Lvl, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Stage vs. Discharge (No Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

ggplot(new_rating_data, aes(x = Final_Corrected_Lvl, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Stage vs. Discharge (Second Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

####################################
#### Plot level with correction ####
####################################
ggplot(new_level_data, aes(x = Final_Corrected_Lvl, y = Actual_Water_Depth_m)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Baro-corrected level vs. Actual water depth at sensor (Third Correction)", x = "Baro-corrected level (m)", y = "Water depth  (m)") +
  theme_minimal()

########################
#### Plot close ups ####
########################
#take the subset of the data for April when PT was moved
Date1 <- as.Date("2025-04-16", "%Y-%m-%d")
Date2 <- as.Date("2025-04-20", "%Y-%m-%d")
subdf <- DVNWT4[DVNWT4$DateTime < Date2 & DVNWT4$DateTime > Date1,]

ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-09-26 12:00:00"), linetype="dashed", color="red") 
ggplot(data=subdf, aes(DateTime,Final_Corrected_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-09-26 12:00:00"), linetype="dashed", color="red") 

#take the subset of the data for June when PT was moved
Date1 <- as.Date("2025-06-02", "%Y-%m-%d")
Date2 <- as.Date("2025-06-06", "%Y-%m-%d")
subdf <- DVNWT4[DVNWT4$DateTime < Date2 & DVNWT4$DateTime > Date1,]

ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-04 12:00:00"), linetype="dashed", color="red") 
ggplot(data=subdf, aes(DateTime,Final_Corrected_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-04 12:00:00"), linetype="dashed", color="red") 

#################################################
#### Standardize final corrected column name ####
#################################################
# alias the fully offset-corrected stage to a consistent name so downstream
# steps (depth conversion, rating curve) don't need to know how many
# site-specific corrections were applied
# DVNWT4 <- DVNWT4 %>%
#   mutate(Final_Corrected_Lvl = Baro_Cor_offset2)

###################
#### Save file ####
###################
write.csv(DVNWT4, "data/offset_DVNWT4.csv")

# this is the "offset" folder
drive_folder_id <- "136WGq6adaNROjaJN2YL63yJExKikM81A"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/offset_DVNWT4.csv",
  path = as_id(drive_folder_id)
)

