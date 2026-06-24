##==============================================================================
## Project: QuEST
## This script is to calculate PT offset for Dog Valley DVMS1 site
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

#DVMS1
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="DVMS1.csv"], 
                            path = "googledrive/DVMS1.csv",
                            overwrite = T)
# load file
DVMS1 <- read.csv("googledrive/DVMS1.csv")

# convert Date column to Date type if not already
# combine Date and Time columns into a new DateTime column
DVMS1$DateTime <- paste(DVMS1$Date.x, DVMS1$TimeOnly, sep = " ")
# convert the DateTime column to POSIXct
DVMS1$DateTime <- as.POSIXct(DVMS1$DateTime, format = "%Y-%m-%d %H:%M:%S")

# filter out rows with missing stage or discharge
rating_data <- DVMS1 %>% 
  filter(!is.na(Baro_Cor_Lvl.m), !is.na(Q))
level_data <- DVMS1 %>% 
  filter(!is.na(Baro_Cor_Lvl.m), !is.na(Actual_Water_Depth_m))

DVMS1$Q..L.s. <- as.numeric(DVMS1$Q)
rating_data$Q..L.s. <- as.numeric(rating_data$Q)

# make compensated backup 
DVMS1$Baro_backup <- DVMS1$Baro_Cor_Lvl.m

########################################
#### Plot pressure compensated data ####
########################################
ggplot(data = DVMS1, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() + ggtitle("DVMS1 compensated level data")

ggplot(data = DVMS1, aes(x = DateTime, y = LEVEL.m)) +
  geom_line() + ggtitle("DVMS1 level data")

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

ggplot(level_data, aes(x = Baro_Cor_Lvl.m, y = Actual_Water_Depth_m)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Baro-corrected level vs. Actual Depth", x = "Baro-corrected level (m)", y = "Actual Water Depth (m)") +
  theme_minimal()

#################################################
#### Find offset, when did the change happen ####
################################################# 
ggplot(DVMS1, aes(x = DateTime, y = LEVEL.m)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-09-26"), linetype="dashed", color="red") + # Moved up one hole 
  geom_vline(xintercept = as.POSIXct("2024-11-19"), linetype="dashed", color="red") + 
  labs(title = "LEVEL", x = "Date", y = "Water Level (m)")

ggplot(DVMS1, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-09-26"), linetype="dashed", color="red") + # Moved up one hole 
  geom_vline(xintercept = as.POSIXct("2024-11-19"), linetype="dashed", color="red") + 
  labs(title = "Baro compensated", x = "Date", y = "Compensated Water Level (m)")

############################
#### Look at it closely ####
############################
# clean beginning of data
Date1 <- as.Date("2024-08-01", "%Y-%m-%d")
Date2 <- as.Date("2024-08-08", "%Y-%m-%d")
subdf <- DVMS1[DVMS1$DateTime < Date2 & DVMS1$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-08-06 08:15:00"), linetype="dashed", color="red") 

# cut zip tie loop
Date1 <- as.Date("2024-11-15", "%Y-%m-%d")
Date2 <- as.Date("2024-11-25", "%Y-%m-%d")
subdf <- DVMS1[DVMS1$DateTime < Date2 & DVMS1$DateTime > Date1,]
# sheet says we logger was out of water at 14:29
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-11-19 00:00:00"), linetype="dashed", color="red")

# Moved up one hole
Date1 <- as.Date("2024-09-25", "%Y-%m-%d")
Date2 <- as.Date("2024-09-27", "%Y-%m-%d")
subdf <- DVMS1[DVMS1$DateTime < Date2 & DVMS1$DateTime > Date1,]
# sheet says we logger was back in the water at 15:50
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-09-26 15:50:00"), linetype="dashed", color="red")

# Downloaded level logger data
Date1 <- as.Date("2025-04-10", "%Y-%m-%d")
Date2 <- as.Date("2025-04-14", "%Y-%m-%d")
subdf <- DVMS1[DVMS1$DateTime < Date2 & DVMS1$DateTime > Date1,]
# spike
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-11 11:30:00"), linetype="dashed", color="red")

# Downloaded level logger data
Date1 <- as.Date("2025-06-03", "%Y-%m-%d")
Date2 <- as.Date("2025-06-04", "%Y-%m-%d")
subdf <- DVMS1[DVMS1$DateTime < Date2 & DVMS1$DateTime > Date1,]
# spike
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-03 13:15:00"), linetype="dashed", color="red")

# Level logger data download, general maintenance
Date1 <- as.Date("2024-11-13", "%Y-%m-%d")
Date2 <- as.Date("2024-11-15", "%Y-%m-%d")
subdf <- DVMS1[DVMS1$DateTime < Date2 & DVMS1$DateTime > Date1,]
# spike
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-11-14 11:00:00"), linetype="dashed", color="red")

######################################################################
#### Remove times where PT was out of the water and error section ####
######################################################################
# now NA the time when the PT was out of water 
time1 <- as.POSIXct("2024-08-06 08:15:00")
time2 <- as.POSIXct("2024-08-06 00:00:00")
time3 <- as.POSIXct("2025-04-11 11:45:00")
time4 <- as.POSIXct("2025-04-11 11:30:00")
time5 <- as.POSIXct("2025-06-03 13:15:00")
time6 <- as.POSIXct("2024-11-14 11:00:00")

DVMS1 <- DVMS1 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time1, NA, Baro_Cor_Lvl.m))
DVMS1 <- DVMS1 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time2, NA, Baro_Cor_Lvl.m))
DVMS1 <- DVMS1 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time3, NA, Baro_Cor_Lvl.m))
DVMS1 <- DVMS1 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time4, NA, Baro_Cor_Lvl.m))
DVMS1 <- DVMS1 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time5, NA, Baro_Cor_Lvl.m))
DVMS1 <- DVMS1 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time6, NA, Baro_Cor_Lvl.m))

# plot after cleaning
ggplot(DVMS1, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-05"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2024-10-24"), linetype="dashed", color="red") +
  labs(title = "Baro_Cor_Lvl.m", x = "Date", y = "Water Level (m)")

###########################################################################
#### Find the average Baro_Cor_Lvl.m TWO HOURS before and after the move ####
###########################################################################
# first move correction (2024-09-26 15:45:00)
move_time1 <- as.POSIXct("2024-09-26 15:50:00")

before_move1 <- DVMS1 %>%
  filter(DateTime >= (move_time1 - hours(2)) & DateTime < move_time1) %>%
  summarize(mean_before1 = mean(Baro_Cor_Lvl.m, na.rm = TRUE))

after_move1 <- DVMS1 %>%
  filter(DateTime >= move_time1 & DateTime < (move_time1 + hours(2))) %>%
  summarize(mean_after1 = mean(Baro_Cor_Lvl.m, na.rm = TRUE))

offset1 <-  after_move1$mean_after1 - before_move1$mean_before1
print(paste("Offset 1:", offset1))

# apply the first correction
DVMS1 <- DVMS1 %>%
  mutate(Baro_Cor_offset1 = if_else(DateTime >= move_time1, Baro_Cor_Lvl.m - offset1, Baro_Cor_Lvl.m))

# second move correction (2024-11-14 11:00:00)
move_time2 <- as.POSIXct("2024-11-14 11:00:00")

before_move2 <- DVMS1 %>%
  filter(DateTime >= (move_time2 - hours(2)) & DateTime < move_time2) %>%
  summarize(mean_before2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1

after_move2 <- DVMS1 %>%
  filter(DateTime >= move_time2 & DateTime < (move_time2 + hours(2))) %>%
  summarize(mean_after2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1

offset2 <- after_move2$mean_after2 - before_move2$mean_before2
print(paste("Offset 2:", offset2))

# apply the second correction
DVMS1 <- DVMS1 %>%
  mutate(Baro_Cor_offset2 = if_else(DateTime >= move_time2, Baro_Cor_offset1 - offset2, Baro_Cor_offset1))

# known offset correction (2024-11-19, 1.50 cm)
move_time3 <- as.POSIXct("2024-11-19 14:29:00")
offset3 <- -0.0150  # m offset from field notes

# apply the third correction
DVMS1 <- DVMS1 %>%
  mutate(Baro_Cor_offset3 = if_else(DateTime >= move_time3,
                                    Baro_Cor_offset2 - offset3,
                                    Baro_Cor_offset2))

### move correction to set everything at cero
# Subtracting 0.05 moves the entire line down
DVMS1 <- DVMS1 %>%
  mutate(Final_Corrected_Lvl = Baro_Cor_offset3 - 0.6)

##############################
#### Plot with Correction ####
##############################
ggplot(DVMS1, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (No Correction)", x = "Date", y = "Water Level (m)")

ggplot(DVMS1, aes(x = DateTime, y = Baro_Cor_offset1)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (First Correction)", x = "Date", y = "Water Level (m)")

ggplot(DVMS1, aes(x = DateTime, y = Baro_Cor_offset2)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (Second Correction)", x = "Date", y = "Water Level (m)")

ggplot(DVMS1, aes(x = DateTime, y = Baro_Cor_offset3)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (Second Correction)", x = "Date", y = "Water Level (m)")

 ggplot(DVMS1, aes(x = DateTime, y = Final_Corrected_Lvl)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (Second Correction)", x = "Date", y = "Water Level (m)")

# discharge from L/s to m3/s in whole data set
DVMS1$Q..L.s. <- as.numeric(DVMS1$Q..L.s.)
DVMS1 <- DVMS1 %>%
  mutate(Q.m3s = Q..L.s./1000)

# filter out rows with missing stage or discharge
new_rating_data <- DVMS1 %>% 
  filter(!is.na(Baro_Cor_offset1), !is.na(Q.m3s))

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

ggplot(new_rating_data, aes(x = Final_Corrected_Lvl, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Stage vs. Discharge (Second Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

####################################
#### Plot level with correction ####
####################################
new_level_data <- DVMS1 %>% 
  filter(!is.na(Baro_Cor_offset1), !is.na(Actual_Water_Depth_m))

ggplot(new_level_data, aes(x = Final_Corrected_Lvl, y = Actual_Water_Depth_m)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Baro-corrected level vs. Actual water depth at sensor (Second Correction)", x = "Baro-corrected level (m)", y = "Water depth  (m)") +
  theme_minimal()

########################
#### Plot close ups ####
########################
#take the subset of the data for September when PT was moved
Date1 <- as.Date("2024-09-26", "%Y-%m-%d")
Date2 <- as.Date("2024-09-27", "%Y-%m-%d")
subdf <- DVMS1[DVMS1$DateTime < Date2 & DVMS1$DateTime > Date1,]

ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-09-26 12:00:00"), linetype="dashed", color="red") 
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset1)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-09-26 12:00:00"), linetype="dashed", color="red") 

#take the subset of the data for November when PT was moved
Date1 <- as.Date("2024-11-14", "%Y-%m-%d")
Date2 <- as.Date("2024-11-15", "%Y-%m-%d")
subdf <- DVMS1[DVMS1$DateTime < Date2 & DVMS1$DateTime > Date1,]

ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-11-14 11:00:00"), linetype="dashed", color="red") 
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset2)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-11-14 11:00:00"), linetype="dashed", color="red") 

#take the subset of the data for November when PT was moved
Date1 <- as.Date("2025-05-01", "%Y-%m-%d")
Date2 <- as.Date("2025-06-30", "%Y-%m-%d")
subdf <- DVMS1[DVMS1$DateTime < Date2 & DVMS1$DateTime > Date1,]

ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-23 08:00:00"), linetype="dashed", color="red") 
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset3)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-23 08:00:00"), linetype="dashed", color="red") 

##########################################################
#### Standardize final corrected column name ####
##########################################################
# alias the fully offset-corrected stage to a consistent name so downstream
# steps (depth conversion, rating curve) don't need to know how many
# # site-specific corrections were applied
# DVMS1 <- DVMS1 %>%
#   mutate(Final_Corrected_Lvl = Baro_Cor_offset3)

###################
#### Save file ####
###################
write.csv(DVMS1, "data/offset_DVMS1.csv")

# this is the "offset" folder
drive_folder_id <- "136WGq6adaNROjaJN2YL63yJExKikM81A"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/offset_DVMS1.csv",
  path = as_id(drive_folder_id)
)

