##==============================================================================
## Project: QuEST
## This script is to calculate PT offset for Santa Fe USF07 site
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
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1EswIfUWCK6bsdcs-ZrAMGW1oYKs4B0Eh")

# list all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3

#USF07
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="USF07.csv"], 
                            path = "googledrive/USF07.csv",
                            overwrite = T)
# load file
USF07 <- read.csv("googledrive/USF07.csv")

# convert Date column to Date type if not already
# combine Date and Time columns into a new DateTime column
USF07$DateTime <- paste(USF07$Date.x, USF07$Time, sep = " ")
# convert the DateTime column to POSIXct
USF07$DateTime <- as.POSIXct(USF07$DateTime, format = "%Y-%m-%d %I:%M:%S %p")

# round seconds to the 00 interval 
USF07$DateTime <- floor_date(USF07$DateTime, unit="minute")

# filter out rows with missing stage or discharge
rating_data <- USF07 %>% 
  filter(!is.na(Baro_Cor_Lvl), !is.na(Q))

USF07$Q..L.s. <- as.numeric(USF07$Q)
rating_data$Q..L.s. <- as.numeric(rating_data$Q)

# make compensated backup 
USF07$Baro_backup <- USF07$Baro_Cor_Lvl

########################################
#### Plot pressure compensated data ####
########################################
ggplot(data = USF07, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() + ggtitle("USF07 compensated level data")
ggplot(data = USF07, aes(x = DateTime, y = LELVEL.m)) +
  geom_line() + ggtitle("USF07 level data")
ggplot(data = USF07, aes(x = DateTime, y = TEMPERATURE)) +
  geom_line() + ggtitle("USF07 temperature data")

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
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m³/s)") +
  theme_minimal()

# pt depth from cm to m
rating_data <- rating_data %>%
  mutate(pt_depth_m = pt_depth_cm/100)

# plot discharge vs manual stage measurement
ggplot(rating_data, aes(x = pt_depth_m, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Manual Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

#################################################
#### Find offset, when did the change happen ####
#################################################
ggplot(USF07, aes(x = DateTime, y = LEVEL)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-07-30"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2024-10-24"), linetype="dashed", color="red") +
  labs(title = "LEVEL", x = "Date", y = "Water Level (m)")

ggplot(USF07, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-07-30"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2024-10-24"), linetype="dashed", color="red") +
  labs(title = "LEVEL", x = "Date", y = "Water Level (m)")

############################
#### Look at it closely ####
############################
#take the subset of the data for when PT was moved
Date1 <- as.Date("2024-10-24", "%Y-%m-%d")
Date2 <- as.Date("2024-10-25", "%Y-%m-%d")
subdf <- USF07[USF07$DateTime < Date2 & USF07$DateTime > Date1,]
# sheet says we got to the site at 12:00:00
ggplot(data=subdf, aes(DateTime,LEVEL)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-24 15:00:00"), linetype="dashed", color="red") 
# sheet says we got to the site at 12:00:00
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-24 15:00:00"), linetype="dashed", color="red") 

Date1 <- as.Date("2024-12-01", "%Y-%m-%d")
Date2 <- as.Date("2025-03-05", "%Y-%m-%d")
subdf <- USF07[USF07$DateTime < Date2 & USF07$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,LEVEL)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-04 15:30:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-04 15:30:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,TEMPERATURE)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-04 15:30:00"), linetype="dashed", color="red")

Date1 <- as.Date("2024-07-08", "%Y-%m-%d")
Date2 <- as.Date("2024-07-10", "%Y-%m-%d")
subdf <- USF07[USF07$DateTime < Date2 & USF07$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,LEVEL)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-07-08 15:00:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-07-08 19:15:00"), linetype="dashed", color="red")

Date1 <- as.Date("2024-05-23", "%Y-%m-%d")
Date2 <- as.Date("2024-05-24", "%Y-%m-%d")
subdf <- USF07[USF07$DateTime < Date2 & USF07$DateTime > Date1,]
# records say logger was moved at 15:00
ggplot(data=subdf, aes(DateTime,LEVEL)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-05-23 15:00:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-05-23 15:45:00"), linetype="dashed", color="red")

Date1 <- as.Date("2024-10-16", "%Y-%m-%d")
Date2 <- as.Date("2024-10-17", "%Y-%m-%d")
subdf <- USF07[USF07$DateTime < Date2 & USF07$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,LEVEL)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-16 10:45:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-16 10:45:00"), linetype="dashed", color="red")

Date1 <- as.Date("2025-06-16", "%Y-%m-%d")
Date2 <- as.Date("2025-06-17", "%Y-%m-%d")
subdf <- USF07[USF07$DateTime < Date2 & USF07$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-16 12:19:00"), linetype="dashed", color="red")

Date1 <- as.Date("2025-09-12", "%Y-%m-%d")
Date2 <- as.Date("2025-09-14", "%Y-%m-%d")
subdf <- USF07[USF07$DateTime < Date2 & USF07$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-09-12 11:30:00"), linetype="dashed", color="red")

Date1 <- as.Date("2025-02-04", "%Y-%m-%d")
Date2 <- as.Date("2025-02-06", "%Y-%m-%d")
subdf <- USF07[USF07$DateTime < Date2 & USF07$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-09-12 11:30:00"), linetype="dashed", color="red")

######################################################################
#### Remove times where PT was out of the water and error section ####
######################################################################
# remove what looks like error section in January and part of February
Date1 <- as.Date("2025-01-07", "%Y-%m-%d")
Date2 <- as.Date("2025-02-05", "%Y-%m-%d")

USF07$Baro_Cor_Lvl[USF07$DateTime >= Date1 & USF07$DateTime <= Date2] <- NA

# plot after cleaning
ggplot(USF07, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-01-07"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2025-02-05"), linetype="dashed", color="red") +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")

# remove error section in July 8th
Date1 <- as.POSIXct("2024-07-08 15:00:00", "%Y-%m-%d %H:%M:%S", tz = "America/Denver")
Date2 <- as.POSIXct("2024-07-08 19:00:00", "%Y-%m-%d %H:%M:%S", tz = "America/Denver")

USF07$Baro_Cor_Lvl[USF07$DateTime >= Date1 & USF07$DateTime <= Date2] <- NA

# now NA the time when the PT was out of water 
time1 <- as.POSIXct("2024-10-24 12:15:00")
time2 <- as.POSIXct("2024-10-24 14:15:00")
time3 <- as.POSIXct("2024-10-24 14:30:00")
time4 <- as.POSIXct("2024-10-24 14:00:00")
time5 <- as.POSIXct("2025-06-16 12:19:00")

USF07 <- USF07 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time1, NA, Baro_Cor_Lvl))
USF07 <- USF07 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time2, NA, Baro_Cor_Lvl))
USF07 <- USF07 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time3, NA, Baro_Cor_Lvl))
USF07 <- USF07 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time4, NA, Baro_Cor_Lvl))
USF07 <- USF07 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time5, NA, Baro_Cor_Lvl))

# plot after cleaning
ggplot(USF07, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-05"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2024-10-24"), linetype="dashed", color="red") +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")

# get rid of 2024-10-16.. pissing me off
time5 <- as.POSIXct("2024-10-16 11:00:00")
time6 <- as.POSIXct("2024-07-30 12:45:00")

USF07 <- USF07 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time5, NA, Baro_Cor_Lvl))
USF07 <- USF07 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time6, NA, Baro_Cor_Lvl))

###########################################################################
#### Find the average Baro_Cor_Lvl TWO HOURS before and after the move ####
###########################################################################
# first move correction (2024-10-24 12:15:00)
move_time1 <- as.POSIXct("2024-10-24 11:00:00")
before_move1 <- USF07 %>%
  filter(DateTime >= (move_time1 - hours(2)) & DateTime < move_time1) %>%
  summarize(mean_before1 = mean(Baro_Cor_Lvl, na.rm = TRUE))
after_move1 <- USF07 %>%
  filter(DateTime >= move_time1 & DateTime < (move_time1 + hours(2))) %>%
  summarize(mean_after1 = mean(Baro_Cor_Lvl, na.rm = TRUE))
offset1 <-  after_move1$mean_after1 - before_move1$mean_before1
print(paste("Offset 1:", offset1))

# apply the first correction
USF07 <- USF07 %>%
  mutate(Baro_Cor_offset1 = if_else(DateTime >= move_time1, Baro_Cor_Lvl - offset1, Baro_Cor_Lvl))
# second move correction (2024-05-23 15:00:00)
move_time2 <- as.POSIXct("2024-05-23 15:15:00")
before_move2 <- USF07 %>%
  filter(DateTime >= (move_time2 - hours(2)) & DateTime < move_time2) %>%
  summarize(mean_before2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1
after_move2 <- USF07 %>%
  filter(DateTime >= move_time2 & DateTime < (move_time2 + hours(2))) %>%
  summarize(mean_after2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1
offset2 <- after_move2$mean_after2 - before_move2$mean_before2
print(paste("Offset 2:", offset2))
# apply the second correction
USF07 <- USF07 %>%
  mutate(Baro_Cor_offset2 = if_else(DateTime >= move_time2, Baro_Cor_offset1 - offset2, Baro_Cor_offset1))

# third move correction (2024-07-08 15:30:00)
move_time3 <- as.POSIXct("2024-07-08 14:45:00")
before_move3 <- USF07 %>%
  filter(DateTime >= (move_time3 - hours(2)) & DateTime < move_time3) %>%
  summarize(mean_before3 = mean(Baro_Cor_offset2, na.rm = TRUE))
after_move3 <- USF07 %>%
  filter(DateTime >= move_time3 & DateTime < (move_time3 + hours(2))) %>%
  summarize(mean_after3 = mean(Baro_Cor_offset2, na.rm = TRUE))
offset3 <- after_move3$mean_after3 - before_move3$mean_before3
print(paste("Offset 3:", offset3))
# apply the third correction
USF07 <- USF07 %>%
  mutate(Baro_Cor_offset3 = if_else(DateTime >= move_time3, Baro_Cor_offset2 - offset3, Baro_Cor_offset2))

# fourth move correction
move_time4 <- as.POSIXct("2025-09-12 11:30:00")
before_move4 <- USF07 %>%
  filter(DateTime >= (move_time4 - hours(2)) & DateTime < move_time4) %>%
  summarize(mean_before4 = mean(Baro_Cor_offset3, na.rm = TRUE))
after_move4 <- USF07 %>%
  filter(DateTime >= move_time4 & DateTime < (move_time4 + hours(2))) %>%
  summarize(mean_after4 = mean(Baro_Cor_offset3, na.rm = TRUE))
offset4 <- after_move4$mean_after4 - before_move4$mean_before4
print(paste("Offset 4:", offset4))
# apply the fourth correction
USF07 <- USF07 %>%
  mutate(Baro_Cor_offset4 = if_else(DateTime >= move_time4, Baro_Cor_offset3 - offset4, Baro_Cor_offset3))

# fourth (winter) move correction
move_time5 <- as.POSIXct("2025-01-06 23:45:00")
before_move5 <- USF07 %>%
  filter(DateTime >= (move_time5 - hours(2)) & DateTime < move_time5) %>%
  summarize(mean_before5 = mean(Baro_Cor_offset4, na.rm = TRUE))
move_time5 <- as.POSIXct("2025-02-05 00:00:00")
after_move5 <- USF07 %>%
  filter(DateTime >= move_time5 & DateTime < (move_time5 + hours(2))) %>%
  summarize(mean_after5 = mean(Baro_Cor_offset4, na.rm = TRUE))
offset5 <- after_move5$mean_after5 - before_move5$mean_before5
print(paste("Offset 5:", offset5))
# apply the fourth correction
USF07 <- USF07 %>%
  mutate(Baro_Cor_offset5 = if_else(DateTime >= move_time5, Baro_Cor_offset4 - offset5, Baro_Cor_offset4))

##############################
#### Plot with Correction ####
##############################
ggplot(USF07, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (No Correction)", x = "Date", y = "Water Level (m)")
ggplot(USF07, aes(x = DateTime, y = Baro_Cor_offset1)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (First Correction)", x = "Date", y = "Water Level (m)")
ggplot(USF07, aes(x = DateTime, y = Baro_Cor_offset2)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (Second Correction)", x = "Date", y = "Water Level (m)")
ggplot(USF07, aes(x = DateTime, y = Baro_Cor_offset3)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (Third Correction)", x = "Date", y = "Water Level (m)")
ggplot(USF07, aes(x = DateTime, y = Baro_Cor_offset4)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (Fourth Correction)", x = "Date", y = "Water Level (m)")
ggplot(USF07, aes(x = DateTime, y = Baro_Cor_offset5)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (Fourth Correction)", x = "Date", y = "Water Level (m)")

# discharge from L/s to m3/s in whole data set
USF07$Q..L.s. <- as.numeric(USF07$Q..L.s.)
USF07 <- USF07 %>%
  mutate(Q.m3s = Q..L.s./1000)

# filter out rows with missing stage or discharge
new_rating_data <- USF07 %>% 
  filter(!is.na(Baro_Cor_offset1), !is.na(Q.m3s))

ggplot(new_rating_data, aes(x = Baro_Cor_Lvl, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Stage vs. Discharge (No Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q L/s)") +
  theme_minimal()
ggplot(new_rating_data, aes(x = Baro_Cor_offset1, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Stage vs. Discharge (First Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q L/s)") +
  theme_minimal()
ggplot(new_rating_data, aes(x = Baro_Cor_offset2, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Stage vs. Discharge (Second Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q L/s)") +
  theme_minimal()
ggplot(new_rating_data, aes(x = Baro_Cor_offset3, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Stage vs. Discharge (Third Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q L/s)") +
  theme_minimal()
ggplot(new_rating_data, aes(x = Baro_Cor_offset4, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Stage vs. Discharge (Fourth Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q L/s)") +
  theme_minimal()
ggplot(new_rating_data, aes(x = Baro_Cor_offset5, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
  labs(title = "Stage vs. Discharge (Fourth Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q L/s)") +
  theme_minimal()

########################
#### Plot close ups ####
########################
#take the subset of the data for October when PT was moved
Date1 <- as.Date("2024-10-24", "%Y-%m-%d")
Date2 <- as.Date("2024-10-25", "%Y-%m-%d")
subdf <- USF07[USF07$DateTime < Date2 & USF07$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-24 14:00:00"), linetype="dashed", color="red") 
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset1)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-24 12:15:00"), linetype="dashed", color="red") 

#take the subset of the data for July 
Date1 <- as.Date("2024-07-08", "%Y-%m-%d")
Date2 <- as.Date("2024-07-10", "%Y-%m-%d")
subdf <- USF07[USF07$DateTime < Date2 & USF07$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-07-08 15:15:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset3)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-07-08 19:30:00"), linetype="dashed", color="red")

#take the subset of the data for May when PT was moved
Date1 <- as.Date("2024-05-23", "%Y-%m-%d")
Date2 <- as.Date("2024-05-24", "%Y-%m-%d")
subdf <- USF07[USF07$DateTime < Date2 & USF07$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset1)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-05-23 15:15:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset2)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-05-23 15:15:00"), linetype="dashed", color="red")

Date1 <- as.Date("2025-09-10", "%Y-%m-%d")
Date2 <- as.Date("2025-09-15", "%Y-%m-%d")
subdf <- USF07[USF07$DateTime < Date2 & USF07$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-16 10:00:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset4)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-16 10:00:00"), linetype="dashed", color="red")

###################
#### Save file ####
###################
write.csv(USF07, "data/offset_USF07.csv")

# this is the "offset" folder
drive_folder_id <- "1VIonkS5GXUsn34FEPu1lpkMgsgCvPXFw"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/offset_USF07.csv",
  path = as_id(drive_folder_id)
)

