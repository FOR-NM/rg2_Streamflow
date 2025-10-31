##==============================================================================
## Project: QuEST
## This script is to calculate PT offset for Santa Fe USF14 site
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

#USF14
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="USF14.csv"], 
                            path = "googledrive/USF14.csv",
                            overwrite = T)
# load file
USF14 <- read.csv("googledrive/USF14.csv")

# convert Date column to Date type if not already
# combine Date and Time columns into a new DateTime column
USF14$DateTime <- paste(USF14$Date.x, USF14$Time, sep = " ")
# convert the DateTime column to POSIXct
USF14$DateTime <- as.POSIXct(USF14$DateTime, format = "%Y-%m-%d %I:%M:%S %p")

# filter out rows with missing stage or discharge
rating_data <- USF14 %>% 
  filter(!is.na(Baro_Cor_Lvl), !is.na(Q))

USF14$Q..L.s. <- as.numeric(USF14$Q)
rating_data$Q..L.s. <- as.numeric(rating_data$Q)

# make compensated backup 
USF14$Baro_backup <- USF14$Baro_Cor_Lvl

########################################
#### Plot pressure compensated data ####
########################################
# # filter out the one row with negative baro lvl value 
# USF14 <- USF14 %>% 
#   filter(Baro_Cor_Lvl >= 0)

ggplot(data = USF14, aes(x = DateTime, y = Baro_Cor_Lvl )) +
  geom_line() + ggtitle("USF14 compensated level data")
ggplot(data = USF14, aes(x = DateTime, y = LEVEL )) +
  geom_line() + ggtitle("USF14 level data")
ggplot(data = USF14, aes(x = DateTime, y = TEMPERATURE.x)) +
  geom_line() + ggtitle("USF14 temperature data")

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
ggplot(USF14, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-05"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2024-10-29"), linetype="dashed", color="red") +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")

#take the subset of the data for when PT was moved
Date1 <- as.Date("2024-12-01", "%Y-%m-%d")
Date2 <- as.Date("2025-01-15", "%Y-%m-%d")
subdf <- USF14[USF14$DateTime < Date2 & USF14$DateTime > Date1,]
# sheet says we got to the site at 13:15:00
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-29 13:00:00"), linetype="dashed", color="red") 

#take the subset of the data for when PT was moved
Date1 <- as.Date("2025-06-04", "%Y-%m-%d")
Date2 <- as.Date("2025-06-06", "%Y-%m-%d")
subdf <- USF14[USF14$DateTime < Date2 & USF14$DateTime > Date1,]
# sheet says we got to the site at 13:15:00
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-05 10:45:00"), linetype="dashed", color="red") 

####################################################
#### Remove times where PT was out of the water ####
####################################################
# # remove what looks like error section in January and part of February
# Date1 <- as.Date("2024-12-06", "%Y-%m-%d")
# Date2 <- as.Date("2025-01-18", "%Y-%m-%d")
# USF14$Baro_Cor_Lvl[USF14$DateTime >= Date1 & USF14$DateTime <= Date2] <- NA
# ggplot(data = USF14, aes(x = DateTime, y = Baro_Cor_Lvl)) +
#   geom_line() + ggtitle("USF14 compensated level data")

time1 <- as.POSIXct("2024-10-29 10:45:00")
time2 <- as.POSIXct("2024-10-29 11:00:00")
time3 <- as.POSIXct("2025-06-05 10:45:40")

USF14 <- USF14 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time1, NA, Baro_Cor_Lvl))
USF14 <- USF14 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time2, NA, Baro_Cor_Lvl))
USF14 <- USF14 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time3, NA, Baro_Cor_Lvl))

# plot after cleaning
ggplot(USF14, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")

###########################################################################
#### Find the average Baro_Cor_Lvl TWO HOURS before and after the move ####
###########################################################################
# first move correction (2024-10-29 11:15:00)
move_time1 <- as.POSIXct("2024-10-29 10:30:00")
before_move1 <- USF14 %>%
  filter(DateTime >= (move_time1 - hours(2)) & DateTime < move_time1) %>%
  summarize(mean_before1 = mean(Baro_Cor_Lvl, na.rm = TRUE))
after_move1 <- USF14 %>%
  filter(DateTime >= move_time1 & DateTime < (move_time1 + hours(2))) %>%
  summarize(mean_after1 = mean(Baro_Cor_Lvl, na.rm = TRUE))
offset1 <-  after_move1$mean_after1 - before_move1$mean_before1
print(paste("Offset 1:", offset1))
# apply the first correction
USF14 <- USF14 %>%
  mutate(Baro_Cor_offset1 = if_else(DateTime >= move_time1, Baro_Cor_Lvl - offset1, Baro_Cor_Lvl))

# second move correction (2025-06-05 10:30:00)
move_time2 <- as.POSIXct("2025-06-05 11:00:40")
before_move2 <- USF14 %>%
  filter(DateTime >= (move_time2 - hours(2)) & DateTime < move_time2) %>%
  summarize(mean_before2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1
after_move2 <- USF14 %>%
  filter(DateTime >= move_time2 & DateTime < (move_time2 + hours(2))) %>%
  summarize(mean_after2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1
offset2 <- after_move2$mean_after2 - before_move2$mean_before2
print(paste("Offset 2:", offset2))
# apply the second correction
USF14 <- USF14 %>%
  mutate(Baro_Cor_offset2 = if_else(DateTime >= move_time2, Baro_Cor_offset1 - offset2, Baro_Cor_offset1))

##############################
#### Plot with Correction ####
##############################
ggplot(USF14, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (No Correction)", x = "Date", y = "Water Level (m)")
ggplot(USF14, aes(x = DateTime, y = Baro_Cor_offset1)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (First Correction)", x = "Date", y = "Water Level (m)")
ggplot(USF14, aes(x = DateTime, y = Baro_Cor_offset2)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (Second Correction)", x = "Date", y = "Water Level (m)")

# discharge from L/s to m3/s in whole data set
USF14$Q..L.s. <- as.numeric(USF14$Q..L.s.)
USF14 <- USF14 %>%
  mutate(Q.m3s = Q..L.s./1000)

# filter out rows with missing stage or discharge
new_rating_data <- USF14 %>% 
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

########################
#### Plot close ups ####
########################
Date1 <- as.Date("2024-10-27", "%Y-%m-%d")
Date2 <- as.Date("2024-10-30", "%Y-%m-%d")
subdf <- USF14[USF14$DateTime < Date2 & USF14$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-29 10:30:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset2)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-29 10:30:00"), linetype="dashed", color="red")

Date1 <- as.Date("2025-06-04", "%Y-%m-%d")
Date2 <- as.Date("2025-06-07", "%Y-%m-%d")
subdf <- USF14[USF14$DateTime < Date2 & USF14$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-05 10:00:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset2)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-05 10:00:00"), linetype="dashed", color="red")

###################
#### Save file ####
###################
write.csv(USF14, "data/offset_USF14.csv")

drive_folder_id <- "1VIonkS5GXUsn34FEPu1lpkMgsgCvPXFw"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/offset_USF14.csv",
  path = as_id(drive_folder_id)
)

