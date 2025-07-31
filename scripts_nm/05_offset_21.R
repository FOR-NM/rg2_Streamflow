##==============================================================================
## Project: QuEST
## This script is to calculate PT offset for Santa Fe USF21 site
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

#USF21
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="USF21.csv"], 
                            path = "googledrive/USF21.csv",
                            overwrite = T)
# load file
USF21 <- read.csv("googledrive/USF21.csv")

# convert Date column to Date type if not already
# combine Date and Time columns into a new DateTime column
USF21$DateTime <- paste(USF21$Date.x, USF21$Time, sep = " ")
# convert the DateTime column to POSIXct
USF21$DateTime <- as.POSIXct(USF21$DateTime, format = "%Y-%m-%d %I:%M:%S %p")

# filter out rows with missing stage or discharge
rating_data <- USF21 %>% 
  filter(!is.na(Baro_Cor_Lvl), !is.na(Q))

USF21$Q..L.s. <- as.numeric(USF21$Q)
rating_data$Q..L.s. <- as.numeric(rating_data$Q)

# make compensated backup 
USF21$Baro_backup <- USF21$Baro_Cor_Lvl

########################################
#### Plot pressure compensated data ####
########################################
ggplot(data = USF21, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() + ggtitle("USF21 compensated level data")

ggplot(data = USF21, aes(x = DateTime, y = LEVEL)) +
  geom_line() + ggtitle("USF21 level data")

ggplot(data = USF21, aes(x = DateTime, y = TEMPERATURE.x)) +
  geom_line() + ggtitle("USF21 temperature data")

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
ggplot(USF21, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-05"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2024-10-29"), linetype="dashed", color="red") +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")

# look at it closely, this point is off
Date1 <- as.Date("2025-05-06", "%Y-%m-%d")
Date2 <- as.Date("2025-05-07", "%Y-%m-%d")
subdf <- USF21[USF21$DateTime < Date2 & USF21$DateTime > Date1,]

ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-06-19 10:30:00"), linetype="dashed", color="red")

# look at it closely, this point is off
Date1 <- as.Date("2024-12-10", "%Y-%m-%d")
Date2 <- as.Date("2024-12-22", "%Y-%m-%d")
subdf <- USF21[USF21$DateTime < Date2 & USF21$DateTime > Date1,]

ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-06-19 10:30:00"), linetype="dashed", color="red")

####################################################
#### Remove times where PT was out of the water ####
####################################################
time1 <- as.POSIXct("2025-06-05 10:15:00")
time2 <- as.POSIXct("2024-10-29 10:30:00")
time3 <- as.POSIXct("2024-10-29 12:30:00")

USF21 <- USF21 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time1, NA, Baro_Cor_Lvl))
USF21 <- USF21 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time2, NA, Baro_Cor_Lvl))
USF21 <- USF21 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time3, NA, Baro_Cor_Lvl))

# plot after cleaning 
ggplot(USF21, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-05"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2024-10-29"), linetype="dashed", color="red") +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")

Date1 <- as.Date("2025-05-06", "%Y-%m-%d")
Date2 <- as.Date("2025-05-07", "%Y-%m-%d")
USF21$Baro_Cor_Lvl[USF21$DateTime >= Date1 & USF21$DateTime <= Date2] <- NA

ggplot(data = USF21, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() + ggtitle("USF21 compensated level data")

Date1 <- as.Date("2024-12-10", "%Y-%m-%d")
Date2 <- as.Date("2024-12-22", "%Y-%m-%d")
USF21$Baro_Cor_Lvl[USF21$DateTime >= Date1 & USF21$DateTime <= Date2] <- NA

ggplot(data = USF21, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() + ggtitle("USF21 compensated level data")

###########################################################################
#### Find the average Baro_Cor_Lvl TWO HOURS before and after the move ####
###########################################################################
# first move correction (2024-10-29 10:30:00)
move_time1 <- as.POSIXct("2024-10-29 10:30:00")

before_move1 <- USF21 %>%
  filter(DateTime >= (move_time1 - hours(2)) & DateTime < move_time1) %>%
  summarize(mean_before1 = mean(Baro_Cor_Lvl, na.rm = TRUE))

after_move1 <- USF21 %>%
  filter(DateTime >= move_time1 & DateTime < (move_time1 + hours(2))) %>%
  summarize(mean_after1 = mean(Baro_Cor_Lvl, na.rm = TRUE))

offset1 <-  after_move1$mean_after1 - before_move1$mean_before1
print(paste("Offset 1:", offset1))

# apply the first correction
USF21 <- USF21 %>%
  mutate(Baro_Cor_offset1 = if_else(DateTime >= move_time1, Baro_Cor_Lvl - offset1, Baro_Cor_Lvl))

# # second move correction (2025-06-05 08:30:00)
# move_time2 <- as.POSIXct("2025-06-05 08:30:00")
# 
# before_move2 <- USF21 %>%
#   filter(DateTime >= (move_time2 - hours(2)) & DateTime < move_time2) %>%
#   summarize(mean_before2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1
# 
# after_move2 <- USF21 %>%
#   filter(DateTime >= move_time2 & DateTime < (move_time2 + hours(2))) %>%
#   summarize(mean_after2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1
# 
# offset2 <- after_move2$mean_after2 - before_move2$mean_before2
# print(paste("Offset 2:", offset2))
# 
# # apply the second correction
# USF21 <- USF21 %>%
#   mutate(Baro_Cor_offset2 = if_else(DateTime >= move_time2, Baro_Cor_offset1 - offset2, Baro_Cor_offset1))

##############################
#### Plot with Correction ####
##############################
ggplot(USF21, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (No Correction)", x = "Date", y = "Water Level (m)")

ggplot(USF21, aes(x = DateTime, y = Baro_Cor_offset1)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (First Correction)", x = "Date", y = "Water Level (m)")

# ggplot(USF21, aes(x = DateTime, y = Baro_Cor_offset2)) +
#   geom_line() +
#   labs(title = "Corrected Baro_Cor Over Time (Second Correction)", x = "Date", y = "Water Level (m)")

# discharge from L/s to m3/s in whole data set
USF21$Q..L.s. <- as.numeric(USF21$Q..L.s.)
USF21 <- USF21 %>%
  mutate(Q.m3s = Q..L.s./1000)

# filter out rows with missing stage or discharge
new_rating_data <- USF21 %>% 
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

# ggplot(new_rating_data, aes(x = Baro_Cor_offset2, y = Q.m3s)) +
#   geom_point(color = "blue") +
#   geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
#   labs(title = "Stage vs. Discharge (Second Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q L/s)") +
#   theme_minimal()

########################
#### Plot close ups ####
########################
USF21_october <- USF21%>%
  filter(month(DateTime) == 10)

# plot after cleaning 
ggplot(USF21_october, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")
ggplot(USF21_october, aes(x = DateTime, y = Baro_Cor_offset1)) +
  geom_line() +
  labs(title = "Baro_Cor_offset1", x = "Date", y = "Water Level (m)")

###################
#### Save file ####
###################
write.csv(USF21, "data/offset_USF21.csv")

drive_folder_id <- "1VIonkS5GXUsn34FEPu1lpkMgsgCvPXFw"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/offset_USF21.csv",
  path = as_id(drive_folder_id)
)
