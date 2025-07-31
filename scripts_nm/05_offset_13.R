##==============================================================================
## Project: QuEST
## This script is to calculate PT offset for Santa Fe USF13 site
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

#USF13
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="USF13.csv"], 
                            path = "googledrive/USF13.csv",
                            overwrite = T)
# load file
USF13 <- read.csv("googledrive/USF13.csv")

# convert Date column to Date type if not already
# combine Date and Time columns into a new DateTime column
USF13$DateTime <- paste(USF13$Date.x, USF13$Time, sep = " ")
# convert the DateTime column to POSIXct
USF13$DateTime <- as.POSIXct(USF13$DateTime, format = "%Y-%m-%d %I:%M:%S %p")

# filter out rows with missing stage or discharge
rating_data <- USF13 %>% 
  filter(!is.na(Baro_Cor_Lvl), !is.na(Q))

USF13$Q..L.s. <- as.numeric(USF13$Q)
rating_data$Q..L.s. <- as.numeric(rating_data$Q)

# make compensated backup 
USF13$Baro_backup <- USF13$Baro_Cor_Lvl

########################################
#### Plot pressure compensated data ####
########################################
ggplot(data = USF13, aes(x = DateTime, y = Baro_Cor_Lvl )) +
  geom_line() + ggtitle("USF13 compensated level data")

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
ggplot(USF13, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-05"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2024-10-29"), linetype="dashed", color="red") +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")

Date1 <- as.Date("2024-10-28", "%Y-%m-%d")
Date2 <- as.Date("2024-10-30", "%Y-%m-%d")
subdf <- USF13[USF13$DateTime < Date2 & USF13$DateTime > Date1,]

ggplot(data=subdf, aes(DateTime,LEVEL)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-29 12:00:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-29 12:30:00"), linetype="dashed", color="red")

####################################################
#### Remove times where PT was out of the water ####
####################################################
# time1 <- as.POSIXct("2025-06-05 10:15:00")
# time2 <- as.POSIXct("2024-10-29 11:45:00")
time3 <- as.POSIXct("2024-10-29 12:30:00")

# USF13 <- USF13 %>%
#   mutate(Baro_Cor_Lvl = ifelse(DateTime == time1, NA, Baro_Cor_Lvl))
# USF13 <- USF13 %>%
#   mutate(Baro_Cor_Lvl = ifelse(DateTime == time2, NA, Baro_Cor_Lvl))
USF13 <- USF13 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time3, NA, Baro_Cor_Lvl))
# 
# # plot after cleaning 
# ggplot(USF13, aes(x = DateTime, y = Baro_Cor_Lvl)) +
#   geom_line() +
#   geom_vline(xintercept = as.POSIXct("2025-06-05"), linetype="dashed", color="red") +
#   geom_vline(xintercept = as.POSIXct("2024-10-29"), linetype="dashed", color="red") +
#   labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")

###########################################################################
#### Find the average Baro_Cor_Lvl TWO HOURS before and after the move ####
###########################################################################
# first move correction (2024-10-29 10:30:00)
move_time1 <- as.POSIXct("2024-10-29 11:45:00")

before_move1 <- USF13 %>%
  filter(DateTime >= (move_time1 - hours(2)) & DateTime < move_time1) %>%
  summarize(mean_before1 = mean(Baro_Cor_Lvl, na.rm = TRUE))

after_move1 <- USF13 %>%
  filter(DateTime >= move_time1 & DateTime < (move_time1 + hours(2))) %>%
  summarize(mean_after1 = mean(Baro_Cor_Lvl, na.rm = TRUE))

offset1 <-  after_move1$mean_after1 - before_move1$mean_before1
print(paste("Offset 1:", offset1))

# apply the first correction
USF13 <- USF13 %>%
  mutate(Baro_Cor_offset1 = if_else(DateTime >= move_time1, Baro_Cor_Lvl - offset1, Baro_Cor_Lvl))

# # second move correction (2025-06-05 08:30:00)
# move_time2 <- as.POSIXct("2025-06-05 08:30:00")
# 
# before_move2 <- USF13 %>%
#   filter(DateTime >= (move_time2 - hours(2)) & DateTime < move_time2) %>%
#   summarize(mean_before2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1
# 
# after_move2 <- USF13 %>%
#   filter(DateTime >= move_time2 & DateTime < (move_time2 + hours(2))) %>%
#   summarize(mean_after2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1
# 
# offset2 <- after_move2$mean_after2 - before_move2$mean_before2
# print(paste("Offset 2:", offset2))
# 
# # apply the second correction
# USF13 <- USF13 %>%
#   mutate(Baro_Cor_offset2 = if_else(DateTime >= move_time2, Baro_Cor_offset1 - offset2, Baro_Cor_offset1))

##############################
#### Plot with Correction ####
##############################
ggplot(USF13, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (No Correction)", x = "Date", y = "Water Level (m)")

ggplot(USF13, aes(x = DateTime, y = Baro_Cor_offset1)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (First Correction)", x = "Date", y = "Water Level (m)")

# ggplot(USF13, aes(x = DateTime, y = Baro_Cor_offset2)) +
#   geom_line() +
#   labs(title = "Corrected Baro_Cor Over Time (Second Correction)", x = "Date", y = "Water Level (m)")

# discharge from L/s to m3/s in whole data set
USF13$Q..L.s. <- as.numeric(USF13$Q..L.s.)
USF13 <- USF13 %>%
  mutate(Q.m3s = Q..L.s./1000)

# filter out rows with missing stage or discharge
new_rating_data <- USF13 %>% 
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
USF13_october <- USF13%>%
  filter(month(DateTime) == 10)

# plot after cleaning 
ggplot(USF13_october, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")
ggplot(USF13_october, aes(x = DateTime, y = Baro_Cor_offset1)) +
  geom_line() +
  labs(title = "Baro_Cor_offset1", x = "Date", y = "Water Level (m)")

###################
#### Save file ####
###################
write.csv(USF13, "data/offset_USF13.csv")

drive_folder_id <- "1VIonkS5GXUsn34FEPu1lpkMgsgCvPXFw"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/offset_USF13.csv",
  path = as_id(drive_folder_id)
)
