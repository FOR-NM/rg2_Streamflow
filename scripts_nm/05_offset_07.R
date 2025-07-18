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

########################################
#### Plot pressure compensated data ####
########################################
ggplot(data = USF07, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() + ggtitle("USF07 compensated level data")

ggplot(data = USF07, aes(x = DateTime, y = LELVEL.m)) +
  geom_line() + ggtitle("USF07 level data")

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
  geom_vline(xintercept = as.POSIXct("2024-10-24 12:00:00"), linetype="dashed", color="red") 
# sheet says we got to the site at 12:00:00
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-24 12:00:00"), linetype="dashed", color="red") 

# Date1 <- as.Date("2024-07-29", "%Y-%m-%d")
# Date2 <- as.Date("2024-07-31", "%Y-%m-%d")
# subdf <- USF07[USF07$DateTime < Date2 & USF07$DateTime > Date1,]
# 
# ggplot(data=subdf, aes(DateTime,LEVEL)) + geom_line() +
#   geom_vline(xintercept = as.POSIXct("2024-07-30 15:30:00"), linetype="dashed", color="red") 

# Date1 <- as.Date("2025-02-10", "%Y-%m-%d")
# Date2 <- as.Date("2025-02-14", "%Y-%m-%d")
# subdf <- USF07[USF07$DateTime < Date2 & USF07$DateTime > Date1,]
# 
# ggplot(data=subdf, aes(DateTime,LEVEL)) + geom_line() +
#   geom_vline(xintercept = as.POSIXct("2024-07-30 15:00:00"), linetype="dashed", color="red") 

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

# now NA the time when the PT was out of water 
time1 <- as.POSIXct("2024-10-24 12:15:00")
time2 <- as.POSIXct("2024-10-24 14:15:00")
time3 <- as.POSIXct("2024-10-24 14:30:00")

USF07 <- USF07 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time1, NA, Baro_Cor_Lvl))
USF07 <- USF07 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time2, NA, Baro_Cor_Lvl))
USF07 <- USF07 %>%
  mutate(Baro_Cor_Lvl = ifelse(DateTime == time3, NA, Baro_Cor_Lvl))

# plot after cleaning
ggplot(USF07, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-05"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2024-10-24"), linetype="dashed", color="red") +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")

###########################################################################
#### Find the average Baro_Cor_Lvl TWO HOURS before and after the move ####
###########################################################################
# first move correction (2024-10-24 12:15:00)
move_time1 <- as.POSIXct("2024-10-24 12:00:00")

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

# # second move correction (2024-07-30 15:00:00)
# move_time2 <- as.POSIXct("2024-07-30 15:30:00")
# 
# before_move2 <- USF07 %>%
#   filter(DateTime >= (move_time2 - hours(2)) & DateTime < move_time2) %>%
#   summarize(mean_before2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1
# 
# after_move2 <- USF07 %>%
#   filter(DateTime >= move_time2 & DateTime < (move_time2 + hours(2))) %>%
#   summarize(mean_after2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1
# 
# offset2 <- after_move2$mean_after2 - before_move2$mean_before2
# print(paste("Offset 2:", offset2))
# 
# # apply the second correction
# USF07 <- USF07 %>%
#   mutate(Baro_Cor_offset2 = if_else(DateTime >= move_time2, Baro_Cor_offset1 - offset2, Baro_Cor_offset1))

##############################
#### Plot with Correction ####
##############################
ggplot(USF07, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (No Correction)", x = "Date", y = "Water Level (m)")

ggplot(USF07, aes(x = DateTime, y = Baro_Cor_offset1)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (First Correction)", x = "Date", y = "Water Level (m)")

# ggplot(USF07, aes(x = DateTime, y = Baro_Cor_offset2)) +
#   geom_line() +
#   labs(title = "Corrected Baro_Cor Over Time (Second Correction)", x = "Date", y = "Water Level (m)")

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

# ggplot(new_rating_data, aes(x = Baro_Cor_offset2, y = Q.m3s)) +
#   geom_point(color = "blue") +
#   geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +
#   labs(title = "Stage vs. Discharge (Second Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q L/s)") +
#   theme_minimal()

########################
#### Plot close ups ####
########################
USF07_october <- USF07%>%
  filter(month(DateTime) == 10)

# plot after cleaning 
ggplot(USF07_october, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line() +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")
ggplot(USF07_october, aes(x = DateTime, y = Baro_Cor_offset1)) +
  geom_line() +
  labs(title = "Baro_Cor_offset1", x = "Date", y = "Water Level (m)")

#take the subset of the data for October when PT was moved
Date1 <- as.Date("2024-10-24", "%Y-%m-%d")
Date2 <- as.Date("2024-10-25", "%Y-%m-%d")
subdf <- USF07[USF07$DateTime < Date2 & USF07$DateTime > Date1,]

ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-24 14:00:00"), linetype="dashed", color="red") 
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset1)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-24 12:15:00"), linetype="dashed", color="red") 

# #take the subset of the data for October when PT was moved
# Date1 <- as.Date("2024-07-30", "%Y-%m-%d")
# Date2 <- as.Date("2024-08-01", "%Y-%m-%d")
# subdf <- USF07[USF07$DateTime < Date2 & USF07$DateTime > Date1,]
# 
# ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl)) + geom_line() +
#   geom_vline(xintercept = as.POSIXct("2024-07-30 15:10:00"), linetype="dashed", color="red") 
# ggplot(data=subdf, aes(DateTime,Baro_Cor_offset2)) + geom_line() +
#   geom_vline(xintercept = as.POSIXct("2024-07-30 15:10:00"), linetype="dashed", color="red") 

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
