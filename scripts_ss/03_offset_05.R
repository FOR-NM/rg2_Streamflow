##==============================================================================
## Project: QuEST
## This script is to calculate PT offset for South Sandy SST05 site
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
pt <- googledrive::as_id("https://drive.google.com/drive/folders/11vn2jsiB7YEsrhjI5_NnOSTA579NMtK4")

# list all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3

#SST05
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="SST05.csv"], 
                            path = "googledrive/SST05.csv",
                            overwrite = T)
# load file
SST05 <- read.csv("googledrive/SST05.csv")

# convert Date column to Date type if not already
SST05$Date <- as.Date(SST05$Date)
SST05$DateTime <- as.POSIXct(SST05$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "MST")
head(SST05)

# filter out rows with missing stage or discharge
rating_data <- SST05 %>% 
  filter(!is.na(Baro_Cor_Lvl.m), !is.na(Q..L.s.))

# filter out the one row with negative baro lvl value 
SST05 <- SST05 %>% 
  filter(Baro_Cor_Lvl.m >= 0)

########################################
#### Plot pressure compensated data ####
########################################
ggplot(data = SST05, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() + ggtitle("SST05 compensated level data")

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
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m³/s)") +
  theme_minimal()

# pt depth from cm to m
rating_data <- rating_data %>%
  mutate(pt_depth_m = Depth.above.PT..cm....7/100)

# plot baro level vs manual stage measurement
ggplot(rating_data, aes(x = Baro_Cor_Lvl.m, y = pt_depth_m)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # adds date labels above points
  labs(title = "Baro Level vs. Manual Stage", x = "Baro Level (m)", y = "Manual Stage (m)") +
  theme_minimal()

#################################################
#### Find offset, when did the change happen ####
#################################################
ggplot(SST05, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-09-16"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2024-12-16"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2025-04-23"), linetype="dashed", color="red") +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")

############################
#### Look at it closely ####
############################
Date1 <- as.Date("2024-09-10", "%Y-%m-%d")
Date2 <- as.Date("2024-09-18", "%Y-%m-%d")
subdf <- SST05[SST05$DateTime < Date2 & SST05$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-09-16 13:30:00"), linetype="dashed", color="red") 

Date1 <- as.Date("2024-12-16", "%Y-%m-%d")
Date2 <- as.Date("2024-12-18", "%Y-%m-%d")
subdf <- SST05[SST05$DateTime < Date2 & SST05$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-12-16 12:45:00"), linetype="dashed", color="red") 

Date1 <- as.Date("2025-04-23", "%Y-%m-%d")
Date2 <- as.Date("2025-04-25", "%Y-%m-%d")
subdf <- SST05[SST05$DateTime < Date2 & SST05$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-23 14:00:00"), linetype="dashed", color="red") 

######################################################################
#### Remove times where PT was out of the water and error section ####
######################################################################
# now NA the time when the PT was out of water 
time0 <- as.POSIXct("2024-12-16 12:45:00")
time1 <- as.POSIXct("2024-12-16 13:00:00")
time2 <- as.POSIXct("2024-12-16 12:30:00")
time3 <- as.POSIXct("2025-04-23 14:15:00")

SST05 <- SST05 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time0, NA, Baro_Cor_Lvl.m))
SST05 <- SST05 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time1, NA, Baro_Cor_Lvl.m))
SST05 <- SST05 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time2, NA, Baro_Cor_Lvl.m))
SST05 <- SST05 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time3, NA, Baro_Cor_Lvl.m))

# remove beginning of data (error)
Date1 <- as.Date("2024-06-17", "%Y-%m-%d")
Date2 <- as.Date("2024-06-19", "%Y-%m-%d")
SST05$Baro_Cor_Lvl.m[SST05$DateTime >= Date1 & SST05$DateTime <= Date2] <- NA

# plot after cleaning
ggplot(SST05, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() +
  labs(title = "Baro_Cor_Lvl.m", x = "Date", y = "Water Level (m)")

###########################################################################
#### Find the average Baro_Cor_Lvl TWO HOURS before and after the move ####
###########################################################################
# first move correction
move_time1 <- as.POSIXct("2024-09-16 13:45:00")

before_move1 <- SST05 %>%
  filter(DateTime >= (move_time1 - hours(2)) & DateTime < move_time1) %>%
  summarize(mean_before1 = mean(Baro_Cor_Lvl.m, na.rm = TRUE))

after_move1 <- SST05 %>%
  filter(DateTime >= move_time1 & DateTime < (move_time1 + hours(2))) %>%
  summarize(mean_after1 = mean(Baro_Cor_Lvl.m, na.rm = TRUE))

offset1 <-  after_move1$mean_after1 - before_move1$mean_before1
print(paste("Offset 1:", offset1))

# apply the first correction
SST05 <- SST05 %>%
  mutate(Baro_Cor_offset1 = if_else(DateTime >= move_time1, Baro_Cor_Lvl.m - offset1, Baro_Cor_Lvl.m))

# second move correction
move_time2 <- as.POSIXct("2024-12-16 13:00:00")

before_move2 <- SST05 %>%
  filter(DateTime >= (move_time2 - hours(2)) & DateTime < move_time2) %>%
  summarize(mean_before2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1

after_move2 <- SST05 %>%
  filter(DateTime >= move_time2 & DateTime < (move_time2 + hours(2))) %>%
  summarize(mean_after2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1

offset2 <- after_move2$mean_after2 - before_move2$mean_before2
print(paste("Offset 2:", offset2))

# apply the second correction
SST05 <- SST05 %>%
  mutate(Baro_Cor_offset2 = if_else(DateTime >= move_time2, Baro_Cor_offset1 - offset2, Baro_Cor_offset1))

# third move correction
move_time3 <- as.POSIXct("2025-04-23 14:30:00")

before_move3 <- SST05 %>%
  filter(DateTime >= (move_time3 - hours(2)) & DateTime < move_time3) %>%
  summarize(mean_before3 = mean(Baro_Cor_offset2, na.rm = TRUE)) # Use Baro_Cor_offset2

after_move3 <- SST05 %>%
  filter(DateTime >= move_time3 & DateTime < (move_time3 + hours(2))) %>%
  summarize(mean_after3 = mean(Baro_Cor_offset2, na.rm = TRUE)) # Use Baro_Cor_offset2

offset3 <- after_move3$mean_after3 - before_move3$mean_before3
print(paste("Offset 3:", offset3))

# apply the second correction
SST05 <- SST05 %>%
  mutate(Baro_Cor_offset3 = if_else(DateTime >= move_time3, Baro_Cor_offset2 - offset3, Baro_Cor_offset2))

##############################
#### Plot with Correction ####
##############################
ggplot(SST05, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (First Correction)", x = "Date", y = "Water Level (m)")
ggplot(SST05, aes(x = DateTime, y = Baro_Cor_offset1)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (First Correction)", x = "Date", y = "Water Level (m)")
ggplot(SST05, aes(x = DateTime, y = Baro_Cor_offset2)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (Second Correction)", x = "Date", y = "Water Level (m)")
ggplot(SST05, aes(x = DateTime, y = Baro_Cor_offset3)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (Second Correction)", x = "Date", y = "Water Level (m)")

# discharge from L/s to m3/s in whole data set
SST05$Q..L.s. <- as.numeric(SST05$Q..L.s.)
SST05 <- SST05 %>%
  mutate(Q.m3s = Q..L.s./1000)

# filter out rows with missing stage or discharge
new_rating_data <- SST05 %>% 
  filter(!is.na(Baro_Cor_offset2), !is.na(Q.m3s))

ggplot(new_rating_data, aes(x = Baro_Cor_Lvl.m, y = Q.m3s)) +
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
  labs(title = "Stage vs. Discharge (Second Correction)", x = "Stage (LEVEL m)", y = "Discharge (Q L/s)") +
  theme_minimal()

########################
#### Plot close ups ####
########################
Date1 <- as.Date("2024-09-15", "%Y-%m-%d")
Date2 <- as.Date("2024-09-18", "%Y-%m-%d")
subdf <- SST05[SST05$DateTime < Date2 & SST05$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-09-16 12:15:00"), linetype="dashed", color="red") 
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset2)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-09-16 12:15:00"), linetype="dashed", color="red") 

Date1 <- as.Date("2024-12-16", "%Y-%m-%d")
Date2 <- as.Date("2024-12-17", "%Y-%m-%d")
subdf <- SST05[SST05$DateTime < Date2 & SST05$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-12-16 12:00:00"), linetype="dashed", color="red") 
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset2)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-12-16 12:00:00"), linetype="dashed", color="red") 

Date1 <- as.Date("2025-04-23", "%Y-%m-%d")
Date2 <- as.Date("2025-04-24", "%Y-%m-%d")
subdf <- SST05[SST05$DateTime < Date2 & SST05$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-23 14:45:00"), linetype="dashed", color="red") 
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset3)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-23 14:45:00"), linetype="dashed", color="red") 

###################
#### Save file ####
###################
write.csv(SST05, "data/offset_SST05.csv")

drive_folder_id <- "1GcRdU4UaBrxEmVhZYoZuTae6HxVZt3Gm"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/offset_SST05.csv",
  path = as_id(drive_folder_id)
)
