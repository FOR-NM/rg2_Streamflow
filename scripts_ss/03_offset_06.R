##==============================================================================
## Project: QuEST
## This script is to calculate PT offset for South Sandy SST06 site
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

#SST06
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="2024-12-17_SST06_PTS_SN2186356.csv"], 
                            path = "googledrive/2024-12-17_SST06_PTS_SN2186356.csv",
                            overwrite = T)
# load file
SST06 <- read.csv("googledrive/2024-12-17_SST06_PTS_SN2186356.csv")

# convert Date column to Date type if not already
SST06$Date <- as.Date(SST06$Date)
SST06$DateTime <- as.POSIXct(SST06$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "MST")
head(SST06)

# remove rows from before deployment
SST06 <- SST06[-c(1:33), ]

# filter out rows with missing stage or discharge
rating_data <- SST06 %>% 
  filter(!is.na(Baro_Cor_Lvl.m), !is.na(Q..L.s.))

# filter out the one row with negative baro lvl value 
SST06 <- SST06 %>% 
  filter(Baro_Cor_Lvl.m >= 0)

########################################
#### Plot pressure compensated data ####
########################################
ggplot(data = SST06, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() + ggtitle("SST06 compensated level data")

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

# plot discharge vs manual stage measurement
ggplot(rating_data, aes(x = pt_depth_m, y = Q.m3s)) +
  geom_point(color = "blue") +
  labs(title = "Manual Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

#################################################
#### Find offset, when did the change happen ####
#################################################
ggplot(SST06, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-08-20"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2024-10-14"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2024-11-12"), linetype="dashed", color="red") +
  labs(title = "Baro_Cor_Lvl", x = "Date", y = "Water Level (m)")


###########################################################################
#### Find the average Baro_Cor_Lvl TWO HOURS before and after the move ####
###########################################################################
# first move correction (2024-10-14, 11:21:00)
move_time1 <- as.POSIXct("2024-10-14 11:21")

before_move1 <- SST06 %>%
  filter(DateTime >= (move_time1 - hours(2)) & DateTime < move_time1) %>%
  summarize(mean_before1 = mean(Baro_Cor_Lvl.m, na.rm = TRUE))

after_move1 <- SST06 %>%
  filter(DateTime >= move_time1 & DateTime < (move_time1 + hours(2))) %>%
  summarize(mean_after1 = mean(Baro_Cor_Lvl.m, na.rm = TRUE))

offset1 <-  after_move1$mean_after1 - before_move1$mean_before1
print(paste("Offset 1:", offset1))

# apply the first correction
SST06 <- SST06 %>%
  mutate(Baro_Cor_offset1 = if_else(DateTime >= move_time1, Baro_Cor_Lvl.m - offset1, Baro_Cor_Lvl.m))

# second move correction (2024-11-12, 11:00:00)
move_time2 <- as.POSIXct("2024-11-12 11:00:00")

before_move2 <- SST06 %>%
  filter(DateTime >= (move_time2 - hours(2)) & DateTime < move_time2) %>%
  summarize(mean_before2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1

after_move2 <- SST06 %>%
  filter(DateTime >= move_time2 & DateTime < (move_time2 + hours(2))) %>%
  summarize(mean_after2 = mean(Baro_Cor_offset1, na.rm = TRUE)) # Use Baro_Cor_offset1

offset2 <- after_move2$mean_after2 - before_move2$mean_before2
print(paste("Offset 2:", offset2))

# apply the second correction
SST06 <- SST06 %>%
  mutate(Baro_Cor_offset2 = if_else(DateTime >= move_time2, Baro_Cor_offset1 - offset2, Baro_Cor_offset1))

##############################
#### Plot with Correction ####
##############################
ggplot(SST06, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (First Correction)", x = "Date", y = "Water Level (m)")

ggplot(SST06, aes(x = DateTime, y = Baro_Cor_offset1)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (First Correction)", x = "Date", y = "Water Level (m)")

ggplot(SST06, aes(x = DateTime, y = Baro_Cor_offset2)) +
  geom_line() +
  labs(title = "Corrected Baro_Cor Over Time (Second Correction)", x = "Date", y = "Water Level (m)")

# discharge from L/s to m3/s in whole data set
SST06$Q..L.s. <- as.numeric(SST06$Q..L.s.)
SST06 <- SST06 %>%
  mutate(Q.m3s = Q..L.s./1000)

# filter out rows with missing stage or discharge
new_rating_data <- SST06 %>% 
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

###################
#### Save file ####
###################
write.csv(SST06, "data/offset_SST06.csv")

drive_folder_id <- "11vn2jsiB7YEsrhjI5_NnOSTA579NMtK4"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/offset_SST06.csv",
  path = as_id(drive_folder_id)
)