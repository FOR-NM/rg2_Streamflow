##==============================================================================
## Project: QuEST
## This script is to calculate PT offsed for Brush Creek BRD01 site
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
# List and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data", full.names = TRUE)
file.remove(files)

#################################
#### Import & Visualize Data ####
#################################
#### Load data from Google drive ####
# this is the "depth" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1n17b_9yf5DCO_h6uPya5vBPz2dh13L3v")

# List all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3

#BRD01
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="BRD01.csv"], 
                            path = "googledrive/BRD01.csv",
                            overwrite = T)
# load file
BRD01 <- read.csv("googledrive/BRD01.csv")

# combine Date and Time columns into a new DateTime column
BRD01$DateTime <- paste(BRD01$Date.x, BRD01$Time.x, sep = " ")

# convert the DateTime column to POSIXct
BRD01$DateTime <- as.POSIXct(BRD01$DateTime, format = "%Y-%m-%d %H:%M:%S")

# filter out rows with missing stage or discharge
rating_data <- BRD01 %>% 
  filter(!is.na(Baro_Cor_Lvl.m), !is.na(Q_L_per_s))

# check the structure of the cleaned data
head(rating_data)

########################################
#### Plot pressure compensated data ####
########################################
# filter out rows with missing Baro NAs
BRD01_baro <- BRD01 %>% 
  filter(!is.na(Baro_Cor_Lvl.m))

ggplot(data = BRD01_baro, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_vline(xintercept = as.POSIXct("2025-03-14 12:00:00"), linetype="dashed", color="red") +
  geom_line() + ggtitle("BRD01 compensated level data")

ggplot(data = BRD01_baro, aes(x = DateTime, y = LEVEL.m)) +
  geom_line() + ggtitle("BRD01 compensated level data")

ggplot(data = BRD01_baro, aes(x = DateTime, y = pres_m)) +
  geom_line() + ggtitle("BRD01 level data in m")

##################################
#### Plot Stage vs. Discharge ####
##################################
# discharge from L/s to m3/s
rating_data <- rating_data %>%
  mutate(Q.m3s = Q_L_per_s/1000)

ggplot(rating_data, aes(x = Baro_Cor_Lvl.m, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # adds date labels above points
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

############################
#### Look at it closely ####
############################
Date1 <- as.Date("2024-10-01", "%Y-%m-%d")
Date2 <- as.Date("2024-10-15", "%Y-%m-%d")
subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-11 13:00:00"), linetype="dashed", color="red") 

# tornado storm buried and then out
Date1 <- as.Date("2024-11-01", "%Y-%m-%d")
Date2 <- as.Date("2024-11-10", "%Y-%m-%d")
subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-11-08 12:00:00"), linetype="dashed", color="red") 

Date1 <- as.Date("2024-11-10", "%Y-%m-%d")
Date2 <- as.Date("2024-11-15", "%Y-%m-%d")
subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-11-12 15:00:00"), linetype="dashed", color="red") 

Date1 <- as.Date("2024-12-01", "%Y-%m-%d")
Date2 <- as.Date("2024-12-30", "%Y-%m-%d")
subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-12-12 10:00:00"), linetype="dashed", color="red") 

Date1 <- as.Date("2025-01-15", "%Y-%m-%d")
Date2 <- as.Date("2025-01-27", "%Y-%m-%d")
subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-01-23 12:00:00"), linetype="dashed", color="red")

Date1 <- as.Date("2025-02-15", "%Y-%m-%d")
Date2 <- as.Date("2025-03-05", "%Y-%m-%d")
subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-02-28 17:00:00"), linetype="dashed", color="red") 

Date1 <- as.Date("2025-03-01", "%Y-%m-%d")
Date2 <- as.Date("2025-03-12", "%Y-%m-%d")
subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-03-03 13:00:00"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2025-03-10 08:00:00"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2025-03-26 10:00:00"), linetype="dashed", color="red") 

Date1 <- as.Date("2025-04-14", "%Y-%m-%d")
Date2 <- as.Date("2025-04-20", "%Y-%m-%d")
subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-16 14:00:00"), linetype="dashed", color="red") 

Date1 <- as.Date("2025-04-25", "%Y-%m-%d")
Date2 <- as.Date("2025-05-10", "%Y-%m-%d")
subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-29 09:00:00"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.POSIXct("2025-05-05 15:00:00"), linetype="dashed", color="red") 

Date1 <- as.Date("2025-06-01", "%Y-%m-%d")
Date2 <- as.Date("2025-06-30", "%Y-%m-%d")
subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-17 15:00:00"), linetype="dashed", color="red") 

Date1 <- as.Date("2025-07-01", "%Y-%m-%d")
Date2 <- as.Date("2025-07-30", "%Y-%m-%d")
subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-07-03 10:20:00"), linetype="dashed", color="red") 
Date1 <- as.Date("2025-07-10", "%Y-%m-%d")
Date2 <- as.Date("2025-07-15", "%Y-%m-%d")
subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-07-03 10:20:00"), linetype="dashed", color="red") 

Date1 <- as.Date("2025-08-01", "%Y-%m-%d")
Date2 <- as.Date("2025-08-28", "%Y-%m-%d")
subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-08-22 13:20:00"), linetype="dashed", color="red") 

######################################################################
#### Remove times where PT was out of the water and error section ####
######################################################################
# now NA the time when the PT was out of water 
time1 <- as.POSIXct("2025-04-16 13:00:00")
time2 <- as.POSIXct("2025-05-05 15:00:00")
time3 <- as.POSIXct("2025-05-05 14:00:00")
# time4 <- as.POSIXct("2025-06-17 12:00:00")

BRD01 <- BRD01 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time1, NA, Baro_Cor_Lvl.m))
BRD01 <- BRD01 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time2, NA, Baro_Cor_Lvl.m))
BRD01 <- BRD01 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time3, NA, Baro_Cor_Lvl.m))
# BRD01 <- BRD01 %>%
#   mutate(Baro_Cor_Lvl.m = ifelse(DateTime == time4, NA, Baro_Cor_Lvl.m))

###########################################################################
#### Find the average Baro_Cor_Lvl TWO HOURS before and after the move ####
###########################################################################
### first move correction
move_time1 <- as.POSIXct("2024-10-11 12:00:00")  
before_move <- BRD01 %>%
  filter(DateTime >= (move_time1 - hours(2)) & DateTime < move_time1) %>%
  summarize(mean_before = mean(Baro_Cor_Lvl.m, na.rm = TRUE))
after_move <- BRD01 %>%
  filter(DateTime >= move_time1 & DateTime < (move_time1 + hours(2))) %>%
  summarize(mean_after = mean(Baro_Cor_Lvl.m, na.rm = TRUE))
# compute offset
offset1 <- after_move$mean_after - before_move$mean_before
print(offset1)
# apply the first correction
BRD01 <- BRD01 %>%
  mutate(Baro_Cor_offset1 = if_else(DateTime >= move_time1, Baro_Cor_Lvl.m - offset1, Baro_Cor_Lvl.m))

### second move correction
move_time2 <- as.POSIXct("2024-11-12 15:00:00")
before_move <- BRD01 %>%
  filter(DateTime >= (move_time2 - hours(2)) & DateTime < move_time2) %>%
  summarize(mean_before = mean(Baro_Cor_offset1, na.rm = TRUE))
after_move <- BRD01 %>%
  filter(DateTime >= move_time2 & DateTime < (move_time2 + hours(2))) %>%
  summarize(mean_after = mean(Baro_Cor_offset1, na.rm = TRUE))
# compute offset
offset2 <- after_move$mean_after - before_move$mean_before
print(offset2)
# apply the second correction
BRD01 <- BRD01 %>%
  mutate(Baro_Cor_offset2 = if_else(DateTime >= move_time2, Baro_Cor_offset1 - offset2, Baro_Cor_offset1))

### third move correction
move_time3 <- as.POSIXct("2025-03-03 13:00:00")
before_move <- BRD01 %>%
  filter(DateTime >= (move_time3 - hours(2)) & DateTime < move_time3) %>%
  summarize(mean_before = mean(Baro_Cor_offset2, na.rm = TRUE))
after_move <- BRD01 %>%
  filter(DateTime >= move_time3 & DateTime < (move_time3 + hours(2))) %>%
  summarize(mean_after = mean(Baro_Cor_offset2, na.rm = TRUE))
# compute offset
offset3 <- after_move$mean_after - before_move$mean_before
print(offset3)
# apply the second correction
BRD01 <- BRD01 %>%
  mutate(Baro_Cor_offset3 = if_else(DateTime >= move_time3, Baro_Cor_offset2 - offset3, Baro_Cor_offset2))

move_time4 <- as.POSIXct("2025-04-16 14:00:00")
before_move <- BRD01 %>%
  filter(DateTime >= (move_time4 - hours(2)) & DateTime < move_time4) %>%
  summarize(mean_before = mean(Baro_Cor_offset3, na.rm = TRUE))
after_move <- BRD01 %>%
  filter(DateTime >= move_time4 & DateTime < (move_time4 + hours(2))) %>%
  summarize(mean_after = mean(Baro_Cor_offset3, na.rm = TRUE))
# compute offset
offset4 <- after_move$mean_after - before_move$mean_before
print(offset4)
# apply the fourth correction
BRD01 <- BRD01 %>%
  mutate(Baro_Cor_offset4 = if_else(DateTime >= move_time4, Baro_Cor_offset3 - offset4, Baro_Cor_offset3))

# move_time5 <- as.POSIXct("2025-05-05 16:00:00")
# before_move <- BRD01 %>%
#   filter(DateTime >= (move_time5 - hours(5)) & DateTime < move_time5) %>%
#   summarize(mean_before = mean(Baro_Cor_offset4, na.rm = TRUE))
# after_move <- BRD01 %>%
#   filter(DateTime >= move_time5 & DateTime < (move_time5 + hours(5))) %>%
#   summarize(mean_after = mean(Baro_Cor_offset4, na.rm = TRUE))
# # compute offset
# offset5 <- after_move$mean_after - before_move$mean_before
# print(offset5)
# # apply the fifth correction
# BRD01 <- BRD01 %>%
#   mutate(Baro_Cor_offset5 = if_else(DateTime >= move_time5, Baro_Cor_offset4 - offset5, Baro_Cor_offset4))
# 
# move_time6 <- as.POSIXct("2025-04-29 09:00:00")
# before_move <- BRD01 %>%
#   filter(DateTime >= (move_time6 - hours(2)) & DateTime < move_time6) %>%
#   summarize(mean_before = mean(Baro_Cor_offset5, na.rm = TRUE))
# after_move <- BRD01 %>%
#   filter(DateTime >= move_time6 & DateTime < (move_time6 + hours(2))) %>%
#   summarize(mean_after = mean(Baro_Cor_offset5, na.rm = TRUE))
# # compute offset
# offset6 <- after_move$mean_after - before_move$mean_before
# print(offset6)
# # apply the sixth correction
# BRD01 <- BRD01 %>%
#   mutate(Baro_Cor_offset6 = if_else(DateTime >= move_time6, Baro_Cor_offset5 - offset6, Baro_Cor_offset5))

# move_time7 <- as.POSIXct("2025-06-26 11:00:00")
# before_move <- BRD01 %>%
#   filter(DateTime >= (move_time7 - hours(2)) & DateTime < move_time7) %>%
#   summarize(mean_before = mean(Baro_Cor_offset6, na.rm = TRUE))
# move_time7 <- as.POSIXct("2025-06-26 12:00:00")
# after_move <- BRD01 %>%
#   filter(DateTime >= move_time7 & DateTime < (move_time7 + hours(2))) %>%
#   summarize(mean_after = mean(Baro_Cor_offset6, na.rm = TRUE))
# # compute offset
# offset7 <- after_move$mean_after - before_move$mean_before
# print(offset7)
# # apply the seventh correction
# BRD01 <- BRD01 %>%
#   mutate(Baro_Cor_offset7 = if_else(DateTime >= move_time7, Baro_Cor_offset6 - offset7, Baro_Cor_offset6))

###############################
####  Plot with Correction ####
###############################
ggplot(BRD01, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() +
  #geom_vline(xintercept = as.POSIXct("2024-07-30 15:10:00"), linetype="dashed", color="red") +
  labs(title = "Corrected Baro_Cor Over Time", x = "Date", y = "Water Level (m)")
ggplot(BRD01, aes(x = DateTime, y = Baro_Cor_offset1)) +
  geom_line() +
  #geom_vline(xintercept = as.POSIXct("2024-07-30 15:10:00"), linetype="dashed", color="red") +
  labs(title = "Corrected Baro_Cor Over Time", x = "Date", y = "Water Level (m)")
ggplot(BRD01, aes(x = DateTime, y = Baro_Cor_offset2)) +
  geom_line() +
  #geom_vline(xintercept = as.POSIXct("2024-07-30 15:10:00"), linetype="dashed", color="red") +
  labs(title = "Corrected Baro_Cor Over Time", x = "Date", y = "Water Level (m)")
ggplot(BRD01, aes(x = DateTime, y = Baro_Cor_offset3)) +
  geom_line() +
  #geom_vline(xintercept = as.POSIXct("2024-07-30 15:10:00"), linetype="dashed", color="red") +
  labs(title = "Corrected Baro_Cor Over Time", x = "Date", y = "Water Level (m)")
ggplot(BRD01, aes(x = DateTime, y = Baro_Cor_offset4)) +
  geom_line() +
  #geom_vline(xintercept = as.POSIXct("2024-07-30 15:10:00"), linetype="dashed", color="red") +
  labs(title = "Corrected Baro_Cor Over Time", x = "Date", y = "Water Level (m)")
ggplot(BRD01, aes(x = DateTime, y = Baro_Cor_offset5)) +
  geom_line() +
  #geom_vline(xintercept = as.POSIXct("2024-07-30 15:10:00"), linetype="dashed", color="red") +
  labs(title = "Corrected Baro_Cor Over Time", x = "Date", y = "Water Level (m)")
ggplot(BRD01, aes(x = DateTime, y = Baro_Cor_offset6)) +
  geom_line() +
  #geom_vline(xintercept = as.POSIXct("2024-07-30 15:10:00"), linetype="dashed", color="red") +
  labs(title = "Corrected Baro_Cor Over Time", x = "Date", y = "Water Level (m)")

###################################################
#### Plot Stage vs. Discharge after correction ####
###################################################
# discharge from L/s to m3/s in whole data set
BRD01$Q_L_per_s <- as.numeric(BRD01$Q_L_per_s)
BRD01 <- BRD01 %>%
  mutate(Q.m3s = Q_L_per_s/1000)
# filter out rows with missing stage or discharge
rating_data_offset <- BRD01 %>% 
  filter(!is.na(Baro_Cor_Lvl.m), !is.na(Q.m3s))

ggplot(rating_data_offset, aes(x = Baro_Cor_Lvl.m, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m³/s)") +
  theme_minimal()
ggplot(rating_data_offset, aes(x = Baro_Cor_offset1, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m³/s)") +
  theme_minimal()
ggplot(rating_data_offset, aes(x = Baro_Cor_offset2, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m³/s)") +
  theme_minimal()
ggplot(rating_data_offset, aes(x = Baro_Cor_offset3, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m³/s)") +
  theme_minimal()
ggplot(rating_data_offset, aes(x = Baro_Cor_offset4, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m³/s)") +
  theme_minimal()
# ggplot(rating_data_offset, aes(x = Baro_Cor_offset5, y = Q.m3s)) +
#   geom_point(color = "blue") +
#   geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
#   labs(title = "Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m³/s)") +
#   theme_minimal()
# ggplot(rating_data_offset, aes(x = Baro_Cor_offset6, y = Q.m3s)) +
#   geom_point(color = "blue") +
#   geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
#   labs(title = "Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m³/s)") +
#   theme_minimal()

########################
#### Plot by season ####
########################
# add a "Season" column
rating_data_offset <- rating_data_offset %>%
  mutate(Month = month(Date.x),Season = case_when(
    Month %in% c(12, 1, 2) ~ "Winter",
    Month %in% c(3, 4, 5) ~ "Spring",
    Month %in% c(6, 7, 8) ~ "Summer",
    Month %in% c(9, 10, 11) ~ "Fall"
  ))
# coloring by Season
ggplot(rating_data_offset, aes(x = Baro_Cor_offset4, y = Q.m3s, color = Season)) +
  geom_point(size = 3) +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3, show.legend = FALSE) +
  labs(
    title = "Stage vs. Discharge by Season - BRD01",
    x = "Stage (LEVEL m)",
    y = "Discharge (Q m³/s)",
    color = "Season"
  ) +
  scale_color_manual(
    values = c(
      "Winter" = "#1f77b4","Spring" = "#2ca02c","Summer" = "#ff7f0e","Fall" = "#d62728")) +
  theme_minimal()

#####################################################
#### Plot before and after good baro logger data ####
#####################################################
# add a column for before/after April 13
rating_data_offset <- rating_data_offset %>%
  mutate(
    Date.x = as.Date(Date.x),  # ensure it's a proper Date
    Period = if_else(Date.x < as.Date("2025-04-13"), "Before April 13", "After April 13")
  )
# Plot divided by before/after April 13
ggplot(rating_data_offset, aes(x = Baro_Cor_offset4, y = Q.m3s, color = Period)) +
  geom_point(size = 3) +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3, show.legend = FALSE) +
  labs(
    title = "Stage vs. Discharge (Before and After April 13) - BRD01",
    x = "Stage (LEVEL m)",
    y = "Discharge (Q m³/s)",
    color = "Period"
  ) +
  scale_color_manual(values = c("Before April 13" = "#1f77b4", "After April 13" = "#ff7f0e")) +
  theme_minimal()

########################
#### Plot close ups ####
########################
Date1 <- as.Date("2024-10-01", "%Y-%m-%d")
Date2 <- as.Date("2024-10-25", "%Y-%m-%d")
subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-11 12:00:00"), linetype="dashed", color="red") 
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset4)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-10-11 12:00:00"), linetype="dashed", color="red") 

Date1 <- as.Date("2024-11-01", "%Y-%m-%d")
Date2 <- as.Date("2024-11-20", "%Y-%m-%d")
subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-11-12 15:00:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset4)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2024-11-12 15:00:00"), linetype="dashed", color="red")

Date1 <- as.Date("2025-03-01", "%Y-%m-%d")
Date2 <- as.Date("2025-03-05", "%Y-%m-%d")
subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-03-03 13:00:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset4)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-03-03 13:00:00"), linetype="dashed", color="red")

Date1 <- as.Date("2025-04-15", "%Y-%m-%d")
Date2 <- as.Date("2025-04-19", "%Y-%m-%d")
subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-16 14:00:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset5)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-16 14:00:00"), linetype="dashed", color="red")

Date1 <- as.Date("2025-04-22", "%Y-%m-%d")
Date2 <- as.Date("2025-05-02", "%Y-%m-%d")
subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-29 14:00:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset6)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-29 14:00:00"), linetype="dashed", color="red")

Date1 <- as.Date("2025-05-01", "%Y-%m-%d")
Date2 <- as.Date("2025-05-10", "%Y-%m-%d")
subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-05-05 15:00:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset5)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-05-05 15:00:00"), linetype="dashed", color="red")

Date1 <- as.Date("2025-06-01", "%Y-%m-%d")
Date2 <- as.Date("2025-06-30", "%Y-%m-%d")
subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-17 08:00:00"), linetype="dashed", color="red")
ggplot(data=subdf, aes(DateTime,Baro_Cor_offset5)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-06-17 08:00:00"), linetype="dashed", color="red")

# Date1 <- as.Date("2025-06-23", "%Y-%m-%d")
# Date2 <- as.Date("2025-06-29", "%Y-%m-%d")
# subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
# ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
#   geom_vline(xintercept = as.POSIXct("2025-06-26 11:00:00"), linetype="dashed", color="red") 
# ggplot(data=subdf, aes(DateTime,Baro_Cor_offset5)) + geom_line() +
#   geom_vline(xintercept = as.POSIXct("2025-06-26 11:00:00"), linetype="dashed", color="red") 
# 
# Date1 <- as.Date("2025-07-10", "%Y-%m-%d")
# Date2 <- as.Date("2025-07-25", "%Y-%m-%d")
# subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
# ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
#   geom_vline(xintercept = as.POSIXct("2025-07-17 10:00:00"), linetype="dashed", color="red") 
# ggplot(data=subdf, aes(DateTime,Baro_Cor_offset5)) + geom_line() +
#   geom_vline(xintercept = as.POSIXct("2025-07-17 10:00:00"), linetype="dashed", color="red") 
# Date1 <- as.Date("2025-08-23", "%Y-%m-%d")
# 
# Date2 <- as.Date("2025-08-30", "%Y-%m-%d")
# subdf <- BRD01[BRD01$DateTime < Date2 & BRD01$DateTime > Date1,]
# ggplot(data=subdf, aes(DateTime,Baro_Cor_Lvl.m)) + geom_line() +
#   geom_vline(xintercept = as.POSIXct("2025-08-27 13:00:00"), linetype="dashed", color="red") 
# ggplot(data=subdf, aes(DateTime,Baro_Cor_offset5)) + geom_line() +
#   geom_vline(xintercept = as.POSIXct("2025-08-27 13:00:00"), linetype="dashed", color="red") 

###################
#### Save file ####
###################
write.csv(BRD01, "data/offset_BRD01.csv")

drive_folder_id <- "1E3pAdlfgxluBGmYT4r95obkaAduKIXhQ"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/offset_BRD01.csv",
  path = as_id(drive_folder_id)
)
