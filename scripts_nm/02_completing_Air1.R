##==============================================================================
## Project: QuEST
## This script is to merge PT and barometric (air pressure) data for the New Mexico sites
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

##################
#### Packages ####
##################
library(googledrive) 
library(ggplot2)
library(dplyr)
library(lubridate) 

####################################
## Clear folders that we will use ##
####################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "merged", full.names = TRUE)
file.remove(files)

#########################
#### Import air data ####
#########################
# this is the merged days folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1aUwQq19HnlyIxnMHMH3HtybzvA_Zcrwu")
# list all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="AIR2.csv"], 
                            path = "googledrive/AIR2.csv",
                            overwrite = T)
air_upper <- read.csv("googledrive/AIR2.csv")

googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="AIR3.csv"], 
                            path = "googledrive/AIR3.csv",
                            overwrite = T)
air_middle <- read.csv("googledrive/AIR3.csv")

# this is the inuse folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1i7G-q7FV0_bszqeCdJ6Otz8b9xhpU1cx")
# list all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="2025-06-16_USF_AIR1.csv"], 
                            path = "googledrive/AIR1.csv",
                            overwrite = T)
air_lower <- read.csv("googledrive/AIR1.csv")

# changing some column names
air_upper <- air_upper %>%
  dplyr::rename(Date.air2 = Date,
                Time.air2 = Time,
                Level.air.kPa2 = LEVEL)
air_lower <- air_lower %>%
  dplyr::rename(Date.air1 = Date,
                Time.air1 = Time,
                Level.air.kPa1 = LEVEL)
air_middle <- air_middle %>%
  dplyr::rename(Date.air3 = Date,
                Time.air3 = Time,
                Level.air.kPa3 = LEVEL)

################################
#### Format DateTime column ####
################################
# check datetime format for upper air data
air_upper$DateTime <- paste(air_upper$Date.air, air_upper$Time.air, sep = " ")
# convert the DateTime column to POSIXct
air_upper$DateTime <- as.POSIXct(air_upper$DateTime, format = "%Y-%m-%d %I:%M:%S %p")

# check datetime format for middle air data
air_middle$DateTime <- paste(air_middle$Date.air, air_middle$Time.air, sep = " ")
# convert the DateTime column to POSIXct
air_middle$DateTime <- as.POSIXct(air_middle$DateTime, format = "%Y-%m-%d %I:%M:%S %p")

# check datetime format for lower air data
air_lower$DateTime <- paste(air_lower$Date.air, air_lower$Time.air, sep = " ")
# convert the DateTime column to POSIXct
air_lower$DateTime <- as.POSIXct(air_lower$DateTime, format = "%Y-%m-%d %I:%M:%S %p")

####################################
#### Rounding the time for AIR1 ####
####################################
# create extra columns so you don't erase original time               
air_lower$DateTimeNotRounded <- air_lower$DateTime

# transform to datetime format
air_lower$DateTime <- as.POSIXct(air_lower$DateTime,format = "%Y-%m-%d %H:%M:%S")
air_lower$DateTimeNotRounded <- as.POSIXct(air_lower$DateTimeNotRounded,format = "%Y-%m-%d %H:%M:%S")

# round DateTime to the nearest 15-minute interval 
air_lower$DateTime <- round_date(air_lower$DateTime, unit="15 mins")

# check if it worked!
str(air_lower)

###############################
#### Combine both air data ####
###############################
merged_air <- merge(air_lower, air_middle, by = "DateTime", all.x = TRUE, all.y = TRUE)

###########################################
#### Plot pressure data for both sites ####
###########################################
ggplot(data = air_upper, aes(x = DateTime, y = Level.air.kPa2)) +
  geom_line() + ggtitle("Air 2 pressure")
ggplot(data = merged_air, aes(x = DateTime, y = Level.air.kPa3)) +
  geom_line() + ggtitle("Air 3 pressure")
ggplot(data = merged_air, aes(x = DateTime, y = Level.air.kPa1)) +
  geom_line() + ggtitle("Air 1 pressure")

# plot Air 1 pressure vs Air 3 pressure to visualize their relationship
ggplot(data = merged_air, aes(x = Level.air.kPa3, y = Level.air.kPa1)) +
  geom_point(alpha = 0.6, color = "grey") + # Scatter plot of the data points
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") + # Add linear model line
  labs(title = "Air 1 Pressure vs Air 3 Pressure",
       x = "Air 3 Pressure (kPa)",
       y = "Air 1 Pressure (kPa)") +
  theme_minimal()

######################
#### Linear model ####
######################
# create a linear model to predict Level.air.kPa1 based on Level.air.kPa3
linear_model <- lm(Level.air.kPa1 ~ Level.air.kPa3, data = merged_air)
summary(linear_model)

##########################
#### Predicted linear ####
##########################
# extract the intercept (b) and slope (m) from the linear model
a_linear <- coef(linear_model)[1]  # Intercept (b)
b_linear <- coef(linear_model)[2]  # Slope (m)

# predict Level.air.kPa1 for the entire dataset using the linear model.
# this creates a new column 'Predicted_air1' in 'merged_air'.
merged_air <- merged_air %>%
  mutate(Predicted_air1 = a_linear + b_linear * Level.air.kPa3)

#################################################
#### Fill incomplete section with prediction ####
#################################################
# define the specific date range for filling missing values
start_date_fill <- as.POSIXct("2024-05-15 12:00:00", format = "%Y-%m-%d %H:%M:%S")
end_date_fill <- as.POSIXct("2024-10-24 15:00:00", format = "%Y-%m-%d %H:%M:%S")

# create a new column 'Level.air.kPa1_filled'
merged_air_filled <- merged_air %>%
  mutate(Level.air.kPa1_filled = Level.air.kPa1)

# only fill NA values within the specified date range
# the condition now checks if Level.air.kPa1 is NA AND if the DateTime is within the specified range.
merged_air_filled <- merged_air_filled %>%
  mutate(Level.air.kPa1_filled = ifelse(is.na(Level.air.kPa1) &
                                          DateTime >= start_date_fill &
                                          DateTime <= end_date_fill,
                                        Predicted_air1,
                                        Level.air.kPa1_filled))

# check the number of NA values before and after filling in the specified range
cat("Number of NA values in Level.air.kPa1 before filling:", sum(is.na(merged_air$Level.air.kPa1)), "\n")
cat("Number of NA values in Level.air.kPa1_filled after filling (within specified range):",
    sum(is.na(merged_air_filled$Level.air.kPa1_filled[merged_air_filled$DateTime >= start_date_fill & merged_air_filled$DateTime <= end_date_fill])), "\n")

############################################
#### Visualize original vs. filled data ####
############################################
# plot the original Level.air.kPa1 and the filled Level.air.kPa1_filled
# this plot helps to visualize where the predictions have filled in gaps.
ggplot(data = merged_air_filled, aes(x = DateTime)) +
  geom_line(aes(y = Level.air.kPa1, color = "Original Air 1"), na.rm = TRUE, size = 0.8) +
  geom_line(aes(y = Level.air.kPa1_filled, color = "Filled Air 1"), size = 0.8, linetype = "dashed") +
  labs(title = "Original vs. Predicted/Filled Air 1 Pressure",
       x = "Date and Time",
       y = "Pressure (kPa)",
       color = "Data Series") +
  scale_color_manual(values = c("Original Air 1" = "blue", "Filled Air 1" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# plot new Air 1 pressure vs Air 3 pressure
ggplot(data = merged_air_filled, aes(x = Level.air.kPa3, y = Level.air.kPa1_filled)) +
  geom_point(alpha = 0.6, color = "grey") + # Scatter plot of the data points
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") + # Add linear model line
  labs(title = "New Air 1 Pressure vs Air 3 Pressure",
       x = "Air 3 Pressure (kPa)",
       y = "New Air 1 Pressure (kPa)") +
  theme_minimal()

######################################
#### Making a bit of a better fit ####
######################################
# All of that works but it still looks a bit off. So instead of using the entire time series to predict the missing chunk, 
# we are going to use a shorter, closer window to the missing chink to run the linear model
# Define the time window after the missing section to train the model
train_start <- as.POSIXct("2024-10-25")
train_end   <- as.POSIXct("2024-11-25")

# Filter merged_air to only include this time window
training_data <- merged_air %>%
  filter(DateTime >= train_start & DateTime <= train_end,
         !is.na(Level.air.kPa1), !is.na(Level.air.kPa3))

# Fit a new linear model using only the filtered training data
local_model <- lm(Level.air.kPa1 ~ Level.air.kPa3, data = training_data)

# View summary to inspect slope and intercept
summary(local_model)

# Extract new coefficients
a_local <- coef(local_model)[1]  # Intercept
b_local <- coef(local_model)[2]  # Slope

# Predict Air 1 pressure using the new model
merged_air <- merged_air %>%
  mutate(Predicted_air1_local = a_local + b_local * Level.air.kPa3)

# Now modify the filling step to use Predicted_air1_local instead of the old model:
merged_air_filled <- merged_air %>%
  mutate(Level.air.kPa1_filled = ifelse(is.na(Level.air.kPa1) &
                                          DateTime >= start_date_fill &
                                          DateTime <= end_date_fill,
                                        Predicted_air1_local,
                                        Level.air.kPa1))
# Plot residuals 
merged_air <- merged_air %>%
  mutate(Residual_local = Level.air.kPa1 - Predicted_air1_local)

ggplot(merged_air, aes(x = DateTime, y = Residual_local)) +
  geom_line(color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals Over Time (Air1 - Predicted with Local Model)",
       y = "Residual (kPa)",
       x = "DateTime") +
  theme_minimal()

ggplot(data = merged_air, aes(x = DateTime)) +
  geom_line(aes(y = Level.air.kPa1, color = "Original Air 1"), na.rm = TRUE, size = 0.8) +
  geom_line(aes(y = Predicted_air1_local, color = "Filled Air 1"), size = 0.8, linetype = "dashed") +
  labs(title = "Original vs. Predicted/Filled Air 1 Pressure",
       x = "Date and Time",
       y = "Pressure (kPa)",
       color = "Data Series") +
  scale_color_manual(values = c("Original Air 1" = "blue", "Filled Air 1" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# clean data frame
merged_air <- merged_air[ -c(7:12) ]

merged_air <- merged_air %>%
  mutate(Level.air.kPa_use = ifelse(is.na(Level.air.kPa1) &
                                          DateTime >= start_date_fill &
                                          DateTime <= end_date_fill,
                                        Predicted_air1_local,
                                        Level.air.kPa1))

###################
#### Save file ####
###################
write.csv(merged_air, "data/AIR1.csv")

# this is the "merged days" folder 
drive_folder_id <- "1aUwQq19HnlyIxnMHMH3HtybzvA_Zcrwu"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/AIR1.csv",
  path = as_id(drive_folder_id)
)

