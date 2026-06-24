##==============================================================================
## Project: QuEST
## This script is to calculate discharge from compensated pressure data for Santa Fe DVMS1 site
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
# this is the "depth_corrected" folder (output of script 05b)
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1wlpAOSpJyB6LI2XYkRXRZwhXRkXqe5eG")

# List all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3

#DVMS1
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="depth_corrected_DVMS1.csv"],
                            path = "googledrive/depth_corrected_DVMS1.csv",
                            overwrite = T)
# load file
DVMS1 <- read.csv("googledrive/depth_corrected_DVMS1.csv")

# convert Date column to Date type if not already
DVMS1$Date <- as.Date(DVMS1$Date.x)
# combine Date and Time columns into a new DateTime column
DVMS1$DateTime <- paste(DVMS1$Date.x, DVMS1$Time, sep = " ")
# convert the DateTime column to POSIXct
DVMS1$DateTime <- as.POSIXct(DVMS1$DateTime, format = "%Y-%m-%d %I:%M:%S %p")

########################################
#### Make compensated data positive ####
########################################
# filter out rows with missing stage or discharge
rating_data <- DVMS1 %>% 
  filter(!is.na(Final_Water_Depth_m), !is.na(Q))

write.csv(rating_data, "DVMS1_rating.csv")

# check the structure of the cleaned data
head(rating_data)

########################################
#### Plot pressure compensated data ####
########################################
ggplot(data = DVMS1, aes(x = DateTime, y = Final_Water_Depth_m)) +
  geom_vline(xintercept = as.POSIXct("2025-06-23 12:00:00"), linetype="dashed") +
  geom_line() + ggtitle("DVMS1 compensated level data")

##################################
#### Plot Stage vs. Discharge ####
##################################
# discharge from L/s to m3/s
rating_data <- rating_data %>%
  mutate(Q.m3s = Q/1000)

# plot with date info
ggplot(rating_data, aes(x = Final_Water_Depth_m, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Stage vs. Discharge - DVMS1", x = "Stage (LEVEL m)", y = "Discharge (Q m³/s)") +
  theme_minimal()

###########################################
#### Check for Log-Linear Relationship ####
###########################################
ggplot(rating_data, aes(x = log(Final_Water_Depth_m), y = log(Q))) +
  geom_point(color = "blue") +
  labs(title = "Log-Log Plot of Water Level vs. Discharge", 
       x = "Log(Water Level)", y = "Log(Discharge)") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  theme_minimal()

###################
#### Log model ####
###################
rating_data <- rating_data %>%
  mutate(Log_Stage = log(Final_Water_Depth_m),
         Log_Discharge = log(Q.m3s))

log_model <- lm(Log_Discharge ~ Log_Stage, data = rating_data)

summary(log_model)

a <- exp(coef(log_model)[1])  # Back-transform intercept
b <- coef(log_model)[2]       # Slope

##########################
#### Visualize models ####
##########################
# observed data
plot(rating_data$Final_Water_Depth_m, rating_data$Q.m3s,
     main = "Stage vs. Discharge",
     xlab = "Water Level (m)", ylab = "Discharge (m³/s)",
     pch = 19, col = "blue")

# log-transformed model predictions
pred_log <- exp(predict(log_model, newdata = rating_data))
lines(rating_data$Final_Water_Depth_m, pred_log, col = "red", lwd = 2)

# legend
legend("topleft", legend = c("Observed", "Log-Transformed", "Linear"),
       col = c("blue", "red"), pch = c(19, NA, NA, NA), lty = c(NA, 1, 1, 1), lwd = c(NA, 2, 2, 2))

#######################
#### Predicted log ####
#######################
# log-transformed model parameters
a_log <- exp(coef(log_model)[1])  # Intercept
b_log <- coef(log_model)[2]       # Slope

# predict discharge for the entire dataset
DVMS1 <- DVMS1 %>%
  mutate(Predicted_Discharge_Log_m3s = a_log * (Final_Water_Depth_m ^ b_log))

#############################
#### Compare predictions ####
#############################
# visualize predictions
plot(DVMS1$Final_Water_Depth_m, DVMS1$Predicted_Discharge_Log_m3s, col = "red", type = "l", lwd = 2,
     xlab = "Stage (m)", ylab = "Discharge (m³/s)", main = "Discharge Predictions")
legend("topleft", legend = c("Log-Transformed", "Linear"),
       col = c("red"), lty = 1, lwd = 2)


# discharge from L/s to m3/s for entire dataset
DVMS1 <- DVMS1 %>%
  mutate(Q.m3s = Q/1000)

# compare Predicted vs. Observed Discharge
ggplot(DVMS1, aes(x = Q.m3s)) +
  geom_point(aes(y = Predicted_Discharge_Log_m3s, color = "Log Model")) +
  labs(
    title = "Comparison of Observed vs Predicted Discharge",
    x = "Observed Discharge (m³/s)",
    y = "Predicted Discharge (m³/s)"
  ) +
  scale_color_manual(values = c("red")) +
  theme_minimal()

# residuals
DVMS1 <- DVMS1 %>%
  mutate(
    Residual_Log = Q.m3s - Predicted_Discharge_Log_m3s
  )

ggplot(DVMS1, aes(x = Final_Water_Depth_m)) +
  geom_point(aes(y = Residual_Log, color = "Log Model")) +
  labs(
    title = "Residuals for Different Models",
    x = "Barometric Corrected Level (m)",
    y = "Residuals (Observed - Predicted)"
  ) +
  scale_color_manual(values = c("red")) +
  theme_minimal()

######################################
#### Plot and compare predictions ####
######################################
DVMS1$DateTime <- as.POSIXct(DVMS1$DateTime)

ggplot(DVMS1, aes(x = DateTime, y = Predicted_Discharge_Log_m3s)) +
  geom_line(color = "blue") +
  labs(title = "Predicted Discharge DVMS1", x = "DateTime", y = "Discharge (m3/s)") +
  scale_x_datetime(date_breaks = "2 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(DVMS1, aes(x = DateTime, y = Predicted_Discharge_Log_m3s)) +
  geom_point(color = "blue") +
  labs(title = "Predicted Discharge (Log)", x = "DateTime", y = "Discharge (m3/s)") +
  scale_x_datetime(date_breaks = "2 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(trans = "log")

###################
#### Save file ####
###################
write.csv(DVMS1, "data/discharge_DVMS1.csv")

# this is the "predicted" folder 
drive_folder_id <- "1FQCOmazrRF6MN9VGKHvlUq_WrM7LZC26"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/discharge_DVMS1.csv",
  path = as_id(drive_folder_id)
)

