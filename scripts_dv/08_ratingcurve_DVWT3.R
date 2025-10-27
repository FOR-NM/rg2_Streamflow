##==============================================================================
## Project: QuEST
## This script is to calculate discharge from compensated pressure data for Santa Fe DVWT3 site
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
# this is the "offset" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/136WGq6adaNROjaJN2YL63yJExKikM81A")

# List all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3

# DVWT3
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="offset_DVWT3.csv"], 
                            path = "googledrive/offset_DVWT3.csv",
                            overwrite = T)
# load file
DVWT3 <- read.csv("googledrive/offset_DVWT3.csv")

# convert Date column to Date type if not already
DVWT3$Date <- as.Date(DVWT3$Date.x)
# combine Date and Time columns into a new DateTime column
DVWT3$DateTime <- paste(DVWT3$Date.x, DVWT3$TimeOnly, sep = " ")
# convert the DateTime column to POSIXct
DVWT3$DateTime <- as.POSIXct(DVWT3$DateTime, format = "%Y-%m-%d %H:%M:%S")

########################################
#### Make compensated data positive ####
########################################
# filter out rows with missing stage or discharge
rating_data <- DVWT3 %>% 
  filter(!is.na(Baro_Cor_offset2), !is.na(Q), !Q == 0)
rating_data <- rating_data %>% 
  filter(slug_flag != "Bad")
rating_data <- rating_data %>% 
  filter(slug_flag != c("PW"))

########################################
#### Plot pressure compensated data ####
########################################
ggplot(data = DVWT3, aes(x = DateTime, y = Baro_Cor_offset2)) +
  geom_vline(xintercept = as.POSIXct("2025-06-23 12:00:00"), linetype="dashed") +
  geom_line() + ggtitle("DVWT3 compensated level data")

##################################
#### Plot Stage vs. Discharge ####
##################################
# discharge from L/s to m3/s
rating_data <- rating_data %>%
  mutate(Q.m3s = Q/1000)

# plot with date info
ggplot(rating_data, aes(x = Baro_Cor_offset2, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m³/s)") +
  theme_minimal()

###########################################
#### Check for Log-Linear Relationship ####
###########################################
ggplot(rating_data, aes(x = log(Baro_Cor_offset2), y = log(Q.m3s))) +
  geom_point(color = "blue") +
  labs(title = "Log-Log Plot of Water Level vs. Discharge", 
       x = "Log(Water Level)", y = "Log(Discharge)") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  theme_minimal()

####################
#### Log model? ####
####################
rating_data <- rating_data %>%
  mutate(Log_Stage = log(Baro_Cor_offset2),
         Log_Discharge = log(Q.m3s))
log_model <- lm(Log_Discharge ~ Log_Stage, data = rating_data)

summary(log_model)


a <- exp(coef(log_model)[1])  # Back-transform intercept
b <- coef(log_model)[2]       # Slope

#######################
#### Linear model? ####
#######################
linear_model <- lm(Q.m3s ~ Baro_Cor_offset2, data = rating_data)

summary(linear_model)

##########################
#### Visualize models ####
##########################
# observed data
plot(rating_data$Baro_Cor_offset2, rating_data$Q.m3s,
     main = "Stage vs. Discharge",
     xlab = "Water Level (m)", ylab = "Discharge (m³/s)",
     pch = 19, col = "blue")

# log-transformed model predictions
pred_log <- exp(predict(log_model, newdata = rating_data))
lines(rating_data$Baro_Cor_offset2, pred_log, col = "red", lwd = 2)

# linear model predictions
pred_linear <- predict(linear_model, newdata = rating_data)
lines(rating_data$Baro_Cor_offset2, pred_linear, col = "green", lwd = 2)

# legend
legend("topleft", legend = c("Observed", "Log-Transformed", "Linear"),
       col = c("blue", "red", "green"), pch = c(19, NA, NA, NA), lty = c(NA, 1, 1, 1), lwd = c(NA, 2, 2, 2))

#######################
#### Predicted log ####
#######################
# log-transformed model parameters
a_log <- exp(coef(log_model)[1])  # Intercept
b_log <- coef(log_model)[2]       # Slope

# predict discharge for the entire dataset
DVWT3 <- DVWT3 %>%
  mutate(Predicted_Discharge_Log_m3s = a_log * (Baro_Cor_offset2 ^ b_log))

##########################
#### Predicted linear ####
##########################
# linear model parameters
a_linear <- coef(linear_model)[1]  # Intercept
b_linear <- coef(linear_model)[2]  # Slope

# predict discharge for the entire dataset
DVWT3 <- DVWT3 %>%
  mutate(Predicted_Discharge_Linear_m3s = a_linear + b_linear * Baro_Cor_offset2)

#############################
#### Compare predictions ####
#############################
# visualize predictions
plot(DVWT3$Baro_Cor_offset2, DVWT3$Predicted_Discharge_Log_m3s, col = "red", type = "l", lwd = 2,
     xlab = "Stage (m)", ylab = "Discharge (m³/s)", main = "Discharge Predictions")
lines(DVWT3$Baro_Cor_offset2, DVWT3$Predicted_Discharge_Linear_m3s, col = "green", lwd = 2)
legend("topleft", legend = c("Log-Transformed", "Linear"),
       col = c("red", "green"), lty = 1, lwd = 2)


# discharge from L/s to m3/s for entire dataset
DVWT3 <- DVWT3 %>%
  mutate(Q.m3s = Q/1000)

# compare Predicted vs. Observed Discharge
ggplot(DVWT3, aes(x = Q.m3s)) +
  geom_point(aes(y = Predicted_Discharge_Log_m3s, color = "Log Model")) +
  geom_point(aes(y = Predicted_Discharge_Linear_m3s, color = "Linear Model")) +
  labs(
    title = "Comparison of Observed vs Predicted Discharge",
    x = "Observed Discharge (m³/s)",
    y = "Predicted Discharge (m³/s)"
  ) +
  scale_color_manual(values = c("red", "green")) +
  theme_minimal()

# residuals
DVWT3 <- DVWT3 %>%
  mutate(
    Residual_Log = Q.m3s - Predicted_Discharge_Log_m3s,
    Residual_Linear = Q.m3s - Predicted_Discharge_Linear_m3s
  )

ggplot(DVWT3, aes(x = Baro_Cor_offset2)) +
  geom_point(aes(y = Residual_Log, color = "Log Model")) +
  geom_point(aes(y = Residual_Linear, color = "Linear Model")) +
  labs(
    title = "Residuals for Different Models",
    x = "Barometric Corrected Level (m)",
    y = "Residuals (Observed - Predicted)"
  ) +
  scale_color_manual(values = c("red", "green")) +
  theme_minimal()

######################################
#### Plot and compare predictions ####
######################################
DVWT3$DateTime <- as.POSIXct(DVWT3$DateTime)

ggplot(DVWT3, aes(x = DateTime, y = Predicted_Discharge_Log_m3s)) +
  geom_point(color = "blue") +
  labs(title = "Predicted Discharge", x = "DateTime", y = "Discharge (m3/s)") +
  scale_x_datetime(date_breaks = "2 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(DVWT3, aes(x = DateTime, y = Predicted_Discharge_Log_m3s)) +
  geom_point(color = "blue") +
  labs(title = "Predicted Discharge (Log)", x = "DateTime", y = "Discharge (m3/s)") +
  scale_x_datetime(date_breaks = "2 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(trans = "log")

ggplot(DVWT3, aes(x = DateTime, y = Predicted_Discharge_Linear_m3s)) +
  geom_point(color = "blue") +
  labs(title = "Predicted Discharge (Linear)", x = "DateTime", y = "Discharge (m3/s)") +
  scale_x_datetime(date_breaks = "2 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###################
#### Save file ####
###################
write.csv(DVWT3, "data/discharge_DVWT3.csv")

# this is the "predicted" folder 
drive_folder_id <- "1FQCOmazrRF6MN9VGKHvlUq_WrM7LZC26"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/discharge_DVWT3.csv",
  path = as_id(drive_folder_id)
)

