##==============================================================================
## Project: QuEST
## This script is to calculate discharge from compensated pressure data for BRM01
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
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1E3pAdlfgxluBGmYT4r95obkaAduKIXhQ")

# List all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3

#BRM01
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="offset_BRM01.csv"], 
                            path = "googledrive/offset_BRM01.csv",
                            overwrite = T)
# load file
BRM01 <- read.csv("googledrive/offset_BRM01.csv")

# convert Date column to Date type if not already
BRM01$Date <- as.Date(BRM01$Date.x)
# combine Date and Time columns into a new DateTime column
BRM01$DateTime <- paste(BRM01$Date.x, BRM01$Time.x, sep = " ")
# convert the DateTime column to POSIXct
BRM01$DateTime <- as.POSIXct(BRM01$DateTime, format = "%Y-%m-%d %H:%M:%S")

# filter out rows with missing stage or discharge
rating_data <- BRM01 %>% 
  filter(!is.na(Baro_Cor_offset5), !is.na(Q_L_per_s), is.na(flag))

# check the structure of the cleaned data
head(rating_data)

# filter out rows with missing Baro NAs
BRM01_baro <- BRM01 %>% 
  filter(!is.na(Baro_Cor_offset5))

########################################
#### Plot pressure compensated data ####
########################################
ggplot(data = BRM01_baro, aes(x = DateTime, y = Baro_Cor_offset5)) +
  geom_line() + ggtitle("BRM01 compensated level data")
ggplot(data = BRM01_baro, aes(x = DateTime, y = LEVEL.m)) +
  geom_line() + ggtitle("BRM01 level data in m")
ggplot(data = BRM01_baro, aes(x = DateTime, y = pres_m)) +
  geom_line() + ggtitle("BRM01 level data in m")

##################################
#### Plot Stage vs. Discharge ####
##################################
# discharge from L/s to m3/s
rating_data <- rating_data %>%
  mutate(Q.m3s = Q_L_per_s/1000)

# plot with date info
ggplot(rating_data, aes(x = Baro_Cor_offset5, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m³/s)") +
  theme_minimal()

###########################################
#### Check for Log-Linear Relationship ####
###########################################
ggplot(rating_data, aes(x = log(Baro_Cor_offset5), y = log(Q.m3s))) +
  geom_point(color = "blue") +
  labs(title = "Log-Log Plot of Water Level vs. Discharge", 
       x = "Log(Water Level)", y = "Log(Discharge)") +
  theme_minimal()

####################
#### Log model? ####
####################
rating_data <- rating_data %>%
  mutate(Log_Stage = log(Baro_Cor_offset5),
         Log_Discharge = log(Q.m3s))

log_model <- lm(Log_Discharge ~ Log_Stage, data = rating_data)

summary(log_model)

a <- exp(coef(log_model)[1])  # Back-transform intercept
b <- coef(log_model)[2]       # Slope

#######################
#### Linear model? ####
#######################
linear_model <- lm(Q.m3s ~ Baro_Cor_offset5, data = rating_data)

summary(linear_model)

##########################
#### Visualize models ####
##########################
# observed data
plot(rating_data$Baro_Cor_offset5, rating_data$Q.m3s,
     main = "Stage vs. Discharge",
     xlab = "Water Level (m)", ylab = "Discharge (m³/s)",
     pch = 19, col = "blue")


# log-transformed model predictions
pred_log <- exp(predict(log_model, newdata = rating_data))
lines(rating_data$Baro_Cor_offset5, pred_log, col = "red", lwd = 2)

# linear model predictions
pred_linear <- predict(linear_model, newdata = rating_data)
lines(rating_data$Baro_Cor_offset5, pred_linear, col = "green", lwd = 2)

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
BRM01 <- BRM01 %>%
  mutate(Predicted_Discharge_Log.m3s = a_log * (Baro_Cor_offset5 ^ b_log))

##########################
#### Predicted linear ####
##########################
# linear model parameters
a_linear <- coef(linear_model)[1]  # Intercept
b_linear <- coef(linear_model)[2]  # Slope

# predict discharge for the entire dataset
BRM01 <- BRM01 %>%
  mutate(Predicted_Discharge_Linear = a_linear + b_linear * Baro_Cor_offset5)

#############################
#### Compare predictions ####
#############################
# visualize predictions
plot(BRM01$Baro_Cor_offset5, BRM01$Predicted_Discharge_Log.m3s, col = "red", type = "l", lwd = 2,
     xlab = "Stage (m)", ylab = "Discharge (m³/s)", main = "Discharge Predictions")
lines(BRM01$Baro_Cor_offset5, BRM01$Predicted_Discharge_Linear, col = "green", lwd = 2)
legend("topleft", legend = c("Log-Transformed", "Linear"),
       col = c("red", "green"), lty = 1, lwd = 2)


# discharge from L/s to m3/s for entire dataset
BRM01 <- BRM01 %>%
  mutate(Q.m3s = Q_L_per_s/1000)

# compare Predicted vs. Observed Discharge
ggplot(BRM01, aes(x = Q.m3s)) +
  geom_point(aes(y = Predicted_Discharge_Log.m3s, color = "Log Model")) +
  geom_point(aes(y = Predicted_Discharge_Linear, color = "Linear Model")) +
  labs(
    title = "Comparison of Observed vs Predicted Discharge",
    x = "Observed Discharge (m³/s)",
    y = "Predicted Discharge (m³/s)"
  ) +
  scale_color_manual(values = c("red", "green")) +
  theme_minimal()

# residuals
BRM01 <- BRM01 %>%
  mutate(
    Residual_Log = Q.m3s - Predicted_Discharge_Log.m3s,
    Residual_Linear = Q.m3s - Predicted_Discharge_Linear
  )

ggplot(BRM01, aes(x = Baro_Cor_offset5)) +
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
BRM01$DateTime <- as.POSIXct(BRM01$DateTime)

ggplot(BRM01, aes(x = DateTime, y = Predicted_Discharge_Log.m3s)) +
  geom_point(color = "blue") +
  labs(title = "Predicted Discharge (Log)", x = "DateTime", y = "Discharge (m3/s)") +
  scale_x_datetime(date_breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(BRM01, aes(x = DateTime, y = Predicted_Discharge_Linear)) +
  geom_point(color = "blue") +
  labs(title = "Predicted Discharge (Linear)", x = "DateTime", y = "Discharge (m3/s)") +
  scale_x_datetime(date_breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###################
#### Save file ####
###################

write.csv(BRM01, "data/discharge_BRM01.csv")

drive_folder_id <- "1PNCX_xYwu57gYMFNLtHiAFbBi7m-L1Uf"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/discharge_BRM01.csv",
  path = as_id(drive_folder_id)
)
