##==============================================================================
## Project: QuEST
## This script is to calculate discharge from compensated pressure data for Santa Fe USF21 site
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
# This is the "depth" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1EswIfUWCK6bsdcs-ZrAMGW1oYKs4B0Eh")

# List all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3

#USF21
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="USF21.csv"], 
                            path = "googledrive/USF21.csv",
                            overwrite = T)
# Load file
USF21 <- read.csv("googledrive/USF21.csv")

# Convert Date column to Date type if not already
USF21$Date <- as.Date(USF21$Date)

head(USF21)

# Filter out rows with missing stage or discharge
rating_data <- USF21 %>% 
  filter(!is.na(Baro_Cor_Lvl), !is.na(Q))

# Check the structure of the cleaned data
head(rating_data)

##################################
#### Plot Stage vs. Discharge ####
##################################

ggplot(rating_data, aes(x = Baro_Cor_Lvl, y = Q)) +
  geom_point(color = "blue") +
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL.m)", y = "Discharge (Q)") +
  theme_minimal()

# discharge from L/s to m3/s
rating_data <- rating_data %>%
  mutate(Q.m3s = Q/1000)

ggplot(rating_data, aes(x = Baro_Cor_Lvl, y = Q.m3s)) +
  geom_point(color = "blue") +
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL.m)", y = "Discharge (Q)") +
  theme_minimal()

###########################################
#### Check for Log-Linear Relationship ####
###########################################

ggplot(rating_data, aes(x = log(Baro_Cor_Lvl), y = log(Q))) +
  geom_point(color = "blue") +
  labs(title = "Log-Log Plot of Water Level vs. Discharge", 
       x = "Log(Water Level)", y = "Log(Discharge)") +
  theme_minimal()

ggplot(rating_data, aes(x = log(Baro_Cor_Lvl), y = log(Q.m3s))) +
  geom_point(color = "blue") +
  labs(title = "Log-Log Plot of Water Level vs. Discharge", 
       x = "Log(Water Level)", y = "Log(Discharge)") +
  theme_minimal()

####################
#### Log model? ####
####################

rating_data <- rating_data %>%
  mutate(Log_Stage = log(Baro_Cor_Lvl),
         Log_Discharge = log(Q.m3s))

log_model <- lm(Log_Discharge ~ Log_Stage, data = rating_data)

summary(log_model)

a <- exp(coef(log_model)[1])  # Back-transform intercept
b <- coef(log_model)[2]       # Slope

#######################
#### Linear model? ####
#######################

linear_model <- lm(Q.m3s ~ Baro_Cor_Lvl, data = rating_data)

summary(linear_model)

###########################
#### Polynomial model? ####
###########################

poly_model <- lm(Q.m3s ~ poly(Baro_Cor_Lvl, 2), data = rating_data)

summary(poly_model)

###########################
#### Visualize models  ####
###########################

# Observed data
plot(rating_data$Baro_Cor_Lvl, rating_data$Q.m3s,
     main = "Stage vs. Discharge",
     xlab = "Water Level (m)", ylab = "Discharge (m³/s)",
     pch = 19, col = "blue")

# Log-transformed model predictions
pred_log <- exp(predict(log_model, newdata = rating_data))
lines(rating_data$Baro_Cor_Lvl, pred_log, col = "red", lwd = 2)

# Linear model predictions
pred_linear <- predict(linear_model, newdata = rating_data)
lines(rating_data$Baro_Cor_Lvl, pred_linear, col = "green", lwd = 2)

# Polynomial model predictions
pred_poly <- predict(poly_model, newdata = rating_data)
lines(rating_data$Baro_Cor_Lvl, pred_poly, col = "purple", lwd = 2)

# Legend
legend("topleft", legend = c("Observed", "Log-Transformed", "Linear", "Polynomial"),
       col = c("blue", "red", "green", "purple"), pch = c(19, NA, NA, NA), lty = c(NA, 1, 1, 1), lwd = c(NA, 2, 2, 2))

###################################################
#### Extrapolate model to calculate discharge? ####
###################################################

# # Extract parameters from the fitted model
# params <- coef(rating_curve)
# a <- params["a"]
# b <- params["b"]
# h0 <- params["h0"]
# 
# # Apply the power-law equation to the LEVEL.cm data in the dataset
# USF21 <- USF21 %>%
#   mutate(Discharge = a * (Baro_Cor_Lvl - h0)^b)
# 
# # Check the first few rows with computed Discharge
# head(USF21)
# 
# # Plot 
# ggplot(USF21, aes(x = DateTime, y = Discharge)) +
#   geom_point(color = "blue") +
#   labs(title = "Discharge", x = "DateTime", y = "Discharge (Q)") +
#   theme_minimal()
# 
# 
# # Check the first few rows with computed discharge in m³/s
# head(USF21)

#######################
#### Predicted log ####
#######################

# Log-transformed model parameters
a_log <- exp(coef(log_model)[1])  # Intercept
b_log <- coef(log_model)[2]       # Slope

# Predict discharge for the entire dataset
USF21 <- USF21 %>%
  mutate(Predicted_Discharge_Log = a_log * (Baro_Cor_Lvl ^ b_log))

##########################
#### Predicted linear ####
##########################
# Linear model parameters
a_linear <- coef(linear_model)[1]  # Intercept
b_linear <- coef(linear_model)[2]  # Slope

# Predict discharge for the entire dataset
USF21 <- USF21 %>%
  mutate(Predicted_Discharge_Linear = a_linear + b_linear * Baro_Cor_Lvl)

##############################
#### Predicted Polynomial ####
##############################
# Polynomial model parameters
a_poly <- coef(poly_model)[1]      # Intercept
b1_poly <- coef(poly_model)[2]     # Linear term
b2_poly <- coef(poly_model)[3]     # Quadratic term

# Predict discharge for the entire dataset
USF21 <- USF21 %>%
  mutate(Predicted_Discharge_Poly = a_poly + b1_poly * Baro_Cor_Lvl + b2_poly * Baro_Cor_Lvl^2)

#############################
#### Compare predictions ####
#############################
# Visualize predictions
plot(USF21$Baro_Cor_Lvl, USF21$Predicted_Discharge_Log, col = "red", type = "l", lwd = 2,
     xlab = "Stage (m)", ylab = "Discharge (m³/s)", main = "Discharge Predictions")
lines(USF21$Baro_Cor_Lvl, USF21$Predicted_Discharge_Linear, col = "green", lwd = 2)
lines(USF21$Baro_Cor_Lvl, USF21$Predicted_Discharge_Poly, col = "purple", lwd = 2)
legend("topleft", legend = c("Log-Transformed", "Linear", "Polynomial"),
       col = c("red", "green", "purple"), lty = 1, lwd = 2)

# Compare Predicted vs. Observed Discharge
ggplot(USF21, aes(x = Q.m3s)) +
  geom_point(aes(y = Predicted_Discharge_Log, color = "Log Model")) +
  geom_point(aes(y = Predicted_Discharge_Linear, color = "Linear Model")) +
  geom_point(aes(y = Predicted_Discharge_Poly, color = "Polynomial Model")) +
  labs(
    title = "Comparison of Observed vs Predicted Discharge",
    x = "Observed Discharge (m³/s)",
    y = "Predicted Discharge (m³/s)"
  ) +
  scale_color_manual(values = c("red", "green", "purple")) +
  theme_minimal()

# Residuals
USF21 <- USF21 %>%
  mutate(
    Residual_Log = Q.m3s - Predicted_Discharge_Log,
    Residual_Linear = Q.m3s - Predicted_Discharge_Linear,
    Residual_Poly = Q.m3s - Predicted_Discharge_Poly
  )

ggplot(USF21, aes(x = Baro_Cor_Lvl)) +
  geom_point(aes(y = Residual_Log, color = "Log Model")) +
  geom_point(aes(y = Residual_Linear, color = "Linear Model")) +
  geom_point(aes(y = Residual_Poly, color = "Polynomial Model")) +
  labs(
    title = "Residuals for Different Models",
    x = "Barometric Corrected Level (m)",
    y = "Residuals (Observed - Predicted)"
  ) +
  scale_color_manual(values = c("red", "green", "purple")) +
  theme_minimal()

######################################
#### Plot and compare predictions ####
######################################

USF21$DateTime <- as.POSIXct(USF21$DateTime)

ggplot(USF21, aes(x = DateTime, y = Predicted_Discharge_Log)) +
  geom_point(color = "blue") +
  labs(title = "Predicted Discharge (Log)", x = "DateTime", y = "Discharge (m3/s)") +
  scale_x_datetime(date_breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(USF21, aes(x = DateTime, y = Predicted_Discharge_Linear)) +
  geom_point(color = "blue") +
  labs(title = "Predicted Discharge (Linear)", x = "DateTime", y = "Discharge (m3/s)") +
  scale_x_datetime(date_breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(USF21, aes(x = DateTime, y = Predicted_Discharge_Poly)) +
  geom_point(color = "blue") +
  labs(title = "Predicted Discharge (Poly)", x = "DateTime", y = "Discharge (m3/s)")  +
  scale_x_datetime(date_breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###################
#### Save file ####
###################

write.csv(USF21, "data/discharge_USF21.csv")

drive_folder_id <- "1krhGD6TkA7nf6xb3EXD-MRUhBCDpkfhz"

# Upload file to the specified Google Drive folder
drive_put(
  media = "data/discharge_USF21.csv",
  path = as_id(drive_folder_id)
)
