##==============================================================================
## Project: QuEST
## This script is to calculate discharge from compensated pressure data for Santa Fe USF13 site
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
USF13$Date <- as.Date(USF13$Date.x)
USF13$DateTime <- as.POSIXct(USF13$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "MST")
head(USF13)

# filter out rows with missing stage or discharge
rating_data <- USF13 %>% 
  filter(!is.na(Baro_Cor_Lvl), !is.na(Q))

# check the structure of the cleaned data
head(rating_data)

##################################
#### Plot Stage vs. Discharge ####
##################################

ggplot(rating_data, aes(x = Baro_Cor_Lvl, y = Q)) +
  geom_point(color = "blue") +
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL.m)", y = "Discharge (L/s)") +
  theme_minimal()

# discharge from L/s to m3/s
rating_data <- rating_data %>%
  mutate(Q.m3s = Q/1000)

ggplot(rating_data, aes(x = Baro_Cor_Lvl, y = Q.m3s)) +
  geom_point(color = "blue") +
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

# pt depth from cm to m
rating_data <- rating_data %>%
  mutate(pt_depth_m = pt_depth_cm/100)

# plot discharge vs manual stage measurement
ggplot(rating_data, aes(x = pt_depth_m, y = Q.m3s)) +
  geom_point(color = "blue") +
  labs(title = "Manual Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  theme_minimal()

###########################################
#### Check for Log-Linear Relationship ####
###########################################

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

# observed data
plot(rating_data$Baro_Cor_Lvl, rating_data$Q.m3s,
     main = "Stage vs. Discharge",
     xlab = "Water Level (m)", ylab = "Discharge (m³/s)",
     pch = 19, col = "blue")

# log-transformed model predictions
pred_log <- exp(predict(log_model, newdata = rating_data))
lines(rating_data$Baro_Cor_Lvl, pred_log, col = "red", lwd = 2)

# linear model predictions
pred_linear <- predict(linear_model, newdata = rating_data)
lines(rating_data$Baro_Cor_Lvl, pred_linear, col = "green", lwd = 2)

# polynomial model predictions
pred_poly <- predict(poly_model, newdata = rating_data)
lines(rating_data$Baro_Cor_Lvl, pred_poly, col = "purple", lwd = 2)

# legend
legend("topleft", legend = c("Observed", "Log-Transformed", "Linear", "Polynomial"),
       col = c("blue", "red", "green", "purple"), pch = c(19, NA, NA, NA), lty = c(NA, 1, 1, 1), lwd = c(NA, 2, 2, 2))

###################################################
#### Extrapolate model to calculate discharge? ####
###################################################

# # extract parameters from the fitted model
# params <- coef(rating_curve)
# a <- params["a"]
# b <- params["b"]
# h0 <- params["h0"]
# 
# # apply the power-law equation to the LEVEL.cm data in the dataset
# USF21 <- USF21 %>%
#   mutate(Discharge = a * (Baro_Cor_Lvl - h0)^b)
# 
# # check the first few rows with computed Discharge
# head(USF21)
# 
# # plot 
# ggplot(USF21, aes(x = DateTime, y = Discharge)) +
#   geom_point(color = "blue") +
#   labs(title = "Discharge", x = "DateTime", y = "Discharge (Q)") +
#   theme_minimal()
# 
# 
# # check the first few rows with computed discharge in m³/s
# head(USF21)

#######################
#### Predicted log ####
#######################

# log-transformed model parameters
a_log <- exp(coef(log_model)[1])  # Intercept
b_log <- coef(log_model)[2]       # Slope

# predict discharge for the entire dataset
USF13 <- USF13 %>%
  mutate(Predicted_Discharge_Log = a_log * (Baro_Cor_Lvl ^ b_log))

##########################
#### Predicted linear ####
##########################
# linear model parameters
a_linear <- coef(linear_model)[1]  # Intercept
b_linear <- coef(linear_model)[2]  # Slope

# predict discharge for the entire dataset
USF13 <- USF13 %>%
  mutate(Predicted_Discharge_Linear = a_linear + b_linear * Baro_Cor_Lvl)

##############################
#### Predicted Polynomial ####
##############################
# polynomial model parameters
a_poly <- coef(poly_model)[1]      # Intercept
b1_poly <- coef(poly_model)[2]     # linear term
b2_poly <- coef(poly_model)[3]     # Quadratic term

# predict discharge for the entire dataset
USF13 <- USF13 %>%
  mutate(Predicted_Discharge_Poly = a_poly + b1_poly * Baro_Cor_Lvl + b2_poly * Baro_Cor_Lvl^2)

#############################
#### Compare predictions ####
#############################

# visualize predictions
plot(USF13$Baro_Cor_Lvl, USF13$Predicted_Discharge_Log, col = "red", type = "l", lwd = 2,
     xlab = "Stage (m)", ylab = "Discharge (m³/s)", main = "Discharge Predictions")
lines(USF13$Baro_Cor_Lvl, USF13$Predicted_Discharge_Linear, col = "green", lwd = 2)
lines(USF13$Baro_Cor_Lvl, USF13$Predicted_Discharge_Poly, col = "purple", lwd = 2)
legend("topleft", legend = c("Log-Transformed", "Linear", "Polynomial"),
       col = c("red", "green", "purple"), lty = 1, lwd = 2)

# discharge from L/s to m3/s for entire dataset
USF13 <- USF13 %>%
  mutate(Q.m3s = Q/1000)

# compare Predicted vs. Observed Discharge
ggplot(USF13, aes(x = Q.m3s)) +
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

# residuals
USF13 <- USF13 %>%
  mutate(
    Residual_Log = Q.m3s - Predicted_Discharge_Log,
    Residual_Linear = Q.m3s - Predicted_Discharge_Linear,
    Residual_Poly = Q.m3s - Predicted_Discharge_Poly
  )

ggplot(USF13, aes(x = Baro_Cor_Lvl)) +
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

USF13$DateTime <- as.POSIXct(USF13$DateTime)

ggplot(USF13, aes(x = DateTime, y = Predicted_Discharge_Log)) +
  geom_point(color = "blue") +
  labs(title = "Predicted Discharge (Log)", x = "DateTime", y = "Discharge (m3/s)") +
  scale_x_datetime(date_breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(USF13, aes(x = DateTime, y = Predicted_Discharge_Linear)) +
  geom_point(color = "blue") +
  labs(title = "Predicted Discharge (Linear)", x = "DateTime", y = "Discharge (m3/s)") +
  scale_x_datetime(date_breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(USF13, aes(x = DateTime, y = Predicted_Discharge_Poly)) +
  geom_point(color = "blue") +
  labs(title = "Predicted Discharge (Poly)", x = "DateTime", y = "Discharge (m3/s)")  +
  scale_x_datetime(date_breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###################
#### Save file ####
###################

write.csv(USF13, "data/discharge_USF13.csv")

drive_folder_id <- "1fPDNinUQ3pCFFQXJ1dtLGbqEawyTmPUx"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/discharge_USF13.csv",
  path = as_id(drive_folder_id)
)
