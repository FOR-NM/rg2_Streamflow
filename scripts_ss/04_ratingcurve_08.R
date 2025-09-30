##==============================================================================
## Project: QuEST
## This script is to calculate discharge from compensated pressure data for South Sandy SST08 site
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
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1GcRdU4UaBrxEmVhZYoZuTae6HxVZt3Gm")

# list all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3

#SST08
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="offset_SST08.csv"], 
                            path = "googledrive/offset_SST08.csv",
                            overwrite = T)
# load file
SST08 <- read.csv("googledrive/offset_SST08.csv")

# convert Date column to Date type if not already
SST08$Date <- as.Date(SST08$Date.x)
SST08$DateTime <- as.POSIXct(SST08$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "MST")
head(SST08)

# filter out rows with missing stage or discharge
rating_data <- SST08 %>% 
  filter(!is.na(Baro_Cor_offset3), !is.na(Q..L.s.))

# check the structure of the cleaned data
head(rating_data)

##################################
#### Plot Stage vs. Discharge ####
##################################
# but first stage
ggplot(data = SST08, aes(x = DateTime, y = Baro_Cor_offset3)) +
  geom_line() + ggtitle("SST08 compensated level data")

# discharge from L/s to m3/s
rating_data <- rating_data %>%
  mutate(Q.m3s = Q..L.s./1000)

# plot with date info
ggplot(rating_data, aes(x = Baro_Cor_offset3, y = Q.m3s)) +
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

###########################################
#### Check for Log-Linear Relationship ####
###########################################
ggplot(rating_data, aes(x = log(Baro_Cor_offset3), y = log(Q.m3s))) +
  geom_point(color = "blue") +
  labs(title = "Log-Log Plot of Water Level vs. Discharge", 
       x = "Log(Water Level)", y = "Log(Discharge)") +
  theme_minimal()

####################
#### Log model? ####
####################
rating_data <- rating_data %>%
  mutate(Log_Stage = log(Baro_Cor_offset3),
         Log_Discharge = log(Q.m3s))

log_model <- lm(Log_Discharge ~ Log_Stage, data = rating_data)

summary(log_model)

a <- exp(coef(log_model)[1])  # Back-transform intercept
b <- coef(log_model)[2]       # Slope

#######################
#### Linear model? ####
#######################
linear_model <- lm(Q.m3s ~ Baro_Cor_offset3, data = rating_data)

summary(linear_model)

###########################
#### Visualize models  ####
###########################
# observed data
plot(rating_data$Baro_Cor_offset3, rating_data$Q.m3s,
     main = "Stage vs. Discharge",
     xlab = "Water Level (m)", ylab = "Discharge (m³/s)",
     pch = 19, col = "blue")

# log-transformed model predictions
pred_log <- exp(predict(log_model, newdata = rating_data))
lines(rating_data$Baro_Cor_offset3, pred_log, col = "red", lwd = 2)

# linear model predictions
pred_linear <- predict(linear_model, newdata = rating_data)
lines(rating_data$Baro_Cor_offset3, pred_linear, col = "green", lwd = 2)

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
SST08 <- SST08 %>%
  mutate(Predicted_Discharge_Log = a_log * (Baro_Cor_offset3 ^ b_log))

##########################
#### Predicted linear ####
##########################
# linear model parameters
a_linear <- coef(linear_model)[1]  # Intercept
b_linear <- coef(linear_model)[2]  # Slope

# predict discharge for the entire dataset
SST08 <- SST08 %>%
  mutate(Predicted_Discharge_Linear = a_linear + b_linear * Baro_Cor_offset3)

#############################
#### Compare predictions ####
#############################
# visualize predictions
plot(SST08$Baro_Cor_offset3, SST08$Predicted_Discharge_Log, col = "red", type = "l", lwd = 2,
     xlab = "Stage (m)", ylab = "Discharge (m³/s)", main = "Discharge Predictions")
lines(SST08$Baro_Cor_offset3, SST08$Predicted_Discharge_Linear, col = "green", lwd = 2)
legend("topleft", legend = c("Log-Transformed", "Linear"),
       col = c("red", "green"), lty = 1, lwd = 2)

# discharge from L/s to m3/s for entire dataset
SST08 <- SST08 %>%
  mutate(Q.m3s = Q..L.s./1000)

# compare Predicted vs. Observed Discharge
ggplot(SST08, aes(x = Q.m3s)) +
  geom_point(aes(y = Predicted_Discharge_Log, color = "Log Model")) +
  geom_point(aes(y = Predicted_Discharge_Linear, color = "Linear Model")) +
  labs(
    title = "Comparison of Observed vs Predicted Discharge",
    x = "Observed Discharge (m³/s)",
    y = "Predicted Discharge (m³/s)"
  ) +
  theme_minimal()

# residuals
SST08 <- SST08 %>%
  mutate(
    Residual_Log = Q.m3s - Predicted_Discharge_Log,
    Residual_Linear = Q.m3s - Predicted_Discharge_Linear
  )

ggplot(SST08, aes(x = Baro_Cor_offset3)) +
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
SST08$DateTime <- as.POSIXct(SST08$DateTime)

ggplot(SST08, aes(x = DateTime, y = Predicted_Discharge_Log)) +
  geom_line(color = "blue") +
  labs(title = "Predicted Discharge (Log)", x = "DateTime", y = "Discharge (m3/s)") +
  scale_x_datetime(date_breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(SST08, aes(x = DateTime, y = Predicted_Discharge_Linear)) +
  geom_line(color = "blue") +
  labs(title = "Predicted Discharge (Linear)", x = "DateTime", y = "Discharge (m3/s)") +
  scale_x_datetime(date_breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###################
#### Save file ####
###################
write.csv(SST08, "data/discharge_SST08.csv")

drive_folder_id <- "1tkYDtcrI_wgbb16zufLJizcjfMzePtkw"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/discharge_SST08.csv",
  path = as_id(drive_folder_id)
)

