##==============================================================================
## Project: QuEST
## This script is to calculate discharge from compensated pressure data for BRMQ4
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

#BRMQ4
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="offset_BRMQ4.csv"], 
                            path = "googledrive/offset_BRMQ4.csv",
                            overwrite = T)
# load file
BRMQ4 <- read.csv("googledrive/offset_BRMQ4.csv")
# convert Date column to Date type if not already
BRMQ4$Date <- as.Date(BRMQ4$Date.x)
# combine Date and Time columns into a new DateTime column
BRMQ4$DateTime <- paste(BRMQ4$Date.x, BRMQ4$Time.x, sep = " ")
# convert the DateTime column to POSIXct
BRMQ4$DateTime <- as.POSIXct(BRMQ4$DateTime, format = "%Y-%m-%d %H:%M:%S")

# filter out rows with missing stage or discharge
rating_data <- BRMQ4 %>% 
  filter(!is.na(Baro_Cor_offset1), !is.na(Q_L_per_s))

# check the structure of the cleaned data
head(rating_data)

# filter out rows with missing Baro NAs
BRMQ4_baro <- BRMQ4 %>% 
  filter(!is.na(Baro_Cor_offset1))

########################################
#### Plot pressure compensated data ####
########################################
ggplot(data = BRMQ4_baro, aes(x = DateTime, y = Baro_Cor_offset1)) +
  geom_line() + ggtitle("BRMQ4 compensated level data")

ggplot(data = BRMQ4_baro, aes(x = DateTime, y = pres_m)) +
  geom_line() + ggtitle("BRMQ4 level data in m")

##################################
#### Plot Stage vs. Discharge ####
##################################
# discharge from L/s to m3/s
rating_data <- rating_data %>%
  mutate(Q.m3s = Q_L_per_s/1000)

# plot with date info
ggplot(rating_data, aes(x = Baro_Cor_offset1, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m³/s)") +
  theme_minimal()

# #  filter out discharge that is not working for me
# rating_data <- rating_data %>% 
#   filter(!Date.y %in% c("2025-02-17","2025-02-28", "2025-04-09", "2025-03-10"))

ggplot(rating_data, aes(x = Baro_Cor_offset1, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # Adds date labels above points
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m³/s)") +
  theme_minimal()

###########################################
#### Check for Log-Linear Relationship ####
###########################################
rating_data <- rating_data %>%
  mutate(Baro_Cor_offset_edited = Baro_Cor_offset1 + 10)
BRMQ4 <- BRMQ4 %>%
  mutate(Baro_Cor_offset_edited = Baro_Cor_offset1 + 10)

ggplot(rating_data, aes(x = log(Baro_Cor_offset_edited), y = log(Q.m3s))) +
  geom_point(color = "blue") +
  labs(title = "Log-Log Plot of Water Level vs. Discharge", 
       x = "Log(Water Level)", y = "Log(Discharge)") +
  theme_minimal()

####################
#### Log model? ####
####################
rating_data <- rating_data %>%
  mutate(Baro_Cor_offset_edited = Baro_Cor_offset_edited + 10)
BRMQ4 <- BRMQ4 %>%
  mutate(Baro_Cor_offset_edited = Baro_Cor_offset_edited + 10)

rating_data <- rating_data %>%
  mutate(Log_Stage = log(Baro_Cor_offset_edited),
         Log_Discharge = log(Q.m3s))

log_model <- lm(Log_Discharge ~ Log_Stage, data = rating_data)

summary(log_model)

a <- exp(coef(log_model)[1])  # Back-transform intercept
b <- coef(log_model)[2]       # Slope

#######################
#### Linear model? ####
#######################
linear_model <- lm(Q.m3s ~ Baro_Cor_offset_edited, data = rating_data)

summary(linear_model)

###########################
#### Visualize models  ####
###########################
# observed data
plot(rating_data$Baro_Cor_offset_edited, rating_data$Q.m3s,
     main = "Stage vs. Discharge",
     xlab = "Water Level (m)", ylab = "Discharge (m³/s)",
     pch = 19, col = "blue")

# log-transformed model predictions
pred_log <- exp(predict(log_model, newdata = rating_data))
lines(rating_data$Baro_Cor_offset_edited, pred_log, col = "red", lwd = 2)

# linear model predictions
pred_linear <- predict(linear_model, newdata = rating_data)
lines(rating_data$Baro_Cor_offset_edited, pred_linear, col = "green", lwd = 2)

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
BRMQ4 <- BRMQ4 %>%
  mutate(Predicted_Discharge_Log = a_log * (Baro_Cor_offset_edited ^ b_log))

##########################
#### Predicted linear ####
##########################
# linear model parameters
a_linear <- coef(linear_model)[1]  # Intercept
b_linear <- coef(linear_model)[2]  # Slope

# predict discharge for the entire dataset
BRMQ4 <- BRMQ4 %>%
  mutate(Predicted_Discharge_Linear = a_linear + b_linear * Baro_Cor_offset_edited)

#############################
#### Compare predictions ####
#############################
# visualize predictions
plot(BRMQ4$Baro_Cor_offset_edited, BRMQ4$Predicted_Discharge_Log, col = "red", type = "l", lwd = 2,
     xlab = "Stage (m)", ylab = "Discharge (m³/s)", main = "Discharge Predictions")
lines(BRMQ4$Baro_Cor_offset_edited, BRMQ4$Predicted_Discharge_Linear, col = "green", lwd = 2)
legend("topleft", legend = c("Log-Transformed", "Linear"),
       col = c("red", "green"), lty = 1, lwd = 2)


# discharge from L/s to m3/s for entire dataset
BRMQ4 <- BRMQ4 %>%
  mutate(Q.m3s = Q_L_per_s/1000)

# compare Predicted vs. Observed Discharge
ggplot(BRMQ4, aes(x = Q.m3s)) +
  geom_point(aes(y = Predicted_Discharge_Log, color = "Log Model")) +
  geom_point(aes(y = Predicted_Discharge_Linear, color = "Linear Model")) +
  labs(
    title = "Comparison of Observed vs Predicted Discharge",
    x = "Observed Discharge (m³/s)",
    y = "Predicted Discharge (m³/s)"
  ) +
  scale_color_manual(values = c("red", "green")) +
  theme_minimal()

# residuals
BRMQ4 <- BRMQ4 %>%
  mutate(
    Residual_Log = Q.m3s - Predicted_Discharge_Log,
    Residual_Linear = Q.m3s - Predicted_Discharge_Linear
  )

ggplot(BRMQ4, aes(x = Baro_Cor_offset_edited)) +
  geom_point(aes(y = Residual_Log, color = "Log Model")) +
  geom_point(aes(y = Residual_Linear, color = "Linear Model")) +
  labs(
    title = "Residuals for Different Models",
    x = "Barometric Corrected Level (m)",
    y = "Residuals (Observed - Predicted)"
  ) +
  scale_color_manual(values = c("red", "green")) +
  theme_minimal()

BRMQ4clean <- BRMQ4 %>%
  filter(Predicted_Discharge_Log <= 200)

######################################
#### Plot and compare predictions ####
######################################
BRMQ4$DateTime <- as.POSIXct(BRMQ4$DateTime)

p1 <- ggplot(BRMQ4, aes(x = DateTime, y = Predicted_Discharge_Log)) +
  geom_line(color = "blue") +
  labs(title = "Predicted Discharge (Log)", x = "DateTime", y = "Discharge (m3/s)") +
  scale_x_datetime(date_breaks = "2 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(trans = "log")
p2 <- ggplot(BRMQ4clean, aes(x = DateTime, y = Predicted_Discharge_Log)) +
  geom_point(color = "blue") +
  labs(title = "Predicted Discharge (Log)", x = "DateTime", y = "Discharge (m3/s)") +
  scale_x_datetime(date_breaks = "2 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p3 <- ggplot(BRMQ4, aes(x = DateTime, y = Predicted_Discharge_Log)) +
  geom_line(color = "blue") +
  labs(title = "Predicted Discharge (Log)", x = "DateTime", y = "Discharge (m3/s)") +
  scale_x_datetime(date_breaks = "2 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)
print(p2)
print(p3)

ggsave("figures/BRMQ4_pred_log_scaled.png", p1,
       width = 8, height = 4, dpi = 300)

ggsave("figures/BRMQ4_clean.png", p2,
       width = 8, height = 4, dpi = 300)

ggsave("figures/BRMQ4_pred_log.png", p3,
       width = 8, height = 4, dpi = 300)

###################
#### Save file ####
###################
# Save uncleaned file to drive
write.csv(BRMQ4, "data/discharge_BRMQ4.csv")

drive_folder_id <- "1PNCX_xYwu57gYMFNLtHiAFbBi7m-L1Uf"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/discharge_BRMQ4.csv",
  path = as_id(drive_folder_id)
)

# Save cleaned file to drive
write.csv(BRMQ4clean, "data/discharge_BRMQ4.csv")

drive_folder_id <- "1PNCX_xYwu57gYMFNLtHiAFbBi7m-L1Uf"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/discharge_BRMQ4.csv",
  path = as_id(drive_folder_id)
)

