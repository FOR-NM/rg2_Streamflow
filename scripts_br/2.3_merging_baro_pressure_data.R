##==============================================================================
## Project: QuEST
## Script to ... Fayetteville and baro logger pressure data
## --- 
##==============================================================================
##################
#### Packages ####
##################
library(ggplot2)
library(dplyr)
library(lubridate) 

##############################
#### Import pressure data ####
##############################  
fayetteville <- googledrive::as_id("https://drive.google.com/drive/folders/1FgFNGzv0Rh5t62V8SRFdwK6sd_ktwcRD")
# list all CSV files in the folder
pressure <- googledrive::drive_ls(path = fayetteville)
# choose the specific file by name
pressure <- pressure %>% filter(name == "fayetteville_pressure.csv")
# download the most recent CSV file
drive_download(as_id(pressure$id), path = "googledrive/fayetteville_pressure.csv", overwrite = TRUE)
# fetch the file
fayetteville <- read.csv("googledrive/fayetteville_pressure.csv")

# convert the DateTime column to POSIXct
fayetteville$DateTime <- as.POSIXct(fayetteville$time, format = "%Y-%m-%d %H:%M:%S")

#### plot data ####
ggplot(data = fayetteville, aes(x = DateTime, y = pres_m)) +
  geom_vline(xintercept = as.POSIXct("2025-03-14 12:00:00"), linetype="dashed", color="red") +
  geom_line() + ggtitle("Fayetteville pressure data")

fayetteville$pres_m_fyvl <- (fayetteville$pres_m)

#### import baro logger data ####
baro_logger <- googledrive::as_id("https://drive.google.com/drive/folders/1JZ60Fb1WmfkSZiVXJxL1X9nzkRV7AwHq")

# list all CSV files in the folder
baro <- googledrive::drive_ls(path = baro_logger)
# choose the specific file by name
baro <- baro %>% filter(name == "2025-09-19_BARO_RAW.csv")
# download the most recent CSV file
drive_download(as_id(baro$id), path = "googledrive/2025-09-19_BARO_RAW.csv", overwrite = TRUE)
# fetch the file
baro_log <- read.csv("googledrive/2025-09-19_BARO_RAW.csv", skip = 1)

# convert the DateTime column to POSIXct
baro_log$DateTime <- as.POSIXct(baro_log$Date.Time..GMT.05.00, format = "%m/%d/%y %I:%M:%S %p")

########################
#### Clean up files ####
########################
# rename columns to match
baro_log <- baro_log %>% 
  rename("pres.psi" = "Abs.Pres..psi..LGR.S.N..21312563..SEN.S.N..21312563.",
         "Temp.F" = "Temp...F..LGR.S.N..21312563..SEN.S.N..21312563.")

# # remove some rows
# baro_log <- baro_log[,-c(1, 6:8)]
# fayetteville <- fayetteville[,-c(1, 2)]

# psi to m
baro_log <- baro_log %>%
  mutate(pres_m_baro = (pres.psi * 0.703070))
# 1 psi = 0.703070 m

#### plot baro data ####
ggplot(data = baro_log, aes(x = DateTime, y = pres_m_baro)) +
  geom_vline(xintercept = as.POSIXct("2025-03-14 12:00:00"), linetype="dashed", color="red") +
  geom_line() + ggtitle("Baro pressure data")

#############################################
#### Clean error section in baro logger  ####
#############################################
baro_log <- baro_log %>% mutate(pres_m_clean = replace(pres_m_baro, DateTime<"2025-05-31", NA))

#######################
#### Combine files ####
#######################
pres <- dplyr::left_join(fayetteville, baro_log) %>%
  arrange(DateTime) %>%  #
  distinct(DateTime, .keep_all = TRUE) # remove duplicates

###################
#### Plot data ####
###################
ggplot(data = pres, aes(x = DateTime, y = pres_m_baro)) +
  geom_vline(xintercept = as.POSIXct("2025-03-14 12:00:00"), linetype="dashed", color="red") +
  geom_line() + ggtitle("Pressure data")
ggplot(data = pres, aes(x = DateTime, y = pres_m_clean)) +
  geom_vline(xintercept = as.POSIXct("2025-03-14 12:00:00"), linetype="dashed", color="red") +
  geom_line() + ggtitle("Pressure data")

#########################################
#### Understanding baro logger error ####
#########################################
# filter the data for June (month 6) and July (month 7)
pres_filtered <- pres %>%
  filter(
    as.integer(format(DateTime, "%m")) %in% c(6, 7)
  )

# filter the data for March, April and May
pres_filtered <- pres %>%
  filter(
    as.integer(format(DateTime, "%m")) %in% c(3, 4, 5)
  )

# create the Scatter Plot
pressure_plot <- ggplot(pres_filtered, aes(x = pres_m, y = pres_m_baro)) +
  # add scatter points
  geom_point(alpha = 0.5, color = "dodgerblue") +
  # ad a linear regression line to visualize the correlation (optional but highly recommended)
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  # add labels and title
  labs(
    title = paste("Airport Pressure vs. Barometer Pressure"),
    x = "Airport Data (pres_m)",
    y = "Barometer Data (pres_m_baro)"
  ) +
  theme_minimal() +
  # add a diagonal line for perfect 1:1 comparison (optional)
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "gray50")

# display the plot
print(pressure_plot)

#################################
#### Fill incomplete section ####
#################################
# Define the time window after the missing section to train the model
train_start <- as.POSIXct("2025-05-31") # Date local barologger was installed
train_end <- as.POSIXct("2025-09-19") 

# Filter data where both sensors were running
training_data <- pres %>%
  filter(DateTime >= train_start & DateTime <= train_end,
         !is.na(pres_m_fyvl), !is.na(pres_m_clean))

# Fit the linear model: Local Pressure (Y) ~ Airport Pressure (X)
local_model <- lm(pres_m_clean ~ pres_m_fyvl, data = training_data) 
# summary
summary(local_model) 

# Extract new coefficients
a_local <- coef(local_model)[1] # Intercept
b_local <- coef(local_model)[2] # Slope

# Predict Local Pressure from Airport Data for All Times ---
pres <- pres %>%
  # Use the Airport Data (pres_m_fyvl) with the derived model to get the 
  # 'corrected' or 'local-adjusted' airport pressure.
  mutate(Predicted_Local_Pressure = a_local + b_local * pres_m_fyvl)

# Stitch the Data ---
# Create the final, continuous local pressure time series.
pres <- pres %>%
  mutate(Final_Local_Pressure = ifelse(
    # If when the local barologger data is available AND we are in the training period
    DateTime >= train_start, 
    # THEN use the raw local barologger data (pres_m_clean)
    pres_m_clean, 
    # ELSE (before train_start), use the model-adjusted airport data
    Predicted_Local_Pressure
  ))

# --- Residual Plot (Correction applied to Airport Data) ---
pres <- pres %>%
  # Residual is the difference between the actual Local Barologger and the prediction
  # ONLY calculate this residual for the period when the barologger was running (train_start onwards)
  mutate(Residual_local = ifelse(DateTime >= train_start, pres_m_clean - Predicted_Local_Pressure, NA))

ggplot(pres, aes(x = DateTime, y = Residual_local)) +
  geom_line(color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Local Barologger Residuals (Local - Predicted)",
       subtitle = "Calculated only for the post-installation period",
       y = "Residual (kPa)",
       x = "DateTime") +
  theme_minimal()

# Plot
ggplot(data = pres, aes(x = DateTime)) +
  # 1. Original Airport Data (raw data used for prediction)
  geom_line(aes(y = pres_m_fyvl, color = "1. Original Airport Pressure"), na.rm = TRUE, size = 0.8) +
  # 2. Final Stitched Data (your resulting time series)
  geom_line(aes(y = Final_Local_Pressure, color = "3. Final Stitched Local Pressure"), size = 0.8) +
  # 3. Raw Local Barologger Data (to show the high-quality data used for the latter half)
  geom_point(aes(y = pres_m_clean, color = "2. Raw Local Barologger Pressure"), size = 0.1, alpha = 0.5) +
  labs(title = "Original Airport Data vs. Final Stitched Local Pressure",
       x = "Date and Time",
       y = "Pressure (kPa)",
       color = "Data Series") +
  scale_color_manual(values = c("1. Original Airport Pressure" = "blue", 
                                "3. Final Stitched Local Pressure" = "red", 
                                "2. Raw Local Barologger Pressure" = "green")) +
  theme_minimal() +
  theme(legend.position = "bottom")

############################
#### Save combined file ####
############################
# write files to local data folder
write.csv(pres, 'air_br/air_br.csv')

# this is the baro folder
drive_folder_id <- "1BB6nEoVQOrCd_uHEW9n66kR8HCSUUmbC"

# upload file to the specified Google Drive folder
drive_put(
  media = 'air_br/air_br.csv',
  path = as_id(drive_folder_id)
)
