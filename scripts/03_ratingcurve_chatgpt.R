##==============================================================================
## Project: QuEST
## This script is to calculate discharge from compensated pressure data for USF21 
## press Command+Option+O to collapse all sections and get an overview of the workflow
##==============================================================================

##################
#### Packages ####
##################
library(googledrive)
library(ggplot2)
library(lubridate)
library(dplyr)
library(minpack.lm) # For nonlinear regression

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
  filter(!is.na(LEVEL.cm), !is.na(Q))

# Check the structure of the cleaned data
head(rating_data)

##################################
#### Plot Stage vs. Discharge ####
##################################

ggplot(rating_data, aes(x = LEVEL.cm, y = Q)) +
  geom_point(color = "blue") +
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL.cm)", y = "Discharge (Q)") +
  theme_minimal()

###########################################
#### Check for Log-Linear Relationship ####
###########################################

ggplot(rating_data, aes(x = log(LEVEL.cm), y = log(Q))) +
  geom_point(color = "blue") +
  labs(title = "Log-Log Plot of Stage vs. Discharge", 
       x = "Log(Stage)", y = "Log(Discharge)") +
  theme_minimal()

################
#### Model? ####
################

sum(is.na(USF21$LEVEL.m))     # Count NA values in LEVEL.m
sum(is.na(USF21$Pres.abs.kPa)) # Count NA values in Pres.abs.kPa

# Constants
density_water <- 1000  # kg/m³
g <- 9.81  # m/s²

USF21 <- USF21 %>%
  mutate(WaterDepth = (LELVEL.m - Pres.abs.kPa) / (density_water * g))

# Stage-Discharge dataset
stage_discharge <- data.frame(
  Stage = c(16.0, 22.5, 27.4, 19.8, 16.0),
  Discharge = c(30.04745, 37.65501, 60.43060, 33.64554, 27.67575)
)

# Visualize the data
plot(stage_discharge$Stage, stage_discharge$Discharge,
     main = "Stage vs. Discharge",
     xlab = "Stage (cm)", ylab = "Discharge (m³/s)",
     pch = 19, col = "blue")

# Fit power-law curve
rating_curve <- nls(
  Discharge ~ a * (Stage - h0)^b,
  data = stage_discharge,
  start = list(a = 2, b = 1.5, h0 = 10), # Adjusted initial guesses
  algorithm = "port", # Ensures bounds are respected
  lower = c(a = 0, b = 0, h0 = 0) # Ensures parameters remain non-negative
)

# Model summary
summary(rating_curve)

# Generate predictions
predicted_stage <- seq(min(stage_discharge$Stage), max(stage_discharge$Stage), length.out = 100)
predicted_discharge <- predict(rating_curve, newdata = data.frame(Stage = predicted_stage))

# Plot data and model fit
plot(stage_discharge$Stage, stage_discharge$Discharge,
     main = "Stage vs. Discharge with Fitted Curve",
     xlab = "Stage (cm)", ylab = "Discharge (m³/s)",
     pch = 19, col = "blue")
lines(predicted_stage, predicted_discharge, col = "red", lwd = 2)
legend("topleft", legend = c("Observed", "Fitted Curve"),
       col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2))

####################
#### Discharge? ####
####################

# Extract parameters from the fitted model
params <- coef(rating_curve)
a <- params["a"]
b <- params["b"]
h0 <- params["h0"]

# Apply the power-law equation to the LEVEL.cm data in the dataset
USF21 <- USF21 %>%
  mutate(Discharge = a * (LEVEL.cm - h0)^b)

# Check the first few rows with computed Discharge
head(USF21)

# Plot 
ggplot(USF21, aes(x = DateTime, y = Discharge)) +
  geom_point(color = "blue") +
  labs(title = "Discharge", x = "DateTime", y = "Discharge (Q)") +
  theme_minimal()

# Convert discharge from cm³/s to m³/s
USF21 <- USF21 %>%
  mutate(Discharge_m3s = (a * (LEVEL.cm - h0)^b) / 1e6)

# Check the first few rows with computed discharge in m³/s
head(USF21)

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
