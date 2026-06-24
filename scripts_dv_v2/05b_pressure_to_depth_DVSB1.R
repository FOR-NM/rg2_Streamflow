##==============================================================================
## Project: QuEST
## Script 05b: Convert offset-corrected pressure to actual water depth
## for Dog Valley DVSB1
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##
## This step sits between 05 (offset correction) and 08 (rating curve). By
## this point Final_Corrected_Lvl is a single continuous, internally
## consistent pressure record (sensor moves/redeployments already stitched
## together), but it still includes the sensor's deployment height above the
## streambed. This script uses paired field observations of
## (Final_Corrected_Lvl, Actual_Water_Depth_m) to fit a linear conversion and
## produce a true Final_Water_Depth_m timeseries for DVSB1.
##
## INPUT:  "offset" Google Drive folder (output of script 05) -> offset_DVSB1.csv
## OUTPUT: "depth_corrected" Google Drive folder -> used by script 08
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
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data", full.names = TRUE)
file.remove(files)

#####################
#### Import data ####
#####################
# this is the "offset" folder (output of script 05)
pt <- googledrive::as_id("https://drive.google.com/drive/folders/136WGq6adaNROjaJN2YL63yJExKikM81A")

# list all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")

# download DVSB1 only
googledrive::drive_download(
  file = pt_csvs$id[pt_csvs$name == "offset_DVSB1.csv"],
  path = "googledrive/offset_DVSB1.csv",
  overwrite = TRUE
)

DVSB1 <- read.csv("googledrive/offset_DVSB1.csv")

###################################
#### Change to DateTime format ####
###################################
# DateTime was already fully constructed in script 05 (date + time), so no
# midnight-fill workaround is needed here like in the upstream PT scripts
DVSB1$DateTime <- as.POSIXct(DVSB1$DateTime, format = "%Y-%m-%d %H:%M:%S")

##########################################################
#### Extract paired observations for depth conversion ####
##########################################################
# Force numeric — CSV import sometimes reads these as character if there
# are empty strings or text flags mixed into the column.
DVSB1$Final_Corrected_Lvl  <- suppressWarnings(as.numeric(DVSB1$Final_Corrected_Lvl))
DVSB1$Actual_Water_Depth_m <- suppressWarnings(as.numeric(DVSB1$Actual_Water_Depth_m))

cat("Final_Corrected_Lvl NAs:  ", sum(is.na(DVSB1$Final_Corrected_Lvl)), "\n")
cat("Actual_Water_Depth_m NAs: ", sum(is.na(DVSB1$Actual_Water_Depth_m)), "\n")

# These are the timesteps where we have both a valid, fully offset-corrected
# pressure reading AND a field-measured actual water depth. This is the
# calibration dataset for converting Final_Corrected_Lvl -> Final_Water_Depth_m.
level_data <- DVSB1 %>%
  filter(
    !is.na(Final_Corrected_Lvl),
    !is.na(Actual_Water_Depth_m),
    is.finite(Final_Corrected_Lvl),
    is.finite(Actual_Water_Depth_m)
  )

cat("Number of paired depth observations for DVSB1:", nrow(level_data), "\n")
print(level_data %>% select(Date.x, DateTime, Final_Corrected_Lvl, Actual_Water_Depth_m))

# Require at least 3 paired observations to fit a model.
# If fewer are available, fall back to a single known sensor height offset.
min_obs_for_model <- 3

#############################################
#### Fit pressure-to-depth linear model ####
#############################################
if (nrow(level_data) >= min_obs_for_model) {

  # Linear model: Actual_Water_Depth_m = a + b * Final_Corrected_Lvl
  # The intercept captures the sensor height above the bed;
  # the slope accounts for any unit scaling issues.
  depth_model <- lm(Actual_Water_Depth_m ~ Final_Corrected_Lvl, data = level_data)

  cat("\n--- Depth conversion model summary (DVSB1) ---\n")
  print(summary(depth_model))

  a_depth <- coef(depth_model)[1]  # intercept
  b_depth <- coef(depth_model)[2]  # slope

  cat(sprintf("\nConversion: Final_Water_Depth_m = %.4f + %.4f * Final_Corrected_Lvl\n", a_depth, b_depth))

  # Apply the model to the full timeseries
  DVSB1 <- DVSB1 %>%
    mutate(Final_Water_Depth_m = a_depth + b_depth * Final_Corrected_Lvl)

} else {

  # Fallback: use a single known sensor deployment height from field notes.
  sensor_height_m <- NA  # <-- fill in from field notes if model can't be fit

  if (is.na(sensor_height_m)) {
    stop("Not enough paired observations to fit a depth model for DVSB1,
         and no fallback sensor_height_m provided. Check your field data.")
  }

  cat(sprintf("Too few paired observations. Using fixed sensor height: %.3f m\n", sensor_height_m))

  DVSB1 <- DVSB1 %>%
    mutate(Final_Water_Depth_m = Final_Corrected_Lvl - sensor_height_m)
}

#################################################
#### Diagnostic plots: check the conversion ####
#################################################

# 1. Observed vs. predicted depth at calibration points
level_data <- level_data %>%
  mutate(Predicted_Depth_m = a_depth + b_depth * Final_Corrected_Lvl)

ggplot(level_data, aes(x = Actual_Water_Depth_m, y = Predicted_Depth_m)) +
  geom_point(color = "blue", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_text(aes(label = format(as.Date(Date.x), "%Y-%m-%d")), vjust = -0.7, size = 3) +
  labs(
    title = "DVSB1: Observed vs. Predicted Water Depth",
    subtitle = "Dashed line = 1:1 perfect agreement",
    x = "Observed Water Depth (m)",
    y = "Predicted Water Depth (m)"
  ) +
  theme_minimal()

# 2. Final_Corrected_Lvl vs. Actual_Water_Depth_m with the fitted line
ggplot(level_data, aes(x = Final_Corrected_Lvl, y = Actual_Water_Depth_m)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  geom_text(aes(label = format(as.Date(Date.x), "%Y-%m-%d")), vjust = -0.7, size = 3) +
  labs(
    title = "DVSB1: Offset-corrected pressure vs. actual field depth",
    x = "Final_Corrected_Lvl (m)",
    y = "Actual Water Depth (m)"
  ) +
  theme_minimal()

# 3. Full Final_Water_Depth_m timeseries
ggplot(DVSB1, aes(x = DateTime, y = Final_Water_Depth_m)) +
  geom_line(color = "steelblue") +
  labs(
    title = "DVSB1: Water Depth timeseries (converted from pressure)",
    x = "Date",
    y = "Water Depth (m)"
  ) +
  theme_minimal()

# 4. Side-by-side: corrected pressure vs new depth
# Should have the same shape, just shifted/scaled
ggplot(DVSB1, aes(x = DateTime)) +
  geom_line(aes(y = Final_Corrected_Lvl, color = "Final_Corrected_Lvl (raw pressure)")) +
  geom_line(aes(y = Final_Water_Depth_m, color = "Final_Water_Depth_m (converted)")) +
  labs(
    title = "DVSB1: Pressure vs. depth comparison",
    x = "Date", y = "Value (m)", color = NULL
  ) +
  scale_color_manual(values = c("Final_Corrected_Lvl (raw pressure)" = "gray60",
                                "Final_Water_Depth_m (converted)"    = "steelblue")) +
  theme_minimal()

# 5. Negative depth check — flag any timesteps with implausible values
n_negative <- sum(DVSB1$Final_Water_Depth_m < 0, na.rm = TRUE)
if (n_negative > 0) {
  cat(sprintf(
    "\nWARNING: %d timesteps have negative Final_Water_Depth_m. Review the model fit
    or check for periods when the sensor was out of water.\n", n_negative
  ))
}

###################
#### Save file ####
###################
write.csv(DVSB1, "data/depth_corrected_DVSB1.csv", row.names = FALSE)

# this is the "depth_corrected" folder — create this folder in Drive first
# and paste its ID here (reusing the folder originally set up for DVSB1)
drive_folder_id <- "1wlpAOSpJyB6LI2XYkRXRZwhXRkXqe5eG"  # <-- confirm/replace with your Drive folder ID

drive_put(
  media = "data/depth_corrected_DVSB1.csv",
  path  = as_id(drive_folder_id)
)
