##==============================================================================
## Project: QuEST
## This script is to subset precipitation data for Dog Valley 
##==============================================================================

##################
#### Packages ####
##################
library(tidyverse)
library(lubridate)
library(stringr)
library(ggplot2)
library(plotly)

####################################
####################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data", full.names = TRUE)
file.remove(files)

# ---- Dog Valley RAWS: simple read + precip plots ----
txt_file <- "WRCC_DogValley_RAWS_Data.txt"
# Read the text file
txt_content <- readLines(txt_file)
# Keep only rows that look like data (start with MM/DD/YYYY), drop headers (:) and HTML tags
data_lines <- txt_content %>%
  discard(~ str_detect(.x, "^:")) %>%                # drop header/unit lines
  keep(~ str_detect(.x, "^\\d{2}/\\d{2}/\\d{4}"))    # keep data lines
# Split on whitespace -> data.frame
data_mat <- str_split(data_lines, "\\s+", simplify = TRUE)
# Expect: Date, Time, then 11 numeric cols (total 13 data cols)
# WRCC columns in your sample:
# Date Time Precip_mm WindSpeed_m/s WindDir_deg AvAirTemp_C FuelTemp_C RelHumidity_% Battery_volts FuelMoisture_% Dir_MxGust_deg MxGustSpeed_m/s SolarRad_Wm2
stopifnot(ncol(data_mat) >= 13)
df <- as_tibble(data_mat[, 1:13], .name_repair = "minimal")
colnames(df) <- c(
  "Date", "Time", "Precip_mm",
  "WindSpeed_ms", "WindDir_deg",
  "AvAirTemp_C", "FuelTemp_C",
  "RelHumidity_pct", "Battery_volts",
  "FuelMoisture_pct",
  "Dir_MxGust_deg", "MxGustSpeed_ms",
  "SolarRad_Wm2"
)
# Type conversions
df <- df %>%
  mutate(
    # Combine Date + Time, parse in local tz
    DateTime = mdy_hm(paste(Date, Time), tz = "America/Los_Angeles"),
    across(
      c(Precip_mm, WindSpeed_ms, WindDir_deg, AvAirTemp_C, FuelTemp_C,
        RelHumidity_pct, Battery_volts, FuelMoisture_pct,
        Dir_MxGust_deg, MxGustSpeed_ms, SolarRad_Wm2),
      ~ suppressWarnings(as.numeric(.))
    ),
    Precip_in = Precip_mm / 25.4
  ) %>%
  arrange(DateTime)
# ---- Set your window (edit these) ----
start_time <- ymd_hm("2024-04-01 00:00", tz = "America/Los_Angeles")
end_time   <- ymd_hm("2025-06-01 00:00", tz = "America/Los_Angeles")
df_filt <- df %>%
  filter(!is.na(DateTime)) %>%
  filter(DateTime >= start_time, DateTime < end_time)
# ---- ggplot: precip inches (line) ----
ggplot(df_filt, aes(x = DateTime, y = Precip_in)) +
  geom_line(linewidth = 0.6) +
  labs(x = "Date", y = "Precip (inches)", title = "") +
  theme_classic()
# ---- plotly: precip inches (line) with full datetime in hover ----
plot_ly(
  data = df_filt,
  x = ~DateTime,
  y = ~Precip_in,
  type = "scatter",
  mode = "lines",
  line = list(width = 2),
  hovertemplate = paste(
    "Datetime: %{x|%Y-%m-%d %H:%M}<br>",
    "Precip: %{y:.2f} in<extra></extra>"
  )
) %>%
  layout(
    title = "",
    xaxis = list(title = "DateTime"),
    yaxis = list(title = "Precip (inches)"),
    template = "plotly_white"
  )

###################
#### Save file ####
###################
write.csv(df_filt, "data/precipitation_dv.csv")

# this is the "offset" folder
drive_folder_id <- "1Xn1jx4x6RjuXfEq655xqOkKRIu0rYtbL"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/precipitation_dv.csv",
  path = as_id(drive_folder_id)
)
