##==============================================================================
## Project: QuEST
## Here I'm just fixing pressure data for Brush Creek 2024 
## Data is taken from National Weather Service from the Fayetteville Drake Field station
## Data can be found here: https://www.weather.gov/wrh/timeseries?site=KFYV 
##==============================================================================

library(googledrive)
library(lubridate) 

############################
#### From the internet: ####
############################
# Or actually from this NOAA page: https://www.weather.gov/media/epz/wxcalc/stationPressure.pdf
#
# Station Pressure:
# From the user, a station elevation (h) and an altimeter setting (Pa) are given. Before
# calculation the station pressure, the station elevation must be converted to meters (m)
# using the formula below:
#  
#   hm = 0.3048 × hft
# 
# Also, the altimeter setting must be converted to inches of mercury (inHg)
#   (I think altimeter is already in inHg)
# 
# Then, the station pressure (P) can be calculated using the formula below:
#   
#   Pstn = Pa × ((288 − 0.0065 × hm)/288)^5.2561
# 
# Then, the station pressure can be converted to other pressure units, using the link
# above.

# Weather conditions for:
# Fayetteville, Drake Field, AR (ASOS/AWOS - TSA)
# Elev: 1250.0 ft; Lat/Lon: 36.01028/-94.16778

# hm = 0.3048 × h ft
elevation.m = 0.3048 * 1250.0
# elevation = 381 m 

#### load data ####
chart2024 <- read.csv("br_data/2024_fdf_temp.csv", skip = 1)
altimeter2024 <- read.csv("br_data/2024_fdf_alt.csv", skip = 1)
chart2025 <- read.csv("br_data/2025_fdf_temp.csv",skip = 1)
altimeter2025 <- read.csv("br_data/2025_fdf_alt.csv", skip = 1)

# merge all data sets in case I need any of that 
fdf2024 <- merge(chart2024, altimeter2024, by = "DateTime")
fdf2025 <- merge(chart2025, altimeter2025, by = "DateTime")

#### set datetime #### 
# round time a bit, it's weird
# create extra columns so you don't erase original time               
fdf2024$DateTimeNotRounded <- fdf2024$DateTime
fdf2025$DateTimeNotRounded <- fdf2025$DateTime

# transform to datetime format
fdf2024$DateTime <- as.POSIXct(fdf2024$DateTime, format = "%m/%d/%y %H:%M")
fdf2024$DateTimeNotRounded <- as.POSIXct(fdf2024$DateTime, format = "%m/%d/%y %H:%M")
fdf2025$DateTime <- as.POSIXct(fdf2025$DateTime, format = "%m/%d/%y %H:%M")
fdf2025$DateTimeNotRounded <- as.POSIXct(fdf2025$DateTime, format = "%m/%d/%y %H:%M")

# combine to get continuous time 
fdf <- bind_rows(fdf2024, fdf2025) %>%
  arrange(DateTime) %>%  # ensure chronological order if 'DateTime' exists
  distinct(DateTime, .keep_all = TRUE) # remove duplicates

# round DateTime to the nearest 15-minute interval 
fdf$DateTime <- round_date(fdf$DateTime, unit="30 mins")

# create day and time columns separately
fdf$Date <- as.Date(fdf$DateTime)
# extract Time (HH:MM:SS) as a string
fdf$Time <- format(fdf$DateTime, "%H:%M:%S")


#### calculating station pressure ####
fdf$Pstn.inHg = (fdf$Altimeter.Setting) * (((288 - 0.0065 * 381)/288)^5.2561)

# Pstn is in inHg, to convert to psi divide by 2.03602
fdf$Pstn.psi = fdf$Pstn.inHg / 2.03602

# we need psi in m. 1 psi = 0.703070 m
fdf$Pstn.m = fdf$Pstn.psi * 0.703070

#### save file to drive ####
file <- write.csv(fdf, "data/fdf_stationpressure.csv")
# this is the "merged_days" folder
drive_folder_id <- "1SbXzLapTIa_dt02JVba4PcsbQaJeFZtD"
# upload file to the specified Google Drive folder
drive_put(
  media = "data/fdf_stationpressure.csv",
  path = as_id(drive_folder_id)
)
