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
chart <- read.csv("br_data/fdf_chart.csv")
altimeter <- read.csv("br_data/fdf_altimeter.csv")

# merge all data sets in case I need any of that 
fdf <- merge(chart, altimeter, by = "DateTime")

#### calculating station pressure ####
fdf$Pstn.inHg = (fdf$Altimeter.Setting) * (((288 - 0.0065 * 381)/288)^5.2561)

# Pstn is in inHg, to convert to psi divide by 2.03602
fdf$Pstn.psi = fdf$Pstn.inHg / 2.03602

# we need psi in m. 1 psi = 0.703070 m
fdf$Pstn.m = fdf$Pstn.psi * 0.703070

# round time a bit, it's weird
# create extra columns so you don't errase original time               
fdf$DateTimeNotRounded <- fdf$DateTime

# transform to datetime format
fdf$DateTime <- as.POSIXct(fdf$DateTime,format = "%Y-%m-%d %H:%M:%S")
fdf$DateTimeNotRounded <- as.POSIXct(fdf$DateTimeNotRounded,format = "%Y-%m-%d %H:%M:%S")

# round DateTime to the nearest 15-minute interval 
fdf$DateTime <- round_date(fdf$DateTime, unit="15 mins")

#### save file to drive ####
file <- write.csv(fdf, "data/fdf_stationpressure.csv")
# this is the "merged_days" folder
drive_folder_id <- "1SbXzLapTIa_dt02JVba4PcsbQaJeFZtD"
# Upload file to the specified Google Drive folder
drive_put(
  media = "data/fdf_stationpressure.csv",
  path = as_id(drive_folder_id)
)
