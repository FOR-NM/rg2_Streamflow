##==============================================================================
## Project: QuEST
## This script is to round time up or down to the nearest consistent interval 
##==============================================================================

# example: 10:04 gets converted to 10:05 for this we use the lubridate package

##################
#### Packages ####
##################

library(lubridate) 

###########################
#### Rounding the time ####
###########################

# Load your file
pt <- read.csv("googledrive/09-16-2024_SST07_PTS_SN2192880.csv")
               
# Create extra columns so you don't errase original time               
pt$DateTimeNotRounded <- pt$DateTime

# Transform to datetime format
pt$DateTime <- as.POSIXct(pt$DateTime,format = "%Y-%m-%d %H:%M:%S")
pt$DateTimeNotRounded <- as.POSIXct(pt$DateTimeNotRounded,format = "%Y-%m-%d %H:%M:%S")

# Round DateTime to the nearest 15-minute interval 
pt$DateTime <- round_date(pt$DateTime, unit="15 mins")

# Check if it worked!
str(pt)




