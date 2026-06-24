##==============================================================================
## Project: QuEST
## Script to account for daylight savings time change in Brush Creek
## We are also going to try to tackle the daylight savings time changes 
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
# this is the "merged_days" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1SbXzLapTIa_dt02JVba4PcsbQaJeFZtD")

# list all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3

## call all the files in the salt slugs folder ##
# create empty list to store data frames
pt_list <- list()

# loop over each file in the `pt_csvs` data frame
for (i in seq_along(pt_csvs$id)) {
  # define the local file path
  local_path <- file.path("googledrive", pt_csvs$name[i])
  
  # download the file
  googledrive::drive_download(
    file = pt_csvs$id[i],
    path = local_path,
    overwrite = T
  )
  # read the CSV file and add it to the list
  pt_list[[pt_csvs$name[i]]] <- read.csv(local_path)
}

################################
#### Format DateTime column ####
################################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # combine Date and Time columns into a new DateTime column
  df$DateTime <- paste(df$Date, df$Time, sep = " ")
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
  # update the data frame in the list
  pt_list[[i]] <- df
}

### keep only 1 hour intervals ###
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # filter function
  df <- df %>%
    filter(format(df$DateTime, "%M") %in% c("00"))
  # update the data frame in the list
  pt_list[[i]] <- df
}

# make sure time intervals make sense
# some of these intervals were off, so we have to look at them one by one to check them and round the time correctly
BRA01 <- pt_list[["BRA01.csv"]] # every 15min. Daylight savings time on 2024-12-12? and 2025-03-09 02:00:00 am
BRAA1 <- pt_list[["BRAA1.csv"]] # every 10min, then every 15min, then every 15 but all over the place, then every 15 again. Daylight savings time on 2024-12-13? and 2025-03-09 02:00:00 am
BRD01 <- pt_list[["BRD01.csv"]] # every 10min, then every 15min. Daylight savings time on 2024-12-12? and 2025-03-09 02:00:00 am
BRF01 <- pt_list[["BRF01.csv"]] # every 10min, then every 15min. Daylight savings time on 2024-12-12? Data goes until 2025-01-23
BRM01 <- pt_list[["BRM01.csv"]] # every 15min. Daylight savings time on 2024-12-12? and 2025-03-09 02:00:00 am
BRM02 <- pt_list[["BRM02.csv"]] # every 10min, then every 15min. Daylight savings time on 2024-12-12? and 2025-03-09 02:00:00 am
BRM03 <- pt_list[["BRM03.csv"]] # every 10min, then every 15min. Daylight savings time on 2024-12-12? and 2025-03-09 02:00:00 am
BRM05 <- pt_list[["BRM05.csv"]] # every 10min. Data goes until 2024-10-17
BRM07 <- pt_list[["BRM07.csv"]] # every 10min, then every 15min. Daylight savings time on 2024-12-13? and 2025-03-09 02:00:00 am
BRMQ1 <- pt_list[["BRMQ1.csv"]] # every 10min, then every 15min, then every 15 but all over the place, then every 15 again. Daylight savings time on 2024-12-13? and 2025-03-09 02:00:00 am 

# test
lubridate::tz(BRA01$DateTime)
dst <- lubridate::dst(BRA01$DateTime)
summary(dst)

#################################################
#### Adjust for daylight savings time change ####
#################################################
#### create a "clean" set of time stamps ####
# so they cover the whole range of the data in the correct time zone, then join the actual data to this.
# time zone changes should show up either as blank rows or as duplicate rows, depending on the direction of change. 
# then you can either interpolate or delete them.

# define the local timezone you want to work with (e.g., Central Time)
local_tz <- "America/Chicago"

#### For sites downloaded on 2024-12-12 and 2025-04-17 ####
# create complete time series sequence using min/max of data set
# --- BRA01 --- 
time <- data.frame(
  DateTime = seq.POSIXt(
    from = min(BRA01$DateTime),
    to = max(BRA01$DateTime),
    by = "1 hour",
    tz = local_tz))

lubridate::tz(time$DateTime)

# --- BRM01 --- 
time <- data.frame(
  DateTime = seq.POSIXt(
    from = min(BRM01$DateTime),
    to = max(BRM01$DateTime),
    by = "1 hour"))
# --- BRM02 --- 
time <- data.frame(
  DateTime = seq.POSIXt(
    from = min(BRM02$DateTime),
    to = max(BRM02$DateTime),
    by = "1 hour"))
# --- BRM03 --- 
time <- data.frame(
  DateTime = seq.POSIXt(
    from = min(BRM03$DateTime),
    to = max(BRM03$DateTime),
    by = "1 hour"))

# define the specific transition dates
transition_date_1 <- ymd_hms("2024-12-12 11:00:00", tz = local_tz) # transition from CDT to CST - custom fall-back point
transition_date_2 <- ymd_hms("2025-04-17 12:00:00", tz = local_tz) # transition from CST to CDT - custom spring-forward point

# manually apply the *actual time shifts* to the underlying UTC value
# based on your device's custom behavior.
# the goal is for AdjustedDateTime to have the correct underlying UTC value,
# while still being displayable in the local_tz.
time$AdjustedDateTime <- case_when(
  # period 1: Before Dec 12th 11:00:00 local time
  # device is effectively UTC-5. No adjustment to its underlying UTC value needed.
  time$DateTime < transition_date_1 ~ time$DateTime,
  
  # period 2: From Dec 12th 11:00:00 local time to Apr 17th 12:00:00 local time
  # device's clock is still operating as if it's UTC-5, but it *should* be UTC-6.
  # this means the device's underlying UTC value is 1 hour *too early* for this period.
  # to correct it, ADD 1 hour to the underlying UTC value.
  time$DateTime >= transition_date_1 & time$DateTime < transition_date_2 ~ time$DateTime + hours(1),
  
  # period 3: From Apr 17th 12:00:00 local time onwards
  # Device's clock is now operating as if it's UTC-5 again.
  # It "sprang forward" from the UTC-6 period.
  # This means its underlying UTC value is 1 hour *too late* relative to the UTC-5 offset.
  # To correct it, SUBTRACT 1 hour from the underlying UTC value.
  time$DateTime >= transition_date_2 ~ time$DateTime - hours(1),
  
  TRUE ~ NA_POSIXct_ # Should not happen
)

# test
lubridate::tz(time$DateTime)
lubridate::tz(time$AdjustedDateTime)

# join to clean time stamps              
# --- BRA01 ---   
BRA01.ts = left_join(time, BRA01, by="DateTime")
# --- BRM01 --- 
BRM01.ts = left_join(time, BRM01, by="DateTime")
# --- BRM02 --- 
BRM02.ts = left_join(time, BRM02, by="DateTime")
# --- BRM03 --- 
BRM03.ts = left_join(time, BRM03, by="DateTime")

# assign new real time zone
time$AdjustedDateTime = as.POSIXct(time$AdjustedDateTime, format = "%Y-%m-%d %H:%M:%S", tz = "")

#### For sites downloaded on 2024-12-12 and 2025-04-16 ####
# --- BRD01 --- 
time <- data.frame(
  DateTime = seq.POSIXt(
    from = min(BRD01$DateTime),
    to = max(BRD01$DateTime),
    by = "1 hour"))
# --- BRF01 --- 
time <- data.frame(
  DateTime = seq.POSIXt(
    from = min(BRF01$DateTime),
    to = max(BRF01$DateTime),
    by = "1 hour"))

# define the specific transition dates
transition_date_1 <- ymd_hms("2024-12-12 11:00:00", tz = "UTC") # transition from CDT to CST
transition_date_2 <- ymd_hms("2025-04-16 12:00:00", tz = "UTC") # transition from CST to CDT

# define the offsets as R timezone strings
# note: "Etc/GMT+X" means UTC - X hours (e.g., GMT+5 means UTC-5)
tz_cdt_gmt <- "Etc/GMT+5" # equivalent to UTC-5 (in this case CDT)
tz_cst_gmt <- "Etc/GMT+6" # equivalent to UTC-6 (in this case CST)

# force UTC to handle as naive
time$DateTime = as.POSIXct(time$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# manually apply GMT offsets based on date ranges
time$AdjustedDateTime = case_when(
  # beginning of data till Dec 12th: CDT (UTC-5)
  time$DateTime < transition_date_1 ~ force_tz(time$DateTime, tzone = tz_cdt_gmt),
  # Dec 12th till Apr 16th: CST (UTC-6)
  time$DateTime >= transition_date_1 & time$DateTime < transition_date_2 ~ force_tz(time$DateTime, tzone = tz_cst_gmt),
  # Apr 16th on wards: CDT (UTC-5)
  time$DateTime >= transition_date_2 ~ force_tz(time$DateTime, tzone = tz_cdt_gmt),
  TRUE ~ NA_POSIXct_ # should not happen if all dates covered
)

# test
lubridate::tz(time$DateTime)
lubridate::tz(time$AdjustedDateTime)

# --- BRD01 --- 
BRD01.ts = left_join(time, BRD01, by="DateTime")
# --- BRF01 --- 
BRF01.ts = left_join(time, BRF01, by="DateTime")

#### For sites downloaded on 2024-12-13 and 2025-04-16 ####
# --- BRAA1 --- 
time <- data.frame(
  DateTime = seq.POSIXt(
    from = min(BRAA1$DateTime),
    to = max(BRAA1$DateTime),
    by = "1 hour"))
# --- BRMQ1 --- 
time <- data.frame(
  DateTime = seq.POSIXt(
    from = min(BRMQ1$DateTime),
    to = max(BRMQ1$DateTime),
    by = "1 hour"))

# define the specific transition dates
transition_date_1 <- ymd_hms("2024-12-13 11:00:00", tz = "UTC") # transition from CDT to CST
transition_date_2 <- ymd_hms("2025-04-16 12:00:00", tz = "UTC") # transition from CST to CDT

# define the offsets as R timezone strings
# note: "Etc/GMT+X" means UTC - X hours (e.g., GMT+5 means UTC-5)
tz_cdt_gmt <- "Etc/GMT+5" # equivalent to UTC-5 (in this case CDT)
tz_cst_gmt <- "Etc/GMT+6" # equivalent to UTC-6 (in this case CST)

# force UTC to handle as naive
time$DateTime = as.POSIXct(time$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# manually apply GMT offsets based on date ranges
time$AdjustedDateTime = case_when(
  # beginning of data till Dec 13th: CDT (UTC-5)
  time$DateTime < transition_date_1 ~ force_tz(time$DateTime, tzone = tz_cdt_gmt),
  # Dec 13th till Apr 16th: CST (UTC-6)
  time$DateTime >= transition_date_1 & time$DateTime < transition_date_2 ~ force_tz(time$DateTime, tzone = tz_cst_gmt),
  # Apr 16th on wards: CDT (UTC-5)
  time$DateTime >= transition_date_2 ~ force_tz(time$DateTime, tzone = tz_cdt_gmt),
  TRUE ~ NA_POSIXct_ # should not happen if all dates covered
)

# test
lubridate::tz(time$DateTime)
lubridate::tz(time$AdjustedDateTime)

# --- BRAA1 --- 
BRAA1.ts = left_join(time, BRAA1, by="DateTime")
# --- BRMQ1 --- 
BRMQ1.ts = left_join(time, BRMQ1, by="DateTime")

#### For sites downloaded on 2024-12-13 and 2025-04-18 ####
# --- BRM07 --- 
time <- data.frame(
  DateTime = seq.POSIXt(
    from = min(BRM07$DateTime),
    to = max(BRM07$DateTime),
    by = "1 hour"))
# define the specific transition dates
transition_date_1 <- ymd_hms("2024-12-13 11:00:00", tz = "UTC") # transition from CDT to CST
transition_date_2 <- ymd_hms("2025-04-18 12:00:00", tz = "UTC") # transition from CST to CDT

# define the offsets as R timezone strings
# note: "Etc/GMT+X" means UTC - X hours (e.g., GMT+5 means UTC-5)
tz_cdt_gmt <- "Etc/GMT+5" # equivalent to UTC-5 (in this case CDT)
tz_cst_gmt <- "Etc/GMT+6" # equivalent to UTC-6 (in this case CST)

# force UTC to handle as naive
time$DateTime = as.POSIXct(time$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# manually apply GMT offsets based on date ranges
time$AdjustedDateTime = case_when(
  # beginning of data till Dec 13th: CDT (UTC-5)
  time$DateTime < transition_date_1 ~ force_tz(time$DateTime, tzone = tz_cdt_gmt),
  # Dec 13th till Apr 18th: CST (UTC-6)
  time$DateTime >= transition_date_1 & time$DateTime < transition_date_2 ~ force_tz(time$DateTime, tzone = tz_cst_gmt),
  # Apr 18th on wards: CDT (UTC-5)
  time$DateTime >= transition_date_2 ~ force_tz(time$DateTime, tzone = tz_cdt_gmt),
  TRUE ~ NA_POSIXct_ # should not happen if all dates covered
)

# test
lubridate::tz(time$DateTime)
lubridate::tz(time$AdjustedDateTime)

# --- BRM07 --- 
BRM07.ts = left_join(time, BRM07, by="DateTime")

###########################################
#### Save daylight time adjusted files ####
###########################################
write.csv(BRA01.ts, "data/BRA01.csv")
write.csv(BRM01.ts, "data/BRM01.csv")
write.csv(BRM02.ts, "data/BRM02.csv")
write.csv(BRM03.ts, "data/BRM03.csv")
write.csv(BRD01.ts, "data/BRD01.csv")
write.csv(BRF01.ts, "data/BRF01.csv")
write.csv(BRAA1.ts, "data/BRAA1.csv")
write.csv(BRMQ1.ts, "data/BRMQ1.csv")
write.csv(BRM07.ts, "data/BRM07.csv")
#this one???? write.csv(BRM05.ts, "data/BRM05.csv")

drive_folder_id <- "1lff8pbyXG9w0XoNaToxMGNAIjkKWyjHs"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/BRA01.csv",
  path = as_id(drive_folder_id)
)
drive_put(
  media = "data/BRM01.csv",
  path = as_id(drive_folder_id)
)
drive_put(
  media = "data/BRM02.csv",
  path = as_id(drive_folder_id)
)
drive_put(
  media = "data/BRM03.csv",
  path = as_id(drive_folder_id)
)
drive_put(
  media = "data/BRD01.csv",
  path = as_id(drive_folder_id)
)
drive_put(
  media = "data/BRF01.csv",
  path = as_id(drive_folder_id)
)
drive_put(
  media = "data/BRAA1.csv",
  path = as_id(drive_folder_id)
)
drive_put(
  media = "data/BRMQ1.csv",
  path = as_id(drive_folder_id)
)
drive_put(
  media = "data/BRM07.csv",
  path = as_id(drive_folder_id)
)
drive_put(
  media = "data/BRM05.csv",
  path = as_id(drive_folder_id)
)
