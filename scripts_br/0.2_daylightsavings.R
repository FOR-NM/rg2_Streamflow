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

# --- BRA01 --- 
# create complete time series sequence using min/max of data set
time <- data.frame(
  DateTime = seq.POSIXt(
    from = min(BRA01$DateTime),
    to = max(BRA01$DateTime),
    by = "1 hour"))

# define the specific transition dates
transition_date_1 <- ymd_hms("2024-12-12 11:00:00", tz = "UTC") # transition from CDT to CST
transition_date_2 <- ymd_hms("2025-04-17 12:00:00", tz = "UTC") # transition from CST to CDT

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
  # Apr 16th onwards: CDT (UTC-5)
  time$DateTime >= transition_date_2 ~ force_tz(time$DateTime, tzone = tz_cdt_gmt),
  TRUE ~ NA_POSIXct_ # should not happen if all dates covered
)

# test
lubridate::tz(time$DateTime)
lubridate::tz(time$AdjustedDateTime)
                     
# join to clean time stamps
BRA01.ts = left_join(time, BRA01, by="DateTime")

# assign new real time zone
time$AdjustedDateTime = as.POSIXct(time$AdjustedDateTime, format = "%Y-%m-%d %H:%M:%S", tz = "")

#### 2 ####
#### apply timezone adjustments (CST/CDT) ####
# force_tz will assign the specified timezone.
# roll_dst = "NA" is crucial:
# - for non-existent times (spring forward, e.g., 2025-03-09 02:00:00 AM in CST), it sets DateTime to NA.
# - for ambiguous times (fall back, e.g., 2024-11-03 01:00:00 AM in CST), it also sets DateTime to NA,
#   which helps avoid duplication and forces a single, clear hourly sequence.

# make new empty list to store data frames
pt_daylightsavings <- list()

# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # filter function
  df <- df %>%
    force_tz(df$DateTime, tzone = "America/Chicago", roll_dst = "NA")
  # update the data frame in the list
  pt_daylightsavings[[i]] <- df
}

# assign names to the list elements based on the file names
names(pt_daylightsavings) <- pt_csvs$name

# apply timezone (CST/CDT) 
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # adjust time zone
  df <- df %>%
    force_tz(df$DateTime, tzone = "America/Chicago", roll_dst = "NA")
  # update the data frame in the list
  pt_daylightsavings[[i]] <- df
}

# remove the extra row on 2025-03-09 02:00:00 am
for (i in seq_along(pt_daylightsavings)) {
  # access the current data frame
  df <- pt_daylightsavings[[i]]
  # remove NA times
  df <- df %>%
    filter(!is.na(DateTime))
  # update the data frame in the list
  pt_daylightsavings[[i]] <- df
}

# optional: verify hourly intervals after adjustment
# note: the first row will have NA for TimeDifference
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # remove NA times
  df <- df %>%
    arrange(DateTime) %>% # ensure data is sorted by DateTime
    mutate(TimeDifference_hours = as.numeric(difftime(lead(DateTime), DateTime, units = "hours")))
  # update the data frame in the list
  pt_daylightsavings[[i]] <- df
}

# I just want to look at each file individually
BRA01 <- pt_daylightsavings[["BRA01.csv"]] 
BRAA1 <- pt_daylightsavings[["BRAA1.csv"]] 
BRD01 <- pt_daylightsavings[["BRD01.csv"]] 
BRF01 <- pt_daylightsavings[["BRF01.csv"]] 
BRM01 <- pt_daylightsavings[["BRM01.csv"]] 
BRM02 <- pt_daylightsavings[["BRM02.csv"]] 
BRM03 <- pt_daylightsavings[["BRM03.csv"]] 
BRM05 <- pt_daylightsavings[["BRM05.csv"]] 
BRM07 <- pt_daylightsavings[["BRM07.csv"]] 
BRMQ1 <- pt_daylightsavings[["BRMQ1.csv"]] 

# remove extra rows for upload

###########################################
#### Save daylight time adjusted files ####
###########################################
# write files to local data folder
lapply(names(pt_daylightsavings), function(site) {
  # define file path
  file <- paste0("data/", site)
  # save each data frame
  write.csv(pt_daylightsavings[[site]], file, row.names = FALSE, quote = FALSE)
  # this is the "daylight adjusted" folder
  drive_folder_id <- "1lff8pbyXG9w0XoNaToxMGNAIjkKWyjHs"
  # upload the file to Google Drive
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
})

