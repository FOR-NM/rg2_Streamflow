##==============================================================================
## Project: QuEST
## I totally messed up and did not include the time in the final discharge concentration calculations,
## so now we have date and discharge but no time to pair it to the PT data with
## This script is to merge discharge data from salt slugs with final calculated discharge.
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

###############################################
#### Load salt slug data from Google drive ####
###############################################
(depth <- drive_get("https://docs.google.com/spreadsheets/d/1ZR50uazdnmPdzYzIwpbspcnLNb_ql7KhdMmZ5t_Rppk/edit?gid=0#gid=0"))
3

# download the file as a csv file
drive_download(as_id(depth$id), path = "googledrive/salt.csv", type = "csv", overwrite = T)

# fetch the file
salt <- read.csv("googledrive/salt.csv")

# clean the column names (this removes spaces, special characters, etc.)
salt <- salt %>%
  janitor::clean_names()

# rename columns and convert types
salt <- salt %>%
  # Rename columns
  dplyr::rename(
    DataID = site,
    Date = date,
    Time24h = time_24h_rounded_to_nearest_15_min,
    reach = reach_length_m,
    salt = salt_added_g,
    pt = is_there_a_pt_at_this_site,
    pt_depth_cm = pt_depth_cm_at_the_time_of_the_discharge_measurement_numbers_only
  ) %>%
  # Convert
  mutate(
    pt_depth_cm = as.numeric(as.character(pt_depth_cm)),
    # Change date format
    Date = as.Date(Date, format = "%m/%d/%Y"),
  )

colnames(salt)

# remove rows that I don't want
salt <- salt[ , -c(1:3, 8, 9, 12:14, 18)]

# replace empties with NA
salt["relevant_notes"][salt["relevant_notes"] == ''] <- NA
salt["flag_notes"][salt["flag_notes"] == ''] <- NA

########################################################
#### Combine and format Date and Time in one column ####
########################################################
# combine Date and Time columns into a new DateTime column
salt$DateTime <- paste(salt$Date, salt$Time24h, sep = " ")

# convert the DateTime column to POSIXct
salt$DateTime <- as.POSIXct(salt$DateTime, format = "%Y-%m-%d %H:%M:%S")

#######################################
#### Load Q data from Google drive ####
####################### ################
discharge <- googledrive::as_id("https://drive.google.com/drive/folders/1gpOD-zGjB-4ZtmoL7XonQdOUlYbQbgMm")

# list all CSV files in the folder
discharge_csv <- googledrive::drive_ls(path = discharge, type = "csv")
3

# call the specific file you want (most recent one)
googledrive::drive_download(file = discharge_csv$id[discharge_csv$name=="Q_BR.csv"], 
                            path = "googledrive/Q_BR.csv",
                            overwrite = T)

# load it into R
Q = read.csv("googledrive/Q_BR.csv")

# convert the Date column to Date
Q$Date <- as.Date(Q$Date, format = "%Y-%m-%d", tz = "MST")

########################################
#### Merge depth and discharge data ####
########################################
discharge <- merge(Q, salt, by = c("DataID", "Date"), all.x = TRUE)


###################################
#### Save merged file to Drive ####
###################################
write.csv(discharge, "data/BR_discharge.csv") 

drive_folder_id <- "1gpOD-zGjB-4ZtmoL7XonQdOUlYbQbgMm"

# upload file to the specified Google Drive folder
drive_put(
  media = "data/BR_discharge.csv",
  path = as_id(drive_folder_id)
)
