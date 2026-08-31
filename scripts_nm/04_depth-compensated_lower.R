##==============================================================================
## Project: QuEST
## TO DO: Adapt this script for FOR-NM 

## Script 05 (lower): Merge discharge + field observation data (salt slugs)
## with compensated PT data for the lower NM sites (USF03, USF04, USF05,
## USF07, USF20 — AIR1 baro source).
## press Command+Option+O to collapse all sections and get an overview of the workflow!
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
file.remove(list.files(path = "googledrive", full.names = TRUE))
file.remove(list.files(path = "data", full.names = TRUE))

##############################################
#### Load field observation data (salt slugs) ####
##############################################
(depth <- drive_get("https://docs.google.com/spreadsheets/d/1rIWYWFUoF6UtzTcvN-WpIw2Cw4u9NjI_HuhThoDOvv4/edit?gid=0#gid=0"))

drive_download(as_id(depth$id), path = "googledrive/salt.csv", type = "csv", overwrite = TRUE)

salt <- read.csv("googledrive/salt.csv")

# clean the column names (this removes spaces, special characters, etc.)
salt <- salt %>%
  janitor::clean_names()

# rename columns and convert types
salt <- salt %>%
  dplyr::rename(
    DataID      = site,
    Time24h     = time_24h_rounded_to_nearest_15_min,
    reach       = reach_length_m,
    salt        = salt_added_g,
    pt          = is_there_a_pt_at_this_site,
    pt_depth_cm = pt_depth_cm_at_the_time_of_the_discharge_measurement_numbers_only,
    observation = observed_flow_characterization
  ) %>%
  mutate(
    pt_depth_cm = as.numeric(as.character(pt_depth_cm)),
    date        = as.Date(date, format = "%m/%d/%Y"),
  )

# keep only the columns this pipeline actually uses, by name (not position)
salt <- salt %>%
  dplyr::select(DataID, date, Time24h, reach, salt, pt, pt_depth_cm,
                relevant_notes, flag, flag_notes, observation)

# replace empties with NA
salt["relevant_notes"][salt["relevant_notes"] == ''] <- NA
salt["flag_notes"][salt["flag_notes"] == ''] <- NA

PT_sites <- salt

########################################################
#### Combine and format Date and Time in one column ####
########################################################
PT_sites$DateTime <- paste(PT_sites$date, PT_sites$Time24h, sep = " ")
PT_sites$DateTime <- as.POSIXct(PT_sites$DateTime, format = "%Y-%m-%d %H:%M:%S")

#######################################
#### Load Q data from Google drive ####
#######################################
discharge <- googledrive::as_id("https://drive.google.com/drive/folders/1UkRaYRBePgY9XU90_3DvURNGGEWbCew0")
discharge_csv <- googledrive::drive_ls(path = discharge, type = "csv")

googledrive::drive_download(
  file      = discharge_csv$id[discharge_csv$name == "Q.csv"],
  path      = "googledrive/Q.csv",
  overwrite = TRUE
)

Q <- read.csv("googledrive/Q.csv")
Q$date <- Q$Date
Q$date <- as.Date(Q$date, format = "%Y-%m-%d")

# remove duplicate rows
Q <- Q[, -c(3, 7, 8)]

# discharge from L/s to m3/s, done once here rather than in every offset script
Q$Q_m3s <- as.numeric(Q$Q) / 1000

########################################
#### Merge depth and discharge data ####
########################################
discharge_depth <- merge(Q, PT_sites, by = c("DataID", "date"), all.x = TRUE)

####################################################
#### Load PT compensated data from Google drive ####
####################################################
# this is the "compensated" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1VsT7hirl5OHIGhrrc3b7dxPpSqr1wNC0")
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")

pt_list <- list()
for (i in seq_along(pt_csvs$id)) {
  local_path <- file.path("googledrive", pt_csvs$name[i])
  googledrive::drive_download(file = pt_csvs$id[i], path = local_path, overwrite = TRUE)
  pt_list[[pt_csvs$name[i]]] <- read.csv(local_path)
}

###################################
#### Add DataID column to csvs ####
###################################
for (i in seq_along(pt_list)) {
  df <- pt_list[[i]]
  data_id <- tools::file_path_sans_ext(pt_csvs$name[[i]])
  df <- df %>% dplyr::mutate(DataID = data_id)
  pt_list[[i]] <- df
}

# keep only the lower sites, by name (not position)
lowersites <- c("USF03", "USF04", "USF05", "USF07", "USF20")
pt_list <- pt_list[paste0(lowersites, ".csv")]

########################
#### Parse DateTime ####
########################
for (i in seq_along(pt_list)) {
  df <- pt_list[[i]]
  # DateTime at midnight is missing 00:00:00 time in this file, so fill it in
  df$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$", df$DateTime)] <- paste(
    df$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$", df$DateTime)], "00:00:00")
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
  pt_list[[i]] <- df
}

#######################################
#### Combine depth info to PT data ####
#######################################
depth_merged <- lapply(pt_list, function(df) {
  merge(df, discharge_depth, by = c("DataID", "DateTime"), all.x = TRUE) 
})

depth_merged <- lapply(depth_merged, function(df) {
  df_clean <- df %>%
    select(DataID, DateTime,
           Baro_Cor_Lvl.m, Baro_Cor_Lvl.kPa,
           LEVEL.m, LEVEL.kPa,
           Level_air.kPa,
           TEMPERATURE, Temperature_air.C, 
           date, observation, Q_m3s, flag)
})

# count non-NA values in the 'pt' column for each data frame
non_na_counts <- sapply(depth_merged, function(df) {
  if ("pt" %in% colnames(df)) sum(!is.na(df$pt)) else NA
})
print(non_na_counts)

#######################################
#### Save merged PT files to Drive ####
#######################################
for (i in seq_along(depth_merged)) {
  df <- depth_merged[[i]]
  write.csv(df, paste0("data/", names(depth_merged)[i]), row.names = FALSE, quote = FALSE)
  
  file <- paste0("data/", names(depth_merged)[i])
  # this is the "depth" folder
  drive_folder_id <- "1EswIfUWCK6bsdcs-ZrAMGW1oYKs4B0Eh"
  drive_put(media = file, path = as_id(drive_folder_id))
}