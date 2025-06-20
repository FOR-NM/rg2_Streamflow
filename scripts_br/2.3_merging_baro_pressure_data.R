##==============================================================================
## Project: QuEST
## Script to merge pressure files in one. The Fayetteville and baro logger
##==============================================================================
##############################
#### Import pressure data ####
##############################
fayetteville <- googledrive::as_id("https://drive.google.com/drive/folders/1FgFNGzv0Rh5t62V8SRFdwK6sd_ktwcRD")
# list all CSV files in the folder
pressure <- googledrive::drive_ls(path = fayetteville)
# choose the specific file by name
pressure <- pressure %>% filter(name == "fayetteville_pressure.csv")
# download the most recent CSV file
drive_download(as_id(pressure$id), path = "googledrive/fayetteville_pressure.csv", overwrite = TRUE)
# fetch the file
fayetteville <- read.csv("googledrive/fayetteville_pressure.csv")

# convert the DateTime column to POSIXct
fayetteville$DateTime <- as.POSIXct(fayetteville$time, format = "%Y-%m-%d %H:%M:%S")

#### import baro logger data ####
baro_logger <- googledrive::as_id("https://drive.google.com/drive/folders/1JZ60Fb1WmfkSZiVXJxL1X9nzkRV7AwHq")

# list all CSV files in the folder
baro <- googledrive::drive_ls(path = baro_logger)
# choose the specific file by name
baro <- baro %>% filter(name == "2025-06-04_BARO_RAW.csv")
# download the most recent CSV file
drive_download(as_id(baro$id), path = "googledrive/2025-06-04_BARO_RAW.csv", overwrite = TRUE)
# fetch the file
baro_log <- read.csv("googledrive/2025-06-04_BARO_RAW.csv", skip = 1)

# convert the DateTime column to POSIXct
baro_log$DateTime <- as.POSIXct(baro_log$Date.Time..GMT.05.00, format = "%m/%d/%y %I:%M:%S %p")

########################
#### Clean up files ####
########################
# rename columns to match
baro_log <- baro_log %>% 
  rename("pres.psi" = "Abs.Pres..psi..LGR.S.N..21312563..SEN.S.N..21312563.",
         "Temp.F" = "Temp...F..LGR.S.N..21312563..SEN.S.N..21312563.")

# remove some rows
baro_log <- baro_log[,-c(1, 6:8)]
fayetteville <- fayetteville[,-c(1, 2)]

# psi to m
baro_log <- baro_log %>%
  mutate(pres_m = (pres.psi * 0.703070))
# 1 psi = 0.703070 m

#######################
#### Combine files ####
#######################
pres <- bind_rows(baro_log, fayetteville) %>%
  arrange(DateTime) %>%  # 
  distinct(DateTime, .keep_all = TRUE) # remove duplicates

############################
#### Save combined file ####
############################
# write files to local data folder
write.csv(pres, 'air_br/air_br.csv')

# this is the baro folder
drive_folder_id <- "1BB6nEoVQOrCd_uHEW9n66kR8HCSUUmbC"

# upload file to the specified Google Drive folder
drive_put(
  media = 'air_br/air_br.csv',
  path = as_id(drive_folder_id)
)
