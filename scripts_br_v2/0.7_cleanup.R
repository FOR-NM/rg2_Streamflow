##==============================================================================
## Project: QuEST
## This script is to clean predicted discharge files for NM
##==============================================================================

##################
#### Packages ####
##################
library(googledrive) 
library(ggplot2)
library(dplyr)
library(lubridate) 
library(xts) # for time series
library(zoo) # for rollmean function

####################################
## Clear folders that we will use ##
####################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "pt_figs", full.names = TRUE)
file.remove(files)

#####################
#### Import data ####
#####################
# this is the predicted folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1PNCX_xYwu57gYMFNLtHiAFbBi7m-L1Uf")

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

# look at it
BRMQ4 <- pt_list[["discharge_BRMQ4.csv"]]
BRM02 <- pt_list[["discharge_BRM02.csv"]]

############################
#### Format date column #### 
############################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$time, format = "%Y-%m-%d %H:%M:%S")
  # update the data frame in the list
  pt_list[[i]] <- df
  
  # update the data frame in the list
  pt_list[[i]] <- df
}

# look at it
BRMQ4 <- pt_list[["discharge_BRMQ4.csv"]]
BRM02 <- pt_list[["discharge_BRM02.csv"]]

##################
#### Clean up #### 
##################
# columns that I don't want
drops <- c("X.1", "X.2","X","X.","X.x","X.y","Date.x", "ms", "LEVEL.m","Date.air","Level.air.kPa1","Time.x","Time.y",
           "Date.Time..GMT.05.00", "DateTimeNotRounded","Coupler.Detached..LGR.S.N..21312563.","Coupler.Attached..LGR.S.N..21312563.", 
           "Host.Connected..LGR.S.N..21312563.","End.Of.File..LGR.S.N..21312563.","Comments","background","medianSPC.x","salt.x",
           "Predicted_Discharge_Linear", "Residual_Linear", "Q_L_per_s", "tsun", "snow", "Predicted_Local_Pressure", "Residual_local",
           "Predicted_Discharge_Linear.m3s")

# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # remove columns 
  df <- df[ , !(names(df) %in% drops)]
  # update the data frame in the list
  pt_list[[i]] <- df
}

BRMQ4$Residual_local
# look at it
BRMQ4 <- pt_list[["discharge_BRMQ4.csv"]]
BRMQ1 <- pt_list[["discharge_BRMQ1.csv"]]
BRM01 <- pt_list[["discharge_BRM01.csv"]]
BRM02 <- pt_list[["discharge_BRM02.csv"]]
BRM07 <- pt_list[["discharge_BRM07.csv"]]
BRAA1 <- pt_list[["discharge_BRAA1.csv"]]

# rename discharge column
BRMQ1 <- BRMQ1 %>%
  rename(Discharge.m3s = Predicted_Discharge_Log)
BRMQ4 <- BRMQ4 %>%
  rename(Discharge.m3s = Predicted_Discharge_Log)
BRM01 <- BRM01 %>%
  rename(Discharge.m3s = Predicted_Discharge_Log.m3s)
BRM02 <- BRM02 %>%
  rename(Discharge.m3s = Predicted_Discharge_Log.m3s)
BRM07 <- BRM07 %>%
  rename(Discharge.m3s = Predicted_Discharge_Log)
BRAA1 <- BRAA1 %>%
  rename(Discharge.m3s = Predicted_Discharge_Log.m3s)

# return to list
pt_list <- list()
pt_list$BRMQ1 <- BRMQ1
pt_list$BRMQ4 <- BRMQ4
pt_list$BRM01 <- BRM01
pt_list$BRM02 <- BRM02
pt_list$BRM07 <- BRM07
pt_list$BRAA1 <- BRAA1


ggplot(data=BRAA1, aes(x=DateTime, y=Discharge.m3s)) +
  geom_line() + ggtitle("BRAA1")
ggplot(data=BRM01, aes(x=DateTime, y=Discharge.m3s)) +
  geom_line() + ggtitle("BRM01")
ggplot(data=BRM02, aes(x=DateTime, y=Discharge.m3s)) +
  geom_line() + ggtitle("BRM02")
ggplot(data=BRM07, aes(x=DateTime, y=Discharge.m3s)) +
  geom_line() + ggtitle("BRM07")
ggplot(data=BRMQ1, aes(x=DateTime, y=Discharge.m3s)) +
  geom_line() + ggtitle("BRMQ1")
ggplot(data=BRMQ4, aes(x=DateTime, y=Discharge.m3s)) +
  geom_line() + ggtitle("BRMQ4")

#####################
#### Plot curves ####
#####################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  
  df_name <- names(pt_list)[i]   # name of the df
  df <- pt_list[[i]]
  
  p <- ggplot(data = df, aes(x = DateTime, y = Discharge.m3s)) + 
    geom_line() + ggtitle(df_name)
  
  # save plot
  ggsave(
    filename = paste0(df_name, "_Discharge.png"),
    plot = p,
    width = 8,
    height = 4,
    dpi = 300
  )
}

###########################################
#### one hour rolling window smoothing ####
###########################################
# filter daylight savings empty dates
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # remove NA dates 
  df <- df %>%
    filter(!is.na(DateTime))
  # update the data frame in the list
  pt_list[[i]] <- df
}

# 1. Convert data frame to an xts object
xts_list <- list()

for (i in seq_along(pt_list)) {
  df <- pt_list[[i]]
  # convert to xts object
  xts_list[[i]] <- tryCatch({
    xts(df$Discharge.m3s, order.by = df$DateTime)
  })
}
# keep original file names
names(xts_list) <- names(pt_list)

# 2. Apply the 1-hour moving average (4 observations for 15-min data)
# k = 4 for a 1-hour window with 15-minute data 
# align = "right" means the result is aligned with the end of the window.
# na.pad = TRUE will keep the original length and fill leading NAs.

# make new list
smoothed_xts_list <- list()

for (i in seq_along(xts_list)) {
  # access the current data frame
  df <- xts_list[[i]]
  # smooth
  smoothed_xts_list[[i]] <- rollmean(df, k = 4, align = "right", na.pad = TRUE)
  # update the data frame in the list
  xts_list[[i]] <- df
}

# preserve the names from the original xts_list for easier identification
names(smoothed_xts_list) <- names(xts_list)

# 3. Add the smoothed data back to your original data frame
# make new list
smooth_df <- list()

# Loop through the original data frames and the smoothed xts objects
for (i in seq_along(pt_list)) {
  # Get the original data frame
  df_original <- pt_list[[i]]
  
  # Get the corresponding smoothed xts object
  smoothed_xts_obj <- smoothed_xts_list[[i]]
  # Add the smoothed data as a new column to the original data frame
  df_original$Smooth_Discharge_Log_m3s <- as.numeric(smoothed_xts_obj)
  # Store the modified data frame in the new list
  smooth_df[[i]] <- df_original
}

# preserve the names from the original xts_list for easier identification
names(smooth_df) <- names(smoothed_xts_list)

############################
#### Plot smooth curves ####
############################
# loop through each data frame in the list
for (i in seq_along(smooth_df)) {
  # access the current data frame
  df <- smooth_df[[i]]
  # plot
  p <- ggplot(data = df, aes(x = DateTime, y = Smooth_Discharge_Log_m3s)) + 
    geom_line()
  # save the plot as a PNG file
  # ggsave(paste0("pt_figs/", pt_csvs$name[i], "smooth.png"), plot = p)
  # display the plot in the plot panel
  print(p)
}

#######################################
#### Save merged PT files to Drive ####
#######################################
# loop through each data frame in the list
for (i in seq_along(smooth_df)) {
  # Access the current data frame
  df <- smooth_df[[i]]
  
  # save new data frame
  write.csv(df, paste0("data/", names(smooth_df)[i],".csv"), row.names=FALSE, quote=FALSE)
  
  # define the local folder path and the target folder ID in Google Drive
  file <- paste0("data/", names(smooth_df)[i], ".csv")
  # this is the "smooth" folder
  drive_folder_id <- "14mEgtrF_eMrzB2xjL7pCwEKIokQzbwP2"
  
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}

###############################
#### Do just one at a time ####
###############################
# plot
ggplot(data = USF20, aes(x = DateTime, y = Predicted_Discharge_Log_m3s)) + 
  geom_line() + ggtitle(paste(pt_csvs$name[i])) 

# 1. Convert data frame to an xts object
discharge_xts20 <- xts(USF20$Predicted_Discharge_Log_m3s, order.by = USF20$DateTime)

# 2. Apply the 1-hour moving average (4 observations for 15-min data)
# k = 4 for a 1-hour window with 15-minute data 
# align = "right" means the result is aligned with the end of the window.
# na.pad = TRUE will keep the original length and fill leading NAs.
smoothed_discharge_xts <- rollmean(discharge_xts20, k = 4, align = "right", na.pad = TRUE)

# 3. Add the smoothed data back to your original data frame
USF20$Smoothed_Discharge_Log_m3s <- as.numeric(smoothed_discharge_xts)

# plot smoothed discharge 
ggplot(data = USF20, aes(x = DateTime, y = Smoothed_Discharge_Log_m3s)) +
  geom_line() + ggtitle("USF20 compensated level data")

# View the head of your updated data frame to see the new smoothed column
head(USF20)

