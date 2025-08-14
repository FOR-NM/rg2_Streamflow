##==============================================================================
## Project: QuEST
## This script is to merge PT and barometric (air pressure) data for the New Mexico sites
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

##################
#### Packages ####
##################
library(googledrive) 
library(ggplot2)
library(dplyr)
library(lubridate) 
library(hms)

####################################
## Clear folders that we will use ##
####################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data", full.names = TRUE)
file.remove(files)

#####################
#### Import data ####
#####################
#### load data from Google drive ####
# this is the merged days level folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/12OxuAC-i4_QB08ip6CI4W-OGt2vbg1eP")

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

# check the contents of the list
str(pt_list)

# look at it
DVWT3 <- pt_list[["DVWT3.csv"]]
DVSB1 <- pt_list[["DVSB1.csv"]]
DVMS5 <- pt_list[["DVMS5.csv"]]

##########################################################
#### Combine and format Date and Time into one column #### 
##########################################################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  
  # make date into date format
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  
  # DateTime at midnight is missing 00:00:00 time in lower air df, so filling in that time using grep
  df$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",df$DateTime)] <- paste(
    df$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",df$DateTime)],"00:00:00")
  
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
  
  # update the data frame in the list
  pt_list[[i]] <- df

  # update the data frame in the list
  pt_list[[i]] <- df
}

# look at it
DVWT3 <- pt_list[["DVWT3.csv"]]
DVSB1 <- pt_list[["DVSB1.csv"]]
DVMS5 <- pt_list[["DVMS5.csv"]]
DVNWT4 <- pt_list[["DVNWT4.csv"]]

Date1 <- as.Date("2025-04-01", "%Y-%m-%d")
Date2 <- as.Date("2025-04-30", "%Y-%m-%d")
subdf <- DVSB1[DVSB1$DateTime < Date2 & DVSB1$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,LEVEL)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-18 14:00:00"), linetype="dashed", color="red")

#############################
#### Rename some columns ####
#############################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  
  #rename
  df <- df %>%
    rename(LEVEL.m = LEVEL)
  # update the data frame in the list
  pt_list[[i]] <- df
}

# check the contents of the list and make sure there are no NAs
str(pt_list)

#####################
#### Plot curves ####
#####################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = LEVEL.m)) + 
    geom_line() + ggtitle(paste(pt_csvs$name[i])) 
  # save the plot as a PNG file
  #ggsave(paste0("pt_figs/", pt_csvs$name[i], ".png"), plot = p)
  # display the plot in the plot panel
  print(p)
}

Date1 <- as.Date("2025-04-01", "%Y-%m-%d")
Date2 <- as.Date("2025-04-30", "%Y-%m-%d")
subdf <- DVWT3[DVWT3$DateTime < Date2 & DVWT3$DateTime > Date1,]
ggplot(data=subdf, aes(DateTime,LEVEL)) + geom_line() +
  geom_vline(xintercept = as.POSIXct("2025-04-18 14:00:00"), linetype="dashed", color="red")

#########################
#### Get air pt data ####
#########################
# this is the merged days baro folder
pt_air <- googledrive::as_id("https://drive.google.com/drive/folders/1SeGx6MUt6icUFum4Yu-kUHHqZSbN4AQU")
# list all CSV files in the folder
pt_csvs_air <- googledrive::drive_ls(path = pt_air, type = "csv")
# call the specific file you want
googledrive::drive_download(file = pt_csvs_air$id[pt_csvs_air$name=="DVNWT5.csv"], 
                            path = "googledrive/DVNWT5.csv",
                            overwrite = T)
googledrive::drive_download(file = pt_csvs_air$id[pt_csvs_air$name=="DVO.csv"], 
                            path = "googledrive/DVO.csv",
                            overwrite = T)

air_upper <- read.csv("googledrive/DVNWT5.csv")
air_lower <- read.csv("googledrive/DVO.csv")

# changing some column names
air_lower <- air_lower %>%
  dplyr::rename(Date.air = Date,
                Time.air = Time,
                Level_air.m = LEVEL.m)

################################
#### Format DateTime column ####
################################
# for lower
# check datetime format
air_lower$DateTime <- paste(air_lower$Date.air, air_lower$Time.air, sep = " ")
# convert the DateTime column to POSIXct
air_lower$DateTime <- as.POSIXct(air_lower$DateTime, format = "%Y-%m-%d %H:%M:%S")
# 
# # convert the DateTime column to POSIXct
# air_lower$DateTime <- as.POSIXct(air_lower$DateTime, format = "%Y-%m-%d %H:%M:%S")
# # DateTime at midnight is missing 00:00:00 time in lower air df, so filling in that time using grep
# air_lower$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",air_lower$DateTime)] <- paste(
#   air_lower$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",air_lower$DateTime)],"00:00:00")
# 
# # If you tried to make Date.air and Time.air before, check that code
# air_lower$Date.air <- as.Date(air_lower$DateTime) # Extracts date
# air_lower$Time.air <- format(air_lower$DateTime, "%H:%M:%S") # Extracts time as string

# for upper
# check datetime format
air_upper$DateTime <- paste(air_upper$Date.air, air_upper$Time.air, sep = " ")
# convert the DateTime column to POSIXct
air_upper$DateTime <- as.POSIXct(air_upper$DateTime, format = "%Y-%m-%d %I:%M:%S %p")

#######################
#### Plot air data ####
#######################
ggplot(data = air_upper, aes(x = DateTime, y = Level_air.m)) + 
  geom_line() + ggtitle("Air upper") 

ggplot(data = air_lower, aes(x = DateTime, y = Level_air.m)) + 
  geom_line() + ggtitle("Air lower") 

####################################################
#### Prep data for merging PT with Air pressure ####
####################################################
# list of site names
uppersites <- c("DVWT3","DVWT1","DVNWT5","DVNWT4","DVNWT3","DVMS5","DVMS1","DVET")
lowersites <- c("DVSB1", "DVSB2")

# create an empty lists to store the files
upper_list <- list()
lower_list <- list()

# separate sites in lists
upper_list <- append(upper_list, c(pt_list["DVWT3.csv"], pt_list["DVWT1.csv"], pt_list["DVNWT5.csv"], pt_list["DVNWT4.csv"], 
                                   pt_list["DVNWT3.csv"], pt_list["DVMS5.csv"], pt_list["DVMS1.csv"], pt_list["DVET.csv"]))
lower_list <- append(lower_list, c(pt_list["DVSB1.csv"], pt_list["DVSB2.csv"]))

######################################
#### Merging PT with Air pressure ####
######################################
## upper
# create a list to store merged results
merged_upper <- list()

# loop through each site file in upper_list
for (site in names(upper_list)) {
  # merge each site data with air_upper on the DateTime column
  merged_upper[[site]] <- merge(upper_list[[site]], air_upper, by = "DateTime", all.x = TRUE)
}

# check the contents of the list
str(merged_upper)

## lower
# create a list to store merged results
merged_lower <- list()

# loop through each site file in upper_list
for (site in names(lower_list)) {
  # merge each site data with air_upper on the DateTime column
  merged_lower[[site]] <- merge(lower_list[[site]], air_lower, by = "DateTime", all.x = TRUE)
}

# check the contents of the list
str(merged_lower)

# look at it
DVWT3 <- merged_upper[["DVWT3.csv"]]
DVSB1 <- merged_lower[["DVSB1.csv"]]
DVMS5 <- merged_upper[["DVMS5.csv"]]

########################################
#### Manual Barometric Compensation ####
########################################
## once the units for each column are the same, subtract the barometric column from the Levelogger data 
# to get the true net water level recorded by the Levelogger.

## upper
# create empty compensated list
compensated_upper <- list()

for (i in names(merged_upper)) {
  # access the current data frame
  df <- merged_upper[[i]]
  
  # compensate
  df <- df %>%
    mutate(Baro_Cor_Lvl = (df$LEVEL.m - df$Level_air.m))
  
  compensated_upper[[i]] <-  df
}

# create empty compensated list
compensated_lower <- list()

for (i in names(merged_lower)) {
  # access the current data frame
  df <- merged_lower[[i]]
  
  # compensate
  df <- df %>%
    mutate(Baro_Cor_Lvl = (df$LEVEL.m - df$Level_air.m))
  
  compensated_lower[[i]] <-  df
}

isna <- is.na(compensated_lower$DVSB1.csv)

# look at it
DVSB1 <- compensated_lower[["DVSB1.csv"]]
DVWT3 <- compensated_upper[["DVWT3.csv"]]
DVNWT5 <- compensated_upper[["DVNWT5.csv"]]

#####################################
#### Plot baro compensated level ####
#####################################
# loop through each data frame in the list
for (i in seq_along(compensated_lower)) {
  # access the current data frame
  df <- compensated_lower[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = Baro_Cor_Lvl)) + 
    geom_line() + ggtitle(names(compensated_lower)[i])
  # save the plot as a PNG file
  ggsave(paste0("pt_figs/", names(compensated_lower)[i], "_notcorrected.png"), plot = p)
  # display the plot in the plot panel
  print(p)
}

# loop through each data frame in the list
for (i in seq_along(compensated_upper)) {
  # access the current data frame
  df <- compensated_upper[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = Baro_Cor_Lvl)) + 
    geom_line() + ggtitle(names(compensated_upper)[i])
  # save the plot as a PNG file
  ggsave(paste0("pt_figs/", names(compensated_upper)[i], "_notcorrected.png"), plot = p)
  # display the plot in the plot panel
  print(p)
}

####################################################
#### Save merged and compensated slugs to Drive ####
####################################################
## upper
# loop through each data frame in the list
for (i in seq_along(compensated_upper)) {
  # access the current data frame
  df <- compensated_upper[[i]]
  
  # save new data frame
  write.csv(df, paste0("data/", names(compensated_upper)[i]), row.names=FALSE, quote=FALSE)
  
  # define the local folder path and the target folder ID in Google Drive
  file <- paste0("data/", names(compensated_upper)[i])
  # this is the "compensated" folder
  drive_folder_id <- "15HqdL5fw1BDTt_X6xWsm__GTJcZCIy8w"
  
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}

## lower
# loop through each data frame in the list
for (i in seq_along(compensated_lower)) {
  # access the current data frame
  df <- compensated_lower[[i]]
  
  # save new data frame
  write.csv(df, paste0("data/", names(compensated_lower)[i]), row.names=FALSE, quote=FALSE)
  
  # define the local folder path and the target folder ID in Google Drive
  file <- paste0("data/", names(compensated_lower)[i])
  # this is the "compensated" folder
  drive_folder_id <- "15HqdL5fw1BDTt_X6xWsm__GTJcZCIy8w"
  
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}

