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

files <- list.files(path = "merged", full.names = TRUE)
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
  
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
  # update the data frame in the list
  pt_list[[i]] <- df
  
  # make date into date fomat
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  # update the data frame in the list
  pt_list[[i]] <- df
}

##############################
#### Rename some columns  ####
##############################
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
  ggsave(paste0("pt_figs/", pt_csvs$name[i], ".png"), plot = p)
  # display the plot in the plot panel
  print(p)
}

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

#########################
#### Get air pt data ####
#########################
# this is the merged days folder
pt_air <- googledrive::as_id("https://drive.google.com/drive/folders/1SeGx6MUt6icUFum4Yu-kUHHqZSbN4AQU")
# list all CSV files in the folder
pt_csvs_air <- googledrive::drive_ls(path = pt_air, type = "csv")
# call the specific file you want
googledrive::drive_download(file = pt_csvs_air$id[pt_csvs_air$name=="air_upper.csv"], 
                            path = "googledrive/air_upper.csv",
                            overwrite = T)
googledrive::drive_download(file = pt_csvs_air$id[pt_csvs_air$name=="air_lower.csv"], 
                            path = "googledrive/air_lower.csv",
                            overwrite = T)

air_upper <- read.csv("googledrive/air_upper.csv")
air_lower <- read.csv("googledrive/air_lower.csv")

# changing some column names
air_upper <- air_upper %>%
  dplyr::rename(Date.air = Date,
                Time.air = Time,
                Level.air.kPa = LEVEL)
air_lower <- air_lower %>%
  dplyr::rename(Level_air.m = LEVEL.m)

################################
#### Format DateTime column ####
################################
# for upper
# check datetime format for upper air data
air_upper$DateTime <- paste(air_upper$Date.air, air_upper$Time.air, sep = " ")
# convert the DateTime column to POSIXct
air_upper$DateTime <- as.POSIXct(air_upper$DateTime, format = "%Y-%m-%d %I:%M:%S %p")

# for lower
# DateTime at midnight is missing 00:00:00 time in lower air df, so filling in that time using grep
air_lower$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",air_lower$DateTime)] <- paste(
  air_lower$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",air_lower$DateTime)],"00:00:00")

# convert the DateTime column to POSIXct
air_lower$DateTime <- as.POSIXct(air_lower$DateTime, format = "%Y-%m-%d %H:%M")
str(air_lower$DateTime)

# If you tried to make Date.air and Time.air before, check that code
air_lower$Date.air <- as.Date(air_lower$DateTime) # Extracts date
air_lower$Time.air <- format(air_lower$DateTime, "%H:%M:%S") # Extracts time as string

#########################################
#### Rounding the time to nearest 15 ####
#########################################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  
  # round
  df$DateTime <- round_date(df$DateTime, unit="15 mins")
  # remove duplicates
  df <- df[!duplicated(df$DateTime), ]
  
  # update the data frame in the list
  pt_list[[i]] <- df
}

# look at it
DVWT3 <- pt_list[["DVWT3.csv"]]
DVSB1 <- pt_list[["DVSB1.csv"]]
DVMS5 <- pt_list[["DVMS5.csv"]]
DVNWT4 <- pt_list[["DVNWT4.csv"]]

# round air lower
air_lower$DateTime <- round_date(air_lower$DateTime, unit="15 mins")

air_lower <- air_lower[!duplicated(air_lower$DateTime), ]

#####################################
#### Change all units to meters  ####
#####################################
### upper site
## air pressure in kpa to m
air_upper <- air_upper %>%
  mutate(Level_air.m = (.[[4]] * 0.101972))

#1 kPa = 0.101972 m in common barometric units to water column equivalent conversions
  
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
    mutate(Baro_Cor_Lvl = (.[[7]] - .[[13]]))
  
  compensated_upper[[i]] <-  df
}

## lower
merged_lower[["USF07.csv"]][["DateTime"]] <- floor_date(merged_lower[["USF07.csv"]][["DateTime"]], unit="minute")
USF07 <- merged_lower[["USF07.csv"]]

# create empty compensated list
compensated_lower <- list()

for (i in names(merged_lower)) {
  # access the current data frame
  df <- merged_lower[[i]]
  
  # compensate
  df <- df %>%
    mutate(Baro_Cor_Lvl = (.[[7]] - .[[19]]))
  
  compensated_lower[[i]] <-  df
}

isna <- is.na(compensated_lower$USF20)

# look at it
USF21 <- compensated_upper[["USF21.csv"]]
USF20 <- compensated_lower[["USF20.csv"]]
USF07 <- compensated_lower[["USF07.csv"]]

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
  drive_folder_id <- "1VsT7hirl5OHIGhrrc3b7dxPpSqr1wNC0"
  
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
  drive_folder_id <- "1VsT7hirl5OHIGhrrc3b7dxPpSqr1wNC0"
  
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}

