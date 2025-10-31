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

#####################
#### Import data ####
#####################
#### load data from Google drive ####
# this is the merged days folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1aUwQq19HnlyIxnMHMH3HtybzvA_Zcrwu")

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
USF21 <- pt_list[["USF21.csv"]]
USF20 <- pt_list[["USF20.csv"]]
USF07 <- pt_list[["USF07.csv"]]

##########################################################
#### Combine and format Date and Time into one column #### 
##########################################################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # combine Date and Time columns into a new DateTime column
  df$DateTime <- paste(df$Date, df$Time, sep = " ")
  
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %I:%M:%S %p")
  # update the data frame in the list
  pt_list[[i]] <- df
  
  # make date into date fomat
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  # update the data frame in the list
  pt_list[[i]] <- df
}

#####################################
#### Change all units to meters  ####
#####################################
## water level is in cm, change to m
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  
  #cm to m
  df <- df %>%
    mutate(LELVEL.m = (.[[4]] * 0.01))
  # update the data frame in the list
  pt_list[[i]] <- df
}

# check the contents of the list and make sure there are no NAs
str(pt_list)

# look at it
USF21 <- pt_list[["USF21.csv"]]
USF20 <- pt_list[["USF20.csv"]]
USF07 <- pt_list[["USF07.csv"]]

#####################
#### Plot curves ####
#####################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = LEVEL)) + 
    geom_point() + ggtitle(paste(pt_csvs$name[i])) 
  # save the plot as a PNG file
  #ggsave(paste0("pt_figs/", pt_csvs$name[i], ".png"), plot = p)
  # display the plot in the plot panel
  print(p)
}

####################################################
#### Prep data for merging PT with Air pressure ####
####################################################
# list of site names
uppersites <- c("USF21", "USF13", "USF14", "USF16", "USF19")
middlesites <- c("USF09", "USF10", "USF11")
lowersites <- c("USF03", "USF04", "USF05", "USF07", "USF20")

# create an empty lists to store the files
upper_list <- list()
middle_list <- list()
lower_list <- list()

# separate sites in lists
upper_list <- append(upper_list, c(pt_list["USF21.csv"], pt_list["USF13.csv"], pt_list["USF14.csv"], pt_list["USF16.csv"], pt_list["USF19.csv"]))
middle_list <- append(middle_list, c(pt_list["USF09.csv"], pt_list["USF10.csv"], pt_list["USF11.csv"]))
lower_list <- append(lower_list, c(pt_list["USF03.csv"], pt_list["USF04.csv"], pt_list["USF05.csv"], pt_list["USF07.csv"], pt_list["USF20.csv"]))

#########################
#### Get air pt data ####
#########################
# this is the merged days folder
pt_air <- googledrive::as_id("https://drive.google.com/drive/folders/1BsASDFjFci_7mndSKj6T5uXKxW1UZoaP")
# list all CSV files in the folder
pt_csvs_air <- googledrive::drive_ls(path = pt_air, type = "csv")
# call the specific file you want
googledrive::drive_download(file = pt_csvs_air$id[pt_csvs_air$name=="AIR1_complete.csv"], 
                            path = "googledrive/AIR1_complete.csv",
                            overwrite = T)
googledrive::drive_download(file = pt_csvs_air$id[pt_csvs_air$name=="AIR2.csv"], 
                            path = "googledrive/AIR2.csv",
                            overwrite = T)
googledrive::drive_download(file = pt_csvs_air$id[pt_csvs_air$name=="AIR3.csv"], 
                            path = "googledrive/AIR3.csv",
                            overwrite = T)

air_upper <- read.csv("googledrive/AIR2.csv")
air_middle <-  read.csv("googledrive/AIR3.csv")
air_lower <- read.csv("googledrive/AIR1_complete.csv")

# changing some column names
air_upper <- air_upper %>%
  dplyr::rename(Date.air = Date,
                Time.air = Time,
                Level.air.kPa = LEVEL)

air_lower <- air_lower %>%
  dplyr::rename(Date.air = Date.air1,
                Time.air = Time.air1,
                Level.air.kPa = Level.air.kPa_use)

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
air_lower$Date.air_new <- as.Date(air_lower$DateTime) # Extracts date
air_lower$Time.air <- format(air_lower$DateTime, "%H:%M:%S") # Extracts time as string

#################################################
#### Rounding the time for some of the sites ####
#################################################
USF03 <- lower_list[["USF03.csv"]]
USF04 <- lower_list[["USF04.csv"]]
USF05 <- lower_list[["USF05.csv"]]
USF07 <- lower_list[["USF07.csv"]]
USF13 <- upper_list[["USF13.csv"]]
USF14 <- upper_list[["USF14.csv"]]
USF16 <- upper_list[["USF16.csv"]]
USF19 <- upper_list[["USF19.csv"]]
USF21 <- upper_list[["USF21.csv"]]

# transform to datetime format
USF03$DateTime <- as.POSIXct(USF03$DateTime,format = "%Y-%m-%d %H:%M:%S")
USF04$DateTime <- as.POSIXct(USF04$DateTime,format = "%Y-%m-%d %H:%M:%S")
USF05$DateTime <- as.POSIXct(USF05$DateTime,format = "%Y-%m-%d %H:%M:%S")
USF07$DateTime <- as.POSIXct(USF07$DateTime,format = "%Y-%m-%d %H:%M:%S")
USF13$DateTime <- as.POSIXct(USF13$DateTime,format = "%Y-%m-%d %H:%M:%S")
USF14$DateTime <- as.POSIXct(USF14$DateTime,format = "%Y-%m-%d %H:%M:%S")
USF16$DateTime <- as.POSIXct(USF16$DateTime,format = "%Y-%m-%d %H:%M:%S")
USF19$DateTime <- as.POSIXct(USF19$DateTime,format = "%Y-%m-%d %H:%M:%S")
USF21$DateTime <- as.POSIXct(USF21$DateTime,format = "%Y-%m-%d %H:%M:%S")
air_upper$DateTime <- as.POSIXct(air_upper$DateTime,format = "%Y-%m-%d %H:%M:%S")

#USF13$DateTimeNotRounded <- as.POSIXct(USF13$DateTimeNotRounded,format = "%Y-%m-%d %H:%M:%S")

# round DateTime to the nearest 15-minute interval 
USF07$DateTime <- floor_date(USF07$DateTime, unit="minute")

USF03$DateTime <- round_date(USF03$DateTime, unit="15 mins")
USF04$DateTime <- round_date(USF04$DateTime, unit="15 mins")
USF05$DateTime <- round_date(USF05$DateTime, unit="15 mins")
USF07$DateTime <- round_date(USF07$DateTime, unit="15 mins")
USF13$DateTime <- round_date(USF13$DateTime, unit="15 mins")
USF14$DateTime <- round_date(USF14$DateTime, unit="15 mins")
USF16$DateTime <- round_date(USF16$DateTime, unit="15 mins")
USF19$DateTime <- round_date(USF19$DateTime, unit="15 mins")
USF21$DateTime <- round_date(USF21$DateTime, unit="15 mins")
air_upper$DateTime <- round_date(air_upper$DateTime, unit="15 mins")

# return to list
lower_list$USF03.csv <- USF03
lower_list$USF04.csv <- USF04
lower_list$USF05.csv <- USF05
lower_list$USF07.csv <- USF07
upper_list$USF13.csv <- USF13
upper_list$USF14.csv <- USF14
upper_list$USF16.csv <- USF16
upper_list$USF19.csv <- USF19
upper_list$USF21.csv <- USF21

USF07 <- lower_list[["USF07.csv"]]

#####################################
#### Change all units to meters  ####
#####################################
### upper site
## air pressure in kpa to m
air_upper <- air_upper %>%
  mutate(Level_air.m = (.[[4]] * 0.101972))

### lower site
## air pressure in kpa to m
air_lower <- air_lower %>%
  mutate(Level_air.m = (.[[11]] * 0.101972))

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
USF21 <- merged_upper[["USF21.csv"]]
USF20 <- merged_lower[["USF20.csv"]]
USF07 <- merged_lower[["USF07.csv"]]

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
    mutate(Baro_Cor_Lvl = (LELVEL.m - Level_air.m))
  
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
USF16 <- compensated_upper[["USF16.csv"]]
USF20 <- compensated_lower[["USF20.csv"]]
USF07 <- compensated_lower[["USF07.csv"]]

######################################
#### Plot baro compensated curves ####
######################################
# loop through each data frame in the list
for (i in seq_along(compensated_upper)) {
  # access the current data frame
  df <- compensated_upper[[i]]
  
  # get the name of the current data frame (list element)**
  df_name <- names(compensated_upper)[i]
  
  # plot
  p <- ggplot(data = df, aes(x = DateTime, y = Baro_Cor_Lvl)) + 
    geom_point() +
    # **Add the title using the retrieved name**
    labs(title = df_name)
  
  # save the plot as a PNG file
  ggsave(paste0("pt_figs/", df_name, ".png"), plot = p)
  # display the plot in the plot panel
  print(p)
}

# loop through each data frame in the list
for (i in seq_along(compensated_lower)) {
  # access the current data frame
  df <- compensated_lower[[i]]

  # get the name of the current data frame (list element)**
  df_name <- names(compensated_lower)[i]
  
  # lot
  p <- ggplot(data = df, aes(x = DateTime, y = Baro_Cor_Lvl)) + 
    geom_point() +
    # **Add the title using the retrieved name**
    labs(title = df_name)
  
  # save the plot as a PNG file
  ggsave(paste0("pt_figs/", df_name, ".png"), plot = p)
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

