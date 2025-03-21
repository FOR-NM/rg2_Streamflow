##==============================================================================
## Project: QuEST
## This script is to merge PT and barometric (air pressure) data for the New Hampshire sites
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

##################
#### Packages ####
##################
library(googledrive) 
library(ggplot2)
library(dplyr)

####################################
## Clear folders that we will use ##
####################################
# list and delete al l files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "merged", full.names = TRUE)
file.remove(files)

#################################
#### Import & Visualize Data ####
#################################
#### load data from Google drive ####
# set up Google Drive folder
# this is the "raw" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1EXpGBTsTHikhCdfb1UOIpFl0rmjAvT57")

# create an empty list to store the cleaned data frames
pt_list <- lapply(seq_along(pt_files$name), function(i) {
  googledrive::drive_download(
    file = pt_files$id[i],
    path = paste0("googledrive/", pt_files$name[i]),
    overwrite = TRUE
  )
  
  # read the CSV file, skipping the first 11 rows (header is on row 12)
  read.csv(paste0("googledrive/", pt_files$name[i]), header = TRUE)
})

# assign names to the list elements based on the file names
names(pt_list) <- pt_files$name

# check the contents of the list
str(pt_list)

#############################
#### Format date columns ####
#############################
# for date column
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  
  # make date into date fomat
  df$Date <- as.Date(df$Date, format = "%m/%d/%y")
  # update the data frame in the list
  pt_list[[i]] <- df
}

# for DateTime column
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  
  # combine Date and Time columns into a new DateTime column
  df$DateTime <- paste(df$Date, df$Time, sep = " ")
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
  
  # update the data frame in the list
  pt_list[[i]] <- df
}

# check the contents of the list and make sure there are no NAs
str(pt_list)

#### remove baro files from pt list ####
pt_list = pt_list[-c(9,10)]

#####################
#### Plot curves ####
#####################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # plot
  p <- ggplot(data = df, aes(x = DateTime, y = Water.level.m_sensor)) + 
    geom_line() + ggtitle(paste(pt_files$name[i])) 
  # display the plot in the plot panel
  print(p)
}

####################################################
#### Prep data for merging PT with air pressure ####
####################################################
# this is what it would look like if we were going to use both air pressure data but LMP19 atm data's time is weird rn
# list of site names
ATMSBM <- c("CTB", "LMP01","LMP07", "SMB")
ATMLMP19 <- c("LMP12", "LMP19", "NBR", "NCB", "DDB")	

# assign sites to specific atmospheric loggers
sbm_list <- (pt_list[c("NHCTB_PT_10012048.csv","NHLMP01_PT_21959449.csv", "NHLMP07_PT_21959458.csv", "NHSBM_PT_9951705.csv")])
lmp19_list <- pt_list[c("NHLMP12_PT_21959457.csv", "NHLMP19_PT_21959453.csv", "NHNCBd_PT_21959454.csv",  "NHDDB_PT_21959451.csv")]

# load the air data for the two sites
air_sbm <- read.csv("googledrive/NHATMSMB_PT_9951722.csv")
air_lmp19 <- read.csv("googledrive/NHATMLMP19_PT_21959455.csv")

# format DateTime column #
air_sbm$DateTime <- as.POSIXct(air_sbm$DateTime, format = "%m/%d/%y %H:%M")
air_sbm = air_sbm[-c(6,7)]

#####################################
#### Change all units to meters  ####
#####################################
## air pressure in kpa to m
air_sbm <- air_sbm %>% 
  mutate(Level_air.m = (Pres.abs.kPa * 0.101972))

#1 kPa = 0.101972 m in common barometric units to water column equivalent conversions

## water level is in m already in m, but change kPa to m to do the compensation anyways
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  
  #kPa to m
  df <- df %>%
    mutate(LEVEL.m = (Pres.abs.kPa * 0.101972))
  # update the data frame in the list
  pt_list[[i]] <- df
}
  
######################################
#### Merging PT with Air pressure ####
######################################
# create a list to store merged results
merged <- list()

# loop through each site file in pt_list
for (site in names(pt_list)) {
  # Merge each site data with air_upper on the DateTime column
  merged[[site]] <- merge(pt_list[[site]], air_sbm, by = "DateTime", all.x = TRUE)
}

# check the contents of the list
str(merged)

########################################
#### Manual Barometric Compensation ####
########################################
## once the units for each column are the same, subtract the barometric column from the Levelogger data 
# to get the true net water level recorded by the Levelogger.
# create empty compensated list
compensated <- list()

for (i in names(merged)) {
  # access the current data frame
  df <- merged[[i]]
  
  # compensate
  df <- df %>%
    mutate(Baro_Cor_Lvl = (LEVEL.m - Level_air.m))
  
  compensated[[i]] <-  df
}

#################################
#### Plot final level curves ####
#################################
# loop through each data frame in the list
for (i in seq_along(compensated)) {
  # access the current data frame
  df <- compensated[[i]]
  # plot
  p <- ggplot(data = df, aes(x = DateTime)) +
    geom_line(aes(y = Baro_Cor_Lvl, colour = "Baro_Cor_Lvl")) + 
    geom_line(aes(y = Water.level.m_sensor, colour = "Water.level.m_sensor")) + 
    ggtitle(paste(pt_files$name[i])) 
  # display the plot in the plot panel
  print(p)
  
  # save the plot as a PNG file
  ggsave(paste0("NH_FIGURES/", pt_files$name[i], ".png"), plot = p)
}


####################################################
#### Save merged and compensated slugs to Drive ####
####################################################
# loop through each data frame in the list
for (i in seq_along(compensated)) {
  # Access the current data frame
  df <- compensated[[i]]
  
  # save new data frame
  write.csv(df, paste0("data/", names(compensated)[i], ".csv"), row.names=FALSE, quote=FALSE)
  
  # define the local folder path and the target folder ID in Google Drive
  file <- paste0("data/", names(compensated)[i], ".csv")
  # this is the "compensated" folder
  drive_folder_id <- "1iE76hr5Hw0uyNR2sVLARZJeks7twMZRt"
  
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}
