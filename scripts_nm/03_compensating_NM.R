##==============================================================================
## Project: FOR-NM 

## Script 03: Barometric compensation for NM pressure transducer data —
##
## LEVEL -> LEVEL.kPa -> subtract Level_air.kPa -> Baro_Cor_Lvl.kPa -> back
## to m. Compensation happens in kPa; conversion to meters happens once,
## after. PTs reporting LEVEL in cm (converted
## to m, then to kPa); 
##
## INPUT:  01_merge_timestamps_NM.R output ("merged_days" folder) + 02a_merge_timestamps_NM_air.R output
##      
## OUTPUT: "compensated" folder -> used by 05_merge_field_data_NM_*.R
## This script is to merge PT and barometric (air pressure) data for the New Mexico sites
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

##################
#### Packages ####
##################
library(ggplot2)
library(dplyr)
library(lubridate) 
library(hms)

####################################
## Clear folders that we will use ##
####################################
# list and delete all files in the folder
#files <- list.files(path = "googledrive", full.names = TRUE)
#file.remove(files)

#####################
#### Import PT data ####
#####################
#### load data from local folder ####
# this is the merged days folder
pt<-"data/PT/formatted/"

# list all CSV files in the folder
pt_files <- list.files(path = pt, pattern = "\\.csv$")

## call all the files in the salt slugs folder ##
# create empty list to store data frames
pt_list <- list()

# loop over each file in the `pt_csvs` data frame
pt_list <- lapply(seq_along(pt_files), function(i) {
  
  read.csv(paste0(pt, pt_files[i]), header = TRUE)
})


# check the contents of the list
str(pt_list)


##########################################################
#### Combine and format Date and Time into one column #### 
##########################################################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
  
  # make date into date fomat
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  # update the data frame in the list
  pt_list[[i]] <- df
}

# assign names to the list elements based on the file names
names(pt_list) <- pt_files

############################################################
#### Convert PT LEVEL cm -> m -> kPa ####
############################################################
#FOR-NM note: the raw files say m, but it makes more sense that they are in cm, please check w Alex
# 1 m of water column ~ 9.80665 kPa (inverse of the 0.101972 kPa->m factor
# used everywhere else in this pipeline)
to_level_kpa_solinst <- function(df) {
  df %>% mutate(
    LEVEL.m   = LEVEL, # Level is already in m in  level loggers
    LEVEL.kPa = LEVEL.m * 9.80665
  )
}

pt_list <- lapply(pt_list, to_level_kpa_solinst)

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
  p <- ggplot(data = df, aes(x = DateTime, y = LEVEL)) + 
    geom_point() + ggtitle(paste(pt_files[i])) 
  # save the plot as a PNG file
  #ggsave(paste0("pt_figs/", pt_csvs$name[i], ".png"), plot = p)
  # display the plot in the plot panel
  print(p)
}

####################################################
#### Prep data for merging PT with Air pressure ####
####################################################
#in for_nm case, the pt_list contains all sites we are interested in, there is only one airpt 

#########################
#### Get air pt data ####
#########################
# this is the merged days folder
air_pt <- "data/AirPT/formatted/"
# list all CSV files in the folder, only 1 file in folder 
air_pt_files <- list.files(path = air_pt, pattern = "\\.csv$") 
# call the specific file you want
air_fornm <- read.csv(paste0(air_pt, air_pt_files[1]), header = TRUE)  #only 1 file 

################################
#### Format DateTime column ####
################################

# convert the DateTime column to POSIXct
air_fornm$DateTime <- as.POSIXct(air_fornm$DateTime, format = "%Y-%m-%d %H:%M:%S")

#################################################
#### Rounding the time for some of the sites ####
#################################################
#No need to do this with current FOR NM files, look at QUEST scripts if this becomes issue. 

#############################
#### Rename some columns ####
#############################
#  already on the 15-min grid; LEVEL is raw air pressure in kPa;

air_fornm <- air_fornm %>% 
  rename(
    Level_air.kPa = LEVEL,
    Temperature_air.C = TEMPERATURE
  )
#######################
#### Plot air data ####
#######################
ggplot(data = air_fornm, aes(x = DateTime, y = Level_air.kPa)) +
  geom_line() + ggtitle("Air PT (AIR4)")

  
######################################
#### Merging PT with Air pressure ####
######################################
# All  groups (level and air) are already on the same uniform 15-min
# grid produced upstream in 02_merge_timestamps_NM.R / 02a_merge_timestamps_NM_air.R

merged_fornm <- lapply(pt_list, function(df) {
  merge(df, air_fornm, by = "DateTime", all.x = TRUE)
})

########################################
#### Manual Barometric Compensation ####
########################################
compensate <- function(df) {
  df %>% mutate(Baro_Cor_Lvl.kPa = LEVEL.kPa - Level_air.kPa)
}

compensated_fornm  <- lapply(merged_fornm, compensate)


#### convert kPa -> m ####
to_meters <- function(df) {
  df %>% mutate(Baro_Cor_Lvl.m = Baro_Cor_Lvl.kPa * 0.101972)
}

compensated_fornm  <- lapply(compensated_fornm, to_meters)


######################################
#### Plot baro compensated curves ####
######################################
for (i in seq_along(compensated_fornm)) {
  df      <- compensated_fornm[[i]]
  df_name <- names(compensated_fornm)[i]
  p <- ggplot(data = df, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
    geom_line() + ggtitle(df_name)
  ggsave(paste0("pt_figs/", df_name, "_notcorrected.png"), plot = p)
  print(p)
}

####################################################
#### Save merged and compensated slugs to local folder ####
####################################################

output_path<- "data/compensated/"
dir.create(output_path)
for (i in seq_along(compensated_fornm)) {
  df   <- compensated_fornm[[i]]
  file <- paste0(output_path, names(compensated_fornm)[i])
  write.csv(df, file, row.names = FALSE, quote = FALSE)
}


