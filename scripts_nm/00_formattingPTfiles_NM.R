##==============================================================================
## Project: FOR-NM 
## Script to format all the Pressure Transducer (Level Logger) to a cleaner state and upload them back to Drive
## press Command+Option+O to collapse all sections and get an overview of the workflow!
## FOR-NM PTs are Solonist brand 
##==============================================================================
##############
## Packages ##
##############
#library(googledrive)
library(tidyverse)

########################################
#### Clear folders that we will use ####
########################################
# list and delete all files in the folder
#files <- list.files(path = "googledrive", full.names = TRUE)
#file.remove(files)

#####################
#### Import Data ####
#####################
# set up Google Drive folder
# this is the "raw air" folder
#pt <- googledrive::as_id("https://drive.google.com/drive/folders/1jhw-miI7fPaP8947HCbJ1idnIl6g1A8c")

# list and filter CSV files with "pt" in their names
#pt_files <- googledrive::drive_ls(path = pt, type = "csv")

#local path here 
pt<- "data/PT/raw/"
pt_files<- list.files(path = pt, pattern = "\\.csv$")

output_path<- "data/PT/formatted/"
dir.create(output_path)



# create an empty list to store the cleaned data frames
pt_list <- lapply(seq_along(pt_files), function(i) {
  # read the CSV file, skipping the first 11 rows (header is on row 12)
  read.csv(paste0(pt, pt_files[i]),skip=13,  header = TRUE)
})

# assign names to the list elements based on the file names
names(pt_list) <- pt_files

# check the contents of the list
str(pt_list)



##########################################################
#### Combine and format Date and Time into one column #### 
##########################################################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  
  # make date into date fomat
  df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
  # combine Date and Time columns into a new DateTime column
  df$DateTime <- paste(df$Date, df$Time, sep = " ")
  
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %I:%M:%S %p")
  
  
  # update the data frame in the list
  pt_list[[i]] <- df
}

# check the contents of the list and make sure there are no NAs
str(pt_list)

#########################################
#### Save edited files back to  ####
#########################################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  
  # save new data frame
  write.csv(df, paste0(output_path, pt_files[i]), row.names=FALSE, quote=FALSE)
  
}

