##==============================================================================
## Project: FOR-NM 

# this script might not be necessary for us, should be an aid script for completing 
#missing data from other PTs 
##==============================================================================

##################
#### Packages ####
##################
#ibrary(googledrive) 
library(ggplot2)
library(dplyr)
library(lubridate) 

####################################
## Clear folders that we will use ##
####################################
# list and delete all files in the folder
#files <- list.files(path = "googledrive", full.names = TRUE)
#file.remove(files)

#files <- list.files(path = "merged", full.names = TRUE)
#file.remove(files)

#########################
#### Import air and pt data ####
#########################
# this is the merged days folder
air_pt <- "data/AirPT/formatted/"
pt<-"data/PT/formatted/"


# list all CSV files in the folder, only 1 file in folder 
air_pt_files <- list.files(path = air_pt, pattern = "\\.csv$") 

air_fornm <- read.csv(paste0(air_pt, air_pt_files[1]), header = TRUE)  #only 1 file 


pt_files <- list.files(path = pt, pattern = "\\.csv$")


# create an empty list to store the cleaned data frames
pt_list <- lapply(seq_along(pt_files), function(i) {
  
  read.csv(paste0(pt, pt_files[i]), header = TRUE)
})

# assign names to the list elements based on the file names
names(pt_list) <- pt_files


################################
#### Format DateTime column ####
################################
# convert the DateTime column to POSIXct
air_fornm$DateTime <- as.POSIXct(air_fornm$DateTime, format = "%Y-%m-%d %H:%M:%S")

# format dateTime for PT files again 
for (i in seq_along(pt_list)) {
  df <- pt_list[[i]]
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
  # update the data frame in the list
  pt_list[[i]] <- df
}

###########################################
#### Plot pressure data for airpt ####
###########################################
ggplot(data = air_fornm, aes(x = DateTime, y = LEVEL)) +
  geom_line() + ggtitle("Air 4 pressure")



######################################
#### Plot pressure data for pt  ####
######################################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  
  # get the name of the current data frame (list element)**
  df_name <- names(pt_list)[i]
  
  # plot
  p <- ggplot(data = df, aes(x = DateTime, y = LEVEL)) + 
    geom_point() +
    # **Add the title using the retrieved name**
    labs(title = df_name)
  
  # display the plot in the plot panel
  print(p)
}





