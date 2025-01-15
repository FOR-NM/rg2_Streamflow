##==============================================================================
## Project: QuEST
## This script is to calculate discharge from compensated pressure data for New Mexico sites
## press Command+Option+O to collapse all sections and get an overview of the workflow
##==============================================================================

##################
#### Packages ####
##################
library(googledrive)
library(ggplot2)
library(lubridate)
library(dplyr)

####################################
## Clear folders that we will use ##
####################################
# List and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data", full.names = TRUE)
file.remove(files)

#################################
#### Import & Visualize Data ####
#################################
#### Load data from Google drive ####
# This is the "depth" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1EswIfUWCK6bsdcs-ZrAMGW1oYKs4B0Eh")

# List all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3
## Call all the files in the salt slugs folder ##
# Create empty list to store data frames
pt_list <- list()

# Loop over each file in the `pt_csvs` data frame
for (i in seq_along(pt_csvs$id)) {
  # Define the local file path
  local_path <- file.path("googledrive", pt_csvs$name[i])
  
  # Download the file
  googledrive::drive_download(
    file = pt_csvs$id[i],
    path = local_path,
    overwrite = T
  )
  # Read the CSV file and add it to the list
  pt_list[[pt_csvs$name[i]]] <- read.csv(local_path)
}

# Check the contents of the list
str(pt_list)

#########################
#### Calculate stage ####
#########################

#Create empty discharge list
stage_list <- list()

for (i in names(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  
  # Select rating curve
  df <- df %>%
    mutate(stage = (
      .[[15]] / 1000 * 9.81
    ))
  
  stage_list[[i]] <-  df
}

###################################
#### Transform DateTime format ####
###################################

# Loop through each data frame in the list
for (i in seq_along(stage_list)) {
  # Access the current data frame
  df <- stage_list[[i]]
  
  # Convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
  # Update the data frame in the list
  stage_list[[i]] <- df
}


# Loop through each data frame in the list
for (i in seq_along(stage_list)) {
  # Access the current data frame
  df <- stage_list[[i]]
  
  # Convert the Date column to POSIXct
  df$Date.x <- as.Date(df$Date.x, format = "%Y-%m-%d")
  # Update the data frame in the list
  stage_list[[i]] <- df
}

# Check the contents of the list and make sure there are no NAs
str(stage_list)

######################################
#### Plotting discharge and stage ####
######################################
### Plotting discharge ###
# Loop through each data frame in the list
for (i in seq_along(stage_list)) {
  # Access the current data frame
  df <- stage_list[[i]]
  
  # Plot
  p <- ggplot(data = df, aes(x = Date.x, y = Q), na.rm = TRUE) + 
    geom_point() + 
    ggtitle(paste(pt_csvs$name[i])) +
    scale_x_date(date_breaks = "2 week", date_labels = "%Y/%m/%d") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Display the plot in the plot panel
  print(p)
}

### Plotting stage ###
# Loop through each data frame in the list
for (i in seq_along(stage_list)) {
  # Access the current data frame
  df <- stage_list[[i]]
  
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = stage)) + 
    geom_point() + 
    ggtitle(paste(pt_csvs$name[i])) +
    scale_x_datetime(date_breaks = "2 week", date_labels = "%Y/%m/%d") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Display the plot in the plot panel
  print(p)
}

#####################################
#### Plotting discharge vs stage ####
#####################################
# Loop through each data frame in the list
for (i in seq_along(stage_list)) {
  # Access the current data frame
  df <- stage_list[[i]]
  
  # Plot
  p <- ggplot(data = df, aes(x = Q, y = stage)) + 
    geom_point() + 
    ggtitle(paste(pt_csvs$name[i])) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Display the plot in the plot panel
  print(p)
}

USF19 <- stage_list[["USF19.csv"]]

#################################
#### Fitting Rating Curve??? ####
#################################
for (i in seq_along(stage_list)) {
  df <- stage_list[[i]]
  df <- df[!is.na(df$stage) & !is.na(df$Q), ]
  
  if (nrow(df) > 3) {  # Ensure enough points for fitting
    start_h0 <- min(df$stage, na.rm = TRUE) - 0.01
    fit <- nls(Q ~ a * (stage - h0)^b, 
               data = df, 
               start = list(a = 1, h0 = start_h0, b = 1.5))
    df$fitted_Q <- predict(fit, newdata = df)
    
    # Plot
    p <- ggplot(data = df, aes(x = stage, y = Q)) +
      geom_point() +
      geom_line(aes(y = fitted_Q), color = "blue", size = 1) +
      ggtitle(paste(pt_csvs$name[i])) +
      theme_minimal()
    
    print(p)
  }
}
