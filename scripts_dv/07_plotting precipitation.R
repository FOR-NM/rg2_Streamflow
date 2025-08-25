##==============================================================================
## Project: QuEST
## This script is to subset precipitation data for Dog Valley 
##==============================================================================

##################
#### Packages ####
##################
library(tidyverse)
library(lubridate)
library(stringr)
library(ggplot2)
library(plotly)
library(patchwork)

###################################
#### Import precipitation Data ####
###################################
#### load data from Google drive ####
# this is the "depth" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1Xn1jx4x6RjuXfEq655xqOkKRIu0rYtbL")

# list all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3

#Precip
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="precipitation_dv.csv"], 
                            path = "googledrive/precip.csv",
                            overwrite = T)
# load file
precip <- read.csv("googledrive/precip.csv")

# convert Date column to Date type if not already
precip$Date <- as.Date(precip$Date, format = "%m/%d/%Y")
# combine Date and Time columns into a new DateTime column
precip$DateTime <- paste(precip$Date, precip$Time, sep = " ")
# convert the DateTime column to POSIXct
precip$DateTime <- as.POSIXct(precip$DateTime, format = "%Y-%m-%d %H:%M")

# remove rows that I don't want
precip <- precip[ , -c(1:3, 5:14)]

############################
#### Import logger data ####
############################
#### load data from Google drive ####
# this is the depth folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/136WGq6adaNROjaJN2YL63yJExKikM81A")

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

##########################################################
#### Combine and format Date and Time into one column #### 
##########################################################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  
  # make date into date format
  df$Date <- as.POSIXct(df$DateTime, format = "%Y-%m-%d")
  
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

###################################################
#### Merge logger data with precipitation data ####
###################################################
# create a list to store merged results
merged <- list()

# loop through each site file in pt_list
for (site in names(pt_list)) {
  # merge each site data with air_upper on the DateTime column
  merged[[site]] <- merge(pt_list[[site]], precip, by = "DateTime", all.x = TRUE)
}

DVMS5 <- merged[["offset_DVMS5.csv"]] 
DVWT3 <- merged[["offset_DVWT3.csv"]] 
DVSB2 <- merged[["offset_DVSB2.csv"]] 

### keep rows with hour intervals since precip data is only hourly ###
# loop through each data frame in the list
for (i in seq_along(merged)) {
  # access the current data frame
  df <- merged[[i]]
  #filter function
  df <- df %>%
    filter(format(df$DateTime, "%M") %in% c("00"))
  # update the data frame in the list
  merged[[i]] <- df
  }

#####################
#### Plot curves ####
#####################
# loop through each data frame in the list
for (i in seq_along(merged)) {
  # access the current data frame
  df <- merged[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime)) +
    geom_line(aes(y = LEVEL.m, color = "Level")) +
    geom_line(aes(y = Precip_mm/100, color = "Precipitation")) + # scale precipitation to fit on the same axis
    scale_y_continuous(
      name = "Level (m)",
      sec.axis = sec_axis(~ . * 100, name = "Precipitation (mm)")
    ) +
    ggtitle(paste(pt_csvs$name[i])) +
    labs(color = "Variable") +
    theme_minimal()
  # display the plot in the plot panel
  print(p)
}

# loop through each data frame in the list
for (i in seq_along(merged)) {
  # access the current data frame
  df <- merged[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime)) +
    geom_line(aes(y = LEVEL.m, color = "Level")) +
    # Let's say we still need to scale down the inches data
    geom_line(aes(y = Precip_in, color = "Precipitation")) +
    scale_y_continuous(
      name = "Level (m)",
      # Update the secondary axis transformation and label
      sec.axis = sec_axis(~ ., name = "Precipitation (inches)")
    ) +
    ggtitle(paste(pt_csvs$name[i])) +
    labs(color = "Variable") +
    theme_minimal()
  # display the plot in the plot panel
  print(p)
}

# loop through each data frame in the list
for (i in seq_along(merged)) {
  # access the current data frame
  df <- merged[[i]]
  # Plot level
  p_level <- ggplot(data = df, aes(x = DateTime, y = Baro_Cor_Lvl)) +
    geom_line(color = "blue") +
    ggtitle(paste(pt_csvs$name[i])) +
    ylab("Level (m)") +
    theme_minimal()
  
  # Plot precipitation
  p_precip <- ggplot(data = df, aes(x = DateTime, y = Precip_in)) +
    geom_line(color = "red") +
    # Update the y-axis label to inches
    ylab("Precipitation (inches)") +
    theme_minimal() +
    xlab("Date")
  
  # Combine plots and align them
  combined_plot <- p_level / p_precip
  
  # display the combined plot
  print(combined_plot)
  
  # save the plot as a PNG file
  ggsave(paste0("pt_figs/", pt_csvs$name[i], "_precip.png"), plot = combined_plot)
  
}

# plot individually
# DVMS5
# Plot level
p_level <- ggplot(data = subdf, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line(color = "blue") +
  ggtitle("DVMS5") +
  ylab("Level (m)") +
  geom_vline(xintercept = as.POSIXct("2024-08-06 10:00:00"), linetype="dashed") +
  theme_minimal()

# Plot precipitation
p_precip <- ggplot(data = subdf, aes(x = DateTime, y = Precip_in)) +
  geom_line(color = "red") +
  # Update the y-axis label to inches
  ylab("Precipitation (inches)") +
  geom_vline(xintercept = as.POSIXct("2024-08-06 10:00:00"), linetype="dashed") +
  theme_minimal() +
  xlab("Date")

# Combine plots and align them
combined_plot <- p_level / p_precip

# display the combined plot
print(combined_plot)

#take the subset of the data for August when PT was moved
Date1 <- as.Date("2024-08-01", "%Y-%m-%d")
Date2 <- as.Date("2024-08-09", "%Y-%m-%d")
subdf <- DVMS5[DVMS5$DateTime < Date2 & DVMS5$DateTime > Date1,]

# DVSB2
# Plot level
p_level <- ggplot(data = DVSB2, aes(x = DateTime, y = Baro_Cor_Lvl)) +
  geom_line(color = "blue") +
  ggtitle("DVSB2") +
  ylab("Level (m)") +
  geom_vline(xintercept = as.POSIXct("2024-09-06 12:00:00"), linetype="dashed") +
  theme_minimal()

# Plot precipitation
p_precip <- ggplot(data = DVSB2, aes(x = DateTime, y = Precip_in)) +
  geom_line(color = "red") +
  # Update the y-axis label to inches
  ylab("Precipitation (inches)") +
  geom_vline(xintercept = as.POSIXct("2024-09-06 12:00:00"), linetype="dashed") +
  theme_minimal() +
  xlab("Date")

# Combine plots and align them
combined_plot <- p_level / p_precip

# display the combined plot
print(combined_plot)

