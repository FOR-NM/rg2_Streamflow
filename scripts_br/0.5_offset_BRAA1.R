##==============================================================================
## Project: QuEST
## This script is to calculate PT offsed for Brush Creek BRAA1 site
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
# this is the "depth" folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1n17b_9yf5DCO_h6uPya5vBPz2dh13L3v")

# List all CSV files in the folder
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")
3

#BRAA1
googledrive::drive_download(file = pt_csvs$id[pt_csvs$name=="BRAA1.csv"], 
                            path = "googledrive/BRAA1.csv",
                            overwrite = T)
# load file
BRAA1 <- read.csv("googledrive/BRAA1.csv")

# combine Date and Time columns into a new DateTime column
BRAA1$DateTime <- paste(BRAA1$Date.x, BRAA1$Time.x, sep = " ")

# convert the DateTime column to POSIXct
BRAA1$DateTime <- as.POSIXct(BRAA1$DateTime, format = "%Y-%m-%d %H:%M:%S")

# filter out rows with missing stage or discharge
rating_data <- BRAA1 %>% 
  filter(!is.na(Baro_Cor_Lvl.m), !is.na(Q_L_per_s))

# check the structure of the cleaned data
head(rating_data)

########################################
#### Plot pressure compensated data ####
########################################
# filter out rows with missing Baro NAs
BRAA1_baro <- BRAA1 %>% 
  filter(!is.na(Baro_Cor_Lvl.m))

ggplot(data = BRAA1_baro, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() + ggtitle("BRAA1 compensated level data")


ggplot(data = BRAA1_baro, aes(x = DateTime, y = LEVEL.m)) +
  geom_line() + ggtitle("BRAA1 compensated level data")

ggplot(data = BRAA1_baro, aes(x = DateTime, y = pres_m)) +
  geom_line() + ggtitle("BRAA1 level data in m")

##################################
#### Plot Stage vs. Discharge ####
##################################
# discharge from L/s to m3/s
rating_data <- rating_data %>%
  mutate(Q.m3s = Q_L_per_s/1000)

ggplot(rating_data, aes(x = Baro_Cor_Lvl.m, y = Q.m3s)) +
  geom_point(color = "blue") +
  geom_text(aes(label = Date.x), vjust = -0.5, size = 3) +  # adds date labels above points
  labs(title = "Stage vs. Discharge", x = "Stage (LEVEL m)", y = "Discharge (Q m3/s)") +
  theme_minimal()
