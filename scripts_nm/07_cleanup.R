##==============================================================================
## Project: QuEST
## TO DO: Adapt this script for FOR-NM 

## Script 07b: Clean up predicted discharge files for NM -- keep a minimal
## set of named columns (no positional drops), split into water years, and
## apply a 1-hour rolling smooth to the predicted discharge.
##
## Covers all 13 sites (lower, upper, and middle).
##
## INPUT:  "predicted" folder (output of 07)
## OUTPUT: "smooth" folder + water-year folder -> used by 08_plotting_air.R /
##         09_plotting_together.R
##==============================================================================

##################
#### Packages ####
##################
library(googledrive)
library(ggplot2)
library(dplyr)
library(lubridate)
library(xts) # for time series
library(zoo) # for rollmean function

####################################
## Clear folders that we will use ##
####################################
file.remove(list.files(path = "googledrive", full.names = TRUE))
file.remove(list.files(path = "pt_figs", full.names = TRUE))
file.remove(list.files(path = "data", full.names = TRUE))

#####################
#### Import data ####
#####################
# this is the "predicted" folder (output of 07)
pt      <- googledrive::as_id("https://drive.google.com/drive/folders/1fPDNinUQ3pCFFQXJ1dtLGbqEawyTmPUx")
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")

pt_list <- list()
for (i in seq_along(pt_csvs$id)) {
  local_path <- file.path("googledrive", pt_csvs$name[i])
  googledrive::drive_download(file = pt_csvs$id[i], path = local_path, overwrite = TRUE)
  pt_list[[pt_csvs$name[i]]] <- read.csv(local_path)
}

############################
#### Format date column ####
############################
for (i in seq_along(pt_list)) {
  df <- pt_list[[i]]
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
  pt_list[[i]] <- df
}

#################################################
#### Keep only the columns this pipeline uses ###
#################################################
keep_cols <- c("DateTime", "Final_Corrected_Lvl", "Q_m3s", "Q_m3s_pred",
               "observation", "dry", "dry_score", "flow_confidence",
               "temp_sd_PT", "temp_sd_air", "pearson_r_temp")

sites <- c("USF03", "USF04", "USF05", "USF07", "USF20",
           "USF13", "USF14", "USF16", "USF19", "USF21",
           "USF09", "USF10", "USF11")

pt_list <- lapply(sites, function(site) {
  df <- pt_list[[paste0("discharge_", site, ".csv")]]
  df[, intersect(keep_cols, names(df))]
})
names(pt_list) <- sites

######################################
#### Change discharge column name ####
######################################
pt_list <- lapply(pt_list, function(df) {
  df %>%
    dplyr::rename(Discharge_m3s = Q_m3s_pred) %>%
    mutate(Discharge_m3s = as.numeric(Discharge_m3s))
})

#####################################
#### Split data into water years ####
#####################################
filter_wy24 <- function(df) df %>% filter(DateTime >= "2023-10-01" & DateTime <= "2024-09-30")
filter_wy25 <- function(df) df %>% filter(DateTime >= "2024-10-01" & DateTime <= "2025-09-30")

pt_list_wy24 <- lapply(pt_list, filter_wy24)
pt_list_wy25 <- lapply(pt_list, filter_wy25)

#####################
#### Plot curves ####
#####################
for (i in seq_along(pt_list)) {
  df <- pt_list[[i]]
  p  <- ggplot(data = df, aes(x = DateTime, y = Discharge_m3s)) +
    geom_line() + ggtitle(names(pt_list)[i])
  ggsave(paste0("pt_figs/", names(pt_list)[i], ".png"), plot = p)
  print(p)
}

###########################################
#### one hour rolling window smoothing ####
###########################################
pt_list <- lapply(pt_list, function(df) df %>% filter(!is.na(DateTime)))

xts_list <- lapply(pt_list, function(df) xts(df$Discharge_m3s, order.by = df$DateTime))

# k = 4 for a 1-hour window with 15-minute data; align = "right" aligns the
# result with the end of the window; na.pad keeps the original length
smoothed_xts_list <- lapply(xts_list, function(x) rollmean(x, k = 4, align = "right", na.pad = TRUE))

smooth_df <- lapply(names(pt_list), function(site) {
  df <- pt_list[[site]]
  df$Smooth_Discharge_m3s <- as.numeric(smoothed_xts_list[[site]])
  df
})
names(smooth_df) <- names(pt_list)

############################
#### Plot smooth curves ####
############################
for (i in seq_along(smooth_df)) {
  df <- smooth_df[[i]]
  p  <- ggplot(data = df, aes(x = DateTime, y = Smooth_Discharge_m3s)) +
    geom_line() + ggtitle(names(smooth_df)[i])
  print(p)
}

#######################################
#### Save merged PT files to Drive ####
#######################################
for (i in seq_along(smooth_df)) {
  df   <- smooth_df[[i]]
  file <- paste0("data/", names(smooth_df)[i], ".csv")
  write.csv(df, file, row.names = FALSE, quote = FALSE)
  
  # this is the "smooth" folder
  drive_folder_id <- "1y2bMWCS48cROq_BO5HkaNWmFIxdJUON0"
  drive_put(media = file, path = as_id(drive_folder_id))
}

for (i in seq_along(pt_list_wy25)) {
  df   <- pt_list_wy25[[i]]
  file <- paste0("data/", names(pt_list_wy25)[i], "_wy25.csv")
  write.csv(df, file, row.names = FALSE, quote = FALSE)
  
  # this is the water-year folder
  drive_folder_id <- "1wX6RxDAORaBNrqskCKhHy0ajzWJmOT3s"
  drive_put(media = file, path = as_id(drive_folder_id))
}