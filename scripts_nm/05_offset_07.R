##==============================================================================
## Project: QuEST
## TO DO: Adapt this script for FOR-NM 

## Script 06: PT offset corrections for NM USF07 (lower, AIR1 baro source)
##
## Sensor repositioning events shift the whole record by a constant jump.
## For each known move: take the mean level before and after, subtract the
## jump from everything after the move. Corrections are applied sequentially
## into intermediate columns, then collapsed into one Final_Corrected_Lvl
## column -- the intermediates are dropped before saving.
## Two additional NA date ranges are applied before the offset chain: 2025-01-07 to 2025-02-05 (error section) and 2024-07-08 15:00-19:00 America/Denver (error section).
##
## INPUT:  "depth" folder (output of 05)
## OUTPUT: "offset" folder -> used by 07_ratingcurve_USF07.R
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
file.remove(list.files(path = "googledrive", full.names = TRUE))
file.remove(list.files(path = "data", full.names = TRUE))

#####################
#### Import data ####
#####################
site <- "USF07"

# this is the "depth" folder (output of 05)
pt      <- googledrive::as_id("https://drive.google.com/drive/folders/1EswIfUWCK6bsdcs-ZrAMGW1oYKs4B0Eh")
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")

googledrive::drive_download(file = pt_csvs$id[pt_csvs$name == paste0(site, ".csv")],
                            path = file.path("googledrive", paste0(site, ".csv")),
                            overwrite = TRUE)
USF07 <- read.csv(file.path("googledrive", paste0(site, ".csv")))

# DateTime at midnight is missing 00:00:00 time in this file, so fill it in
USF07$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$", USF07$DateTime)] <- paste(
  USF07$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$", USF07$DateTime)], "00:00:00")
USF07$DateTime <- as.POSIXct(USF07$DateTime, format = "%Y-%m-%d %H:%M:%S")

########################################
#### Plot pressure compensated data ####
########################################
ggplot(USF07, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct(c("2024-10-24 11:00:00", "2024-05-23 15:15:00", "2024-07-08 14:45:00", "2025-09-12 11:30:00", "2025-02-05 00:00:00")),
             linetype = "dashed", color = "red") +
  labs(title = paste(site, "compensated level, pre-correction"), x = "Date", y = "Water Level (m)")

###################################################################
#### Remove times where PT was out of the water / clear errors ####
###################################################################
out_of_water_times <- as.POSIXct(c("2024-10-24 12:15:00", "2024-10-24 14:15:00", "2024-10-24 14:30:00", "2024-10-24 14:00:00", "2025-06-16 12:19:00", "2024-10-16 11:00:00", "2024-07-30 12:45:00"))

USF07 <- USF07 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime %in% out_of_water_times, NA, Baro_Cor_Lvl.m))

# Remove period where PT froze
frozen_start <- as.POSIXct("2025-01-03 00:00:00") 
frozen_end   <- as.POSIXct("2025-02-05 00:00:00")

USF07 <- USF07 %>%
  mutate(
    Baro_Cor_Lvl.m      = if_else(DateTime >= frozen_start & DateTime <= frozen_end,
                                  NA_real_, Baro_Cor_Lvl.m)
  )

# flag column to flag why this chunk is missing
USF07 <- USF07 %>%
  mutate(flag = if_else(DateTime >= frozen_start & DateTime <= frozen_end,
                        "frozen", flag))

#####################################################
#### Sequential sensor-repositioning corrections ####
#####################################################
apply_offset <- function(df, level_col, before_time, after_time = before_time,
                         window_hours = 2, before_window_hours = window_hours,
                         after_window_hours = window_hours) {
  before <- df %>% filter(DateTime >= (before_time - hours(before_window_hours)), DateTime < before_time) %>%
    summarize(m = mean(.data[[level_col]], na.rm = TRUE)) %>% pull(m)
  after  <- df %>% filter(DateTime >= after_time, DateTime < (after_time + hours(after_window_hours))) %>%
    summarize(m = mean(.data[[level_col]], na.rm = TRUE)) %>% pull(m)
  offset <- after - before
  cat(sprintf("  before = %s | after = %s | offset = %.4f m\n", before_time, after_time, offset))
  offset
}

USF07$Baro_Cor_offset0 <- USF07$Baro_Cor_Lvl.m

move_time1 <- as.POSIXct("2024-05-23 15:15:00")
offset1 <- apply_offset(USF07, "Baro_Cor_offset0", move_time1)
USF07 <- USF07 %>%
  mutate(Baro_Cor_offset1 = if_else(DateTime >= move_time1, Baro_Cor_offset0 - offset1, Baro_Cor_offset0))

move_time2 <- as.POSIXct("2025-09-12 11:30:00")
offset2 <- apply_offset(USF07, "Baro_Cor_offset1", move_time2)
USF07 <- USF07 %>%
  mutate(Baro_Cor_offset2 = if_else(DateTime >= move_time2, Baro_Cor_offset1 - offset2, Baro_Cor_offset1))

move_time3 <- as.POSIXct("2025-10-17 13:15:00")
offset3 <- apply_offset(USF07, "Baro_Cor_offset2", move_time3)
USF07 <- USF07 %>%
  mutate(Baro_Cor_offset3 = if_else(DateTime >= move_time3, Baro_Cor_offset2 - offset3, Baro_Cor_offset2))

# before_time4 <- as.POSIXct("2025-01-03 00:15:00")
# after_time4  <- as.POSIXct("2025-02-05 00:00:00")
# offset4 <- apply_offset(USF07, "Baro_Cor_offset3", before_time4, after_time4)
# USF07 <- USF07 %>%
#   mutate(Baro_Cor_offset4 = if_else(DateTime >= after_time4, Baro_Cor_offset3 - offset4, Baro_Cor_offset3))

#############################################
#### Final zero-datum shift and cleanup ####
#############################################
USF07 <- USF07 %>%
  mutate(Final_Corrected_Lvl = Baro_Cor_offset3 + 0.045) %>%
  select(-starts_with("Baro_Cor_offset"))

##############################
#### Plot with correction ####
##############################
ggplot(USF07, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() +
  labs(title = paste(site, "- Final_Corrected_Lvl"), x = "Date", y = "Water Level (m)")
ggplot(USF07, aes(x = DateTime, y = Final_Corrected_Lvl)) +
  geom_line() +
  labs(title = paste(site, "- Final_Corrected_Lvl"), x = "Date", y = "Water Level (m)")
ggsave(filename = paste0("figures/", site, "_final_corrected.png"),
       plot = last_plot(), width = 8, height = 6, dpi = 300)

###################################################
#### Plot Stage vs. Discharge after correction ####
###################################################
rating_data <- USF07 %>%
  filter(!is.na(Final_Corrected_Lvl), !is.na(Q_m3s)) %>%
  mutate(Month = month(DateTime),
         Season = case_when(
           Month %in% c(12, 1, 2) ~ "Winter",
           Month %in% c(3, 4, 5)  ~ "Spring",
           Month %in% c(6, 7, 8)  ~ "Summer",
           Month %in% c(9, 10, 11) ~ "Fall"
         ))

p <- ggplot(rating_data, aes(x = Final_Corrected_Lvl, y = Q_m3s, color = Season)) +
  geom_point(size = 3) +
  geom_text(aes(label = as.Date(DateTime)), vjust = -0.5, size = 3, show.legend = FALSE) +
  labs(title = paste(site, "- Stage vs. Discharge by season"),
       x = "Final_Corrected_Lvl (m)", y = "Discharge (m3/s)", color = "Season") +
  theme_minimal()
print(p)
ggsave(filename = paste0("figures/", site, "_season.png"), plot = p, width = 8, height = 6, dpi = 300)

#####################################
#### Dry/flowing classification ####
#####################################
# set to TRUE for sites that go dry; FALSE for ones that don't go dry 
site_goes_dry <- FALSE

# dates where PT was frozen
frozen_start <- as.POSIXct("2025-01-01 00:00:00")  # set to NA if no frozen period
frozen_end   <- as.POSIXct("2025-02-01 00:00:00")

if (site_goes_dry) {
  # ... dry threshold and scoring code ...
} else {
  USF07 <- USF07 %>%
    mutate(
      dry = "no",
      flow_confidence = case_when(
        !is.na(frozen_start) & DateTime >= frozen_start & DateTime <= frozen_end ~ "frozen PT — excluded",
        TRUE ~ "no dry classification"
      )
    )
}

###################
#### Save file ####
###################
write.csv(USF07, paste0("data/offset_", site, ".csv"), row.names = FALSE, quote = FALSE)

# this is the "offset" folder
drive_folder_id <- "1VIonkS5GXUsn34FEPu1lpkMgsgCvPXFw"
drive_put(media = paste0("data/offset_", site, ".csv"), path = as_id(drive_folder_id))