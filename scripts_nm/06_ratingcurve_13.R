##==============================================================================
## Project: QuEST
## TO DO: Adapt this script for FOR-NM 

## Script 06: PT offset corrections for NM USF13 (upper, AIR2 baro source)
##
## Sensor repositioning events shift the whole record by a constant jump.
## For each known move: take the mean level before and after, subtract the
## jump from everything after the move. Corrections are applied sequentially
## into intermediate columns, then collapsed into one Final_Corrected_Lvl
## column -- the intermediates are dropped before saving.
## No offset corrections were confirmed for this site in the original calibration notes.
##
## INPUT:  "depth" folder (output of 05)
## OUTPUT: "offset" folder -> used by 07_ratingcurve_USF13.R
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
site <- "USF13"

# this is the "depth" folder (output of 05)
pt      <- googledrive::as_id("https://drive.google.com/drive/folders/1EswIfUWCK6bsdcs-ZrAMGW1oYKs4B0Eh")
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")

googledrive::drive_download(file = pt_csvs$id[pt_csvs$name == paste0(site, ".csv")],
                            path = file.path("googledrive", paste0(site, ".csv")),
                            overwrite = TRUE)
USF13 <- read.csv(file.path("googledrive", paste0(site, ".csv")))

# DateTime at midnight is missing 00:00:00 time in this file, so fill it in
USF13$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$", USF13$DateTime)] <- paste(
  USF13$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$", USF13$DateTime)], "00:00:00")
USF13$DateTime <- as.POSIXct(USF13$DateTime, format = "%Y-%m-%d %H:%M:%S")

########################################
#### Plot pressure compensated data ####
########################################
ggplot(USF13, aes(x = DateTime, y = Baro_Cor_Lvl.m)) +
  geom_line() +
  labs(title = paste(site, "compensated level, pre-correction"), x = "Date", y = "Water Level (m)")

###################################################################
#### Remove times where PT was out of the water / clear errors ####
###################################################################
out_of_water_times <- as.POSIXct(c("2025-06-05 11:15:00", "2024-10-29 12:30:00"))

USF13 <- USF13 %>%
  mutate(Baro_Cor_Lvl.m = ifelse(DateTime %in% out_of_water_times, NA, Baro_Cor_Lvl.m))

#####################################################
#### Sequential sensor-repositioning corrections ####
#####################################################
# no sensor-repositioning events identified for this site -- no offset
# corrections applied; Final_Corrected_Lvl is Baro_Cor_Lvl.m directly

USF13$Baro_Cor_offset0 <- USF13$Baro_Cor_Lvl.m

#############################################
#### Final zero-datum shift and cleanup ####
#############################################
USF13 <- USF13 %>%
  mutate(Final_Corrected_Lvl = Baro_Cor_offset0) %>%
  select(-starts_with("Baro_Cor_offset"))

##############################
#### Plot with correction ####
##############################
ggplot(USF13, aes(x = DateTime, y = Final_Corrected_Lvl)) +
  geom_line() +
  labs(title = paste(site, "- Final_Corrected_Lvl"), x = "Date", y = "Water Level (m)")
ggsave(filename = paste0("figures/", site, "_final_corrected.png"),
       plot = last_plot(), width = 8, height = 6, dpi = 300)

###################################################
#### Plot Stage vs. Discharge after correction ####
###################################################
rating_data <- USF13 %>%
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

if (site_goes_dry) {
  # ... dry threshold and scoring code ...
} else {
  USF13 <- USF13 %>%
    mutate(dry = "no", flow_confidence = "no dry classification")
}

###################
#### Save file ####
###################
write.csv(USF13, paste0("data/offset_", site, ".csv"), row.names = FALSE, quote = FALSE)

# this is the "offset" folder
drive_folder_id <- "1VIonkS5GXUsn34FEPu1lpkMgsgCvPXFw"
drive_put(media = paste0("data/offset_", site, ".csv"), path = as_id(drive_folder_id))