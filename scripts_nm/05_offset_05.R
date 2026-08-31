##==============================================================================
## Project: QuEST
## TO DO: Adapt this script for FOR-NM 

## Script 07: Rating curve and discharge prediction for NM USF05 (lower, AIR1 baro source)
##
## Fits Q = a * Final_Corrected_Lvl^b via a log-log linear model on the
## offset-corrected level record and applies it to the full timeseries.
##
## INPUT:  "offset" folder (output of 06)
## OUTPUT: "predicted" folder -> used by 07b_cleanup_NM.R
##==============================================================================

##################
#### Packages ####
##################
library(googledrive)
library(ggplot2)
library(lubridate)
library(dplyr)
library(ggpmisc)

####################################
## Clear folders that we will use ##
####################################
file.remove(list.files(path = "googledrive", full.names = TRUE))
file.remove(list.files(path = "data", full.names = TRUE))

#####################
#### Import data ####
#####################
site <- "USF05"

# this is the "offset" folder (output of 06)
pt      <- googledrive::as_id("https://drive.google.com/drive/folders/1VIonkS5GXUsn34FEPu1lpkMgsgCvPXFw")
pt_csvs <- googledrive::drive_ls(path = pt, type = "csv")

googledrive::drive_download(file = pt_csvs$id[pt_csvs$name == paste0("offset_", site, ".csv")],
                            path = file.path("googledrive", paste0("offset_", site, ".csv")),
                            overwrite = TRUE)
USF05 <- read.csv(file.path("googledrive", paste0("offset_", site, ".csv")))
USF05$DateTime <- as.POSIXct(USF05$DateTime, format = "%Y-%m-%d %H:%M:%S")

#######################################
#### Filter to calibration points ####
#######################################
rating_data <- USF05 %>%
  filter(!is.na(Final_Corrected_Lvl), Final_Corrected_Lvl > 0, !is.na(Q_m3s), Q_m3s > 0)

cat("Rating curve calibration points:", nrow(rating_data), "
")

###########################
#### Fit rating curve ####
###########################
log_model <- lm(log(Q_m3s) ~ log(Final_Corrected_Lvl), data = rating_data)
summary(log_model)

a <- exp(coef(log_model)[1])
b <- coef(log_model)[2]
cat(sprintf("Rating curve: Q_m3s = %.6f * Final_Corrected_Lvl ^ %.4f
", a, b))

############################################
#### Apply rating curve to full record ####
############################################
USF05 <- USF05 %>%
  mutate(Q_m3s_pred = ifelse(Final_Corrected_Lvl > 0, a * (Final_Corrected_Lvl ^ b), NA))

########################################
#### Diagnostic plot: equation + R2 ####
########################################
p_rating_real <- ggplot(rating_data, aes(x = Final_Corrected_Lvl, y = Q_m3s)) +
  geom_point() +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*"))) +
  labs(title = paste(site, "rating curve -- real space (reference only)"),
       subtitle = "Line/R2 here are a plain linear fit on raw units, NOT the power-law model -- see log-log plot for the real fit",
       x = "Final_Corrected_Lvl (m)", y = "Observed Q (m3/s)") +
  theme_classic(base_size = 15)
print(p_rating_real)
ggsave(paste0("figures/", site, "_ratingcurve_realspace.png"), plot = p_rating_real, width = 8, height = 6, dpi = 300)

#################################################################
#### Diagnostic plot 2: log-log space -- this IS the model fit ####
#################################################################
# log_model was fit as log(Q_m3s) ~ log(Final_Corrected_Lvl), so plotting the
# same log-transformed axes and overlaying that model's own coefficients
# (geom_abline, not a re-fit stat_poly_line) guarantees the line and R2 shown
# here are exactly the model actually used downstream -- a straight line here
# is what "the power-law assumption holds" looks like.
r2_log_val <- summary(log_model)$r.squared
p_rating_log <- ggplot(rating_data, aes(x = log(Final_Corrected_Lvl), y = log(Q_m3s))) +
  geom_point() +
  geom_abline(intercept = coef(log_model)[1], slope = coef(log_model)[2],
              color = "steelblue", linewidth = 1) +
  annotate("text", x = -Inf, y = Inf, hjust = -0.05, vjust = 1.3, size = 4,
           label = sprintf("log(Q) = %.4f + %.4f*log(Level)\nR2 (log-log) = %.3f",
                           coef(log_model)[1], coef(log_model)[2], r2_log_val)) +
  labs(title = paste(site, "rating curve -- log-log space"),
       x = "log( Final_Corrected_Lvl )", y = "log( Observed Q )") +
  theme_classic(base_size = 15)
print(p_rating_log)
ggsave(paste0("figures/", site, "_ratingcurve_loglog.png"), plot = p_rating_log, width = 8, height = 6, dpi = 300)

#############################################
#### Time series: predicted vs. observed ####
#############################################
p_ts <- ggplot() +
  geom_line(data = USF05, aes(x = DateTime, y = Q_m3s_pred), linewidth = 1.0, color = "black") +
  geom_point(data = rating_data, aes(x = DateTime, y = Q_m3s), color = "blue", size = 2) +
  labs(x = "", y = "Q (m3/s)", title = paste(site, "predicted discharge")) +
  theme_classic(base_size = 15)
print(p_ts)
ggsave(paste0("figures/", site, "_predicted_Q.png"), plot = p_ts, width = 8, height = 4, dpi = 300)

##################################################################
#### Rating curve diagnostics: extrapolation flag + real R2 ####
##################################################################
source("00_rating_diagnostics_helpers.R")
diag  <- add_rating_diagnostics(USF05, rating_data, log_model)
USF05 <- diag$df

###################
#### Save file ####
###################
write.csv(USF05, paste0("data/discharge_", site, ".csv"), row.names = FALSE, quote = FALSE)

# this is the "predicted" folder
drive_folder_id <- "1fPDNinUQ3pCFFQXJ1dtLGbqEawyTmPUx"
drive_put(media = paste0("data/discharge_", site, ".csv"), path = as_id(drive_folder_id))