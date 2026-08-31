##==============================================================================
## Project: QuEST
## TO DO: Adapt this script for FOR-NM 

## Script 10: Compare USF12 predicted discharge (from rating curve) against
## the co-located USGS gauge (site 08315480).
##
## INPUT:  data/discharge_USF12.csv (output of 07_ratingcurve_USF12.R)
##         USGS NWIS instantaneous discharge (dataRetrieval package)
## OUTPUT: figures/USF12_vs_USGS_timeseries.png
##         figures/USF12_vs_USGS_scatter.png
##         comparison stats printed to console (NSE, RMSE, PBIAS, R2)
##==============================================================================

##################
#### Packages ####
##################
library(dataRetrieval)
library(ggplot2)
library(lubridate)
library(dplyr)

##################################
#### Pull USGS discharge data ####
##################################
siteNo   <- "08315480"    # USF12 USGS gauge
pCode    <- "00060"       # discharge code

# Set these to the window you want to compare. Left as the full range you
# currently have USF12 predictions for -- change if you want a narrower window.
start.date <- "2025-07-16"
end.date   <- "2026-05-07"

USGS <- readNWISuv(siteNumbers = siteNo,
                   parameterCd = pCode,
                   startDate = start.date,
                   endDate = end.date)
USGS <- renameNWISColumns(USGS)
# parameter_units: ft3/s
USGS$DateTime <- USGS$dateTime

### convert ft3/s to m3/s ###
USGS <- USGS %>%
  mutate(Flow_Inst.m = Flow_Inst * 0.02832)

# quick look
ggplot(data = USGS, aes(dateTime, Flow_Inst.m)) +
  geom_line() +
  labs(title = "USGS 08315480 discharge", x = "", y = "Discharge (m3/s)") +
  theme_minimal()

#####################################
#### Load USF12 predicted Q ########
#####################################
USF12 <- read.csv("data/discharge_USF12.csv")
USF12$DateTime <- as.POSIXct(USF12$DateTime, format = "%Y-%m-%d %H:%M:%S")

##############################################
#### Merge USF12 predictions with USGS ######
##############################################
# USGS is typically 15-min instantaneous, same as the PT record, but round
# both to the nearest 15 min before merging so timestamps that are off by a
# few seconds still line up.
USF12 <- USF12 %>%
  mutate(DateTime_round = round_date(DateTime, "15 minutes"))
USGS <- USGS %>%
  mutate(DateTime_round = round_date(DateTime, "15 minutes"))

merged_df <- merge(USGS, USF12, by.x = "DateTime_round", by.y = "DateTime_round", all.x = FALSE, all.y = FALSE) %>%
  select(DateTime = DateTime_round, Flow_Inst.m, Q_m3s_pred, Q_m3s_pred_raw, Q_m3s, Final_Corrected_Lvl, dry, flow_confidence) %>%
  filter(!is.na(Flow_Inst.m), !is.na(Q_m3s_pred))

cat("Merged/overlapping records:", nrow(merged_df), "\n")

############################################
#### Plot: USF12 predicted vs. USGS gauge ####
############################################
ggplot(data = merged_df, aes(x = DateTime)) +
  geom_line(aes(y = Flow_Inst.m, color = "USGS 08315480")) +
  geom_line(aes(y = Q_m3s_pred, color = "USF12 (predicted)")) +
  geom_point(data = merged_df %>% filter(!is.na(Q_m3s)),
             aes(y = Q_m3s, color = "USF12 (field measured)"), size = 2) +
  labs(x = "", y = "Discharge (m3/s)", color = "",
       title = "USF12 predicted discharge vs. co-located USGS gauge") +
  theme_minimal()
ggsave("figures/USF12_vs_USGS_timeseries.png", width = 10, height = 5, dpi = 300)

######################################
#### Scatter: 1:1 agreement check ####
######################################
lims <- range(c(merged_df$Flow_Inst.m, merged_df$Q_m3s_pred), na.rm = TRUE)
ggplot(merged_df, aes(x = Flow_Inst.m, y = Q_m3s_pred)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  coord_equal(xlim = lims, ylim = lims) +
  labs(x = "USGS 08315480 (m3/s)", y = "USF12 predicted (m3/s)",
       title = "USF12 vs. USGS -- 1:1 line in red") +
  theme_minimal()
ggsave("figures/USF12_vs_USGS_scatter.png", width = 6, height = 6, dpi = 300)

##############################
#### Agreement statistics ####
##############################
obs  <- merged_df$Flow_Inst.m   # "truth" = USGS gauge
pred <- merged_df$Q_m3s_pred    # USF12 rating-curve prediction

nse   <- 1 - sum((obs - pred)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
rmse  <- sqrt(mean((obs - pred)^2, na.rm = TRUE))
mae   <- mean(abs(obs - pred), na.rm = TRUE)
pbias <- 100 * sum(pred - obs, na.rm = TRUE) / sum(obs, na.rm = TRUE)
r2    <- cor(obs, pred, use = "complete.obs")^2

cat(sprintf("
--- USF12 vs. USGS 08315480 agreement (%s to %s) ---
  n           = %d
  R2          = %.3f
  NSE         = %.3f   (1 = perfect, <0 = worse than using the mean)
  RMSE        = %.5f m3/s
  MAE         = %.5f m3/s
  PBIAS       = %.1f%%   (positive = USF12 over-predicts vs. USGS)
",
            start.date, end.date, nrow(merged_df), r2, nse, rmse, mae, pbias))

write.csv(merged_df, "data/USF12_vs_USGS_merged.csv", row.names = FALSE, quote = FALSE)