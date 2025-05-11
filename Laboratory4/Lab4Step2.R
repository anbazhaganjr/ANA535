##############################################################
# ANA 535 – Laboratory #4 Step 2
# ------------------------------------------------------------
# Original Script Author: Dr. Marvine Hamner (April 2025)
#
# Modifications by: Naresh Anbazhagan, Graduate Student – ANA 535
# - Modularized the forecasting workflow with helper functions
# - Enhanced plot exports with `save_plot()` utility
# - Streamlined logging and diagnostics for reproducibility
# - Adapted the code for macOS compatibility and automation
# - Ensured consistent file output for APA-style report inclusion
#
# Environment: R 4.5.0 on macOS, RStudio
##############################################################

# ======================
# Required Packages
# ======================

reqd_pkgs <- c(
  "fpp3", "dplyr", "tidyverse", "ggplot2", "tsibble", "tsibbledata",
  "fable", "feasts", "lubridate", "zoo", "forecast", "TSA", "tseries", "ggpubr",
  "reshape2"
)

installed <- rownames(installed.packages())
to_install <- setdiff(reqd_pkgs, installed)
if (length(to_install) > 0) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
invisible(lapply(reqd_pkgs, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))
cat("All libraries successfully loaded for Lab 4.\n\n")

# ======================
# Working Directory Setup
# ======================

setwd("/Users/anbzhnjr/learning/DataAnalytics/rprojects/ANA535/Lab4")
dir.create("data", recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)
dir.create("output/logs/step2", recursive = TRUE, showWarnings = FALSE)
dir.create("output/plots/step2", recursive = TRUE, showWarnings = FALSE)

# ======================
# Helper Functions
# ======================

save_plot <- function(filename, expr, width = 800, height = 600, res = 120) {
  png(file = paste0("output/plots/step2/", filename), width = width, height = height, res = res)
  print(expr)
  dev.off()
  invisible(NULL)
}

start_log <- function(filename = "output/logs/step2/ana535_lab4_step2_output_log.txt") {
  while (sink.number() > 0) sink(NULL)
  sink(file = filename, append = FALSE, split = TRUE)
  start_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  cat("\n===== ANA535 Lab 4 Execution Started: ", start_time, " =====\n\n")
}

end_log <- function() {
  end_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  cat("\n===== ANA535 Lab 4 Execution Ended: ", end_time, " =====\n\n")
  sink()
}

log_section <- function(title) {
  cat("\n\n===== ", title, " =====\n\n")
}

start_log("output/logs/step2/ana535_lab4_step2_output_log.txt")

#
# ***************************** Section 1 ************************************
#

log_section("Step2: Load Amtrak Data and Initial ACF")

# Read the CSV
amtrak_path <- "data/Amtrak1991-2024.csv"
Amtrak <- read.csv(amtrak_path)
colnames(Amtrak) <- c("Month", "Ridership", "PassengerMiles", "RidersReported")

# Format the Month column
Amtrak$Month <- mdy(Amtrak$Month)

# Scale PassengerMiles to millions
Amtrak$PassengerMiles <- Amtrak$PassengerMiles / 1e6

# Create ts and tsibble for 1991–2016
Amtrak.ts.91.16 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2017, 1), frequency = 12)
Amtrak.tsb.91.16 <- as_tsibble(Amtrak.ts.91.16)

# ACF plot of the original data
acf_initial <- Amtrak.tsb.91.16 |>
  ACF() |>
  autoplot() +
  labs(title = "ACF of Amtrak Passenger Miles (1991–2016)")
save_plot("step2_acf_initial.png", acf_initial)

#
# ***************************** Section 2 ************************************
#
log_section("Step 2: Decompose and Detrend Amtrak Data")

# Re-create the ts object explicitly in case prior steps changed it
Amtrak.ts.91.16 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2017, 1), frequency = 12)

# Decompose time series
Amtrak.comp.91.16 <- decompose(Amtrak.ts.91.16)

# Save decomposition plot
p_decomp <- autoplot(Amtrak.comp.91.16)
save_plot("step2_decomp.png", p_decomp)

# Log structure
log_section("Structure of Amtrak.comp.91.16")
print(str(Amtrak.comp.91.16))

# Detrend and deseasonalize
Amtrak.ts.91.16_desea <- Amtrak.comp.91.16$x - Amtrak.comp.91.16$seasonal
Amtrak.ts.91.16_detren <- Amtrak.ts.91.16_desea - Amtrak.comp.91.16$trend

# Decompose residual time series
Amtrak.ts.91.16_de <- decompose(Amtrak.ts.91.16_detren)

# Save decomposition of residuals
p_resid_decomp <- autoplot(Amtrak.ts.91.16_de)
save_plot("step2_decomp_detrended.png", p_resid_decomp)

# Convert detrended time series to tsibble
Amtrak.tsb.91.16_de <- as_tsibble(Amtrak.ts.91.16_detren)

# ACF and PACF plots after detrending
acf_detrended <- Amtrak.tsb.91.16_de |>
  ACF() |>
  autoplot() +
  labs(title = "ACF of Detrended Amtrak Passenger Miles")
save_plot("step2_acf_detrended.png", acf_detrended)

pacf_detrended <- Amtrak.tsb.91.16_de |>
  PACF() |>
  autoplot() +
  labs(title = "PACF of Detrended Amtrak Passenger Miles")
save_plot("step2_pacf_detrended.png", pacf_detrended)

#
# ***************************** Section 3 ************************************
#

log_section("Step 2: Centered 3-Month Moving Average")

# Start from deseasonalized + detrended tsibble
Amtrak_3MA <- Amtrak.tsb.91.16_de

# Calculate 3-month centered moving average
Amtrak_3MA.19.16 <- Amtrak_3MA |>
  mutate(`3-MA` = slider::slide_dbl(value, mean, .before = 1, .after = 1, .complete = TRUE))

# Plot and save
p_centered_ma <- Amtrak_3MA.19.16 |>
  autoplot(Amtrak_3MA$value) +
  geom_line(aes(y = `3-MA`), colour = "#D55E00") +
  labs(
    y = "Passenger Miles",
    title = "Amtrak Passenger Miles by Month and Centered Moving Average"
  )

save_plot("step2_centered_3ma.png", p_centered_ma)

#
# ***************************** Section 4 ************************************
#

log_section("Step 2: 3-Month MA Zoom-In (2001–2003)")

# Check structure of detrended ts
str(Amtrak.ts.91.16_detren)

# Extract windowed series (2001 to 2004)
Amtrak_MAZoom_2001_03 <- window(
  Amtrak.ts.91.16_detren,
  start = c(2001, 1), end = c(2004, 1), frequency = 12
)
str(Amtrak_MAZoom_2001_03)

# Convert to tsibble
Amtrak.tsb_2001_03 <- as_tsibble(Amtrak_MAZoom_2001_03)

# Compute centered 3-month moving average
Amtrak_3MA_2001_03 <- Amtrak.tsb_2001_03 |>
  mutate(`3-MA` = slider::slide_dbl(value, mean, .before = 1, .after = 1, .complete = TRUE))

# Plot centered MA
p_centered_zoom <- Amtrak_3MA_2001_03 |>
  autoplot(Amtrak.tsb_2001_03$value) +
  geom_line(aes(y = `3-MA`), colour = "#D55E00") +
  labs(y = "Passenger Miles", title = "Amtrak Centered 3-MA (2001–2003)")

save_plot("step2_centered_ma_zoom.png", p_centered_zoom)

#
# ***************************** Section 5 ************************************
#
log_section("Step 2: 3-Month Trailing MA (2001–2003)")

# Compute trailing 3-month moving average
Amtrak_3MATR <- Amtrak_3MA_2001_03 |>
  mutate(`3-MA` = slider::slide_dbl(value, mean, .before = 2, .after = 0, .complete = TRUE))

# Plot trailing MA
p_trailing_ma <- Amtrak_3MATR |>
  autoplot(Amtrak.tsb_2001_03$value) +
  geom_line(aes(y = `3-MA`), colour = "#D55E00") +
  labs(y = "Passenger Miles", title = "Amtrak Trailing 3-MA (2001–2003)")

save_plot("step2_trailing_ma.png", p_trailing_ma)

#
# ***************************** Section 6 ************************************
#

log_section("Step 2: Augmented Dickey-Fuller Test (ADF)")

# Run ADF test on original Passenger Miles data
Amtrak_adf <- adf.test(Amtrak.ts.91.16)
print(Amtrak_adf)

#
# ***************************** Section 7 ************************************
#
log_section("Step 2: Shmueli Example - Moving Averages on Ridership")

# Reload dataset (simulating clean workspace)
amtrak_path <- "data/Amtrak1991-2024.csv"
Amtrak <- read.csv(amtrak_path)
colnames(Amtrak) <- c("Month", "Ridership", "PassengerMiles", "RidersReported")
Amtrak$Month <- mdy(Amtrak$Month)

# Create ts and tsibble for Ridership (1991–2016)
AmtrakRiders.ts.91.16 <- ts(Amtrak$Ridership, start = c(1991, 1), end = c(2017, 1), frequency = 12)
AmtrakRiders.tsb.91.16 <- as_tsibble(AmtrakRiders.ts.91.16)

# Compute moving averages
ma.trailing <- zoo::rollmean(AmtrakRiders.ts.91.16, k = 12, align = "right")
ma.centered <- forecast::ma(AmtrakRiders.ts.91.16, order = 12)

# Create time plot
png("output/plots/step2/step2_shmueli_ma_plot.png", width = 800, height = 600, res = 120)
plot(AmtrakRiders.ts.91.16, ylab = "Ridership", xlab = "Time", bty = "l", main = "Shmueli MA Comparison")
lines(ma.centered, lwd = 2)
lines(ma.trailing, lwd = 2)
legend("topleft", c("Ridership", "Centered Moving Average", "Trailing Moving Average"),
       lty = c(1, 1, 2), lwd = c(1, 2, 2), bty = "n")
dev.off()

log_section("Step 2: Shmueli Zoom-In MA (2001–2003)")

# Create Ridership time series for 2001–2003
AmtrakRiders.ts.01.03 <- ts(Amtrak$Ridership, start = c(2001, 1), end = c(2004, 1), frequency = 12)

# Compute 12-month MAs
ma.trailing_12 <- zoo::rollmean(AmtrakRiders.ts.01.03, k = 12, align = "right")
ma.centered_12 <- forecast::ma(AmtrakRiders.ts.01.03, order = 12)

# Plot 12-month MAs
png("output/plots/step2/step2_shmueli_ma_zoom_12.png", width = 800, height = 600, res = 120)
plot(AmtrakRiders.ts.01.03, ylab = "Ridership", xlab = "Time", bty = "l", main = "Zoom: 12-Month MAs (2001–2003)")
lines(ma.centered_12, lwd = 2)
lines(ma.trailing_12, lwd = 2, lty = 2)
legend("top", legend = c("Ridership", "Centered Moving Average", "Trailing Moving Average"), 
       lty = c(1, 1, 2), lwd = c(1, 2, 2), bty = "n")
dev.off()

# Compute 3-month MAs
ma.trailing_3 <- zoo::rollmean(AmtrakRiders.ts.01.03, k = 3, align = "right")
ma.centered_3 <- forecast::ma(AmtrakRiders.ts.01.03, order = 3)

# Plot 3-month MAs
png("output/plots/step2/step2_shmueli_ma_zoom_3.png", width = 800, height = 600, res = 120)
plot(AmtrakRiders.ts.01.03, ylab = "Ridership", xlab = "Time", bty = "l", main = "Zoom: 3-Month MAs (2001–2003)")
lines(ma.centered_3, lwd = 2)
lines(ma.trailing_3, lwd = 2, lty = 2)
legend("top", legend = c("Ridership", "Centered Moving Average", "Trailing Moving Average"), 
       lty = c(1, 1, 2), lwd = c(1, 2, 2), bty = "n")
dev.off()

log_section("Step 2: Cleanup and Reload Amtrak Dataset")

# Clear previous Amtrak objects
rm(list = ls(pattern = "Amtrak"))

# Reload CSV
Amtrak <- read.csv("data/Amtrak1991-2024.csv")
colnames(Amtrak) <- c("Month", "Ridership", "PassengerMiles", "RidersReported")
Amtrak$Month <- mdy(Amtrak$Month)

log_section("Step 2: Stationarity Checks - ADF and KPSS")

# Reload date format and scale Passenger Miles
Amtrak$Month <- mdy(Amtrak$Month)
Amtrak$PassengerMiles <- Amtrak$PassengerMiles / 1e6

# Create time series and tsibble for 1991–2016
Amtrak.ts.91.16 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2017, 1), frequency = 12)
Amtrak.tsb.91.16 <- as_tsibble(Amtrak.ts.91.16)

# Run KPSS test (trend stationarity)
Amtrak_kpss <- kpss.test(Amtrak.ts.91.16, null = "Trend")
print(Amtrak_kpss)

# Run ADF test (unit root test)
Amtrak_adf <- adf.test(Amtrak.ts.91.16)
print(Amtrak_adf)

#
# ***************************** Section 8 ************************************
#

log_section("Step 2: Unit Root Differencing Checks")

# Determine minimum number of differences needed
Amtrak_unitroot <- unitroot_ndiffs(Amtrak.ts.91.16)
print(Amtrak_unitroot)

# Seasonal differencing estimation via features() — might be a logic issue, see note below
Amtrak_seasdiff <- Amtrak.tsb.91.16 |>
  mutate(seasonal_diff = difference(value, lag = 12)) |>
  features(seasonal_diff, unitroot_ndiffs)
print(Amtrak_seasdiff)

# Visual decomposition of the original series
p_decomp_b <- autoplot(decompose(Amtrak.ts.91.16))
save_plot("step2_decomp_repeat.png", p_decomp_b)

# Apply 1 seasonal difference (lag 12)
Amtrak_seasdiff_1 <- diff(Amtrak.ts.91.16, lag = 12)
p_seasdiff1_decomp <- autoplot(decompose(Amtrak_seasdiff_1))
save_plot("step2_decomp_seasdiff1.png", p_seasdiff1_decomp)

# Check number of differences needed after seasonal difference
Amtrak_seasdiff_ndiffs_1 <- unitroot_ndiffs(Amtrak_seasdiff_1)
print(Amtrak_seasdiff_ndiffs_1)

# Run KPSS and ADF tests again
Amtrak_kpss <- kpss.test(Amtrak_seasdiff_1, null = "Trend")
print(Amtrak_kpss)

Amtrak_adf <- adf.test(Amtrak_seasdiff_1)
print(Amtrak_adf)

#
# ***************************** Section 9 ************************************
#

log_section("Step 2: Second Differencing")

# Apply 2nd difference to remove remaining trend
Amtrak_seasdiff_2 <- diff(Amtrak_seasdiff_1, lag = 1)
p_seasdiff2_decomp <- autoplot(decompose(Amtrak_seasdiff_2))
save_plot("step2_decomp_seasdiff2.png", p_seasdiff2_decomp)

# Check number of differences again
Amtrak_seasdiff_ndiffs_2 <- unitroot_ndiffs(Amtrak_seasdiff_2)
print(Amtrak_seasdiff_ndiffs_2)

# Run KPSS and ADF again for final differenced data
Amtrak_kpss <- kpss.test(Amtrak_seasdiff_2, null = "Trend")
print(Amtrak_kpss)

Amtrak_adf <- adf.test(Amtrak_seasdiff_2)
print(Amtrak_adf)

#
# ***************************** Section 10************************************
#

log_section("Step 2: ARIMA Modeling on Twice-Differenced Data")

# Convert twice-differenced data into ts and tsibble
Amtrak_2.ts <- ts(Amtrak_seasdiff_2)
Amtrak_2.tsb <- as_tsibble(Amtrak_seasdiff_2)

# Time plot of stationary series
p_diff2_plot <- Amtrak_2.tsb |>
  autoplot() +
  labs(title = "Amtrak Twice-Differenced Data", y = "Passenger Miles")

save_plot("step2_amtrak_diff2_ts_plot.png", p_diff2_plot)

# Fit ARIMA model using forecast::auto.arima()
library(forecast)
model1 <- auto.arima(Amtrak_2.ts)
cat("\n--- forecast::auto.arima() Summary ---\n")
print(summary(model1))

# Display partial ACF of differenced data
tryCatch({
  p_diff2_pacf <- Amtrak_2.tsb |>
    gg_tsdisplay(value, plot_type = 'partial') +
    labs(title = "Partial ACF of Twice-Differenced Amtrak Data")
  save_plot("step2_diff2_pacf.png", p_diff2_pacf)
}, error = function(e) {
  cat("step2_diff2_pacf plot failed:", e$message, "\n")
})


# Fit multiple models using fable
Amtrak_2_fit <- Amtrak_2.tsb |>
  model(
    arima504 = ARIMA(value ~ pdq(5,0,4)),
    arima101 = ARIMA(value ~ pdq(1,0,1)),
    stepwise = ARIMA(value),
    search = ARIMA(value, stepwise = FALSE, approximation = FALSE)
  )

cat("\n--- fable::ARIMA() Model Report ---\n")
print(report(Amtrak_2_fit))

#
# ***************************** Section 11 ************************************
#

log_section("Step 2: Forecast with forecast::auto.arima Model")

# Forecast using forecast::auto.arima model on twice-differenced data
f <- forecast(model1, level = c(95), h = 5 * 12)
save_plot("step2_forecast_model1.png", {
  plot(f, main = "5-Year Forecast: Twice-Differenced ARIMA Model (forecast pkg)", ylab = "Passenger Miles")
})

#
# ***************************** Section 12 ************************************
#

log_section("Step 2: Forecast with forecast::auto.arima on Original Series")

# Re-create ts objects from original data
Amtrak.ts <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2017, 1), frequency = 12)
AmtrakR.ts <- ts(Amtrak$Ridership, start = c(1991, 1), end = c(2017, 1), frequency = 12)

# Fit auto.arima on both PassengerMiles and Ridership
model2 <- auto.arima(Amtrak.ts)
model3 <- auto.arima(AmtrakR.ts)

cat("\n--- Forecast::ARIMA for Passenger Miles ---\n")
print(summary(model2))

cat("\n--- Forecast::ARIMA for Ridership ---\n")
print(summary(model3))

# Forecast 5 years using model2
f2 <- forecast(model2, level = c(95), h = 5 * 12)
save_plot("step2_forecast_model2.png", {
  plot(f2, main = "5-Year Forecast: ARIMA on Original Passenger Miles", ylab = "Passenger Miles")
})

# Forecast 5 years using model3
f3 <- forecast(model3, level = c(95), h = 5 * 12)
save_plot("step2_forecast_model3.png", {
  plot(f3, main = "5-Year Forecast: ARIMA on Original Ridership", ylab = "Ridership")
})

# Forecast 5 years using model4: ARIMA(3,1,0)
model4 <- Arima(Amtrak.ts, order = c(3, 1, 0))
f4 <- forecast(model4, level = c(95), h = 5 * 12)
save_plot("step2_forecast_model4.png", {
  plot(f4, main = "5-Year Forecast: ARIMA(3,1,0)", ylab = "Passenger Miles")
})

#
# ***************************** Section 13 ************************************
#

log_section("Step 2: Forecast with fable::ARIMA on Original Series")

# Create tsibble again if needed
Amtrak.tsb.91.16 <- as_tsibble(Amtrak.ts.91.16)

# Partial ACF display
p_pacf_orig <- Amtrak.tsb.91.16 |>
  gg_tsdisplay(difference(value), plot_type = 'partial') +
  labs(title = "PACF of Original Amtrak Data")
save_plot("step2_pacf_original.png", p_pacf_orig)

# Fit multiple models with fable
Amtrak_fit <- Amtrak.tsb.91.16 |>
  model(
    arima013 = ARIMA(value ~ pdq(0,1,3)),
    arima101 = ARIMA(value ~ pdq(1,0,1)),
    stepwise = ARIMA(value),
    search = ARIMA(value, stepwise = FALSE, approximation = FALSE)
  )

cat("\n--- fable::ARIMA Models on Original Data ---\n")
print(report(Amtrak_fit))

# Forecast using chosen model (arima013)
p_forecast_fable <- forecast(Amtrak_fit, h = 60) |>
  filter(.model == 'arima013') |>
  autoplot(Amtrak.tsb.91.16) +
  labs(title = "Amtrak Data 1991–2016 + 5-Year Forecast (fable::arima013)",
       y = "Passenger Miles")
save_plot("step2_forecast_fable_arima013.png", p_forecast_fable)

#
# ***************************** Section 14 ************************************
#

log_section("Step 2: Forecast Comparison - All Models")

# Overlay forecasts from all models
p_forecast_all_models <- autoplot(Amtrak.ts) +
  autolayer(f2$mean, series = "Auto ARIMA (PassengerMiles)", PI = FALSE) +
  autolayer(f3$mean, series = "Auto ARIMA (Ridership)", PI = FALSE) +
  autolayer(f4$mean, series = "ARIMA(3,1,0)", PI = FALSE) +
  labs(
    title = "Comparison of Forecasts from Multiple ARIMA Models",
    y = "Passenger Miles",
    x = "Year"
  )

save_plot("step2_forecast_all_models_comparison.png", p_forecast_all_models)

#
# ***************************** Section 15 ************************************
#

log_section("Exercise 3A: Turkey GDP Analysis")

# Filter for Turkey
turkey <- global_economy |> filter(Country == "Turkey")

# Time plot of raw and log GDP
p_turkey_gdp <- turkey |> autoplot(GDP) + labs(title = "Turkey GDP")
p_turkey_log_gdp <- turkey |> autoplot(log(GDP)) + labs(title = "Turkey Log(GDP)")
save_plot("step2_turkey_gdp.png", p_turkey_gdp)
save_plot("step2_turkey_log_gdp.png", p_turkey_log_gdp)

# Difference of log(GDP)
p_turkey_log_diff <- turkey |> autoplot(log(GDP) |> difference()) +
  labs(title = "Turkey Log(GDP) Differenced")
save_plot("step2_turkey_log_diff.png", p_turkey_log_diff)

# Lambda recommendation using Guerrero
lambda_turkey <- turkey |> features(GDP, guerrero)
cat("\n--- Guerrero Lambda for Turkey GDP ---\n")
print(lambda_turkey)

# Display raw series using tsdisplay
p_turkey_tsdisplay <- gg_tsdisplay(turkey, GDP, plot_type = "partial")
save_plot("step2_turkey_tsdisplay.png", p_turkey_tsdisplay)

# Strip down to GDP column only
turkey_gdp_prepped <- turkey |> dplyr::select(GDP)

# Estimate BoxCox lambda
lambda_est <- BoxCox.lambda(turkey_gdp_prepped$GDP)
cat("\n--- Estimated Box-Cox Lambda for Turkey GDP ---\n")
print(lambda_est)

log_section("Exercise 3B: Tasmania Accommodation Takings")

# Filter for Tasmania
tasmania_takings <- aus_accommodation |> filter(State == "Tasmania")

# Time series display (raw)
p_tasmania_raw <- gg_tsdisplay(tasmania_takings, Takings, plot_type = "partial")
save_plot("step2_tasmania_tsdisplay_raw.png", p_tasmania_raw)

# Prepare data frame
tasmania_takings_prepped <- tasmania_takings |> dplyr::select(Takings)

# Estimate Box-Cox lambda
lambda_tas <- BoxCox.lambda(tasmania_takings_prepped$Takings)
cat("\n--- Estimated Box-Cox Lambda for Tasmania Takings ---\n")
print(lambda_tas)

# Apply Box-Cox transformation
tasmania_takings_prepped$Takings <- box_cox(tasmania_takings_prepped$Takings, lambda_tas)

# Plot transformed series
p_tasmania_trans <- gg_tsdisplay(tasmania_takings_prepped, Takings, plot_type = "partial")
save_plot("step2_tasmania_tsdisplay_transformed.png", p_tasmania_trans)

# Differencing diagnostics
n_diff_tas <- ndiffs(tasmania_takings_prepped$Takings)
cat("\n--- Number of Differences Required for Stationarity (Tasmania): ---\n")
print(n_diff_tas)

# Plot first difference
p_tasmania_diff <- ggtsdisplay(diff(tasmania_takings_prepped$Takings))
save_plot("step2_tasmania_diff_check.png", p_tasmania_diff)

# -- Alternative approach (textbook-based) --

log_section("Exercise 3B: Tasmania - Alternative Visualization")

tas <- aus_accommodation |> filter(State == "Tasmania")

# Plot raw
p_tas_raw <- autoplot(tas, Takings) + labs(title = "Tasmania Accommodation Takings")
save_plot("step2_tasmania_alt_raw.png", p_tas_raw)

# Plot log and differences
p_tas_log <- autoplot(tas, log(Takings)) + labs(title = "Log of Takings")
p_tas_log_diff <- autoplot(tas, log(Takings) |> difference(lag = 4)) +
  labs(title = "Lag-4 Difference of Log(Takings)")
p_tas_log_diff2 <- autoplot(tas, log(Takings) |> difference(lag = 4) |> difference()) +
  labs(title = "Second Difference of Lag-4 Log(Takings)")

save_plot("step2_tasmania_log.png", p_tas_log)
save_plot("step2_tasmania_log_diff.png", p_tas_log_diff)
save_plot("step2_tasmania_log_diff2.png", p_tas_log_diff2)

# Guerrero lambda
lambda_tas_alt <- tas |> features(Takings, guerrero)
cat("\n--- Guerrero Lambda via Alternative Path (Tasmania) ---\n")
print(lambda_tas_alt)

log_section("Exercise 3C: Souvenir Sales")

# Ensure souvenirs dataset is loaded
if (!exists("souvenirs")) {
  data("souvenirs", package = "tsibbledata")
}

# Initial display of raw data
tryCatch({
  p_souvenir_raw <- gg_tsdisplay(souvenirs, Sales, plot_type = "partial")
  save_plot("step2_souvenir_tsdisplay_raw.png", p_souvenir_raw)
}, error = function(e) {
  cat("souvenir_tsdisplay_raw failed:", e$message, "\n")
})

# Copy data for transformation
souvenirs_prepped <- souvenirs

# Estimate lambda
lambda_souvenir <- BoxCox.lambda(souvenirs_prepped$Sales)
cat("\n--- Estimated Box-Cox Lambda for Souvenir Sales ---\n")
print(lambda_souvenir)

# Apply Box-Cox transformation
souvenirs_prepped$Sales <- box_cox(souvenirs_prepped$Sales, lambda_souvenir)

# Display transformed data
tryCatch({
  p_souvenir_trans <- gg_tsdisplay(souvenirs_prepped, Sales, plot_type = "partial")
  save_plot("step2_souvenir_tsdisplay_transformed.png", p_souvenir_trans)
}, error = function(e) {
  cat("souvenir_tsdisplay_transformed failed:", e$message, "\n")
})

# Stationarity check
n_diff_souvenir <- ndiffs(souvenirs_prepped$Sales)
cat("\n--- Number of Differences Required for Souvenirs ---\n")
print(n_diff_souvenir)

# First difference diagnostic plot
tryCatch({
  p_souvenir_diff <- ggtsdisplay(diff(souvenirs_prepped$Sales))
  save_plot("step2_souvenir_diff_check.png", p_souvenir_diff)
}, error = function(e) {
  cat("souvenir_diff_check failed:", e$message, "\n")
})

# -- Alternative approach (textbook-style) --

log_section("Exercise 3C: Souvenirs - Alternative Visualizations")

tryCatch({
  p_souvenir_alt1 <- souvenirs |> autoplot(Sales) + labs(title = "Souvenir Sales - Raw")
  save_plot("step2_souvenir_alt_raw.png", p_souvenir_alt1)
}, error = function(e) {
  cat("souvenir_alt_raw failed:", e$message, "\n")
})

tryCatch({
  p_souvenir_alt2 <- souvenirs |> autoplot(log(Sales)) + labs(title = "Log(Sales)")
  save_plot("step2_souvenir_alt_log.png", p_souvenir_alt2)
}, error = function(e) {
  cat("souvenir_alt_log failed:", e$message, "\n")
})

tryCatch({
  p_souvenir_alt3 <- souvenirs |> autoplot(log(Sales) |> difference(lag = 12)) + 
    labs(title = "Lag-12 Difference of Log(Sales)")
  save_plot("step2_souvenir_alt_log_diff.png", p_souvenir_alt3)
}, error = function(e) {
  cat("souvenir_alt_log_diff failed:", e$message, "\n")
})

tryCatch({
  p_souvenir_alt4 <- souvenirs |> autoplot(log(Sales) |> difference(lag = 12) |> difference()) + 
    labs(title = "Second Difference of Lag-12 Log(Sales)")
  save_plot("step2_souvenir_alt_log_diff2.png", p_souvenir_alt4)
}, error = function(e) {
  cat("souvenir_alt_log_diff2 failed:", e$message, "\n")
})

# Guerrero estimate
lambda_souvenir_alt <- souvenirs |> features(Sales, guerrero)
cat("\n--- Guerrero Lambda via Alternative Path (Souvenirs) ---\n")
print(lambda_souvenir_alt)

log_section("Exercise 8A: United States GDP Analysis")

# Load and filter USA GDP from global_economy
usa_gdp <- global_economy %>%
  filter(Country == "United States") %>%
  dplyr::select(GDP)

# Plot raw GDP
p_usa_gdp_raw <- usa_gdp %>% autoplot(GDP) + labs(title = "US GDP (Raw)")
save_plot("step2_usa_gdp_raw.png", p_usa_gdp_raw)

# Estimate Box-Cox lambda using Guerrero method
lambda_usa <- usa_gdp |>
  features(GDP, features = guerrero) |>
  pull(lambda_guerrero)
cat("\n--- Estimated Box-Cox Lambda (USA GDP) ---\n")
print(lambda_usa)

# Apply Box-Cox transformation
usa_gdp$GDP <- box_cox(usa_gdp$GDP, lambda_usa)

#
# ***************************** Section 16 ************************************
#

log_section("Exercise 8: USA GDP First Difference Check")

# Plot first difference of USA GDP
p_usa_diff_check <- ggtsdisplay(diff(usa_gdp$GDP))
save_plot("step2_usa_diff_check.png", p_usa_diff_check)

# Plot transformed GDP
p_usa_gdp_trans <- usa_gdp %>% gg_tsdisplay(GDP, plot_type = "partial")
save_plot("step2_usa_gdp_transformed.png", p_usa_gdp_trans)

log_section("Exercise 8B: ARIMA Model Fitting on USA GDP")

# Simple auto ARIMA model
fit_simple <- usa_gdp |>
  model(ARIMA(GDP))
report(fit_simple)

# Exhaustive model grid
fit_grid <- usa_gdp |>
  model(
    arima010 = ARIMA(GDP ~ 1 + pdq(0, 1, 0)),
    arima011 = ARIMA(GDP ~ 1 + pdq(0, 1, 1)),
    arima012 = ARIMA(GDP ~ 1 + pdq(0, 1, 2)),
    arima013 = ARIMA(GDP ~ 1 + pdq(0, 1, 3)),
    arima110 = ARIMA(GDP ~ 1 + pdq(1, 1, 0)),
    arima111 = ARIMA(GDP ~ 1 + pdq(1, 1, 1)),
    arima112 = ARIMA(GDP ~ 1 + pdq(1, 1, 2)),
    arima113 = ARIMA(GDP ~ 1 + pdq(1, 1, 3)),
    arima210 = ARIMA(GDP ~ 1 + pdq(2, 1, 0)),
    arima211 = ARIMA(GDP ~ 1 + pdq(2, 1, 1)),
    arima212 = ARIMA(GDP ~ 1 + pdq(2, 1, 2)),
    arima213 = ARIMA(GDP ~ 1 + pdq(2, 1, 3)),
    arima310 = ARIMA(GDP ~ 1 + pdq(3, 1, 0)),
    arima311 = ARIMA(GDP ~ 1 + pdq(3, 1, 1)),
    arima312 = ARIMA(GDP ~ 1 + pdq(3, 1, 2)),
    arima313 = ARIMA(GDP ~ 1 + pdq(3, 1, 3))
  )

# Model comparison by AICc
fit_grid |>
  glance() |>
  arrange(AICc) |>
  select(.model, AICc) |>
  print()

log_section("Exercise 8D: Best Fit and Residuals")

# Best model based on previous lambda
best_fit <- usa_gdp |>
  model(ARIMA(box_cox(GDP, lambda_usa) ~ 1 + pdq(1, 1, 0)))

report(best_fit)

# Residual diagnostics
p_usa_resid <- best_fit |> gg_tsresiduals()
save_plot("step2_usa_bestfit_residuals.png", p_usa_resid)

log_section("Exercise 8E: Forecast Plots (ARIMA vs Grid Fit)")

# Redefine the grid search model object (if cleared)
fit <- usa_gdp |>
  model(
    arima010 = ARIMA(GDP ~ 1 + pdq(0, 1, 0)),
    arima011 = ARIMA(GDP ~ 1 + pdq(0, 1, 1)),
    arima012 = ARIMA(GDP ~ 1 + pdq(0, 1, 2)),
    arima013 = ARIMA(GDP ~ 1 + pdq(0, 1, 3)),
    arima110 = ARIMA(GDP ~ 1 + pdq(1, 1, 0)),
    arima111 = ARIMA(GDP ~ 1 + pdq(1, 1, 1)),
    arima112 = ARIMA(GDP ~ 1 + pdq(1, 1, 2)),
    arima113 = ARIMA(GDP ~ 1 + pdq(1, 1, 3)),
    arima210 = ARIMA(GDP ~ 1 + pdq(2, 1, 0)),
    arima211 = ARIMA(GDP ~ 1 + pdq(2, 1, 1)),
    arima212 = ARIMA(GDP ~ 1 + pdq(2, 1, 2)),
    arima213 = ARIMA(GDP ~ 1 + pdq(2, 1, 3)),
    arima310 = ARIMA(GDP ~ 1 + pdq(3, 1, 0)),
    arima311 = ARIMA(GDP ~ 1 + pdq(3, 1, 1)),
    arima312 = ARIMA(GDP ~ 1 + pdq(3, 1, 2)),
    arima313 = ARIMA(GDP ~ 1 + pdq(3, 1, 3))
  )

# Forecast from grid fit
p_grid_forecast <- fit %>%
  forecast(h = 10) %>%
  autoplot(usa_gdp) +
  labs(title = "Forecast from ARIMA Grid Fit (h = 10)", y = "GDP", x = "Year")

# Save plot
save_plot("step2_usa_forecast_grid_fit.png", p_grid_forecast)

log_section("Exercise 8E: Fitted vs Actuals for Best Fit")

aug_df <- best_fit %>% augment()
print(colnames(aug_df))
summary(aug_df$GDP)
summary(aug_df$.fitted)

p_gdp_fitted_vs_actual <- ggplot(aug_df, aes(x = Year)) +
  geom_line(aes(y = GDP, color = "Actual"), linewidth = 1.25) +
  geom_line(aes(y = .fitted, color = "Fitted"), linewidth = 1.25, linetype = "dashed") +
  labs(
    title = "Fitted vs Actuals: ARIMA(1,1,0) on USA GDP",
    subtitle = "Dashed line = model fit; solid line = observed values",
    y = "GDP (Billions)", x = "Year", color = NULL
  ) +
  scale_color_manual(values = c("Actual" = "black", "Fitted" = "blue")) +
  theme_minimal(base_size = 13)
save_plot("step2_usa_gdp_fitted_vs_actuals.png", p_gdp_fitted_vs_actual, width = 1000, height = 600, res = 150)

log_section("Exercise 8F: ETS Model and Forecast")

# Run ETS on Box-Cox transformed GDP
ets_check <- ets(usa_gdp$GDP)
print(ets_check)

# Fit and forecast using fable's ETS
p_ets_forecast <- usa_gdp |>
  model(ETS(GDP)) |>
  forecast(h = 10) |>
  autoplot(usa_gdp) +
  labs(title = "ETS Forecast (h = 10)")
save_plot("step2_usa_forecast_ets.png", p_ets_forecast)

log_section("Step 2: Cross-Validation with Train/Test Split")

# Define training set (before 2009)
AmtrakTraining <- Amtrak.tsb.91.16 |>
  filter(year(index) < 2009)

# Visualize training data
p_train_overlay <- autoplot(Amtrak.tsb.91.16, value) +
  autolayer(AmtrakTraining, value, colour = "red") +
  labs(title = "Training vs Full Data (Before 2009)", y = "Passenger Miles")
save_plot("step2_train_test_overlay.png", p_train_overlay)

# Fit Seasonal Naive model on training set
fit_snaive <- AmtrakTraining |>
  model(SNAIVE(value))

# Residual diagnostics
p_snaive_resid <- fit_snaive |> gg_tsresiduals()
save_plot("step2_snaive_residuals.png", p_snaive_resid)

# Forecast on holdout period
fcast_snaive <- fit_snaive |>
  forecast(new_data = anti_join(Amtrak.tsb.91.16, AmtrakTraining))

# Plot forecast vs actual
p_forecast_vs_actual <- fcast_snaive |> autoplot(Amtrak.tsb.91.16) +
  labs(title = "SNAIVE Forecast vs Actuals (Post-2009)", y = "Passenger Miles")
save_plot("step2_snaive_forecast_vs_actual.png", p_forecast_vs_actual)

# Accuracy comparison
log_section("Forecast Accuracy (Train vs Test)")
acc_results <- bind_rows(
  accuracy(fit_snaive),
  accuracy(fcast_snaive, Amtrak.tsb.91.16)
) |>
  select(-.model)
print(acc_results)

log_section("Step 2: Hyndman-style Time Series Cross-Validation")

# Plot the time series with TSCV overlay note
save_plot("step2_tscv_input_plot.png",
          plot(Amtrak.ts.91.16, ylab = "Passenger Miles", xlab = "Time", main = "TSCV Benchmarking Input")
)

# Hyndman-style TSCV with 3 models
k <- 60
n <- length(Amtrak.ts.91.16)
mae1 <- mae2 <- mae3 <- matrix(NA, n - k, 12)
st <- tsp(Amtrak.ts.91.16)[1] + (k - 2) / 12

for (i in 1:(n - k)) {
  xshort <- window(Amtrak.ts.91.16, end = st + i / 12)
  xnext <- window(Amtrak.ts.91.16, start = st + (i + 1) / 12, end = st + (i + 12) / 12)
  
  fit1 <- tslm(xshort ~ trend + season, lambda = 0)
  fcast1 <- forecast(fit1, h = 12)
  
  fit2 <- Arima(xshort, order = c(3, 0, 1), seasonal = list(order = c(0, 1, 1), period = 12),
                include.drift = TRUE, lambda = 0, method = "ML")
  fcast2 <- forecast(fit2, h = 12)
  
  fit3 <- ets(xshort, model = "MMM", damped = TRUE)
  fcast3 <- forecast(fit3, h = 12)
  
  mae1[i, 1:length(xnext)] <- abs(fcast1[['mean']] - xnext)
  mae2[i, 1:length(xnext)] <- abs(fcast2[['mean']] - xnext)
  mae3[i, 1:length(xnext)] <- abs(fcast3[['mean']] - xnext)
}

# Compute mean MAE per horizon
mae1Cols <- colMeans(mae1, na.rm = TRUE)
mae2Cols <- colMeans(mae2, na.rm = TRUE)
mae3Cols <- colMeans(mae3, na.rm = TRUE)

# Combine and reshape for ggplot
maes <- data.frame(
  Horizon = 1:12,
  LM = mae1Cols,
  ARIMA = mae2Cols,
  ETS = mae3Cols
)

library(reshape2)
maes_long <- melt(maes, id.vars = "Horizon")

# Final plot comparing MAE for 1- to 12-month horizons
p_tscv_mae <- ggplot(maes_long, aes(x = Horizon, y = value, colour = variable)) +
  geom_line(size = 1) +
  labs(title = "Forecast MAE by Model (1–12 Month Horizon)",
       x = "Forecast Horizon (Months)",
       y = "Mean Absolute Error (MAE)",
       colour = "Model") +
  theme_minimal()

save_plot("step2_tscv_mae_comparison.png", p_tscv_mae)

cat("\n\n===== PLOT DIAGNOSTIC =====\n\n")
plots_created <- list.files("output/plots/step2", pattern = "\\.png$")
cat("Plots found in output/plots/step2/:\n")
print(sort(plots_created))
cat("\nTotal plots created:", length(plots_created), "\n")

end_log()

