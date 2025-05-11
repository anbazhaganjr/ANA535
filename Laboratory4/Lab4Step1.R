##############################################################
# ANA 535 – Laboratory #4 Step 1
# ------------------------------------------------------------
# Original Script Author: Dr. Marvine Hamner (April 2025)
#
# Modifications by: Naresh Anbazhagan, Graduate Student – ANA 535
# - Added helper methods for plot saving (`save_plot()`)
# - Integrated structured logging with sink()
# - Replaced platform-incompatible packages (e.g., seasonal) 
#   with macOS-compatible alternatives (e.g., STL)
# - Reorganized sections for reproducibility and automation
#
# Environment: R 4.5.0 on macOS, RStudio
##############################################################

# ======================
# Required Packages
# ======================

reqd_pkgs <- c(
  "fpp3", "dplyr", "tidyverse", "ggplot2", "tsibble", "tsibbledata",
  "fable", "feasts", "lubridate", "zoo", "forecast", "TSA", "tseries", "ggpubr"
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
dir.create("output/logs/step1", recursive = TRUE, showWarnings = FALSE)
dir.create("output/plots/step1", recursive = TRUE, showWarnings = FALSE)

# ======================
# Helper Functions
# ======================

save_plot <- function(filename, expr, width = 800, height = 600, res = 120) {
  png(file = paste0("output/plots/step1/", filename), width = width, height = height, res = res)
  print(expr)
  dev.off()
  invisible(NULL)
}

start_log <- function(filename = "output/logs/step1/ana535_lab4_step1_output_log.txt") {
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

reset_par <- function() {
  par(mfrow = c(1, 1))
}


#
#********************************** Step 1a **********************
#
start_log()
log_section("Step 1a: Simulated White Noise (AR(0))")

# Generate white noise with mean = 0, variance = 16 (sd = 4)
n = 1000
e = rnorm(n, mean = 0.0, sd = 4.0)

# Original base R plot (replaced with saved ggplot version)
e_df <- tibble(index = 1:n, value = e)
p_raw <- ggplot(e_df, aes(x = index, y = value)) +
  geom_line() +
  labs(title = "Simulated White Noise (Raw Plot)", x = "Index", y = "Value")
save_plot("step1a_raw_plot.png", p_raw)

# Create ts object with sampling frequency of 10 Hz
e.ts <- ts(e, frequency = 10)

# Print structure of ts object
str(e.ts)

# Time series plot
p_ts <- autoplot(e.ts) +
  labs(title = "Simulated White Noise (AR(0)) - Time Series",
       x = "Time (seconds)", y = "Magnitude")
save_plot("step1a_ar0_ts_plot.png", p_ts)

# ACF plot
p_acf <- ggAcf(e.ts) +
  ggtitle("Sample ACF for e sim data")
save_plot("step1a_ar0_acf.png", p_acf)

# ADF test
log_section("ADF Test for AR(0)")
e.ts.adf <- adf.test(e.ts)
print(e.ts.adf)

# KPSS test
log_section("KPSS Test for AR(0)")
e.ts.kpss <- kpss.test(e.ts)
print(e.ts.kpss)

#
#********************************** Step 1b **********************
#

log_section("Step 1b: Simulated AR(1) Process")

# Generate AR(1) simulated data: y[t] = 0.6 * y[t-1] + e[t]
y <- numeric(1000)
e2 <- rnorm(1000)
for(i in 2:1000)
  y[i] <- 0.6 * y[i - 1] + e2[i]

# Convert to tsibble then ts
e2.tsb <- tsibble(idx = seq_len(1000), y = y, index = idx)
e2.ts <- ts(e2.tsb[, 2], frequency = 10)

# Plot time series
p_ts <- autoplot(e2.ts) +
  labs(title = "Simulated AR(1) Process", x = "Time (seconds)", y = "Magnitude")
save_plot("step1b_ar1_ts_plot.png", p_ts)

# ACF and PACF plots
e2Acf <- ggAcf(e2.ts) + ggtitle("Sample ACF for e2 sim data")
save_plot("step1b_ar1_acf.png", e2Acf)

e2Pacf <- ggPacf(e2.ts) + ggtitle("Sample PACF for e2 sim data")
save_plot("step1b_ar1_pacf.png", e2Pacf)

# Print structure to log
log_section("Structure of e2.ts")
str(e2.ts)

# ADF Test
log_section("ADF Test for AR(1)")
e2.ts.adf <- adf.test(e2.ts)
print(e2.ts.adf)

# KPSS Test
log_section("KPSS Test for AR(1)")
e2.ts.kpss <- kpss.test(e2.ts)
print(e2.ts.kpss)

#
#********************************** Step 1d **********************
#

log_section("Step 1d: Simulated MA(1) Process")

# Simulate MA(1) with theta = 0.6
e3.ts <- arima.sim(model = list(ma = 0.6), n = 1000)

# Time series plot
p_ma1_ts <- autoplot(e3.ts) +
  labs(title = "Simulated MA(1) Process", x = "Time", y = "Value")
save_plot("step1d_ma1_ts_plot.png", p_ma1_ts)

# ACF and PACF
e3Acf <- ggAcf(e3.ts) + ggtitle("Sample ACF for MA(1) simulated data")
save_plot("step1d_ma1_acf.png", e3Acf)

e3Pacf <- ggPacf(e3.ts) + ggtitle("Sample PACF for MA(1) simulated data")
save_plot("step1d_ma1_pacf.png", e3Pacf)

# Structure
log_section("Structure of e3.ts")
str(e3.ts)

# ADF Test
log_section("ADF Test for MA(1)")
e3.ts.adf <- adf.test(e3.ts)
print(e3.ts.adf)

# KPSS Test
log_section("KPSS Test for MA(1)")
e3.ts.kpss <- kpss.test(e3.ts)
print(e3.ts.kpss)

# Additional comparison: AR(1) and MA(1) simulated with arima.sim()
log_section("Step 1d: Simulated AR(1) with arima.sim for comparison")
e2b.ts <- arima.sim(model = list(ar = 0.6), n = 1000)

# Comparison plots
e2bAcf <- ggAcf(e2b.ts) + ggtitle("ACF for AR(1) (arima.sim)")
e2bPacf <- ggPacf(e2b.ts) + ggtitle("PACF for AR(1) (arima.sim)")

# Combined ACF and PACF comparison
comparison_plot <- ggarrange(e2bAcf, e2bPacf, e3Acf, e3Pacf, ncol = 2, nrow = 2)
save_plot("step1d_ma1_vs_ar1_comparison.png", comparison_plot)

#
#********************************** Step 1c **********************
#

log_section("Step 1c: Simulated AR(1) with Drift")

# Simulate AR(1) with drift (constant c = 10.0)
c <- 10.0
y <- numeric(1000)
e2wd <- rnorm(1000)
for(i in 2:1000) {
  y[i] <- c + 0.6 * y[i - 1] + e2wd[i]
}

# Convert to tsibble and then ts
e2wd.tsb <- tsibble(idx = seq_len(1000), y = y, index = idx)
e2wd.ts <- ts(e2wd.tsb[, 2], frequency = 10)

# Time series plot
p_drift_ts <- autoplot(e2wd.ts) +
  labs(title = "Simulated AR(1) with Drift (c = 10)", x = "Time", y = "Value")
save_plot("step1c_ar1_drift_ts.png", p_drift_ts)

# STL decomposition (optional but insightful)
e2wd.comp <- decompose(e2wd.ts)
p_decomp <- autoplot(e2wd.comp)
save_plot("step1c_ar1_drift_decompose.png", p_decomp)

# ACF and PACF
e2wdAcf <- ggAcf(e2wd.ts) + ggtitle("ACF for AR(1) with Drift")
save_plot("step1c_ar1_drift_acf.png", e2wdAcf)

e2wdPacf <- ggPacf(e2wd.ts) + ggtitle("PACF for AR(1) with Drift")
save_plot("step1c_ar1_drift_pacf.png", e2wdPacf)

# Combined ACF/PACF comparison with Step 1b (AR(1) without drift)
comparison_drift <- ggarrange(e2Acf, e2Pacf, e2wdAcf, e2wdPacf, ncol = 2, nrow = 2)
save_plot("step1c_ar1_vs_ar1_drift_acf_pacf.png", comparison_drift)

# Structure
log_section("Structure of e2wd.ts")
str(e2wd.ts)

# ADF Test
log_section("ADF Test for AR(1) with Drift")
e2wd.ts.adf <- adf.test(e2wd.ts)
print(e2wd.ts.adf)

# KPSS Test
log_section("KPSS Test for AR(1) with Drift")
e2wd.ts.kpss <- kpss.test(e2wd.ts)
print(e2wd.ts.kpss)


#
#********************************** Step 1e **********************
#

log_section("Step 1e: Earthquake Magnitude Data (1933–1976)")

# Read the earthquake data (uploaded version)
quakes_path <- "data/quakes.v2.csv"
quakes <- read.csv(quakes_path)
colnames(quakes) <- c("Year", "Mag", "Name", "State", "Country")

# Drop first record and extract magnitude series (as in original script)
quakes.ts <- ts(quakes$Mag[2:297])

# Log structure
log_section("Structure of quakes.ts")
print(str(quakes.ts))

# Plot magnitude series
p_quakes_ts <- autoplot(quakes.ts) +
  labs(title = "Earthquake Magnitudes (1933–1976)", y = "Magnitude")
save_plot("step1e_quakes_ts.png", p_quakes_ts)

# Linear regression fits
trend_model1 <- tslm(quakes.ts ~ trend)
trend_model2 <- tslm(quakes.ts ~ trend + I(trend^2))

# Plot linear trend fit
plot1 <- autoplot(quakes.ts) +
  autolayer(trend_model1$fitted, series = "Linear", color = "red") +
  labs(title = "Quakes: Linear Trend Fit", y = "Magnitude")
save_plot("step1e_quakes_trend_linear.png", plot1)

# Plot quadratic trend fit
plot2 <- autoplot(quakes.ts) +
  autolayer(trend_model2$fitted, series = "Quadratic", color = "blue") +
  labs(title = "Quakes: Quadratic Trend Fit", y = "Magnitude")
save_plot("step1e_quakes_trend_quad.png", plot2)

# ACF/PACF plots
p_quakes_acf <- ggAcf(quakes.ts) +
  ggtitle("ACF for Earthquake Magnitudes")
p_quakes_pacf <- ggPacf(quakes.ts) +
  ggtitle("PACF for Earthquake Magnitudes")
save_plot("step1e_quakes_acf.png", p_quakes_acf)
save_plot("step1e_quakes_pacf.png", p_quakes_pacf)

# Attempt decomposition (warns about missing frequency)
log_section("Attempting STL Decomposition")
quakes.ts <- ts(quakes$Mag[2:297], frequency = 12)
tryCatch({
  quakes_decomp <- decompose(quakes.ts)
  save_plot("step1e_quakes_decomp.png", autoplot(quakes_decomp))
}, error = function(e) {
  cat("Decomposition failed: ", e$message, "\n")
})

#
#********************************** Step 1f **********************
#

log_section("Step 1f: Google Stock Price Data (Last 5 Years)")

# Read the uploaded CSV
google_path <- "data/GoogleStockData-5yr.v2.csv"
google <- read.csv(google_path)

# Rename columns
colnames(google) <- c("Date", "Close", "Volume", "Open", "High", "Low")

# Convert Date to Date format
google$Date <- lubridate::mdy(google$Date)

# Log structure
log_section("Structure of Google Stock Data")
str(google)

# Create ts object of closing prices (no frequency set due to irregular spacing)
google.ts <- ts(google$Close)

# Plot time series
p_google_ts <- autoplot(google.ts) +
  labs(title = "Google Closing Stock Prices (Raw)", y = "Closing Price")
save_plot("step1f_google_ts_plot.png", p_google_ts)

# ADF Test
log_section("ADF Test for Google Closing Prices")
google.adf <- adf.test(google.ts)
print(google.adf)

# ACF and PACF
p_google_acf <- ggAcf(google.ts) + ggtitle("ACF: Google Closing Prices")
p_google_pacf <- ggPacf(google.ts) + ggtitle("PACF: Google Closing Prices")
save_plot("step1f_google_acf.png", p_google_acf)
save_plot("step1f_google_pacf.png", p_google_pacf)

#
#********************************** Step 1g **********************
#

log_section("Step 1g: Auto ARIMA Modeling")

# ---- Simulated AR(1) Data (e2.ts) ----
log_section("Auto ARIMA: Simulated AR(1) Data (e2.ts)")

p_e2 <- e2.tsb |> 
  autoplot() +
  labs(title = "Simulated data 'e2'", y = "Magnitude")
save_plot("step1g_e2_autoplot.png", p_e2)

p_e2_diff <- e2.tsb |>
  gg_tsdisplay(difference(y), plot_type = 'partial')
save_plot("step1g_e2_diff_display.png", p_e2_diff)

e2_fit <- e2.tsb |>
  model(stepwise = ARIMA(y),
        search = ARIMA(y, stepwise = FALSE))
report(e2_fit)

# ---- Simulated MA(1) Data (e3.ts) ----
log_section("Auto ARIMA: Simulated MA(1) Data (e3.ts)")

e3.tsb <- as_tsibble(e3.ts)

p_e3 <- e3.tsb |> 
  autoplot() +
  labs(title = "Simulated data 'e3'", y = "Magnitude")
save_plot("step1g_e3_autoplot.png", p_e3)

p_e3_diff <- e3.tsb |>
  gg_tsdisplay(difference(value), plot_type = 'partial')
save_plot("step1g_e3_diff_display.png", p_e3_diff)

e3_fit <- e3.tsb |>
  model(arima001 = ARIMA(value ~ pdq(0,0,1)),
        stepwise = ARIMA(value),
        search = ARIMA(value, stepwise = FALSE))
report(e3_fit)

# ---- Earthquake Data (quakes.ts) ----
log_section("Auto ARIMA: Earthquake Data (quakes.ts)")

quakes.tsb <- as_tsibble(quakes.ts)

p_quakes <- quakes.tsb |> 
  autoplot() +
  labs(title = "Earthquake Magnitudes", y = "Magnitude")
save_plot("step1g_quakes_autoplot.png", p_quakes)

p_quakes_diff <- quakes.tsb |>
  gg_tsdisplay(difference(value[2:297]), plot_type = 'partial')
save_plot("step1g_quakes_diff_display.png", p_quakes_diff)

quakes_fit <- quakes.tsb |>
  model(arima100 = ARIMA(value[2:297] ~ pdq(1,1,0)),
        stepwise = ARIMA(value[2:297]),
        search = ARIMA(value[2:297], stepwise = FALSE, approximation = FALSE))
report(quakes_fit)

# ---- Google Stock Data (google.ts) ----
log_section("Auto ARIMA: Google Stock Data (google.ts)")

google.tsb <- as_tsibble(google.ts)

p_google <- google.tsb |> 
  autoplot() +
  labs(title = "Google Stock Closing Prices", y = "Price")
save_plot("step1g_google_autoplot.png", p_google)

p_google_diff <- google.tsb |>
  gg_tsdisplay(difference(value), plot_type = 'partial')
save_plot("step1g_google_diff_display.png", p_google_diff)

google_fit <- google.tsb |>
  model(arima110 = ARIMA(value ~ pdq(1,1,0)),
        arima310 = ARIMA(value ~ pdq(3,1,0)),
        stepwise = ARIMA(value),
        search = ARIMA(value, stepwise = FALSE, approximation = FALSE))
report(google_fit)

cat("\n\n===== PLOT DIAGNOSTIC =====\n\n")
plots_created <- list.files("output/plots/step1", pattern = "\\.png$")
cat("Plots found in output/plots/step1/:\n")
print(sort(plots_created))
cat("\nTotal plots created:", length(plots_created), "\n")

end_log()