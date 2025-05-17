# ANA 535 – Course Project: UK vs US Industrial Production Forecasting
# Steps 1–6: Aggregate UK-US IPI Analysis
# Steps 7-10: Sector Analysis
# Fixed by: Naresh Anbazhagan (May 2025)
# Fixes: Modular function, helper methods for: 
# write_to_csv, generic paths, error handling, simplified plot saving

# Required Libraries
reqd_pkgs <- c(
  "readxl", "tidyverse", "ggplot2", "forecast", "tseries", 
  "gridExtra", "grid", "janitor", "Metrics", "zoo",
  "lubridate", "stringr", "scales", "ggplotify", 
  "R.utils", "parallel", "doParallel", "pbapply", "purrr"
)

installed <- rownames(installed.packages())
to_install <- setdiff(reqd_pkgs, installed)
if (length(to_install) > 0) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
invisible(lapply(reqd_pkgs, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))

cat("All libraries successfully loaded.\n\n")

# Working Directory Setup
setwd(getwd()) # Use current working directory
dir.create("data", recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)
dir.create("output/logs", recursive = TRUE, showWarnings = FALSE)
dir.create("output/plots", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("data/transformed", recursive = TRUE, showWarnings = FALSE)

# Logging Functions
start_log <- function(filename = "output/logs/industrialProductionForecasting_log.txt") {
  if (!dir.exists(dirname(filename))) {
    dir.create(dirname(filename), recursive = TRUE)
  }
  while (sink.number() > 0) sink(NULL)
  sink(file = filename, append = FALSE, split = TRUE)
  start_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  cat("\n===== ANA535 Course Project Execution Started: ", start_time, " =====\n\n")
}

end_log <- function() {
  end_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  cat("\n===== ANA535 Course Project Execution Ended: ", end_time, " =====\n\n")
  sink()
}

log_section <- function(title) {
  cat("\n\n===== ", title, " =====\n\n")
}

save_plot <- function(filename, expr, width = 800, height = 600, res = 120) {
  full_path <- file.path("output/plots", filename)
  png(file = full_path, width = width, height = height, res = res)
  print(expr)
  dev.off()
  cat("Saved plot:", full_path, "\n")
  invisible(NULL)
}

save_plot1 <- function(filename, plot, width = 1000, height = 800, res = 100) {
  output_path <- file.path("output/plots", filename)
  tryCatch({
    ggsave(filename = output_path, plot = plot, width = width/100, height = height/100, dpi = res, units = "in", device = "png")
    if (file.exists(output_path)) {
      cat("Successfully saved:", output_path, "\n")
    } else {
      cat("Failed to save:", output_path, "\n")
    }
  }, error = function(e) {
    cat("Error saving", filename, ":", conditionMessage(e), "\n")
  })
  return(file.exists(output_path))
}

write_to_csv <- function(df, filename, path_prefix = NULL) {
  if (is.null(path_prefix)) {
    if (grepl("forecast|summary|metrics|correlation|acf", filename, ignore.case = TRUE)) {
      path_prefix <- "output/tables"
    } else if (grepl("residuals|diff", filename, ignore.case = TRUE)) {
      path_prefix <- "data/transformed"
    } else {
      path_prefix <- "output/tables"
    }
  }
  if (!dir.exists(path_prefix)) {
    dir.create(path_prefix, recursive = TRUE, showWarnings = FALSE)
  }
  full_path <- file.path(path_prefix, filename)
  tryCatch(
    {
      write_csv(df, full_path)
      cat("Saved file:", full_path, "\n")
    },
    error = function(e) stop("Error writing CSV:", e$message)
  )
}

# Start Logging
start_log()
log_section("Step 1: Load Excel and Plot Raw Data")

##******************************* STEP 1: Initial Analysis  ************************##

# Step 1.1: Load UK-US Aggregate Data
data_file <- "data/UK_Comp_US_GBRPROINDMISMEI.xlsx"
if (!file.exists(data_file)) stop("Data file not found:", data_file)
data <- tryCatch(
  read_excel(data_file),
  error = function(e) stop("Error loading Excel:", e$message)
)
data$DATE <- as.Date(data$DATE)

cat("UK-US Aggregated Data Loaded\n")
cat("Number of missing values:", sum(is.na(data)), "\n")

# Step 1.2: Plot UK vs US Index
ts_plot <- ggplot(data, aes(x = DATE)) +
  geom_line(aes(y = GBRPROINDMISMEI, color = "UK")) +
  geom_line(aes(y = USAPROINDMISMEI, color = "US")) +
  labs(title = "Industrial Production Index (UK vs US)",
       x = "Date", y = "Index Value", color = "Country") +
  theme_minimal()

print(ts_plot)
save_plot("step1_uk_us_index_plot.png", ts_plot)

# Step 1.3: Summary Statistics Table
summary_stats <- data %>%
  select(-DATE) %>%
  summarise_all(list(
    Mean = mean,
    SD = sd,
    Min = min,
    Max = max,
    Median = median
  ), na.rm = TRUE)

summary_long <- summary_stats %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  separate(Variable, into = c("Country", "Statistic"), sep = "_", extra = "merge") %>%
  pivot_wider(names_from = Statistic, values_from = Value)

summary_table <- summary_long %>%
  rename(`Std Dev` = SD) %>%
  select(Country, Mean, `Std Dev`, Min, Max, Median) %>%
  mutate(Country = recode(Country,
                          "GBRPROINDMISMEI" = "UK",
                          "USAPROINDMISMEI" = "US"))

print(summary_table)
write_to_csv(summary_table, "step1_summary_table_uk_us.csv")

# Step 1.4: Create Time Series Objects
uk_ts <- tryCatch(
  ts(data$GBRPROINDMISMEI, start = c(1948, 1), frequency = 12),
  error = function(e) stop("Error creating UK time series:", e$message)
)
us_ts <- tryCatch(
  ts(data$USAPROINDMISMEI, start = c(1948, 1), frequency = 12),
  error = function(e) stop("Error creating US time series:", e$message)
)

# Step 1.5: Basic EDA Plots
save_plot("step1_uk_run_sequence.png", autoplot(uk_ts) + ggtitle("UK Run Sequence Plot") + ylab("Index"))
save_plot("step1_us_run_sequence.png", autoplot(us_ts) + ggtitle("US Run Sequence Plot") + ylab("Index"))

save_plot("step1_uk_lag_plot.png", lag.plot(uk_ts, lags = 1, main = "UK Lag Plot"))
save_plot("step1_us_lag_plot.png", lag.plot(us_ts, lags = 1, main = "US Lag Plot"))

save_plot("step1_uk_histogram.png", hist(uk_ts, breaks = 30, main = "Histogram of UK Index", xlab = "UK Index", col = "lightblue"))
save_plot("step1_us_histogram.png", hist(us_ts, breaks = 30, main = "Histogram of US Index", xlab = "US Index", col = "lightgreen"))

save_plot("step1_uk_qq_plot.png", {
  qqnorm(uk_ts, main = "Normal Q-Q Plot - UK")
  qqline(uk_ts)
})

save_plot("step1_us_qq_plot.png", {
  qqnorm(us_ts, main = "Normal Q-Q Plot - US")
  qqline(uk_ts)
})

# Step 1.5a: Scatter plot for linearity (IPI vs. Time)
scatter_uk_time <- ggplot(data, aes(x = DATE, y = GBRPROINDMISMEI)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatter Plot: UK Industrial Production Index vs. Time (1948–2025)",
       x = "Date", y = "UK Index") +
  theme_minimal()
save_plot("step1_uk_scatter_time.png", scatter_uk_time)

scatter_us_time <- ggplot(data, aes(x = DATE, y = USAPROINDMISMEI)) +
  geom_point(alpha = 0.5, color = "orange") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatter Plot: US Industrial Production Index vs. Time (1948–2025)",
       x = "Date", y = "US Index") +
  theme_minimal()
save_plot("step1_us_scatter_time.png", scatter_us_time)

# Step 1.6: Composite Diagnostic Grids – UK and US
p1_uk <- ggplot(data, aes(x = DATE, y = GBRPROINDMISMEI)) +
  geom_line(color = "steelblue") +
  labs(title = "Run Sequence Plot", x = "Date", y = "UK Index") +
  theme_minimal()

lag_df_uk <- data.frame(x = head(uk_ts, -1), y = tail(uk_ts, -1))
p2_uk <- ggplot(lag_df_uk, aes(x = x, y = y)) +
  geom_point(color = "darkgreen") +
  labs(title = "Lag Plot (Lag 1)", x = "Lag(x)", y = "x") +
  theme_minimal()

p3_uk <- ggplot(data, aes(x = GBRPROINDMISMEI)) +
  geom_histogram(fill = "orange", color = "black", bins = 30) +
  labs(title = "Histogram", x = "UK Index", y = "Frequency") +
  theme_minimal()

p4_uk <- ggplot(data, aes(sample = GBRPROINDMISMEI)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

save_plot("step1_uk_diagnostic_composite.png", grid.arrange(p1_uk, p2_uk, p3_uk, p4_uk, ncol = 2), width = 1000, height = 800)

p1_us <- ggplot(data, aes(x = DATE, y = USAPROINDMISMEI)) +
  geom_line(color = "steelblue") +
  labs(title = "Run Sequence Plot", x = "Date", y = "US Index") +
  theme_minimal()

lag_df_us <- data.frame(x = head(us_ts, -1), y = tail(us_ts, -1))
p2_us <- ggplot(lag_df_us, aes(x = x, y = y)) +
  geom_point(color = "darkgreen") +
  labs(title = "Lag Plot (Lag 1)", x = "Lag(x)", y = "x") +
  theme_minimal()

p3_us <- ggplot(data, aes(x = USAPROINDMISMEI)) +
  geom_histogram(fill = "orange", color = "black", bins = 30) +
  labs(title = "Histogram", x = "US Index", y = "Frequency") +
  theme_minimal()

p4_us <- ggplot(data, aes(sample = USAPROINDMISMEI)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

save_plot("step1_us_diagnostic_composite.png", grid.arrange(p1_us, p2_us, p3_us, p4_us, ncol = 2), width = 1000, height = 800)

##************************* STEP 2: Correlation & Stationarity Diagnostics ************************##

log_section("Step 2: Correlation and Stationarity Diagnostics")

# Step 2.1: Correlation between UK and US
correlation_value <- cor(data$GBRPROINDMISMEI, data$USAPROINDMISMEI, use = "complete.obs")
correlation_table <- tibble(
  Variables = "UK vs US Industrial Production",
  Correlation = round(correlation_value, 4)
)
print(correlation_table)
write_to_csv(correlation_table, "step2_correlation_uk_us.csv")

# Scatter plot to visualize correlation
scatter_uk_us <- ggplot(data, aes(x = GBRPROINDMISMEI, y = USAPROINDMISMEI)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatter Plot: UK vs US Industrial Production Index (1948–2025)",
       x = "UK Index (GBRPROINDMISMEI)", y = "US Index (USAPROINDMISMEI)") +
  theme_minimal()
save_plot("step2_uk_us_scatter.png", scatter_uk_us, width = 800, height = 600, res = 120)

# Step 2.2: ADF Test for Stationarity
adf_uk <- adf.test(uk_ts)
adf_uk_result <- tibble(
  Series = "UK",
  ADF_Statistic = adf_uk$statistic,
  P_Value = adf_uk$p.value,
  Method = adf_uk$method
)
print(adf_uk_result)

adf_us <- adf.test(us_ts)
adf_us_result <- tibble(
  Series = "US",
  ADF_Statistic = adf_us$statistic,
  P_Value = adf_us$p.value,
  Method = adf_us$method
)
print(adf_us_result)

adf_results <- bind_rows(adf_uk_result, adf_us_result)
write_to_csv(adf_results, "step2_adf_test_results.csv")

##************************* Step 3: ACF/PACF and Differencing ************************## 
##*
log_section("Step 3: ACF/PACF and Differencing")

# Step 3.1: ACF/PACF for Original Series
save_plot("step3_uk_acf_original.png", Acf(uk_ts, main = "ACF – UK Original Series"))
save_plot("step3_uk_pacf_original.png", Pacf(uk_ts, main = "PACF – UK Original Series"))
save_plot("step3_us_acf_original.png", Acf(us_ts, main = "ACF – US Original Series"))
save_plot("step3_us_pacf_original.png", Pacf(us_ts, main = "PACF – US Original Series"))

# Step 3.2: First Differencing
uk_diff <- diff(uk_ts)
us_diff <- diff(us_ts)

write_to_csv(tibble(Date = time(uk_diff), UK_Diff = as.numeric(uk_diff)),
             "step3_uk_diff.csv")
write_to_csv(tibble(Date = time(us_diff), US_Diff = as.numeric(us_diff)),
             "step3_us_diff.csv")

# Step 3.3: ACF/PACF for Differenced Series
save_plot("step3_uk_acf_diff.png", Acf(uk_diff, main = "ACF – UK Differenced Series"))
save_plot("step3_uk_pacf_diff.png", Pacf(uk_diff, main = "PACF – UK Differenced Series"))
save_plot("step3_us_acf_diff.png", Acf(us_diff, main = "ACF – US Differenced Series"))
save_plot("step3_us_pacf_diff.png", Pacf(us_diff, main = "PACF – US Differenced Series"))

# Step 3.4: Flatten and Export ACF Values
acf_uk_df <- tibble(Lag = as.vector(Acf(uk_ts, plot = FALSE)$lag),
                    ACF = as.vector(Acf(uk_ts, plot = FALSE)$acf))
acf_us_df <- tibble(Lag = as.vector(Acf(us_ts, plot = FALSE)$lag),
                    ACF = as.vector(Acf(us_ts, plot = FALSE)$acf))
acf_uk_diff_df <- tibble(Lag = as.vector(Acf(uk_diff, plot = FALSE)$lag),
                         ACF = as.vector(Acf(uk_diff, plot = FALSE)$acf))
acf_us_diff_df <- tibble(Lag = as.vector(Acf(us_diff, plot = FALSE)$lag),
                         ACF = as.vector(Acf(us_diff, plot = FALSE)$acf))

write_to_csv(acf_uk_df, "step3_acf_uk_original.csv")
write_to_csv(acf_us_df, "step3_acf_us_original.csv")
write_to_csv(acf_uk_diff_df, "step3_acf_uk_diff.csv")
write_to_csv(acf_us_diff_df, "step3_acf_us_diff.csv")

# Step 3.5: STL Decomposition
log_section("Step 3.5: STL Decomposition")

p_uk_stl <- autoplot(stl(uk_ts, s.window = "periodic")) + ggtitle("UK STL Decomposition")
p_us_stl <- autoplot(stl(us_ts, s.window = "periodic")) + ggtitle("US STL Decomposition")

print(p_uk_stl)
save_plot("uk_stl_decomposition.png", p_uk_stl, width = 1200, height = 900, res = 150)

print(p_us_stl)
save_plot("us_stl_decomposition.png", p_us_stl, width = 1200, height = 900, res = 150)

##************************* STEP 4:  Model Fitting – ARIMA and ETS *****************************#
##*
log_section("Step 4: ARIMA and ETS Model Fitting")

# UK: ARIMA and ETS
fit_arima_uk <- auto.arima(uk_ts)
summary(fit_arima_uk)
fit_ets_uk <- ets(uk_ts)
summary(fit_ets_uk)
fc_arima_uk <- forecast(fit_arima_uk, h = 24)
fc_ets_uk <- forecast(fit_ets_uk, h = 24)

write_to_csv(as_tibble(fc_arima_uk), "step4_forecast_arima_uk.csv")
write_to_csv(as_tibble(fc_ets_uk), "step4_forecast_ets_uk.csv")

write_to_csv(tibble(Date = time(residuals(fit_arima_uk)),
                    Residuals = as.numeric(residuals(fit_arima_uk))),
             "step4_residuals_arima_uk.csv")
write_to_csv(tibble(Date = time(residuals(fit_ets_uk)),
                    Residuals = as.numeric(residuals(fit_ets_uk))),
             "step4_residuals_ets_uk.csv")

save_plot("step4_uk_residuals_arima_acf.png", Acf(residuals(fit_arima_uk), main = "ARIMA Residuals – UK"))
save_plot("step4_uk_residuals_ets_acf.png", Acf(residuals(fit_ets_uk), main = "ETS Residuals – UK"))
save_plot("step4_uk_forecast_arima.png", autoplot(fc_arima_uk) + ggtitle("ARIMA Forecast – UK"))
save_plot("step4_uk_forecast_ets.png", autoplot(fc_ets_uk) + ggtitle("ETS Forecast – UK"))

# US: ARIMA and ETS
fit_arima_us <- auto.arima(us_ts)
summary(fit_arima_us)
fit_ets_us <- ets(us_ts)
summary(fit_ets_us)
fc_arima_us <- forecast(fit_arima_us, h = 24)
fc_ets_us <- forecast(fit_ets_us, h = 24)

write_to_csv(as_tibble(fc_arima_us), "step4_forecast_arima_us.csv")
write_to_csv(as_tibble(fc_ets_us), "step4_forecast_ets_us.csv")

write_to_csv(tibble(Date = time(residuals(fit_arima_us)),
                    Residuals = as.numeric(residuals(fit_arima_us))),
             "step4_residuals_arima_us.csv")
write_to_csv(tibble(Date = time(residuals(fit_ets_us)),
                    Residuals = as.numeric(residuals(fit_ets_us))),
             "step4_residuals_ets_us.csv")

save_plot("step4_us_residuals_arima_acf.png", Acf(residuals(fit_arima_us), main = "ARIMA Residuals – US"))
save_plot("step4_us_residuals_ets_acf.png", Acf(residuals(fit_ets_us), main = "ETS Residuals – US"))
save_plot("step4_us_forecast_arima.png", autoplot(fc_arima_us) + ggtitle("ARIMA Forecast – US"))
save_plot("step4_us_forecast_ets.png", autoplot(fc_ets_us) + ggtitle("ETS Forecast – US"))

#****************** Step 5: Time Series Linear Regression Forecast – UK and US *************#
log_section("Step 5: Time Series Linear Regression Forecast – UK and US")

# Debug: Check working directory and output directory
cat("Current working directory:", getwd(), "\n")
if (!dir.exists("output/plots")) {
  dir.create("output/plots", recursive = TRUE)
  cat("Created output/plots directory\n")
}
cat("Files in output/plots/ before running Step 5:\n")
print(list.files("output/plots"))

# Debug: Verify input data
cat("uk_ts exists:", exists("uk_ts"), "\n")
cat("us_ts exists:", exists("us_ts"), "\n")
if (exists("uk_ts")) {
  cat("Summary of uk_ts:\n")
  print(summary(uk_ts))
} else {
  stop("uk_ts is not defined. Ensure Step 1 has run successfully.")
}
if (exists("us_ts")) {
  cat("Summary of us_ts:\n")
  print(summary(us_ts))
} else {
  stop("us_ts is not defined. Ensure Step 1 has run successfully.")
}

plot_lm_model_with_labels_single <- function(ts_data, label = "UK") {
  # Convert ts to data frame
  df <- data.frame(
    Time = time(ts_data),
    Value = as.numeric(ts_data)
  )
  
  # Debug: Check data
  cat("Label:", label, "\n")
  cat("nrow(df):", nrow(df), "\n")
  cat("Time range:", range(df$Time), "\n")
  
  # Define training and validation periods
  train_end <- which(df$Time >= 2001)[1] - 1
  valid_end <- which(df$Time >= 2004)[1] - 1
  
  # Check if splits are valid
  if (is.na(train_end) || is.na(valid_end) || train_end <= 0 || valid_end <= train_end) {
    stop("Invalid data split for ", label, ": train_end = ", train_end, ", valid_end = ", valid_end)
  }
  
  # Split data
  train <- df[1:train_end, ]
  valid <- df[(train_end + 1):valid_end, ]
  future <- df[(valid_end + 1):nrow(df), ]
  
  # Debug: Check split sizes
  cat("nrow(train):", nrow(train), "\n")
  cat("nrow(valid):", nrow(valid), "\n")
  cat("nrow(future):", nrow(future), "\n")
  
  # Fit linear model
  lm_model <- lm(Value ~ Time, data = train)
  
  # Predictions
  valid$Predicted <- predict(lm_model, newdata = valid)
  future$Predicted <- predict(lm_model, newdata = future)
  
  # Assign set labels
  train$Set <- "Training"
  valid$Set <- "Validation"
  future$Set <- "Future"
  
  # Calculate y-axis limits
  y_min <- min(df$Value, na.rm = TRUE)
  y_max <- max(df$Value, na.rm = TRUE)
  y_pad <- (y_max - y_min) * 0.15
  
  # Forecast Plot
  p_forecast <- ggplot() +
    geom_line(data = df, aes(x = Time, y = Value), color = "black") +
    geom_line(data = train, aes(x = Time, y = Value), color = "blue", size = 0.6) +
    geom_line(data = valid, aes(x = Time, y = Predicted), color = "skyblue", size = 0.8) +
    geom_line(data = future, aes(x = Time, y = Predicted), color = "orange", size = 0.8) +
    geom_vline(xintercept = df$Time[c(train_end, valid_end)], linetype = "dashed") +
    annotate("text", x = mean(train$Time), y = y_max + y_pad, label = "Training", size = 4, fontface = "bold") +
    annotate("text", x = mean(valid$Time), y = y_max + y_pad, label = "Validation", size = 4, fontface = "bold") +
    annotate("text", x = mean(future$Time), y = y_max + y_pad, label = "Future", size = 4, fontface = "bold") +
    ylim(y_min, y_max + y_pad * 1.5) +
    labs(title = paste("Time Series Linear Regression Forecast -", label),
         x = "Time", y = paste(label, "Index")) +
    theme_minimal()
  
  # Residuals Plot
  train$Residuals <- resid(lm_model)
  valid$Residuals <- valid$Value - valid$Predicted
  residuals_df <- bind_rows(train[, c("Time", "Residuals")], valid[, c("Time", "Residuals")])
  
  # Calculate residuals limits
  res_min <- min(residuals_df$Residuals, na.rm = TRUE)
  res_max <- max(residuals_df$Residuals, na.rm = TRUE)
  res_pad <- (res_max - res_min) * 0.15
  
  p_resid <- ggplot(residuals_df, aes(x = Time, y = Residuals)) +
    geom_line(color = "black") +
    geom_vline(xintercept = df$Time[c(train_end, valid_end)], linetype = "dashed") +
    annotate("text", x = mean(train$Time), y = res_max + res_pad, label = "Training", size = 4, fontface = "bold") +
    annotate("text", x = mean(valid$Time), y = res_max + res_pad, label = "Validation", size = 4, fontface = "bold") +
    labs(title = paste("Residuals from Linear Model -", label),
         x = "Time", y = "Residuals") +
    ylim(res_min, res_max + res_pad * 1.5) +
    theme_minimal()
  
  # Combine plots
  arranged_plot <- grid.arrange(p_forecast, p_resid, nrow = 2)
  return(arranged_plot)
}

# --- Generate and Save Plots ---
# UK
tryCatch({
  uk_lm_plot <- plot_lm_model_with_labels_single(uk_ts, label = "UK")
  save_plot1("uk_linear_model_forecast.png", uk_lm_plot, width = 1000, height = 800)
  cat("UK plot exists:", file.exists("output/plots/uk_linear_model_forecast.png"), "\n")
}, error = function(e) {
  cat("Error generating UK plot:", conditionMessage(e), "\n")
})

# US
tryCatch({
  us_lm_plot <- plot_lm_model_with_labels_single(us_ts, label = "US")
  save_plot1("us_linear_model_forecast.png", us_lm_plot, width = 1000, height = 800)
  cat("US plot exists:", file.exists("output/plots/us_linear_model_forecast.png"), "\n")
}, error = function(e) {
  cat("Error generating US plot:", conditionMessage(e), "\n")
})


#********************* Step 6: Model Comparison – Metrics ***********************#
#*
log_section("Step 6: Model Comparison – Metrics")

compare_models <- function(actual, model, label) {
  fc <- forecast(model, h = 12)
  fitted_vals <- fitted(model)
  resids <- residuals(model)
  
  tibble(
    Model = label,
    AIC = AIC(model),
    BIC = BIC(model),
    RMSE = rmse(actual, fitted_vals),
    MAE = mae(actual, fitted_vals),
    MAPE = mape(actual, fitted_vals) * 100
  )
}

uk_metrics_arima <- compare_models(uk_ts, fit_arima_uk, "UK_ARIMA")
uk_metrics_ets <- compare_models(uk_ts, fit_ets_uk, "UK_ETS")
us_metrics_arima <- compare_models(us_ts, fit_arima_us, "US_ARIMA")
us_metrics_ets <- compare_models(us_ts, fit_ets_us, "US_ETS")

model_comparison <- bind_rows(uk_metrics_arima, uk_metrics_ets, us_metrics_arima, us_metrics_ets)
print(model_comparison)
write_to_csv(model_comparison, "step6_model_comparison_metrics.csv")

##*********************** Step 7: Final Forecast Comparison Plots ******************************##
##*
log_section("Step 7: Final Forecast Visualizations")

uk_combined_plot <- autoplot(uk_ts) +
  autolayer(fc_arima_uk$mean, series = "ARIMA Forecast", color = "blue") +
  autolayer(fc_ets_uk$mean, series = "ETS Forecast", color = "red") +
  labs(title = "UK Industrial Production – ARIMA vs ETS Forecast",
       x = "Time", y = "Index") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()

save_plot("step7_uk_forecast_comparison.png", uk_combined_plot)

us_combined_plot <- autoplot(us_ts) +
  autolayer(fc_arima_us$mean, series = "ARIMA Forecast", color = "blue") +
  autolayer(fc_ets_us$mean, series = "ETS Forecast", color = "red") +
  labs(title = "US Industrial Production – ARIMA vs ETS Forecast",
       x = "Time", y = "Index") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()

save_plot("step7_us_forecast_comparison.png", us_combined_plot)

#**************** Step 8: US Sector-Level EDA ******************************#

log_section("Step 8: US Sector-Level Data Cleaning (Updated)")

# Load US data
us_file <- "data/US_IndexOfProduction.csv"
if (!file.exists(us_file)) stop("US data file not found:", us_file)

# Debug: Inspect column names and first rows
us_raw <- read_csv(us_file, col_names = TRUE, na = c("", "NA", "."), n_max = 5)
cat("Column names (first 20):\n")
cat(paste(names(us_raw)[1:20], collapse = ", "), "\n")
cat("First 5 rows:\n")
print(head(us_raw, 5))

# Load full dataset
us_raw <- read_csv(us_file, col_names = TRUE, na = c("", "NA", "."), skip = 0)

# Debug: Print all series IDs
cat("Available series IDs:\n")
cat(paste(sort(unique(us_raw$`Series Name:`)), collapse = ", "), "\n")

# Verify series column exists
if (!"Series Name:" %in% names(us_raw)) {
  stop("Column 'Series Name:' not found; check CSV structure")
}

# US sector mapping (same as before)
us_sector_map <- tibble(
  Series_Name = c(
    "IP.B50001.S",  # Total Index
    "IP.G2211A2.S", "IP.G2211.S", "IP.G2212.S",  # Utilities
    "IP.B50030.S",  # Final Products
    "IP.B51000.S", "IP.B51100.S", "IP.B51110.S",  # Consumer Goods
    "IP.B53000.S", "IP.B53100.S", "IP.B53200.S",  # Manufacturing
    "IP.G21.S",     # Mining
    "IP.B52000.S"   # Business Equipment
  ),
  Short_Sector = c(
    "Total Index",
    "Electric and Gas Utilities", "Electric Power", "Natural Gas",
    "Final Products",
    "Consumer Goods", "Durable Consumer Goods", "Automotive Products",
    "Manufacturing Total", "Durable Manufacturing", "Nondurable Manufacturing",
    "Mining",
    "Business Equipment"
  ),
  Group = c(
    "Total",
    "Utilities", "Utilities", "Utilities",
    "Products",
    "Consumer Goods", "Consumer Goods", "Consumer Goods",
    "Manufacturing", "Manufacturing", "Manufacturing",
    "Mining",
    "Equipment"
  )
)

# Detect date columns
ts_cols <- grep("^\\d{4}-\\d{2}$", names(us_raw), value = TRUE)
if (length(ts_cols) == 0) {
  original_names <- names(us_raw)
  names(us_raw) <- make.names(names(us_raw), unique = TRUE)
  ts_cols <- grep("^X?\\d{4}\\.\\d{2}$", names(us_raw), value = TRUE)
  if (length(ts_cols) == 0) {
    stop("No YYYY-MM or transformed date columns found; check data structure")
  }
  ts_cols <- original_names[match(ts_cols, names(us_raw))]
}

cat("Detected date columns (first 10):\n")
cat(paste(head(ts_cols, 10), collapse = ", "), "\n")

# Pivot to long format (raw indices)
us_long <- us_raw %>%
  rename(series_name = `Series Name:`) %>%
  select(series_name, all_of(ts_cols)) %>%
  pivot_longer(cols = all_of(ts_cols), names_to = "Period", values_to = "Value") %>%
  mutate(Date = as.Date(paste0(Period, "-01"), format = "%Y-%m-%d")) %>%
  left_join(us_sector_map, by = c("series_name" = "Series_Name")) %>%
  filter(!is.na(Value), !is.na(Short_Sector), !is.na(Date), Date >= as.Date("1948-01-01")) %>%
  select(Date, Short_Sector, Group, Value)

# Calculate growth rates
us_growth <- us_long %>%
  group_by(Short_Sector, Group) %>%
  arrange(Date) %>%
  mutate(
    # Month-over-month growth (momy gr)
    Value_1mo_lag = lag(Value, n = 1),
    momy_gr = (Value / Value_1mo_lag - 1) * 100,
    # 3-month-over-3-month year-over-year growth (3mo3my gr)
    Value_3mo_lag = lag(Value, n = 3),
    Value_15mo_lag = lag(Value, n = 15),
    three_mo_avg = (Value + lag(Value, 1) + lag(Value, 2)) / 3,
    three_mo_avg_1yr_ago = (Value_15mo_lag + lag(Value_15mo_lag, 1) + lag(Value_15mo_lag, 2)) / 3,
    three_mo_yr_gr = (three_mo_avg / three_mo_avg_1yr_ago - 1) * 100,
    # 3-month-over-3-month growth (3mo3m gr)
    three_mo_avg_3mo_ago = (Value_3mo_lag + lag(Value_3mo_lag, 1) + lag(Value_3mo_lag, 2)) / 3,
    three_mo_gr = (three_mo_avg / three_mo_avg_3mo_ago - 1) * 100
  ) %>%
  ungroup()

# Reshape to long format including growth rates
us_long_with_growth <- us_growth %>%
  pivot_longer(
    cols = c(Value, momy_gr, three_mo_yr_gr, three_mo_gr),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Short_Sector = case_when(
      Metric == "Value" ~ Short_Sector,
      Metric == "momy_gr" ~ paste0(Short_Sector, " Growth"),
      Metric == "three_mo_yr_gr" ~ paste0(Short_Sector, " 3mo3my Growth"),
      Metric == "three_mo_gr" ~ paste0(Short_Sector, " 3mo3m Growth")
    ),
    Group = case_when(
      Metric == "Value" ~ Group,
      Metric == "momy_gr" ~ paste0(Group, " Growth"),
      Metric == "three_mo_yr_gr" ~ paste0(Group, " 3mo3my Growth"),
      Metric == "three_mo_gr" ~ paste0(Group, " 3mo3m Growth")
    )
  ) %>%
  filter(!is.na(Value)) %>%
  select(Date, Short_Sector, Group, Value)

write_to_csv(us_long_with_growth, "step8_us_sector_long_grouped_with_growth.csv")
write_to_csv(us_sector_map, "step8_us_sector_map.csv")

# Summary statistics
us_sector_summary <- us_long_with_growth %>%
  group_by(Short_Sector, Group) %>%
  summarise(
    Observations = sum(!is.na(Value)),
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = suppressWarnings(min(Value, na.rm = TRUE)),
    Max = suppressWarnings(max(Value, na.rm = TRUE)),
    Median = median(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Min = ifelse(is.infinite(Min), NA, Min),
    Max = ifelse(is.infinite(Max), NA, Max)
  )
write_to_csv(us_sector_summary, "step8_us_sector_summary_stats_with_growth.csv")

# Trend plots (separate for indices and growth rates)
# Plot for raw indices
us_index_data <- us_long_with_growth %>%
  filter(!str_detect(Group, "Growth"))
if (nrow(us_index_data) > 0) {
  us_index_plot <- ggplot(us_index_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "US Sector Trends (1948–2025) - Raw Indices", x = "Date", y = "Index") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step8_us_sector_trends_by_group_indices.png", us_index_plot, width = 1200, height = 900, res = 150)
}

# Plot for growth rates (momy gr)
us_momy_growth_data <- us_long_with_growth %>%
  filter(str_detect(Group, "Growth") & !str_detect(Group, "3mo3my|3mo3m"))
if (nrow(us_momy_growth_data) > 0) {
  us_momy_growth_plot <- ggplot(us_momy_growth_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "US Sector Growth Rates (1948–2025) - Month-over-Month", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step8_us_sector_momy_growth.png", us_momy_growth_plot, width = 1200, height = 900, res = 150)
}

# Plot for growth rates (3mo3my gr)
us_three_mo_yearly_growth_data <- us_long_with_growth %>%
  filter(str_detect(Group, "3mo3my Growth"))
if (nrow(us_three_mo_yearly_growth_data) > 0) {
  us_three_mo_yearly_growth_plot <- ggplot(us_three_mo_yearly_growth_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "US Sector Growth Rates (1948–2025) - 3-Month-over-3-Month Year-over-Year", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step8_us_sector_3mo3my_growth.png", us_three_mo_yearly_growth_plot, width = 1200, height = 900, res = 150)
}

# Plot for growth rates (3mo3m gr)
us_three_mo_growth_data <- us_long_with_growth %>%
  filter(str_detect(Group, "3mo3m Growth"))
if (nrow(us_three_mo_growth_data) > 0) {
  us_three_mo_growth_plot <- ggplot(us_three_mo_growth_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "US Sector Growth Rates (1948–2025) - 3-Month-over-3-Month", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step8_us_sector_3mo3m_growth.png", us_three_mo_growth_plot, width = 1200, height = 900, res = 150)
}

# Histogram and Q-Q plots for expanded key sectors
key_sectors <- c("Total Index", "US Electric and Gas Utilities", "US Manufacturing Total", "US Mining")
for (sector in key_sectors) {
  sector_data <- us_long_with_growth %>% filter(Short_Sector == sector)
  if (nrow(sector_data) > 0) {
    hist_plot <- ggplot(sector_data, aes(x = Value)) +
      geom_histogram(fill = "lightblue", color = "black", bins = 30) +
      labs(title = paste("Histogram:", sector), x = "Index", y = "Frequency") +
      theme_minimal()
    save_plot(paste0("step8_us_histogram_", tolower(gsub(" ", "_", sector)), ".png"), hist_plot)
    
    qq_plot <- ggplot(sector_data, aes(sample = Value)) +
      stat_qq() +
      stat_qq_line(color = "red") +
      labs(title = paste("Q-Q Plot:", sector), x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_minimal()
    save_plot(paste0("step8_us_qq_", tolower(gsub(" ", "_", sector)), ".png"), qq_plot)
  } else {
    cat("No data for sector:", sector, "\n")
  }
}

# Diagnostics
us_diagnostics <- us_long_with_growth %>%
  group_by(Short_Sector, Group) %>%
  summarise(
    ValidRows = sum(!is.na(Value) & !is.na(Date)),
    MinDate = min(Date, na.rm = TRUE),
    MaxDate = max(Date, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    MissingValues = sum(is.na(Value)),
    .groups = "drop"
  )
write_to_csv(us_diagnostics, "step8_us_sector_diagnostics_with_growth.csv")

#********************** Step 9: UK Sector-Level EDA *************************#
log_section("Step 9: UK Sector-Level Data Cleaning (Final)")

# Load UK data
uk_file <- "data/UKIndexOfProduction-15Aug2024.csv"
if (!file.exists(uk_file)) stop("UK data file not found:", uk_file)
uk_raw <- read_csv(uk_file, col_names = TRUE)

# Debug: Inspect raw data
cat("First 10 rows of raw UK data:\n")
print(head(uk_raw, 10))
cat("Last 5 rows of raw UK data:\n")
print(tail(uk_raw, 5))

# Check non-NA data for key sectors
cat("Rows with non-NA data for Manufacturing Total:\n")
print(sum(!is.na(uk_raw$`IOP: C:MANUFACTURING: CVMSA`)))
cat("Rows with non-NA data for Pharmaceuticals (CVMSA):\n")
print(sum(!is.na(uk_raw$`IOP: 21:Manuf Of Basic Pharmaceutical Prods & Pharmaceutical Preparation:CVMNSA`)))

# UK sector mapping (unchanged, for reference)
uk_sector_map <- tibble(
  Code = make.names(c(
    "IOP: C:MANUFACTURING: CVMSA",
    "IOP: 21:Manuf Of Basic Pharmaceutical Prods & Pharmaceutical Preparation:CVMNSA",
    "IOP: 35:Electricity Gas Steam And Air Conditioning Supply: CVMSA",
    "IOP: B:MINING AND QUARRYING: CVMSA",
    "MIG.COG.Main.Industrial.Groupings...Consumer.Goods..CVM..momy.gr...664",
    "MIG.CND.Main.Industrial.Groupings..Consumer.Non.Durables.CVM.momy.gr...666",
    "CC.Manufacture.of.Wood...Paper.Products.and.Printing..CVM..momy.gr...602",
    "B.MINING.AND.QUARRYING..CVM..momy.gr...583",
    "MIG.COG.Main.Industrial.Groupings...Consumer.Goods.CVM.3mo3my.gr...842",
    "MIG.CND.Main.Industrial.Groupings..Consumer.Non.Durables.CVM.3mo3my.gr...844",
    "CC.Manufacture.of.Wood...Paper.Products.and.Printing..CVM.3mo3my.gr...780",
    "B.MINING.AND.QUARRYING..CVM.3mo3my.gr...761",
    "MIG.COG.Main.Industrial.Groupings...Consumer.Goods..CVM.3mo3m.gr...931",
    "MIG.CND.Main.Industrial.Groupings..Consumer.Non.Durables.CVM.3mo3m.gr...933",
    "CC.Manufacture.of.Wood...Paper.Products.and.Printing..CVM.3mo3m.gr...958",
    "B.MINING.AND.QUARRYING..CVM.3mo3m.gr...939",
    "CE.Manufacture.of.Chemicals.and.Chemical.Products..CVM..momy.gr...608",
    "CE.Manufacture.of.Chemicals.and.Chemical.Products.CVM.3mo3my.gr...786",
    "CE.Manufacture.of.Chemicals.and.Chemical.Products.CVM.3mo3m.gr...964",
    "EAI.Engineering.and.Allied.Industries..CVM..momy.gr...660",
    "EAI.Engineering.and.Allied.Industries.CVM.3mo3my.gr...838",
    "EAI.Engineering.and.Allied.Industries.CVM.3mo3m.gr...1016"
  )),
  Short_Sector = c(
    "Manufacturing Total",
    "Pharmaceuticals",
    "Electric and Gas Utilities",
    "Mining",
    "MIG Consumer Goods Growth",
    "MIG Consumer Non-Durables Growth",
    "Wood and Paper Products Growth",
    "Mining Growth",
    "MIG Consumer Goods 3mo3my Growth",
    "MIG Consumer Non-Durables 3mo3my Growth",
    "Wood and Paper Products 3mo3my Growth",
    "Mining 3mo3my Growth",
    "MIG Consumer Goods 3mo3m Growth",
    "MIG Consumer Non-Durables 3mo3m Growth",
    "Wood and Paper Products 3mo3m Growth",
    "Mining 3mo3m Growth",
    "Chemicals Growth",
    "Chemicals 3mo3my Growth",
    "Chemicals 3mo3m Growth",
    "Engineering and Allied Industries Growth",
    "Engineering and Allied Industries 3mo3my Growth",
    "Engineering and Allied Industries 3mo3m Growth"
  ),
  Group = c(
    "Manufacturing",
    "Manufacturing",
    "Utilities",
    "Mining",
    "Consumer Goods Growth",
    "Consumer Goods Growth",
    "Manufacturing Growth",
    "Mining Growth",
    "Consumer Goods 3mo3my Growth",
    "Consumer Goods 3mo3my Growth",
    "Manufacturing 3mo3my Growth",
    "Mining 3mo3my Growth",
    "Consumer Goods 3mo3m Growth",
    "Consumer Goods 3mo3m Growth",
    "Manufacturing 3mo3m Growth",
    "Mining 3mo3m Growth",
    "Chemicals Growth",
    "Chemicals 3mo3my Growth",
    "Chemicals 3mo3m Growth",
    "Aggregate Growth",
    "Aggregate 3mo3my Growth",
    "Aggregate 3mo3m Growth"
  )
)

# Normalize column names
colnames(uk_raw) <- make.names(colnames(uk_raw))

# Numerical data (full time series)
data_clean <- uk_raw %>%
  rename(Period = Title) %>%
  filter(!is.na(Period), Period != "") %>%
  mutate(
    Period = str_trim(Period),
    Date = as.Date(paste0("01 ", Period), format = "%d %Y %b")
  ) %>%
  filter(!is.na(Date)) %>%
  select(-Period) %>%
  mutate(across(.cols = -Date, .fns = as.numeric))

# Debug
cat("Number of rows in data_clean:", nrow(data_clean), "\n")
cat("Date range in data_clean:\n")
print(range(data_clean$Date, na.rm = TRUE))

# Reshape to long format
uk_long <- data_clean %>%
  pivot_longer(cols = -Date, names_to = "Code", values_to = "Value") %>%
  left_join(uk_sector_map, by = "Code") %>%
  filter(!is.na(Short_Sector), !is.na(Value))

# Debug
cat("Number of rows in uk_long:", nrow(uk_long), "\n")
cat("Unique Short_Sector values in uk_long:\n")
cat(paste(unique(uk_long$Short_Sector), collapse = ", "), "\n")
cat("Missing values per sector:\n")
print(uk_long %>%
        group_by(Short_Sector) %>%
        summarise(NA_Count = sum(is.na(Value)), Total_Rows = n(), NA_Percentage = NA_Count / Total_Rows * 100))

uk_long <- uk_long %>%
  select(Date, Short_Sector, Group, Value)

write_to_csv(uk_long, "step9_uk_sector_long_grouped_final.csv")
write_to_csv(uk_sector_map, "step9_uk_sector_map_final.csv")

# Summary statistics
uk_sector_summary <- uk_long %>%
  group_by(Short_Sector, Group) %>%
  summarise(
    Observations = sum(!is.na(Value)),
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = suppressWarnings(min(Value, na.rm = TRUE)),
    Max = suppressWarnings(max(Value, na.rm = TRUE)),
    Median = median(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Min = ifelse(is.infinite(Min), NA, Min),
    Max = ifelse(is.infinite(Max), NA, Max)
  )
write_to_csv(uk_sector_summary, "step9_uk_sector_summary_stats_final.csv")

# Plot for raw indices
index_data <- uk_long %>%
  filter(!str_detect(Group, "Growth"))
if (nrow(index_data) > 0) {
  index_plot <- ggplot(index_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "UK Sector Trends (1948–2024) - Raw Indices", x = "Date", y = "Index") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step9_uk_sector_trends_by_group_final_indices.png", index_plot, width = 1200, height = 900, res = 150)
}

# Histogram and Q-Q plots for key sectors
key_sectors <- c("Manufacturing Total", "Electric and Gas Utilities", "Mining")
for (sector in key_sectors) {
  sector_data <- uk_long %>% filter(Short_Sector == sector)
  if (nrow(sector_data) > 0) {
    hist_plot <- ggplot(sector_data, aes(x = Value)) +
      geom_histogram(fill = "lightblue", color = "black", bins = 30) +
      labs(title = paste("Histogram:", sector), x = "Index", y = "Frequency") +
      theme_minimal()
    save_plot(paste0("step9_uk_histogram_", tolower(gsub(" ", "_", sector)), ".png"), hist_plot)
    
    qq_plot <- ggplot(sector_data, aes(sample = Value)) +
      stat_qq() +
      stat_qq_line(color = "red") +
      labs(title = paste("Q-Q Plot:", sector), x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_minimal()
    save_plot(paste0("step9_uk_qq_", tolower(gsub(" ", "_", sector)), ".png"), qq_plot)
  } else {
    cat("No data for sector:", sector, "\n")
  }
}

# Plot for growth rates (momy gr) - Chemical-related sectors
chemical_momy_growth_data <- uk_long %>%
  filter(str_detect(Group, "Growth") & !str_detect(Group, "3mo3my|3mo3m"),
         str_detect(Short_Sector, "Chemicals|Paints and Varnishes|Coke and Refined Petroleum"))
if (nrow(chemical_momy_growth_data) > 0) {
  chemical_momy_growth_plot <- ggplot(chemical_momy_growth_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "UK Chemical Sector Growth Rates (1948–2024) - Month-over-Month", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step9_uk_chemical_sector_momy_growth_final.png", chemical_momy_growth_plot, width = 1200, height = 900, res = 150)
}

# Plot for growth rates (momy gr) - Consumer goods sectors
consumer_momy_growth_data <- uk_long %>%
  filter(str_detect(Group, "Growth") & !str_detect(Group, "3mo3my|3mo3m"),
         str_detect(Short_Sector, "MIG Consumer Goods|MIG Consumer Non-Durables|Soft Drinks|Textiles and Apparel"))
if (nrow(consumer_momy_growth_data) > 0) {
  consumer_momy_growth_plot <- ggplot(consumer_momy_growth_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "UK Consumer Goods Growth Rates (1948–2024) - Month-over-Month", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step9_uk_consumer_sector_momy_growth_final.png", consumer_momy_growth_plot, width = 1200, height = 900, res = 150)
}

# Plot for growth rates (momy gr) - Traditional manufacturing sectors
traditional_momy_growth_data <- uk_long %>%
  filter(str_detect(Group, "Growth") & !str_detect(Group, "3mo3my|3mo3m"),
         str_detect(Short_Sector, "Wood and Paper Products"))
if (nrow(traditional_momy_growth_data) > 0) {
  traditional_momy_growth_plot <- ggplot(traditional_momy_growth_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "UK Traditional Manufacturing Growth Rates (1948–2024) - Month-over-Month", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step9_uk_traditional_sector_momy_growth_final.png", traditional_momy_growth_plot, width = 1200, height = 900, res = 150)
}

# Plot for growth rates (momy gr) - Mining sectors
mining_momy_growth_data <- uk_long %>%
  filter(str_detect(Group, "Growth") & !str_detect(Group, "3mo3my|3mo3m"),
         str_detect(Short_Sector, "Mining|Coal and Lignite Mining|Crude Petroleum and Natural Gas Extraction|Other Mining and Support Services"))
if (nrow(mining_momy_growth_data) > 0) {
  mining_momy_growth_plot <- ggplot(mining_momy_growth_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "UK Mining Sector Growth Rates (1948–2024) - Month-over-Month", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step9_uk_mining_sector_momy_growth_final.png", mining_momy_growth_plot, width = 1200, height = 900, res = 150)
}

# Plot for growth rates (momy gr) - Aggregate sectors
aggregate_momy_growth_data <- uk_long %>%
  filter(str_detect(Group, "Growth") & !str_detect(Group, "3mo3my|3mo3m"),
         str_detect(Short_Sector, "MIG|Engineering and Allied Industries|Mining, Quarrying, Energy and Water"))
if (nrow(aggregate_momy_growth_data) > 0) {
  aggregate_momy_growth_plot <- ggplot(aggregate_momy_growth_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "UK Aggregate Sector Growth Rates (1948–2024) - Month-over-Month", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step9_uk_aggregate_sector_momy_growth_final.png", aggregate_momy_growth_plot, width = 1200, height = 900, res = 150)
}

# Plot for growth rates (momy gr) - Other sectors
other_momy_growth_data <- uk_long %>%
  filter(str_detect(Group, "Growth") & !str_detect(Group, "3mo3my|3mo3m"),
         !str_detect(Short_Sector, "Chemicals|Paints and Varnishes|Coke and Refined Petroleum|MIG Consumer|Soft Drinks|Textiles and Apparel|Wood and Paper Products|Mining|Coal and Lignite Mining|Crude Petroleum and Natural Gas Extraction|Other Mining and Support Services|MIG|Engineering and Allied Industries|Mining, Quarrying, Energy and Water"))
if (nrow(other_momy_growth_data) > 0) {
  other_momy_growth_plot <- ggplot(other_momy_growth_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "UK Other Sector Growth Rates (1948–2024) - Month-over-Month", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step9_uk_other_sector_momy_growth_final.png", other_momy_growth_plot, width = 1200, height = 900, res = 150)
}

# Plot for growth rates (3mo3my gr) - Chemical-related sectors
chemical_three_mo_yearly_growth_data <- uk_long %>%
  filter(str_detect(Group, "3mo3my Growth"),
         str_detect(Short_Sector, "Chemicals|Paints and Varnishes|Coke and Refined Petroleum"))
if (nrow(chemical_three_mo_yearly_growth_data) > 0) {
  chemical_three_mo_yearly_growth_plot <- ggplot(chemical_three_mo_yearly_growth_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "UK Chemical Sector Growth Rates (1948–2024) - 3-Month-over-3-Month Year-over-Year", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step9_uk_chemical_sector_3mo3my_growth_final.png", chemical_three_mo_yearly_growth_plot, width = 1200, height = 900, res = 150)
}

# Plot for growth rates (3mo3my gr) - Consumer goods sectors
consumer_three_mo_yearly_growth_data <- uk_long %>%
  filter(str_detect(Group, "3mo3my Growth"),
         str_detect(Short_Sector, "MIG Consumer Goods|MIG Consumer Non-Durables|Soft Drinks|Textiles and Apparel"))
if (nrow(consumer_three_mo_yearly_growth_data) > 0) {
  consumer_three_mo_yearly_growth_plot <- ggplot(consumer_three_mo_yearly_growth_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "UK Consumer Goods Growth Rates (1948–2024) - 3-Month-over-3-Month Year-over-Year", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step9_uk_consumer_sector_3mo3my_growth_final.png", consumer_three_mo_yearly_growth_plot, width = 1200, height = 900, res = 150)
}

# Plot for growth rates (3mo3my gr) - Traditional manufacturing sectors
traditional_three_mo_yearly_growth_data <- uk_long %>%
  filter(str_detect(Group, "3mo3my Growth"),
         str_detect(Short_Sector, "Wood and Paper Products"))
if (nrow(traditional_three_mo_yearly_growth_data) > 0) {
  traditional_three_mo_yearly_growth_plot <- ggplot(traditional_three_mo_yearly_growth_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "UK Traditional Manufacturing Growth Rates (1948–2024) - 3-Month-over-3-Month Year-over-Year", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step9_uk_traditional_sector_3mo3my_growth_final.png", traditional_three_mo_yearly_growth_plot, width = 1200, height = 900, res = 150)
}

# Plot for growth rates (3mo3my gr) - Mining sectors
mining_three_mo_yearly_growth_data <- uk_long %>%
  filter(str_detect(Group, "3mo3my Growth"),
         str_detect(Short_Sector, "Mining|Coal and Lignite Mining|Crude Petroleum and Natural Gas Extraction|Other Mining and Support Services"))
if (nrow(mining_three_mo_yearly_growth_data) > 0) {
  mining_three_mo_yearly_growth_plot <- ggplot(mining_three_mo_yearly_growth_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "UK Mining Sector Growth Rates (1948–2024) - 3-Month-over-3-Month Year-over-Year", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step9_uk_mining_sector_3mo3my_growth_final.png", mining_three_mo_yearly_growth_plot, width = 1200, height = 900, res = 150)
}

# Plot for growth rates (3mo3my gr) - Aggregate sectors
aggregate_three_mo_yearly_growth_data <- uk_long %>%
  filter(str_detect(Group, "3mo3my Growth"),
         str_detect(Short_Sector, "MIG|Engineering and Allied Industries|Mining, Quarrying, Energy and Water"))
if (nrow(aggregate_three_mo_yearly_growth_data) > 0) {
  aggregate_three_mo_yearly_growth_plot <- ggplot(aggregate_three_mo_yearly_growth_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "UK Aggregate Sector Growth Rates (1948–2024) - 3-Month-over-3-Month Year-over-Year", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step9_uk_aggregate_sector_3mo3my_growth_final.png", aggregate_three_mo_yearly_growth_plot, width = 1200, height = 900, res = 150)
}

# Plot for growth rates (3mo3my gr) - Other sectors
other_three_mo_yearly_growth_data <- uk_long %>%
  filter(str_detect(Group, "3mo3my Growth"),
         !str_detect(Short_Sector, "Chemicals|Paints and Varnishes|Coke and Refined Petroleum|MIG Consumer|Soft Drinks|Textiles and Apparel|Wood and Paper Products|Mining|Coal and Lignite Mining|Crude Petroleum and Natural Gas Extraction|Other Mining and Support Services|MIG|Engineering and Allied Industries|Mining, Quarrying, Energy and Water"))
if (nrow(other_three_mo_yearly_growth_data) > 0) {
  other_three_mo_yearly_growth_plot <- ggplot(other_three_mo_yearly_growth_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "UK Other Sector Growth Rates (1948–2024) - 3-Month-over-3-Month Year-over-Year", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step9_uk_other_sector_3mo3my_growth_final.png", other_three_mo_yearly_growth_plot, width = 1200, height = 900, res = 150)
}

# Plot for growth rates (3mo3m gr) - Chemical-related sectors
chemical_three_mo_growth_data <- uk_long %>%
  filter(str_detect(Group, "3mo3m Growth"),
         str_detect(Short_Sector, "Chemicals|Paints and Varnishes|Coke and Refined Petroleum"))
if (nrow(chemical_three_mo_growth_data) > 0) {
  chemical_three_mo_growth_plot <- ggplot(chemical_three_mo_growth_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "UK Chemical Sector Growth Rates (1948–2024) - 3-Month-over-3-Month", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step9_uk_chemical_sector_3mo3m_growth_final.png", chemical_three_mo_growth_plot, width = 1200, height = 900, res = 150)
}

# Plot for growth rates (3mo3m gr) - Consumer goods sectors
consumer_three_mo_growth_data <- uk_long %>%
  filter(str_detect(Group, "3mo3m Growth"),
         str_detect(Short_Sector, "MIG Consumer Goods|MIG Consumer Non-Durables|Soft Drinks|Textiles and Apparel"))
if (nrow(consumer_three_mo_growth_data) > 0) {
  consumer_three_mo_growth_plot <- ggplot(consumer_three_mo_growth_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "UK Consumer Goods Growth Rates (1948–2024) - 3-Month-over-3-Month", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step9_uk_consumer_sector_3mo3m_growth_final.png", consumer_three_mo_growth_plot, width = 1200, height = 900, res = 150)
}

# Plot for growth rates (3mo3m gr) - Traditional manufacturing sectors
traditional_three_mo_growth_data <- uk_long %>%
  filter(str_detect(Group, "3mo3m Growth"),
         str_detect(Short_Sector, "Wood and Paper Products"))
if (nrow(traditional_three_mo_growth_data) > 0) {
  traditional_three_mo_growth_plot <- ggplot(traditional_three_mo_growth_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "UK Traditional Manufacturing Growth Rates (1948–2024) - 3-Month-over-3-Month", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step9_uk_traditional_sector_3mo3m_growth_final.png", traditional_three_mo_growth_plot, width = 1200, height = 900, res = 150)
}

# Plot for growth rates (3mo3m gr) - Mining sectors
mining_three_mo_growth_data <- uk_long %>%
  filter(str_detect(Group, "3mo3m Growth"),
         str_detect(Short_Sector, "Mining|Coal and Lignite Mining|Crude Petroleum and Natural Gas Extraction|Other Mining and Support Services"))
if (nrow(mining_three_mo_growth_data) > 0) {
  mining_three_mo_growth_plot <- ggplot(mining_three_mo_growth_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "UK Mining Sector Growth Rates (1948–2024) - 3-Month-over-3-Month", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step9_uk_mining_sector_3mo3m_growth_final.png", mining_three_mo_growth_plot, width = 1200, height = 900, res = 150)
}

# Plot for growth rates (3mo3m gr) - Aggregate sectors
aggregate_three_mo_growth_data <- uk_long %>%
  filter(str_detect(Group, "3mo3m Growth"),
         str_detect(Short_Sector, "MIG|Engineering and Allied Industries|Mining, Quarrying, Energy and Water"))
if (nrow(aggregate_three_mo_growth_data) > 0) {
  aggregate_three_mo_growth_plot <- ggplot(aggregate_three_mo_growth_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "UK Aggregate Sector Growth Rates (1948–2024) - 3-Month-over-3-Month", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step9_uk_aggregate_sector_3mo3m_growth_final.png", aggregate_three_mo_growth_plot, width = 1200, height = 900, res = 150)
}

# Plot for growth rates (3mo3m gr) - Other sectors
other_three_mo_growth_data <- uk_long %>%
  filter(str_detect(Group, "3mo3m Growth"),
         !str_detect(Short_Sector, "Chemicals|Paints and Varnishes|Coke and Refined Petroleum|MIG Consumer|Soft Drinks|Textiles and Apparel|Wood and Paper Products|Mining|Coal and Lignite Mining|Crude Petroleum and Natural Gas Extraction|Other Mining and Support Services|MIG|Engineering and Allied Industries|Mining, Quarrying, Energy and Water"))
if (nrow(other_three_mo_growth_data) > 0) {
  other_three_mo_growth_plot <- ggplot(other_three_mo_growth_data, aes(x = Date, y = Value, color = Short_Sector)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Group, scales = "free_y") +
    theme_minimal() +
    labs(title = "UK Other Sector Growth Rates (1948–2024) - 3-Month-over-3-Month", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step9_uk_other_sector_3mo3m_growth_final.png", other_three_mo_growth_plot, width = 1200, height = 900, res = 150)
}

# Sector-specific forecasts (placeholder for step9_uk_sector_forecasts.png)
key_sectors_forecast <- c("Manufacturing Total", "Electric and Gas Utilities", "Mining")
forecast_plots <- list()
for (sector in key_sectors_forecast) {
  sector_data <- uk_long %>%
    filter(Short_Sector == sector) %>%
    arrange(Date)
  if (nrow(sector_data) > 0) {
    sector_ts <- ts(sector_data$Value, start = c(1948, 1), frequency = 12)
    fit_arima <- auto.arima(sector_ts)
    fc_arima <- forecast(fit_arima, h = 12)
    fc_df <- data.frame(
      Date = seq(max(sector_data$Date) + months(1), by = "month", length.out = 12),
      Forecast = as.numeric(fc_arima$mean),
      Lower = as.numeric(fc_arima$lower[, 2]),
      Upper = as.numeric(fc_arima$upper[, 2])
    )
    plot <- ggplot() +
      geom_line(data = sector_data, aes(x = Date, y = Value), color = "black") +
      geom_line(data = fc_df, aes(x = Date, y = Forecast), color = "blue", linetype = "dashed") +
      geom_ribbon(data = fc_df, aes(x = Date, ymin = Lower, ymax = Upper), alpha = 0.2, fill = "blue") +
      labs(title = paste("ARIMA Forecast:", sector, "(12 Months)"), x = "Date", y = "Index") +
      theme_minimal()
    forecast_plots[[sector]] <- plot
  }
}
# Combine forecast plots
if (length(forecast_plots) > 0) {
  combined_forecast_plot <- grid.arrange(grobs = forecast_plots, ncol = 1)
  save_plot("step9_uk_sector_forecasts.png", combined_forecast_plot, width = 1200, height = 900, res = 150)
}

#**********  Step 10: Comparative Analysis of US and UK Sector Data **********#

log_section("Step 10: Comparative Analysis of US and UK Sector Data (Full Period)")

# Load US and UK data
us_data <- read_csv("output/tables/step8_us_sector_long_grouped_with_growth.csv") %>%
  mutate(Region = "US")
uk_data <- read_csv("output/tables/step9_uk_sector_long_grouped_final.csv") %>%
  mutate(Region = "UK")

# Combine data
combined_data <- bind_rows(us_data, uk_data)

# Debug: Inspect combined data
cat("Number of rows in combined_data:", nrow(combined_data), "\n")
cat("Unique Short_Sector values in combined_data:\n")
cat(paste(unique(combined_data$Short_Sector), collapse = ", "), "\n")

write_to_csv(combined_data, "step10_combined_sector_data_full.csv")

# Summary statistics by Region and Short_Sector
combined_summary <- combined_data %>%
  group_by(Region, Short_Sector, Group) %>%
  summarise(
    Observations = sum(!is.na(Value)),
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = suppressWarnings(min(Value, na.rm = TRUE)),
    Max = suppressWarnings(max(Value, na.rm = TRUE)),
    Median = median(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Min = ifelse(is.infinite(Min), NA, Min),
    Max = ifelse(is.infinite(Max), NA, Max)
  )
write_to_csv(combined_summary, "step10_combined_summary_stats_full.csv")

# Separate into index and growth rate data
index_data <- combined_data %>%
  filter(!str_detect(Group, "Growth"))
momy_growth_data <- combined_data %>%
  filter(str_detect(Group, "Growth") & !str_detect(Group, "3mo3my|3mo3m"))
three_mo_yearly_growth_data <- combined_data %>%
  filter(str_detect(Group, "3mo3my Growth"))
three_mo_growth_data <- combined_data %>%
  filter(str_detect(Group, "3mo3m Growth"))

# Overlapping sectors for each category
index_overlapping_sectors <- intersect(
  unique(index_data$Short_Sector[index_data$Region == "US"]),
  unique(index_data$Short_Sector[index_data$Region == "UK"])
)
momy_overlapping_sectors <- intersect(
  unique(momy_growth_data$Short_Sector[momy_growth_data$Region == "US"]),
  unique(momy_growth_data$Short_Sector[momy_growth_data$Region == "UK"])
)
three_mo_yearly_overlapping_sectors <- intersect(
  unique(three_mo_yearly_growth_data$Short_Sector[three_mo_yearly_growth_data$Region == "US"]),
  unique(three_mo_yearly_growth_data$Short_Sector[three_mo_yearly_growth_data$Region == "UK"])
)
three_mo_overlapping_sectors <- intersect(
  unique(three_mo_growth_data$Short_Sector[three_mo_growth_data$Region == "US"]),
  unique(three_mo_growth_data$Short_Sector[three_mo_growth_data$Region == "UK"])
)

# Plot for raw indices (US vs. UK)
if (length(index_overlapping_sectors) > 0) {
  index_plot <- ggplot(index_data %>% filter(Short_Sector %in% index_overlapping_sectors),
                       aes(x = Date, y = Value, color = Region)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Short_Sector, scales = "free_y") +
    theme_minimal() +
    labs(title = "US vs UK Sector Trends (1948–2024) - Raw Indices", x = "Date", y = "Index") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step10_combined_sector_trends_index_full.png", index_plot, width = 1200, height = 900, res = 150)
  
  # Scatter plot for raw indices
  index_scatter_data <- index_data %>%
    filter(Short_Sector %in% index_overlapping_sectors) %>%
    pivot_wider(names_from = Region, values_from = Value)
  index_scatter_plot <- ggplot(index_scatter_data, aes(x = UK, y = US)) +
    geom_point(alpha = 0.5, color = "blue") +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    facet_wrap(~ Short_Sector, scales = "free") +
    theme_minimal() +
    labs(title = "US vs UK Sector Scatter Plot (1948–2024) - Raw Indices",
         x = "UK Index", y = "US Index") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step10_combined_sector_scatter_index_full.png", index_scatter_plot, width = 1200, height = 900, res = 150)
} else {
  cat("Skipping index trend and scatter plots: No overlapping sectors available\n")
}

# Plot for growth rates (momy gr, US vs. UK)
if (length(momy_overlapping_sectors) > 0) {
  momy_growth_plot <- ggplot(momy_growth_data %>% filter(Short_Sector %in% momy_overlapping_sectors),
                             aes(x = Date, y = Value, color = Region)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Short_Sector, scales = "free_y") +
    theme_minimal() +
    labs(title = "US vs UK Sector Growth Rates (1948–2024) - Month-over-Month", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step10_combined_sector_momy_growth_full.png", momy_growth_plot, width = 1200, height = 900, res = 150)
  
  # Scatter plot for momy growth rates
  momy_scatter_data <- momy_growth_data %>%
    filter(Short_Sector %in% momy_overlapping_sectors) %>%
    pivot_wider(names_from = Region, values_from = Value)
  momy_scatter_plot <- ggplot(momy_scatter_data, aes(x = UK, y = US)) +
    geom_point(alpha = 0.5, color = "blue") +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    facet_wrap(~ Short_Sector, scales = "free") +
    theme_minimal() +
    labs(title = "US vs UK Sector Scatter Plot (1948–2024) - Month-over-Month Growth",
         x = "UK Growth Rate (%)", y = "US Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step10_combined_sector_scatter_momy_growth_full.png", momy_scatter_plot, width = 1200, height = 900, res = 150)
} else {
  cat("Skipping momy growth trend and scatter plots: No overlapping sectors available\n")
}

# Plot for growth rates (3mo3my gr, US vs. UK)
if (length(three_mo_yearly_overlapping_sectors) > 0) {
  three_mo_yearly_growth_plot <- ggplot(three_mo_yearly_growth_data %>% filter(Short_Sector %in% three_mo_yearly_overlapping_sectors),
                                        aes(x = Date, y = Value, color = Region)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Short_Sector, scales = "free_y") +
    theme_minimal() +
    labs(title = "US vs UK Sector Growth Rates (1948–2024) - 3-Month-over-3-Month Year-over-Year", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step10_combined_sector_3mo3my_growth_full.png", three_mo_yearly_growth_plot, width = 1200, height = 900, res = 150)
  
  # Scatter plot for 3mo3my growth rates
  three_mo_yearly_scatter_data <- three_mo_yearly_growth_data %>%
    filter(Short_Sector %in% three_mo_yearly_overlapping_sectors) %>%
    pivot_wider(names_from = Region, values_from = Value)
  three_mo_yearly_scatter_plot <- ggplot(three_mo_yearly_scatter_data, aes(x = UK, y = US)) +
    geom_point(alpha = 0.5, color = "blue") +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    facet_wrap(~ Short_Sector, scales = "free") +
    theme_minimal() +
    labs(title = "US vs UK Sector Scatter Plot (1948–2024) - 3-Month-over-3-Month Year-over-Year Growth",
         x = "UK Growth Rate (%)", y = "US Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step10_combined_sector_scatter_3mo3my_growth_full.png", three_mo_yearly_scatter_plot, width = 1200, height = 900, res = 150)
} else {
  cat("Skipping 3mo3my growth trend and scatter plots: No overlapping sectors available\n")
}

# Plot for growth rates (3mo3m gr, US vs. UK)
if (length(three_mo_overlapping_sectors) > 0) {
  three_mo_growth_plot <- ggplot(three_mo_growth_data %>% filter(Short_Sector %in% three_mo_overlapping_sectors),
                                 aes(x = Date, y = Value, color = Region)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Short_Sector, scales = "free_y") +
    theme_minimal() +
    labs(title = "US vs UK Sector Growth Rates (1948–2024) - 3-Month-over-3-Month", x = "Date", y = "Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step10_combined_sector_3mo3m_growth_full.png", three_mo_growth_plot, width = 1200, height = 900, res = 150)
  
  # Scatter plot for 3mo3m growth rates
  three_mo_scatter_data <- three_mo_growth_data %>%
    filter(Short_Sector %in% three_mo_overlapping_sectors) %>%
    pivot_wider(names_from = Region, values_from = Value)
  three_mo_scatter_plot <- ggplot(three_mo_scatter_data, aes(x = UK, y = US)) +
    geom_point(alpha = 0.5, color = "blue") +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    facet_wrap(~ Short_Sector, scales = "free") +
    theme_minimal() +
    labs(title = "US vs UK Sector Scatter Plot (1948–2024) - 3-Month-over-3-Month Growth",
         x = "UK Growth Rate (%)", y = "US Growth Rate (%)") +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  save_plot("step10_combined_sector_scatter_3mo3m_growth_full.png", three_mo_scatter_plot, width = 1200, height = 900, res = 150)
} else {
  cat("Skipping 3mo3m growth trend and scatter plots: No overlapping sectors available\n")
}

# Correlation between US and UK for overlapping sectors (raw indices only)
correlation_data <- index_data %>%
  filter(Short_Sector %in% index_overlapping_sectors) %>%
  pivot_wider(names_from = Region, values_from = Value) %>%
  group_by(Short_Sector) %>%
  summarise(
    Correlation = cor(US, UK, use = "pairwise.complete.obs"),
    .groups = "drop"
  )
write_to_csv(correlation_data, "step9_sector_correlations_full.csv")

# Diagnostics
combined_diagnostics <- combined_data %>%
  group_by(Region, Short_Sector, Group) %>%
  summarise(
    ValidRows = sum(!is.na(Value) & !is.na(Date)),
    MinDate = min(Date, na.rm = TRUE),
    MaxDate = max(Date, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    MissingValues = sum(is.na(Value)),
    .groups = "drop"
  )
write_to_csv(combined_diagnostics, "step10_combined_diagnostics_full.csv")


#******************* Step 11: Forecasting and Final Report Exports ************************##
log_section("Step 11: Forecasting for Next Decade (2025–2035) with Full Data)")

# Load combined data
combined_data <- read_csv("output/tables/step10_combined_sector_data_full.csv")

# US Forecasting (1948–2025)
us_forecast_data <- combined_data %>%
  filter(Short_Sector == "Manufacturing Total", Region == "US") %>%
  arrange(Date)

# Convert to time series (1948–2025)
library(forecast)
us_ts <- ts(us_forecast_data$Value, start = c(1948, 1), frequency = 12)

# Fit ARIMA model
us_model <- auto.arima(us_ts)

# Forecast for next 10 years (120 months, 2025–2035)
us_forecast <- forecast(us_model, h = 120)
us_forecast_df <- data.frame(
  Date = seq(as.Date("2025-04-01"), as.Date("2035-03-01"), by = "month"),
  Region = "US",
  Short_Sector = "Manufacturing Total",
  Forecast = as.numeric(us_forecast$mean),
  Lower = as.numeric(us_forecast$lower[, 2]),  # 95% CI
  Upper = as.numeric(us_forecast$upper[, 2])   # 95% CI
)

# UK Forecasting (1948–2024)
uk_forecast_data <- combined_data %>%
  filter(Short_Sector == "Manufacturing Total", Region == "UK") %>%
  arrange(Date)

uk_ts <- ts(uk_forecast_data$Value, start = c(1948, 1), frequency = 12)
uk_model <- auto.arima(uk_ts)
uk_forecast <- forecast(uk_model, h = 120)
uk_forecast_df <- data.frame(
  Date = seq(as.Date("2025-04-01"), as.Date("2035-03-01"), by = "month"),
  Region = "UK",
  Short_Sector = "Manufacturing Total",
  Forecast = as.numeric(uk_forecast$mean),
  Lower = as.numeric(uk_forecast$lower[, 2]),
  Upper = as.numeric(uk_forecast$upper[, 2])
)

# Combine forecasts
forecast_results <- bind_rows(us_forecast_df, uk_forecast_df)
write_to_csv(forecast_results, "step11_forecast_results_long_term.csv")

# Forecast plot (focus on 2025–2035)
forecast_plot <- ggplot() +
  geom_line(data = forecast_results, aes(x = Date, y = Forecast, color = Region), linetype = "dashed") +
  geom_ribbon(data = forecast_results, aes(x = Date, ymin = Lower, ymax = Upper, fill = Region), alpha = 0.2) +
  facet_wrap(~ Short_Sector, scales = "free_y") +
  theme_minimal() +
  labs(title = "Manufacturing Total Forecast (2025–2035)", x = "Date", y = "Index") +
  theme(legend.position = "bottom", legend.text = element_text(size = 8))
save_plot("step11_manufacturing_total_forecast_long_term.png", forecast_plot, width = 1200, height = 900, res = 150)

# Summary report (expanded to include additional sectors, growth rates, and aggregates)
summary_report <- combined_data %>%
  filter(Short_Sector %in% c("Manufacturing Total", "Pharmaceuticals", "Computer and Electronic Products", "Electrical Equipment", "Chemicals", "Mining", "Rubber and Plastic Products", "Basic Metals", "Paper and Paper Products", "Coke and Refined Petroleum", "Water Supply and Sewerage", "Soft Drinks", "Textiles", "Wearing Apparel", "Leather Products", "Wood Products", "Printing", "Food Products and Tobacco 3mo3my Growth", "Paints and Varnishes 3mo3my Growth", "Energy 3mo3my Growth", "Soft Drinks 3mo3my Growth", "Textiles and Apparel 3mo3my Growth", "MIG Intermediate Goods 3mo3my Growth", "MIG Consumer Goods 3mo3my Growth", "MIG Capital Goods 3mo3my Growth")) %>%
  group_by(Region, Short_Sector) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    MAD = median(abs(Value - median(Value, na.rm = TRUE)), na.rm = TRUE),
    .groups = "drop"
  )
write_to_csv(summary_report, "step11_summary_report_full.csv")

# Diagnostics for forecasting
forecast_diagnostics <- combined_data %>%
  filter(Short_Sector == "Manufacturing Total") %>%
  group_by(Region, Short_Sector) %>%
  summarise(
    ValidRows = sum(!is.na(Value) & !is.na(Date)),
    MinDate = min(Date, na.rm = TRUE),
    MaxDate = max(Date, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    MissingValues = sum(is.na(Value)),
    .groups = "drop"
  )
write_to_csv(forecast_diagnostics, "step11_forecast_diagnostics_full.csv")

# Final Report Preparation
log_section("Final Report Preparation")

# Load key data
summary_report <- read_csv("output/tables/step11_summary_report_full.csv")
forecast_results <- read_csv("output/tables/step11_forecast_results_long_term.csv")
correlations <- read_csv("output/tables/step9_sector_correlations_full.csv")

# Create a summary table for the report
final_summary <- summary_report %>%
  left_join(correlations, by = "Short_Sector") %>%
  mutate(
    Correlation = ifelse(is.na(Correlation), "N/A (UK only)", round(Correlation, 2))
  )
write_to_csv(final_summary, "final_report_summary.csv")

# Forecasting paragraph for the next decade
us_2035_forecast <- forecast_results %>%
  filter(Region == "US", Date == as.Date("2035-03-01")) %>%
  pull(Forecast) %>%
  round(1)
uk_2035_forecast <- forecast_results %>%
  filter(Region == "UK", Date == as.Date("2035-03-01")) %>%
  pull(Forecast) %>%
  round(1)
forecast_paragraph <- paste0(
  "Over the next decade (2025–2035), the US Manufacturing Total index is projected to grow from 114 in 2024 to approximately ",
  us_2035_forecast, ", reflecting a steady growth rate driven by historical trends (1948–2025). The UK Manufacturing Total index, based on historical trends (1948–2024), is expected to rise from 104.5 to around ",
  uk_2035_forecast, ". The US is likely to maintain its lead in industrial output, supported by a larger, more diversified industrial base."
)
write_to_csv(data.frame(Paragraph = forecast_paragraph), "final_report_forecast_paragraph.csv")

# Note: For the presentation slide
cat("Final report preparation complete. Use the following for your presentation slide:\n")
cat("- UK vs US Aggregate Scatter Plot (1948–2025): output/plots/step2_uk_us_scatter.png\n")
cat("- US vs UK Raw Index Trends (1948–2024): output/plots/step8_combined_sector_trends_index_full.png\n")
cat("- US vs UK Raw Index Scatter Plot (1948–2024): output/plots/step8_combined_sector_scatter_index_full.png\n")
cat("- US vs UK Month-over-Month Growth Rates (1948–2024): output/plots/step8_combined_sector_momy_growth_full.png\n")
cat("- US vs UK Month-over-Month Growth Scatter Plot (1948–2024): output/plots/step8_combined_sector_scatter_momy_growth_full.png\n")
cat("- US vs UK 3-Month-over-3-Month Year-over-Year Growth Rates (1948–2024): output/plots/step8_combined_sector_3mo3my_growth_full.png\n")
cat("- US vs UK 3-Month-over-3-Month Year-over-Year Growth Scatter Plot (1948–2024): output/plots/step8_combined_sector_scatter_3mo3my_growth_full.png\n")
cat("- US vs UK 3-Month-over-3-Month Growth Rates (1948–2024): output/plots/step8_combined_sector_3mo3m_growth_full.png\n")
cat("- US vs UK 3-Month-over-3-Month Growth Scatter Plot (1948–2024): output/plots/step8_combined_sector_scatter_3mo3m_growth_full.png\n")
cat("- UK Chemical Sector Growth Rates (Month-over-Month): output/plots/step7_uk_chemical_sector_momy_growth_final.png\n")
cat("- UK Consumer Goods Growth Rates (Month-over-Month): output/plots/step7_uk_consumer_sector_momy_growth_final.png\n")
cat("- UK Traditional Manufacturing Growth Rates (Month-over-Month): output/plots/step7_uk_traditional_sector_momy_growth_final.png\n")
cat("- UK Mining Sector Growth Rates (Month-over-Month): output/plots/step7_uk_mining_sector_momy_growth_final.png\n")
cat("- UK Aggregate Sector Growth Rates (Month-over-Month): output/plots/step7_uk_aggregate_sector_momy_growth_final.png\n")
cat("- UK Other Sector Growth Rates (Month-over-Month): output/plots/step7_uk_other_sector_momy_growth_final.png\n")
cat("- UK Chemical Sector Growth Rates (3-Month-over-3-Month Year-over-Year): output/plots/step7_uk_chemical_sector_3mo3my_growth_final.png\n")
cat("- UK Consumer Goods Growth Rates (3-Month-over-3-Month Year-over-Year): output/plots/step7_uk_consumer_sector_3mo3my_growth_final.png\n")
cat("- UK Traditional Manufacturing Growth Rates (3-Month-over-3-Month Year-over-Year): output/plots/step7_uk_traditional_sector_3mo3my_growth_final.png\n")
cat("- UK Mining Sector Growth Rates (3-Month-over-3-Month Year-over-Year): output/plots/step7_uk_mining_sector_3mo3my_growth_final.png\n")
cat("- UK Aggregate Sector Growth Rates (3-Month-over-3-Month Year-over-Year): output/plots/step7_uk_aggregate_sector_3mo3my_growth_final.png\n")
cat("- UK Other Sector Growth Rates (3-Month-over-3-Month Year-over-Year): output/plots/step7_uk_other_sector_3mo3my_growth_final.png\n")
cat("- UK Chemical Sector Growth Rates (3-Month-over-3-Month): output/plots/step7_uk_chemical_sector_3mo3m_growth_final.png\n")
cat("- UK Consumer Goods Growth Rates (3-Month-over-3-Month): output/plots/step7_uk_consumer_sector_3mo3m_growth_final.png\n")
cat("- UK Traditional Manufacturing Growth Rates (3-Month-over-3-Month): output/plots/step7_uk_traditional_sector_3mo3m_growth_final.png\n")
cat("- UK Mining Sector Growth Rates (3-Month-over-3-Month): output/plots/step7_uk_mining_sector_3mo3m_growth_final.png\n")
cat("- UK Aggregate Sector Growth Rates (3-Month-over-3-Month): output/plots/step7_uk_aggregate_sector_3mo3m_growth_final.png\n")
cat("- UK Other Sector Growth Rates (3-Month-over-3-Month): output/plots/step7_uk_other_sector_3mo3m_growth_final.png\n")
cat("- Forecast (2025–2035): output/plots/step11_manufacturing_total_forecast_long_term.png\n")
cat("- Summary: output/tables/final_report_summary.csv\n")
cat("- Decade Forecast: See output/tables/final_report_forecast_paragraph.csv\n")


# Fetch and Print All Files in Plots, Tables, and Transformed Directories
log_section("Listing All Generated Files for Report")

# Define directories
dirs <- c("output/plots", "output/tables", "data/transformed")

# Function to list files in a directory
list_files_in_dir <- function(dir_path) {
  if (dir.exists(dir_path)) {
    files <- list.files(path = dir_path, full.names = TRUE, recursive = FALSE)
    cat("Files in", dir_path, ":\n")
    cat(paste(basename(files), collapse = "\n"), "\n\n")
  } else {
    cat("Directory not found:", dir_path, "\n\n")
  }
}

# Apply to all directories
lapply(dirs, list_files_in_dir)

# End of script
cat("File listing complete.\n")

end_log()