#Lab1Complete.R
#Written by Marvine Hamner
#February 25, 2025

# Modified the base code with checks and to export data
# Modified by Anbazhagan, Naresh
# April 04, 2025

# ==== Setup: Package Management ====

reqd_pkgs <- c(
  "tidyverse", "lubridate", "tsibble", "tsibbledata", 
  "fable", "fabletools", "feasts", "forecast", "scales", "readr", "patchwork",
  "distributional", "slider", "vctrs", "tibbletime", "zoo", "naniar"
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


# ==== Working Directory & Folder Setup ====

setwd("/Users/anbzhnjr/learning/DataAnalytics/rprojects/ANA535/Lab1")

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/plots", showWarnings = FALSE)
dir.create("outputs/logs", showWarnings = FALSE)


# ==== Helpers ====

format_axis_labels <- function(x) {
  sapply(x, function(val) {
    if (abs(val) >= 1e6) paste0(formatC(val / 1e6, format = "f", digits = 1), "M")
    else if (abs(val) >= 1e3) paste0(formatC(val / 1e3, format = "f", digits = 0), "K")
    else as.character(val)
  })
}

save_base_plot <- function(plot_expr, filename, width = 10, height = 6) {
  filepath <- file.path("outputs/plots", filename)
  png(filename = filepath, width = width, height = height, units = "in", res = 300)
  par(mar = c(5, 6, 5, 2))  # Bottom, Left, Top, Right
  options(scipen = 999)
  plot_expr
  dev.off()
  cat("Saved plot to:", filepath, "\n")
}

save_plot <- function(plot_obj, filename, width = 8, height = 5) {
  ggsave(
    filename = file.path("outputs/plots", filename),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = 300
  )
}


# ==== Begin Logging ====
# Cleaning sink stack before starting new logging

while (sink.number() > 0) {
  sink(NULL)
}

sink("outputs/logs/analysis_output.txt", split = TRUE)
cat("===== Amtrak EDA Report =====\n")
cat("Generated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")


# ================== Step 1: Data Import & Preprocessing ==================

cat("## Step 1: Importing and Preprocessing Data ##\n")

Amtrak <- read.csv("Amtrak1991-2024.csv")
colnames(Amtrak) <- c("Date", "Ridership", "PassengerMiles", "RidersReported")

cat("\nInitial glimpse of the data:\n")
glimpse(Amtrak)

cat("\n Summary of the data: \n")
summary(Amtrak)

Amtrak <- Amtrak %>%
  mutate(Date = mdy(Date))

Amtrak_tsibble <- Amtrak %>%
  as_tsibble(index = Date)

cat("\nData Glimpse after converting to tsibble:\n")
glimpse(Amtrak_tsibble)


# ================== Step 2: Missing Data Check ==================

cat("\n## Step 2: Missing Data Check ##\n")

cat("\nTotal missing values in the dataset:", sum(is.na(Amtrak)), "\n")

cat("\nMissing values per column:\n")
print(colSums(is.na(Amtrak)))

plot_missing <- gg_miss_var(Amtrak) +
  labs(title = "Missing Data Summary - Amtrak Dataset")

print(plot_missing)
save_plot(plot_missing, "amtrak_missing_data.png")


# ================== Step 3: Time Series Plot ==================

cat("\n===== Step 3: Time Series Plots =====\n")

# --- Ridership Plot ---
Amtrak_ridership_ts <- ts(Amtrak$Ridership, start = c(1991, 1), end = c(2024, 6), frequency = 12)
cat("\nStructure of Ridership Time Series:\n")
str(Amtrak_ridership_ts)

save_base_plot(
  plot_expr = {
    plot(
      Amtrak_ridership_ts,
      col = "blue",
      xlab = "Time",
      ylab = "Monthly Ridership",
      main = "Monthly Amtrak Ridership (1991–2024)",
      bty = "l",
      las = 1,
      axes = FALSE
    )
    axis(1)
    axis(2, at = pretty(Amtrak_ridership_ts), labels = format_axis_labels(pretty(Amtrak_ridership_ts)), las = 1)
    box()
  },
  filename = "amtrak_ridership_ts_clean.png"
)

# --- Passenger Miles Plot ---
Amtrak_passmiles_ts <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2024, 6), frequency = 12)
cat("\nStructure of Passenger Miles Time Series:\n")
str(Amtrak_passmiles_ts)

save_base_plot(
  plot_expr = {
    plot(
      Amtrak_passmiles_ts,
      col = "teal",
      xlab = "Time",
      ylab = "Passenger Miles",
      main = "Monthly Amtrak Passenger Miles (1991–2024)",
      bty = "l",
      las = 1,
      axes = FALSE
    )
    axis(1)
    axis(2, at = pretty(Amtrak_passmiles_ts), labels = format_axis_labels(pretty(Amtrak_passmiles_ts)), las = 1)
    box()
  },
  filename = "amtrak_passenger_miles_ts_clean.png"
)

# ================== Step 3: Filering & Autocorrelation Check ==================

cat("\n===== Step 4: Filtering and Autocorrelation Check =====\n")

# -- 1. Filter without saving (Jan 1999 to Dec 1999 only) --
cat("\nFiltered View: One-Year Period (1999-01-01 to 1999-12-01)\n")
print(
  Amtrak %>%
    filter(between(Date, as.Date('1999-01-01'), as.Date('1999-12-01')))
)

# -- 2. Rearranged Amtrak dataset in descending order --
cat("\nFull Dataset Sorted in Descending Order by Date:\n")
print(
  Amtrak %>%
    arrange(desc(Date))
)

# -- 3. Filter using format-based extraction (1999 only), saved as object --
cat("\nCreating a filtered object for 1999 using formatted year:\n")
Amtrak_1999 <- Amtrak %>%
  filter(format(Date, "%Y") == "1999")

str(Amtrak_1999)
print(Amtrak_1999)

# -- 4. ACF Check: Create ts object from 1991 to 2016 (PassengerMiles only) --
cat("\nCreating time series object from 1991 to 2016 for PassengerMiles:\n")
Amtrak_ts_91_16 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2017, 1), frequency = 12)

cat("\nConverting to tsibble and plotting ACF:\n")
Amtrak_tsb_91_16 <- as_tsibble(Amtrak_ts_91_16)

acf_plot <- Amtrak_tsb_91_16 |>
  ACF() |> autoplot() +
  labs(
    title = "Autocorrelation: Passenger Miles (1991–2016)",
    subtitle = "Shows trend + seasonality (non-stationary)"
  )

print(acf_plot)
save_plot(acf_plot, "acf_amtrak_passenger_miles_1991_2016.png")

# ================== Step 5: Trendline Modeling (Traditional + ggplot) ==================

cat("\n===== Step 5: Trendline Modeling =====\n")

# -- Base R Approach using tslm() on PassengerMiles --
Amtrak_miles_ts <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2024, 6), frequency = 12)
cat("\nStructure of PassengerMiles Time Series:\n")
str(Amtrak_miles_ts)

# Fit linear + polynomial trend
model_trend <- tslm(Amtrak_miles_ts ~ trend + I(trend^2) + I(trend^3))

# Plot: Full Data with Fitted Trend
save_base_plot(
  plot_expr = {
    plot(
      Amtrak_miles_ts,
      xlab = "Time",
      ylab = "Passenger Miles",
      col = "darkgreen",
      main = "Trend Model - Full Series",
      bty = "l",
      las = 1
    )
    lines(model_trend$fitted.values, col = "black", lwd = 2)
  },
  filename = "trend_fit_passenger_miles_full.png"
)

# Zoomed View (1997–2000)
Amtrak_miles_ts_zoom <- window(Amtrak_miles_ts, start = c(1997, 1), end = c(2000, 12))
save_base_plot(
  plot_expr = {
    plot(
      Amtrak_miles_ts_zoom,
      xlab = "Time",
      ylab = "Passenger Miles",
      col = "darkgreen",
      main = "Passenger Miles: Zoomed View (1997–2000)",
      bty = "l",
      las = 1
    )
  },
  filename = "trend_fit_passenger_miles_zoom.png"
)

# ================== Step 6: Modern Visualization with ggplot & tsibble ==================

cat("\n===== Step 6: Modern ggplot Visualization =====\n")

# Create tsibble from ts
Amtrak_miles_tsibble <- as_tsibble(Amtrak_miles_ts)

# Autoplot version
autoplot_miles <- autoplot(Amtrak_miles_tsibble) +
  labs(title = "Passenger Miles - Autoplot (Tsibble)", y = "Miles", x = "Date")
save_plot(autoplot_miles, "gg_tsibble_autoplot_passenger_miles.png")

# Scatter plot with cubic trend (ggplot + tsibble)
plot_cubic_trend <- ggplot(Amtrak_miles_tsibble, aes(x = index, y = value)) +
  geom_point(color = "darkgray") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "blue") +
  labs(title = "Passenger Miles with Cubic Trend (ggplot)", x = "Date", y = "Passenger Miles")
save_plot(plot_cubic_trend, "gg_cubic_trend_passenger_miles.png")

# Trend using original data (Amtrak)
amtrak_miles_df <- Amtrak %>% select(Date, PassengerMiles)
plot_line_trend <- ggplot(amtrak_miles_df, aes(x = Date, y = PassengerMiles)) +
  geom_line(color = "darkblue") +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), color = "red") +
  labs(title = "Passenger Miles with Polynomial Trend (Original Data)", x = "Date", y = "Passenger Miles")
save_plot(plot_line_trend, "gg_poly_trend_passenger_miles_original.png")

# Optional: Trend for Ridership
plot_ridership_trend <- ggplot(Amtrak, aes(x = Date, y = Ridership)) +
  geom_line(color = "steelblue") +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), color = "orange") +
  labs(title = "Ridership with Quadratic Trend", x = "Date", y = "Ridership")
save_plot(plot_ridership_trend, "gg_quadratic_trend_ridership.png")

cat("\n===== Step 7: Hierarchical Sales Data Exploration =====\n")

# Read the data
sales <- read.csv("hierarchical_sales_data.csv")
cat("\nInitial structure of sales dataset:\n")
str(sales)

# Select the first 10 product columns + DATE
df10 <- sales[, 1:11]
df10$DATE <- mdy(df10$DATE)

# Convert to tidy tibble
df10_tb <- as_tibble(df10)

# Long format (key = product name)
df10_tb_long <- df10_tb %>%
  pivot_longer(cols = -DATE, names_to = "Product", values_to = "Sales")

cat("\nStructure of long tibble:\n")
str(df10_tb_long)
cat("\nSample of long data:\n")
print(head(df10_tb_long))

# Convert to tsibble: index = DATE, key = Product
sales10_tsb <- df10_tb_long %>%
  as_tsibble(index = DATE, key = Product)

cat("\nStructure of tsibble:\n")
str(sales10_tsb)

# Plot: trend per product
sales_plot <- sales10_tsb %>%
  ggplot(aes(x = DATE, y = Sales, color = Product)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Monthly Sales Trend by Product",
    x = "Date", y = "Sales ($)",
    subtitle = "Hierarchical Sales Data"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(sales_plot)
save_plot(sales_plot, "hierarchical_sales_products_trend.png")
sink(NULL)
