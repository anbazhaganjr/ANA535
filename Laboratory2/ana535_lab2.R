#
#Lab2Complete.R
#Written by Marvine Hamner
#February 25, 2025
#
#This is the code for Laboratory #2.  It contains all the commands you
#need to answer the online questions and complete your laboratory written
#report.
#
#In addition to this code you will need the Amtrak dataset and the
#COandNOx2.txt file/dataset. 
#
#Note that there are additional commands that I have commented out.  If 
#you are comfortable you might want to try some.  Be careful, I don't 
#guarantee that these will work depending on what libraries you have 
#loaded, etc. 
#

# Modified by Anbazhagan, Naresh on APR 11, 2025
# Changelog:
# - Added automatic package installation and silent library loading
# - Implemented `save_plot()` helper function to programmatically save all plots
# - Implemented directory creation logic for reproducible output structure
# - Wrapped all `plot()` and `autoplot()` calls inside `save_plot()` with descriptive filenames
# - Added `start_log()` and `end_log()` helper functions for optional logging
# - Commented out X-11 decomposition using `seasonal` due to macOS incompatibility
# - Replaced X-11 logic with STL decomposition using `feasts::STL()`
# - Added composite STL visualization using `ggplot2`
# - Ensured no calls to `View()` or system-dependent functions remain for reproducibility
# - Final cleanup: ensured all plots restore `par(mfrow)` to default after multi-panel plots
# - Added titles, axis labels, and consistent formatting to all plots
# - Verified that all steps, from FFT to decomposition, execute correctly on macOS without additional dependencies


reqd_pkgs <- c(
  "fpp3",         # Main forecasting framework (includes tsibble, fable, feasts)
  "dplyr",        # Data manipulation
  "tidyverse",    # Data science toolchain
  "ggplot2",      # Plotting
  "tsibble",      # Time series data structure
  "tsibbledata",  # Example time series data
  "fable",        # Forecast modeling
  "feasts",       # Decomposition & spectral analysis
  "lubridate",    # Date handling
  "zoo",          # Time-series helper functions
  "forecast",     # Legacy forecasting tools (e.g., tslm)
  "TSA"          # Periodogram tools
)

installed <- rownames(installed.packages())
to_install <- setdiff(c(reqd_pkgs), installed)

if (length(to_install) > 0) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}

invisible(lapply(c(reqd_pkgs), function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))

cat("All libraries successfully loaded.\n\n")

# ======================
# Working Directory Setup
# ======================

setwd("/Users/anbzhnjr/learning/DataAnalytics/rprojects/ANA535/Lab2")
dir.create("data", recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)
dir.create("output/logs", recursive = TRUE, showWarnings = FALSE)
dir.create("output/plots", recursive = TRUE, showWarnings = FALSE)

# ======================
# Helper Functions
# ======================

# Helper to save a plot to 'output/plots' folder
save_plot <- function(filename, expr, width = 800, height = 600, res = 120) {
  png(file = paste0("output/plots/", filename), width = width, height = height, res = res)
  expr
  dev.off()
}

# Helper to manage logging
start_log <- function(filename = "output/logs/ana535_lab2_output_log.txt") {
  sink(file = filename, append = TRUE, split = TRUE)
}
end_log <- function() {
  sink()
}

start_log()
cat("===== ANA535 Lab 2 Execution Started =====\n")

# Paths
lab1_path <- "/Users/anbzhnjr/learning/DataAnalytics/rprojects/ANA535/Lab1"       # Adjust this if Lab1 is elsewhere
lab2_path <- "data"

# Files to copy
files_to_copy <- c("Amtrak1991-2024.csv", "COandNOx2.txt")

# Copy only if not already present
for (f in files_to_copy) {
  from <- file.path(lab1_path, f)
  to <- file.path(lab2_path, f)
  
  if (!file.exists(to)) {
    file.copy(from = from, to = to)
    message(paste("Copied:", f))
  } else {
    message(paste("Already exists, skipped:", f))
  }
}


#
#********************** Step 1 *************************
#
cat("\n--- Step 1: Simple Sine Wave Analysis ---\n")

#
#First complete the analysis of a simple sine wave at a specified freq.
#Setup the parameters.  You are given that the data are collected at
#a sampling frequency (Fs) of 1,000 Hertz.  Hertz are cycles per second.
#That is am important distinction.  Later we will compute frequencies and
#need to put time in the denominator, i.e. 1/time.  The period, T, is 
#1/Fs.There are 1,500 data points available. The independent variable is 
#time (t).  
#

Fs <- 1000
T = 1/Fs
L = 1500
t = (0:L-1)*T

#
#Generate the data and plot it
#

Sig1 <- sin(2*pi*120*t)
save_plot("step1_sinewave_timeplot.png", {
  plot(1000*t[1:100], Sig1[1:100], type = "l", main = "Sine Wave (120Hz)", xlab = "Time (ms)", ylab = "Amplitude")
})


#
#Conduct the FFT and plot the resulting power spectrum
#

Y <- fft(Sig1)
P2 <- abs(Y/L)
m <- (length(Y)/2)+1
P1 <- P2[1:m]
P1 <- 2*P1
f = Fs*(0:(L/2)/L)
save_plot("step1_sinewave_fft_powerspectrum.png", {
  plot(f, P1, type = "l", main = "FFT Power Spectrum", xlab = "Frequency (Hz)", ylab = "Magnitude")
})


#
#There are several ways you can look at the energy in the frequency
#spectrum.  One way is to generate a perriodogram.  Another way
#is to generate the frequency spectrum.  First, we'll look at
#the periodogram.
#

p = periodogram(Sig1)
dd=data.frame(freq=p$freq, spec=p$spec)
order=dd[order(-dd$spec),]
top1=head(order, 1)
top1

save_plot("step1_sinewave_periodogram.png", {
  periodogram(Sig1)
})

#
#If you multiply the frequency output by the TSA periodogram by
#the sampling frequency you'll get 119.7917 or about 120 Hz
#which is the original signal frequency.  The periodogram is great 
#for a univariate signal.  It does not work well for complex signals 
#composed of many frequencies.  
#

#
#********************** Step 2 ************************
#
cat("\n--- Step 2: Complex Wave Analysis ---\n")

#Now work on the combined, complex wave problem
#wave1 has a freq of 100 and amplitude of 5
#wave2 has a freq of 200 and amplitude of 10
#wave3 has a freq of 400 and amplitude of 15
#

#
#Setup the parameters
#

Fs <- 1000
T = 1/Fs
L = 1500
t = (0:L-1)*T

#
#Generate the data and plot it
#

wave1 <- 5*sin(2*pi*100*t)
wave2 <- 10*sin(2*pi*200*t)
wave3 <- 15*sin(2*pi*400*t)

save_plot("step2_component_waves.png", {
  par(mfrow = c(2,2))
  plot(1000*t[1:100], wave1[1:100], type = "l", main = "Wave 1 (100Hz, A=5)")
  plot(1000*t[1:100], wave2[1:100], type = "l", main = "Wave 2 (200Hz, A=10)")
  plot(1000*t[1:100], wave3[1:100], type = "l", main = "Wave 3 (400Hz, A=15)")
})


#
#Combine the three waves into one complex wave and plot
#

par(mfrow = c(1,1))
wave4 <- wave1 + wave2 + wave3

save_plot("step2_complex_wave.png", {
  plot(1000*t[1:100], wave4[1:100], type = "l", 
       main = "Combined Wave: 100Hz+200Hz+400Hz", xlab = "Time (ms)", ylab = "Amplitude")
})


#
#******************** Step 3 *************************
#

cat("\n--- Step 3: Phase Shift Analysis ---\n")

#
#Now let's look at what a phase shift does.  A phase shift by pi will setup
#destructive interference.  First, setup a sequence of points over 180 degrees.
#

xs <- seq(-100*pi, 100*pi, pi/100)

#
#Then, set up the sine waves. And, just to make things interesting
#let's combine them into a complex wave. 
#

wavea <- sin(0.1*xs)
waveb <- sin(0.333*xs)
wavec <- wavea + waveb

save_plot("step3_original_wave_combination.png", {
  par(mfrow = c(3,1))
  plot(xs, wavea, type = "l", ylim = c(-1,1), main = "Wave A (No Shift)"); abline(h=0, lty=3)
  plot(xs, waveb, type = "l", ylim = c(-1,1), main = "Wave B (No Shift)"); abline(h=0, lty=3)
  plot(xs, wavec, type = "l", ylim = c(-1,1), main = "Combined (No Shift)"); abline(h=0, lty=3)
})


#
#Next setup a phase shift by 180 degrees or pi.
#

wavea2 <- sin(0.1*xs + pi)
waveb2 <- sin(0.333*xs + pi)
wavec2 <- wavea2 + waveb2

#
#Now let's plot the original complex wave, the wave shifted by pi
#or 180 degrees, and the combination of these two waves. As you 
#can see, the combination of the original wave and the phase
#shifted wave is zero, total destructive interference.  
#

save_plot("step3_phase_shift_and_destruction.png", {
  par(mfrow = c(3,1))
  plot(xs, wavec, type = "l", ylim = c(-1,1), main = "Original Combined Wave"); abline(h=0, lty=3)
  plot(xs, wavec2, type = "l", ylim = c(-1,1), main = "Phase Shifted Combined Wave"); abline(h=0, lty=3)
  waved <- wavec + wavec2
  plot(xs, waved, type = "l", ylim = c(-1,1), main = "Destructive Interference (Sum)"); abline(h=0, lty=3)
})


#
#Conduct the fft analysis.  But now, since we have several waves
#combined, we'll  plot the resulting power spectrum rather than a periodogram.  
#

Y <- fft(wave4)
P2 <- abs(Y/L)
m <- (length(Y)/2)+1
P1 <- P2[1:m]
P1 <- 2*P1
f = Fs*(0:(L/2)/L)

save_plot("step3_combined_wave_fft.png", {
  par(mfrow = c(1,1))
  plot(f, P1, type = "l", main = "FFT of Combined Wave", xlab = "Frequency", ylab = "Magnitude")
})


#
#******************** Additional example from real-world
#******************** signal acquisition for air pollution
#******************** due to street traffic. 
#

#
#Second complete the analysis of a CO signal from an air monitoring
#station by a roadway in Italy. 
#

#
#Setup the parameters.  Note that the data were acquired hourly so the sampling
#frequency is (60 sec * 60 min = 3600 or 0.000278 Hertz). 
#

Fs <- 0.000278
T = 1/Fs
L = 8760
t = (0:L-1)*T

#
#Conduct the fft analysis and plot the resulting power spectrum
#
#Read in the data and add variable names
#

COandNOx2 <- read.csv("data/COandNOx2.txt", header=FALSE)
colnames(COandNOx2) <- c("CO.GT", "NOx.GT", "T", "RH", "AH")

Y <- fft(COandNOx2$CO.GT)
P2 <- abs(Y/L)
m <- (length(Y)/2)+1
P1 <- P2[1:m]
P1 <- 2*P1
f = Fs*(0:(L/2)/L)
save_plot("step3_co_power_spectrum_full.png", {
  par(mfrow = c(1,1))
  plot(f, P1, type = "l", ylim = c(0, 1),
       main = "CO Power Spectrum", xlab = "Frequency", ylab = "Magnitude")
})


#
#Zoom into power spectrum
#

save_plot("step3_co_power_spectrum_zoomed.png", {
  plot(f, P1, type = "l", xlim = c(0, 0.00002), ylim = c(0, 1),
       main = "CO Power Spectrum (Zoomed)", xlab = "Frequency", ylab = "Magnitude")
})



#
#If there is a daily (24-hour) periodicity we would expect a spike at
#24*60*60 or 1.157 x e-005 which we see when we zoom into the power
#spectrum.  
#

#
#*********************** Step 4 ***********************
#

cat("\n--- Step 4: Amtrak Decomposition & STL ---\n")


Amtrak <- read.csv("data/Amtrak1991-2024.csv")
colnames(Amtrak) <- c("Month", "Ridership", "PassengerMiles", "RidersReported")
View(Amtrak)

Amtrak$Month <- mdy(Amtrak$Month)
Amtrak
str(Amtrak)

#Amtrak$Month <- zoo::as.yearmon(Amtrak$Month)
#Amtrak 

#Amtrak |>
#  mutate(Month = yearmonth(Month)) |>
#  as_tsibble(index = Month)


#
#Setup the parameters
#

Fs <- 1/(30.4*24*60*60)
T = 1/Fs
L = 402
t = (0:L-1)*T

#
#Conduct the fft analysis and plot the resulting power spectrum
#

Y <- fft(Amtrak$PassengerMiles)
P2 <- abs(Y/L)
m <- (length(Y)/2)+1
P1 <- P2[1:m]
P1 <- 2*P1
f = Fs*(0:(L/2)/L)

save_plot("step4_amtrak_power_spectrum.png", {
  library(scales)
  par(mfrow = c(1,1))
  plot(f, P1, 
       type = "l", 
       xlim = c(0, 2e-7),
       ylim = c(0, max(P1, na.rm = TRUE)), 
       main = "Amtrak Power Spectrum",
       xlab = "Frequency (Hz)",
       ylab = "Magnitude",
       yaxt = "n")
  axis(2, at = pretty(P1), labels = label_number(scale = 1e-6, suffix = "M")(pretty(P1)))
})


#
#To look for annual periodicity we need to look for a spike at 
#1/(365 * 24 * 60 * 60 = 3.1536 x e07) or 3.1710 x e-08 and there is a nice
#large spike at about 3 x e-08.  
#
#Because months have different numbers of days and so on, looking at plots on
#this scale is a really crude approximation.  The main point is that there are 
#multiple periods in the Amtrak data as we will see in the time series
#decomposition.  
#

#
#Code to analyze Amtrak example/case from Practical Time Series Forecasting
#  with R: A Hands-on Guide (G. Shmueli and K.C. Lichtendahl Jr.) and fpp3
#

#
#Plot Amtrak data in time plot
#

save_plot("step4_amtrak_passengermiles_timeseries.png", {
  print(
    ggplot(data = Amtrak) +
      geom_line(mapping = aes(x = Month, y = PassengerMiles)) +
      scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
      labs(title = "Amtrak Passenger Miles by Month", x = "Month", y = "Passenger Miles (Millions)")
  )
})


#
#The data is rough.  First, the drop due to the Covid lockdowns is very obvious. 
#There is also a small dip from the 2008 financial meltdown. It looks like there 
#may be a seasonal component...  First, split out the desired variable (Ridership
#or PassengerMiles) numbers as a time series. 
#

PassengerMiles.ts <- ts(Amtrak$PassengerMiles, start = c(1991,1), end = c(2024, 6), frequency = 12)
Amtrak.comp <- decompose(PassengerMiles.ts)
save_plot("step4_amtrak_decomposition_classical.png", {
  par(mfrow = c(4,1))
  print(
    autoplot(Amtrak.comp) +
      labs(title = 'Amtrak Passenger Miles Decomposition')
  )
})

#
#Let's look at just the seasonal component of the Amtrak data by 
#zooming into two years from that single plot
#

save_plot("step4_amtrak_seasonal_zoom.png", {
  library(scales)
  par(mfrow = c(1,1))
  plot(Amtrak.comp$seasonal, 
       xlim = c(1994, 1997), 
       main = "Seasonal Component (1994–1997)",
       xlab = "Time", 
       ylab = "Seasonal Effect",
       yaxt = "n")
  axis(2, at = pretty(Amtrak.comp$seasonal), 
       labels = label_number(scale = 1e-6, suffix = "M")(pretty(Amtrak.comp$seasonal)))
})

#
#As we saw when we looked at the power spectrum of the Amtrak data,
#there are a number of periods in the data, e.g. annual or every year periodicity
#is pretty obvious but the data also repeat or have periodicity every 6-months,  
#every roughly 3 months, etc.  
#

#
#Let's look at what a seasonal adjustment would be for the Amtrak data. 
#Keep in mind that there are many ways to do this. I'll show you three ways
#here. Let's start with a tried and true method by using lags.  I know we 
#haven't covered lags yet but we will.  Just keep this in mind for later 
#when you'll use it more extensively.  
#


PassengerMiles.ts <- ts(Amtrak$PassengerMiles, start = c(1991,1), end = c(2024, 6), frequency = 12)

par(mfrow = c(1,1))

#
#The first way is to use the diff() command from the forecast package in R
#You can use either the syntax to find values of components of a time series 
#with a lag = 12 first, or just put all the commands into the plot() command 
#directly.
#
#The best webpage I've found with examples of the diff() command is at
#https://atsa-es.github.io/atsa-labs/sec-tslab-differencing-to-remove-a-trend-or-seasonal-effects.html
#

save_plot("step4_differencing_lag12.png", {
  library(scales)
  par(mfrow = c(1, 2))
  d12 <- diff(PassengerMiles.ts, lag = 12)
  
  plot(d12, ylab = "Lag-12 Difference", xlab = "Time", bty = "l", xlim = c(1991, 2024.25),
       main = "Differenced (Lag-12)", yaxt = "n")
  axis(2, at = axTicks(2), labels = label_number(scale = 1e-6, suffix = "M")(axTicks(2)))
  
  plot(diff(PassengerMiles.ts, lag = 12), ylab = "Lag-12 Difference", xlab = "Time", bty = "l", 
       xlim = c(1991, 2024.25), main = "Differenced (Lag-12) [Alt Syntax]", yaxt = "n")
  axis(2, at = axTicks(2), labels = label_number(scale = 1e-6, suffix = "M")(axTicks(2)))
})


#
#You can see from the side-by-side plots that either syntax returns the same
#result.
#

save_plot("step4_differencing_twice_thrice.png", {
  library(scales)
  par(mfrow = c(1, 2))
  
  diff_twice <- diff(diff(PassengerMiles.ts, lag = 12), lag = 6)
  diff_thrice <- diff(diff_twice, lag = 3)
  
  plot(diff_twice, ylab = "Differenced Passenger Miles", xlab = "Time", bty = "l",
       xlim = c(1991, 2024.25), main = "Lag-12, then Lag-6", yaxt = "n")
  axis(2, at = axTicks(2), labels = label_number(scale = 1e-6, suffix = "M")(axTicks(2)))
  
  plot(diff_thrice, ylab = "Differenced Passenger Miles", xlab = "Time", bty = "l",
       xlim = c(1991, 2024.25), main = "Lag-12, Lag-6, Lag-3", yaxt = "n")
  axis(2, at = axTicks(2), labels = label_number(scale = 1e-6, suffix = "M")(axTicks(2)))
  
  par(mfrow = c(1, 1))  # reset for next plots
})

#
#Here is the 2nd way to make a "seasonal adjustment" to time series data. It uses
#data from the decompose() command. 
#

PassengerMiles.SeasAdj <- PassengerMiles.ts - Amtrak.comp$seasonal 

save_plot("step4_seasonal_adjustment_classical.png", {
  library(scales)
  par(mfrow = c(2, 2))
  
  # Original series
  plot(PassengerMiles.ts, xlab = "Time", ylab = "Passenger Miles", type = "l",
       main = "Original Series", yaxt = "n")
  axis(2, at = axTicks(2), labels = label_number(scale = 1e-6, suffix = "M")(axTicks(2)))
  
  # Seasonally adjusted
  plot(PassengerMiles.SeasAdj, xlab = "Time", ylab = "Passenger Miles", type = "l",
       main = "Seasonally Adjusted", yaxt = "n")
  axis(2, at = axTicks(2), labels = label_number(scale = 1e-6, suffix = "M")(axTicks(2)))
  
  # Zoomed-in view
  PassengerMiles.seas.zoom <- window(PassengerMiles.SeasAdj, start = c(1997, 1), end = c(2000, 12))
  plot(PassengerMiles.seas.zoom, xlab = "Time", ylab = "Passenger Miles", type = "l",
       main = "Zoomed-In Adjustment (1997–2000)", yaxt = "n")
  axis(2, at = axTicks(2), labels = label_number(scale = 1e-6, suffix = "M")(axTicks(2)))
  
  par(mfrow = c(1, 1))
})



#
#Again, there doesn't appear to be a obvious periodicity in this zoomed in plot.
#

#
#Here is the 3rd way to make a seasonal adjustment.  It is from your textbook,
#Section 3.5. To use this you will need to install the "seasonal" package in R/
#RStudio. Also, to use the "seasonal" package we need to transform out time
#series data from .ts into a tsibble. 
#

# ----- COMMENTING since the X-11 decomposition using 'seasonal' is not compatible on macOS) -----
# PassengerMiles.tsb <- as_tsibble(PassengerMiles.ts)
# PassengerMiles.tsb
# x11_dcmp <- PassengerMiles.tsb |>
#   model(x11 = X_13ARIMA_SEATS(value ~ x11())) |>
#   components()
# autoplot(x11_dcmp) +
#   labs(title = "Decomposition of total Amtrak Passenger Miles using X-11.")

# x11_dcmp |>
#   ggplot(aes(x = index)) +
#   geom_line(aes(y = value, colour = "Data")) +
#   geom_line(aes(y = season_adjust, colour = "Seasonally Adjusted")) +
#   geom_line(aes(y = trend, colour = "Trend")) +
#   labs(y = "Passenger Miles", title = "Total Amtrak Passenger Miles") +
#   scale_colour_manual(values = c("gray", "#0072B2", "#D55E00"), breaks = c("Data", "Seasonally Adjusted", "Trend"))

# ----- REPLACING WITH: STL Decomposition using feasts::STL -----

PassengerMiles.tsb <- tsibble::as_tsibble(PassengerMiles.ts)
PassengerMiles.stl <- PassengerMiles.tsb |>
  model(STL(value ~ season(window = "periodic"))) |>
  components()

save_plot("step4_amtrak_stl_decomposition.png", {
  print(
    autoplot(PassengerMiles.stl) +
      labs(title = "STL Decomposition of Amtrak Passenger Miles")
  )
})



#
#This decomposition looks a little different than the decomposition using the
#decompose() command.  Notice the spike in the trend data just before the year 
#2015?  It isn't nearly as sharp of a spike in the trend using the decompose()
#command from the "forecast" package.  
#
#Let's finish this off with a nice way to illustrate the decomposition of a
#time series.
#

save_plot("step4_amtrak_stl_final_composite.png", {
  library(scales)
  PassengerMiles.stl |>
    ggplot(aes(x = index)) +
    geom_line(aes(y = value, colour = "Data")) +
    geom_line(aes(y = season_adjust, colour = "Seasonally Adjusted")) +
    geom_line(aes(y = trend, colour = "Trend")) +
    labs(x = "Time",
         y = "Passenger Miles",
         title = "STL Trend & Seasonality Decomposition") +
    scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
    scale_colour_manual(
      values = c("gray", "#0072B2", "#D55E00"),
      breaks = c("Data", "Seasonally Adjusted", "Trend")
    )
})

#
#The zoomed in figure isn't a very pretty figure but it is hard to see any 
#repeating pattern in it that would suggest any residual periodicity. You might
#have to make more than one adjustment to the data.  What you are doing is 
#removing the trend and seasonality to make the data stationary.  The textbook
#does a good job of covering what this means but you may have to read and reread
#a couple sections to get it clearly set in your mind.This is an important
#part of time series analysis so you want to make sure you really do 
#understand what it means for your data to be stationary and what you have to
#do to achieve that! 
#

# Refit the quadratic model
trend <- 1:length(PassengerMiles.ts)
PassengerMiles.lm <- tslm(PassengerMiles.ts ~ trend + I(trend^2))

# Save the quadratic fit plot with consistent Y-axis
save_plot("step4_quadratic_fit.png", {
  library(scales)
  plot(PassengerMiles.ts, xlab = "Time", ylab = "Passenger Miles", type = "l",
       main = "Quadratic Trend Fit", ylim = c(0, max(PassengerMiles.ts, na.rm = TRUE)),
       yaxt = "n")
  
  lines(PassengerMiles.lm$fitted.values, col = "blue", lwd = 2)
  
  axis(2, at = axTicks(2), labels = label_number(scale = 1e-6, suffix = "M")(axTicks(2)))
  
  legend("topright", legend = "Quadratic Fit", col = "blue", lwd = 2, bty = "n")
})


#
#From this plot it doesn't look like a quadratic model fits the data fairly well.
#Remember the last plot from the first laboratory?
#It was pretty obvious that the quadratic was not really working. So, let's add
#a cubic term and see if we get the same result as the last lab.
#

# Ensure trend is explicitly defined
trend <- 1:length(PassengerMiles.ts)

# Refit the cubic model
PassengerMiles.lm <- tslm(PassengerMiles.ts ~ trend + I(trend^2) + I(trend^3))

# Save the plot with formatted Y-axis
save_plot("step4_cubic_fit.png", {
  library(scales)
  plot(PassengerMiles.ts, xlab = "Time", ylab = "Passenger Miles", type = "l",
       main = "Cubic Trend Fit", ylim = c(0, max(PassengerMiles.ts, na.rm = TRUE)),
       yaxt = "n")
  
  # Add fitted line
  lines(PassengerMiles.lm$fitted.values, col = "red", lwd = 2)
  
  # Format y-axis using millions
  axis(2, at = axTicks(2), labels = label_number(scale = 1e-6, suffix = "M")(axTicks(2)))
  
  # Add legend
  legend("topright", legend = "Cubic Fit", col = "red", lwd = 2, bty = "n")
})

cat("===== ANA535 Lab 2 Execution Completed =====\n")

end_log()

