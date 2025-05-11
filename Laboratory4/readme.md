# ANA 535 Laboratory #4 – ARIMA Models and Forecasts

This repository contains all code, outputs, plots, and documentation related to Laboratory #4 for ANA 535: Forecasting and Time Series Analysis. The focus of this lab is on building and validating ARIMA models through simulation, diagnostics, and real-world forecasting applications.

---

## 🔍 Project Overview

This lab was divided into two major steps:

### **Step 1: Time Series Simulation and Diagnostics**
- Simulated white noise, AR(1), MA(1), and AR(1) with drift processes
- Applied ACF, PACF, ADF, and KPSS tests
- Performed trend fitting and decomposition on:
  - Historical Earthquake Magnitude data
  - Google Stock Closing Prices
- Generated 32 figures to illustrate structure, autocorrelation, and transformations

### **Step 2: ARIMA Forecasting and Model Evaluation**
- Built and validated ARIMA models on Amtrak monthly passenger miles (1991–2016)
- Compared model performance using AIC, BIC, and residual diagnostics
- Applied ARIMA modeling to additional datasets:
  - Turkey Industrial Production
  - Tasmania Tourism and Accommodations
  - U.S. Souvenir Sales
  - U.S. GDP (with transformation and structural fitting)
- Generated 53 figures and multiple validation metrics (MAPE, RMSE, MAE)

---

## 📂 Repository Structure
Laboratory4
├── Lab4Step1.R # Step 1 simulation & diagnostics script
├── Lab4Step2.R # Step 2 forecasting & ARIMA model script
├── ana535_lab4_step1_output_log.txt # Console output log for Step 1
├── ana535_lab4_step2_output_log.txt # Console output log for Step 2
├── Amtrak1991-2024.csv # Amtrak dataset
├── GoogleStockData-5yr.v2.csv # Google stock prices
├── quakes.v2.csv # Earthquake magnitude data
├── output/
│ └── plots/
│ ├── step1/ # 32 plots for simulation/diagnostics
│ └── step2/ # 53 plots for ARIMA modeling
├── ANA535_Laboratory4_Final_Written_Report.docx # Final APA-formatted report
└── README.md # This file
