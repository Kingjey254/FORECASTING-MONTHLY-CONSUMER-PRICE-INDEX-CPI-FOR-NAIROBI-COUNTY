
# Load necessary libraries
install.packages("forecast")
install.packages("tseries")
install.packages("fpp2") 
install.packages("readr")
install.packages("urca")
install.packages("urca")
install.packages("tidyverse")
install.packages("zoo")
install.packages("xts")
install.packages("ggplot2")

library(xts)
library(zoo)
library(tidyverse)
library(urca)
library(urca)
library(forecast)
library(tseries)
library(fpp2)
library(readr)
library(ggplot2)

# Read the CSV file
ts_data <- read.csv(file.choose())
ts_data
# Create a data frame from the provided data
data <- data.frame(
  period = c("12-Jan", "12-Feb", "12-Mar", "12-Apr", "12-May", "12-Jun", "12-Jul", "12-Aug", "12-Sep", "12-Oct", "12-Nov", "12-Dec",
             "13-Jan", "13-Feb", "13-Mar", "13-Apr", "13-May", "13-Jun", "13-Jul", "13-Aug", "13-Sep", "13-Oct", "13-Nov", "13-Dec",
             "14-Jan", "14-Feb", "14-Mar", "14-Apr", "14-May", "14-Jun", "14-Jul", "14-Aug", "14-Sep", "14-Oct", "14-Nov", "14-Dec",
             "15-Jan", "15-Feb", "15-Mar", "15-Apr", "15-May", "15-Jun", "15-Jul", "15-Aug", "15-Sep", "15-Oct", "15-Nov", "15-Dec",
             "16-Jan", "16-Feb", "16-Mar", "16-Apr", "16-May", "16-Jun", "16-Jul", "16-Aug", "16-Sep", "16-Oct", "16-Nov", "16-Dec",
             "17-Jan", "17-Feb", "17-Mar", "17-Apr", "17-May", "17-Jun", "17-Jul", "17-Aug", "17-Sep", "17-Oct", "17-Nov", "17-Dec",
             "18-Jan", "18-Feb", "18-Mar", "18-Apr", "18-May", "18-Jun", "18-Jul", "18-Aug", "18-Sep", "18-Oct", "18-Nov", "18-Dec",
             "19-Jan", "19-Feb", "19-Mar", "19-Apr", "19-May", "19-Jun", "19-Jul", "19-Aug", "19-Sep", "19-Oct", "19-Nov", "19-Dec",
             "20-Jan", "20-Feb", "20-Mar", "20-Apr", "20-May", "20-Jun", "20-Jul", "20-Aug", "20-Sep", "20-Oct", "20-Nov", "20-Dec",
             "21-Jan", "21-Feb", "21-Mar", "21-Apr", "21-May", "21-Jun", "21-Jul", "21-Aug", "21-Sep", "21-Oct", "21-Nov", "21-Dec",
             "22-Jan", "22-Feb", "22-Mar", "22-Apr", "22-May", "22-Jun", "22-Jul", "22-Aug", "22-Sep", "22-Oct", "22-Nov", "22-Dec"),
  index = c(68.51, 68.54, 69.62, 70.05, 70.22, 69.5, 68.64, 68.19, 68.4, 68.71, 69.11, 69.64, 70.64, 71.18, 71.88, 72.62, 72.52, 72.43,
            72.62, 72.59, 74.54, 74.37, 74.38, 74.65, 75.4, 75.52, 75.86, 76.22, 76.84, 76.88, 77.13, 77.9, 77.9, 77.69, 77.53, 78.04,
            78.7, 79.03, 80.08, 81.26, 81.52, 81.9, 82.1, 82.22, 82.4, 82.8, 83.44, 84.44, 84.38, 84.19, 84.92, 85.61, 86.08, 87.04,
            87.29, 87.35, 87.49, 88.04, 88.71, 88.97, 89.9, 91.61, 93.38, 95.11, 95.48, 94.13, 93.27, 93.95, 93.73, 93.83, 93.65,
            94.23, 95.59, 96.83, 98.36, 99.22, 100.05, 99.35, 98.24, 97.89, 98.48, 97.41, 97.5, 98.2, 98.96, 100, 101.16, 101.92,
            102.52, 102.87, 103.11, 103.2, 103.39, 103.64, 104.26, 104.88, 105.48, 106.16, 106.5, 107.27, 107.24, 107.22, 107.14,
            107.21, 107.3, 107.79, 108.41, 109.74, 110.3, 111.11, 111.64, 112.36, 112.6, 112.73, 113.19, 113.31, 113.7, 114, 114.26,
            115.49, 115.79, 116.35, 117.32, 119.07, 119.89, 120.98, 121.87, 122.39, 123.57, 124.39, 124.74, 125.22)
)# Convert the 'period' column to a valid date format
data$period <- as.Date(paste0("20", data$period, "-01"), format = "%Y-%b-%d")
data$period
# Remove rows with missing values
clean_data <- na.omit(data)
clean_data
library(ggplot2)
# Plot the cleaned data
ggplot(clean_data, aes(x = period, y = index)) +
  geom_line(color = "blue") +
  labs(title = "Time Series of Returns", x = "Time", y = "Returns")
#Extract the "index" column
y <- ts_data$index
y
library(quantmod)
#Calculate Return
ret=ROC(y)
ret
# Define the 'period' and 'ret' vectors
period <- c("12-Jan", "12-Feb", "12-Mar", "12-Apr", "12-May", "12-Jun", "12-Jul", "12-Aug", "12-Sep", "12-Oct", "12-Nov", "12-Dec",
             "13-Jan", "13-Feb", "13-Mar", "13-Apr", "13-May", "13-Jun", "13-Jul", "13-Aug", "13-Sep", "13-Oct", "13-Nov", "13-Dec",
             "14-Jan", "14-Feb", "14-Mar", "14-Apr", "14-May", "14-Jun", "14-Jul", "14-Aug", "14-Sep", "14-Oct", "14-Nov", "14-Dec",
             "15-Jan", "15-Feb", "15-Mar", "15-Apr", "15-May", "15-Jun", "15-Jul", "15-Aug", "15-Sep", "15-Oct", "15-Nov", "15-Dec",
             "16-Jan", "16-Feb", "16-Mar", "16-Apr", "16-May", "16-Jun", "16-Jul", "16-Aug", "16-Sep", "16-Oct", "16-Nov", "16-Dec",
             "17-Jan", "17-Feb", "17-Mar", "17-Apr", "17-May", "17-Jun", "17-Jul", "17-Aug", "17-Sep", "17-Oct", "17-Nov", "17-Dec",
             "18-Jan", "18-Feb", "18-Mar", "18-Apr", "18-May", "18-Jun", "18-Jul", "18-Aug", "18-Sep", "18-Oct", "18-Nov", "18-Dec",
             "19-Jan", "19-Feb", "19-Mar", "19-Apr", "19-May", "19-Jun", "19-Jul", "19-Aug", "19-Sep", "19-Oct", "19-Nov", "19-Dec",
             "20-Jan", "20-Feb", "20-Mar", "20-Apr", "20-May", "20-Jun", "20-Jul", "20-Aug", "20-Sep", "20-Oct", "20-Nov", "20-Dec",
             "21-Jan", "21-Feb", "21-Mar", "21-Apr", "21-May", "21-Jun", "21-Jul", "21-Aug")

ret <- c(NA, 0.0004377964, 0.0156343659, 0.0061573904, 0.0024238980, -0.0103064182, -0.0124512974, -0.0065775286, 0.0030748981, 0.0045219244,
         0.0058046891, 0.0076396768, 0.0142574414, 0.0076153238, 0.0097861818, 0.0102423041, -0.0013779801, -0.0012418077, 0.0026197878, -0.0004131947,
         0.0265087228, -0.0022832593, 0.0001344538, 0.0036234355, 0.0099967510, 0.0015902468, 0.0044920144, 0.0047343592, 0.0081014425, 0.0005204268,
         0.0032465453, 0.0099336429, 0.0000000000, -0.0026994039, -0.0020615907, 0.0065565575, 0.0084216397, 0.0041843718, 0.0131986078, 0.0146277556,
         0.0031944984, 0.0046506019, 0.0024390256, 0.0014605650, 0.0021868554, 0.0048426245, 0.0076997493, 0.0119134123, -0.0007108163, -0.0022542573,
         0.0086334875, 0.0080924619, 0.0054749977, 0.0110906867, 0.0028681256, 0.0006871278, 0.0016014645, 0.0062667555, 0.0075813659, 0.0029266117,
         0.0103987072, 0.0188424945, 0.0191367535, 0.0183569270, 0.0038826850, -0.0142399959, -0.0091782928, 0.0072642131, -0.0023444171, 0.0010663255,
         -0.0019202054, 0.0061741733, 0.0143296099, 0.0128886515, 0.0156773537, 0.0087053895, 0.0083304542, -0.0070210920, -0.0112355046, -0.0035690651,
         0.0060090826, -0.0109246071, 0.0009235032, 0.0071538374, 0.0077095127, 0.0104544579, 0.0115332358, 0.0074847700, 0.0058697097, 0.0034081536,
         0.0023303244, 0.0008724735, 0.0018393926, 0.0024151101, 0.0059644236, 0.0059290601, 0.0057045220, 0.0064260285, 0.0031975951, 0.0072040355,
         -0.0002797072, -0.0001865150, -0.0007464079, 0.0006531374, 0.0008391218, 0.0045562402, 0.0057354459, 0.0121935960, 0.0050899946, 0.0073167753)
# Update the period vector to match the length of the ret vector
period <- head(period, length(ret))
period
# Create a data frame
data <- data.frame(period = period, ret = ret)
data
# Remove rows with missing values
clean_data <- na.omit(data)
clean_data
str(clean_data)
unique(clean_data$period)
# Convert 'period' to a factor
clean_data$period <- factor(clean_data$period, levels = unique(clean_data$period))
clean_data$period
# Plot the data
plot(clean_data$period, clean_data$ret, type = "l", col = "blue", 
     main = "Time Series of Returns", xlab = "Time", ylab = "Returns")
#Stationarity
# Check for missing or infinite values in log_return
any(is.na(ret))
any(is.infinite(ret))
# If there are missing or infinite values, remove or impute them
ret<- ret[!is.na(ret) & is.finite(ret)]
ret
library(urca)
# Run the ADF test
adf_test <- ur.df(ret, type = "trend", lags = 4)
adf_test
summary(adf_test)
#Normality test
# Perform Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(ret)
shapiro_test
summary(shapiro_test)
# ACF and PACF plots for returns
par(mfrow = c(1, 2))
acf(ret, main = "ACF of log_return")
pacf(ret, main = "PACF of log_return")
# Fit an ARMA model to the returns
arma_model <- arima(ret, order = c(1, 0, 1))
arma_model
summary(arma_model)
# Plot the residuals
# Define the labels for the X-axis ticks
month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
month_labels
# Repeat the month labels for each year
year_labels <- rep(month_labels, 10)
year_labels
# Define the months
months <- seq_along(arma_model$residuals)
months
# Subset year_labels to match the length of months
year_labels <- year_labels[1:length(months)]
year_labels
# Plot the residuals
plot(arma_model$residuals, main = "Residuals Plot", ylab = "Residuals", type = "p", xaxt = "n")
# Suppress X-axis
axis(1, at = months, labels = year_labels)
# Forecast returns for the next 12 months
forecast_steps <- 12
forecast_values <- forecast::forecast(arma_model, h = forecast_steps)
forecast_values
accuracy(forecast_values)
# Plot the forecasted values
plot(forecast_values, main = "ARMA Forecast for Returns", xlab = "Time", ylab = "Returns")
# Define the years from 2013 to 2023
years <- 2013:2023
years
# Plot historical CPI
plot(1:length(ret), ret, type = "l", col = "blue", xlab = "Time", ylab = "CPI", main = "Historical and Forecasted CPI", xaxt = "n")
# Calculate the number of years
num_years <- length(years)
num_years
# Determine the tick positions (assuming data is monthly)
tick_positions <- seq(1, length(ret), by = 12)
tick_positions
# Remove any tick positions exceeding the length of 'ret'
tick_positions <- tick_positions[tick_positions <= length(ret)]
tick_positions
# Adjust the number of years to match the length of 'tick_positions'
num_years <- min(num_years, length(tick_positions))
num_years
# Add x-axis with custom ticks and labels
axis(1, at = c(tick_positions, length(ret) + 1), labels = c(years[1:num_years], "2023"), las = 2)
# Add lines for forecasted values
lines(length(ret) + seq_along(forecasted_values), forecasted_values, col = "red")
# Add legend
legend("topleft", legend = c("Historical CPI", "Forecasted CPI"), col = c("blue", "red"), lty = 1)

