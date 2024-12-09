# Load necessary libraries
library(tidyverse)
library(forecast)
library(tseries)
library(ggplot2)
library(GGally)
library(tidyverse)

# set wd to current directory
setwd("C:\\Users\\utkar\\Desktop\\attention-spans-research\\notebooks")

# 1. Load the dataset
data <- read_csv("./weekly_time_series.csv") # Update the path if needed
head(data)

# 2. Visualize the avg_comment_length weekly time series data
data <- data %>%
  mutate(week = as.Date(week)) # Ensure week column is in Date format

ggplot(data, aes(x = seq_along(avg_comment_length), y = avg_comment_length)) +
    geom_line() +
    labs(title = "Weekly Avg Comment Length", x = "Week", y = "Average Comment Length") +
    theme(aspect.ratio = 9/16) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "in"), plot.background = element_rect(size = 6))

# 3. Perform differencing twice to remove the trend and visualize the differenced data
data_ts <- ts(data$avg_comment_length, frequency = 52) # Assuming weekly data with seasonality of 52
diff_data <- diff(data_ts, differences = 2)

plot(diff_data, type = "l", main = "Twice Differenced Data", xlab = "Time", ylab = "Differenced Values")

# 4. Perform 1 round of seasonal differencing and visualize this differenced data
seasonal_diff_data <- diff(diff_data, lag = 52)

plot(seasonal_diff_data, type = "l", main = "Seasonally Differenced Data", xlab = "Time", ylab = "Differenced Values")

# 5. Visualize the ACF and PACF of the cleaned time series
par(mfrow = c(1, 2)) # Plot side-by-side
acf(seasonal_diff_data, main = "ACF of Cleaned Data")
pacf(seasonal_diff_data, main = "PACF of Cleaned Data")

# 6. Run an ADF test on the cleaned data for stationarity
adf_test <- adf.test(seasonal_diff_data, alternative = "stationary")
print(adf_test)

# split data_ts into train and test
# 90% train data and 10% test data

# Training data: Up to the end of Year 6
train_data <- window(data_ts, end = c(5, 52))

# Test data: From the start of Year 7 onwards
test_data <- window(data_ts, start = c(6, 1))

# Display the train and test datasets
train_data
test_data

# fit a SARIMA(1, 1, 0)(0, 2, 0) model to the training data
sarima_model <- Arima(train_data, order = c(1, 1, 0), seasonal = list(order = c(1, 2, 1)))

# Forecast the test data using the SARIMA model
sarima_forecast <- forecast(sarima_model, h = length(test_data))

# Plot the forecasted values along with the test data for comparison using ggplot2

sarima_forecast_df <- data.frame(
  week = time(sarima_forecast$mean),
  avg_comment_length = sarima_forecast$mean,
  lower = sarima_forecast$lower,
  upper = sarima_forecast$upper,
  actual = test_data
)

plt <- ggplot(sarima_forecast_df, aes(x = seq_along(avg_comment_length)), dpi=600) +
    geom_line(aes(y = avg_comment_length, color = "Forecast"), size = 0.5) +
    geom_line(aes(y = actual, color = "Actual"), size = 0.5) +
    labs(title = "SARIMA Forecast vs Actual", x = "Week", y = "Average Comment Length") +
    theme(aspect.ratio = 9/16) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"), plot.background = element_rect(size = 6)) +
    theme(legend.position = "right") +
    scale_color_manual(values = c("Forecast" = "red", "Actual" = "black"), name = "Legend")

ggsave("sarima_forecast.png", plt, height = 6, units = "in", dpi = 600)

# Calculate the RMSE of the SARIMA model
sarima_rmse <- sqrt(mean((sarima_forecast$mean - test_data)^2))
sarima_rmse
