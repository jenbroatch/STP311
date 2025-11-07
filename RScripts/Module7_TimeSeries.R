
#Module 7 

# Load packages
library(ggplot2)
library(dplyr)
library(forecast)

# Assume you already have a data frame named buypower with columns: year, value

buypower<- read.csv("https://raw.githubusercontent.com/jenbroatch/STP311/refs/heads/master/DataSets/Buypwer.csv", header=F)
names(buypower) <- c("year", "TimePeriod", "value")

# Create lagged variable
buypower <- buypower %>%
  mutate(value_lag = dplyr::lag(value))

ggplot(buypower, aes(x = year, y = value)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Plot of Buying Power Data",
       x = "Year",
       y = "Buying Power") +
  theme_minimal()

# Lag Plot
ggplot(buypower, aes(x = value_lag, y = value)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Lag Plot of Buying Power Data",
       x = "Lagged Value",
       y = "Current Value") +
  theme_minimal()

#create a ts object 
buypower_ts <- ts(buypower$value, start = min(buypower$year), frequency = 1)

# ACF plot — shows correlation between current and lagged values
acf(buypower_ts, main = "ACF of Buying Power")

# PACF plot — shows correlation after removing effects of previous lags
pacf(buypower_ts, main = "PACF of Buying Power")

# Load required package
library(tseries)

# Ljung–Box test (use lag = 10 or 20 commonly)
Box.test(buypower_ts, lag = 10, type = "Ljung-Box")

# Durbin–Watson test
library(lmtest)
dwtest(buypower_ts ~ 1)

# Difference the series
diff_buypower <- diff(buypower_ts)

# Plot ACF and PACF of differenced data
acf(diff_buypower, main = "ACF of Differenced Buying Power")
pacf(diff_buypower, main = "PACF of Differenced Buying Power")

library(forecast) 
fit_arima <- Arima(buypower_ts, order = c(1,1,0))
summary(fit_arima)
checkresiduals(fit_arima)

fit_011 <- Arima(buypower_ts, order = c(0,1,1))
summary(fit_011)
checkresiduals(fit_011)

forecast_5yr <- forecast(fit_arima, h = 5)
autoplot(forecast_5yr) + ggtitle("Forecast of Buying Power (ARIMA(1,1,0))")
forecast_5yr

forecast_5yr <- forecast(fit_011, h = 5)
autoplot(forecast_5yr) + ggtitle("Forecast of Buying Power (ARIMA(0,1,1))")


# Install if needed

install.packages("fpp3")

library(fpp3)
library(dplyr)


# Use Australian retail trade data — monthly retail turnover by industry (1982–2020)
data <- aus_retail %>%
  filter(Industry == "Department stores") %>%
  select(Month, Turnover = Turnover)

autoplot(data, Turnover) +
  ggtitle("Monthly Department Store Turnover in Australia") +
  ylab("Turnover ($ millions)") + xlab("Year")

# Fit ARIMA automatically
fit_arima <- data %>%
  model(ARIMA(Turnover ~ PDQ(0,0,0) + pdq(1,1,1)))
report(fit_arima)

# Fit pure AR model (AR(2))
fit_ar <- data %>%
  model(ARIMA(Turnover ~ pdq(2,0,0)))
report(fit_ar)

# Fit pure MA model (MA(2))
fit_ma <- data %>%
  model(ARIMA(Turnover ~ pdq(0,0,2)))
report(fit_ma)

# Forecast 12 months ahead
fc <- fit_arima %>% forecast(h = "12 months")
autoplot(fc, data)