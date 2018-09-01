library('ggplot2')
library('forecast')
library('tseries')

daily_data <- read.csv("bike_data/day.csv", header = TRUE,
                       stringsAsFactors = FALSE)

daily_data$date <- as.Date(daily_data$dteday)

graph <- ggplot(data = daily_data) +
  geom_line(aes(date, cnt)) + scale_x_date('month')

ggsave(filename = "original.png", plot = graph, width = 6, height = 4)

# clean the data from the time dimension
daily_ts <- ts(daily_data[, c('cnt')])

# tsclean() to remove time series outliers
daily_data$clean_cnt <- tsclean(daily_ts)

graph <- ggplot(data = daily_data) +
  geom_line(aes(date, clean_cnt)) +
  scale_x_date('month') +
  ylab('Cleaned Bicycle Count') 

ggsave(filename = "cleaned.png", plot = graph, width = 6, height = 4)

# Moving Average (ma) demo

# weekly moving average
daily_data$cnt_ma <- ma(daily_data$clean_cnt, order = 7)
# monthly moving average
daily_data$cnt_ma30 = ma(daily_data$clean_cnt, order = 30)

graph <- ggplot(data = daily_data) +
  geom_line(aes(x = date, y = clean_cnt, colour = "Counts")) +
  geom_line(aes(x = date, y = cnt_ma, colour = "Weekly moving average")) +
  geom_line(aes(x = date, y = cnt_ma30, colour = "Monthly moving average")) +
  scale_x_date('month') +
  ylab('Bicycle Count')

ggsave(filename = "moving_average.png", plot = graph, width = 6, height = 4)

# Decomposition

# calculate seasonal component of data using stl()
ts_ma <- ts(na.omit(daily_data$cnt_ma), frequency = 30)
decom <- decompose(ts_ma, "additive")
deseasonal_cnt <- seasadj(decom)

png(filename = "deseasonal.png", width = 1024, height = 800)
plot(decom)
dev.off()

# Augmented Dickey-Fuller Test (ADF) - test if data is stationary
stationary_test <- adf.test(ts_ma, alternative = "stationary")
#print(stationary_test)

# Fitting an arima model
fit <- auto.arima(deseasonal_cnt, seasonal = FALSE)

# calculate ACF and PACF and choose correct order for p and q
png(filename = "fit_non_seasonal_residuals.png", width = 1024, height = 800)
tsdisplay(residuals(fit), lag.max = 45, main = "(1,1,1) Model Residuals")
dev.off()

# see that AIC is smaller for this order
fit2 <- arima(deseasonal_cnt, order = c(1,1,7))

png(filename = "fit_order_1_1_7.png", width = 1024, height = 800)
tsdisplay(residuals(fit2), lag.max = 45, main = "(1,1,7) Model Residuals")
dev.off()

# forecast
fcast <- forecast(fit2, h = 30)
png(filename = "forecast_1_1_7.png", width = 1024, height = 800)
plot(fcast)
dev.off()

# fit with seasonality
fit_w_season <- auto.arima(deseasonal_cnt, seasonal = TRUE)

# residuals
png(filename = "fit_w_season_residuals.png", width = 1024, height = 800)
tsdisplay(residuals(fit_w_season), lag.max = 45,
          main = "Seasonal Model Residuals")
dev.off()

# forecast - better prediction
fcast <- forecast(fit_w_season, h = 30)
png(filename = "forecast_seasonal.png", width = 1024, height = 800)
plot(fcast)
dev.off()