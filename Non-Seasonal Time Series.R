library(stats)
library(tseries)
library(tidyverse)
library(TSA)
library(ggplot2)
library(rugarch)
library(forecast)
library(dplyr)

df <- read.csv("C:/Users/Aston/Downloads/Academic Projects/Time Series Project/twitter-stocks.csv")
df <- select(df, Date, Close)

df$Date <- as.Date(df$Date, "%Y-%m-%d")
num_rows <- nrow(df)
new_dates <- seq(from = as.Date("2013-11-01"), by = "days", length.out = num_rows)
df$Date <- new_dates
start_date <- as.Date("2013-11-01")
end_date <- as.Date("2019-12-31")
filtered_data <- subset(df, Date >= start_date & Date <= end_date)
head(filtered_data)
tail(filtered_data)

ggplot(filtered_data, aes(x = Date, y = Close)) +
  geom_line() +
  labs(title = "Time Series of Close Prices(Original)",
       x = "Date",
       y = "Close Price")

train_data <- subset(df, Date >= start_date & Date <= as.Date("2019-10-31"))
test_data <- subset(df, Date > as.Date("2019-10-31") & Date <= end_date)
tail(train_data)
data1 <- train_data$Close

acf(data1, lag.max = 150, main='ACF of Original data')
#exponentially dying 

pacf(data1, lag.max = 150, main='PACF of Original data')
#Seems like AR(1)
eacf(data1)

#Checking for stationarity
adf_test = adf.test(data1)
print(adf_test)
print("Since p-value=0.3565 > 0.05, the TS is non-stationary")
#Since p-value=0.3215 > 0.05, the TS is non-stationary

close_diff <- diff(data1)
plot(close_diff, type = 'l', xlab = 'Date', ylab = 'Value', main = 'Line graph for log-differencing')

adf_test = adf.test(close_diff)
print(adf_test)
print("Since p-value=0.01 < 0.05, the TS is stationary")
#Since p-value=0.01 < 0.05, the TS is stationary

acf(close_diff, main='ACF of log-differenced data')

pacf(close_diff, main='PACF of log-differenced data')

eacf(close_diff)
plot_arima_resid <- function(model) {
  resid <- resid(model)
  resid_ts <- ts(resid, start=c(2013, 311), end=c(2020,300), frequency=365)
  p <- model$arma[1]
  q <- model$arma[2]
  plot(resid_ts, xlab = "Year", ylab = 'Residuals', main=paste('Residual Plot',"(", p, ", 1, ", q, ")"))
  acf(resid_ts[1:length(resid_ts)],lag.max=150, main=paste('ACF of Residual',"(", p, ", 1, ", q, ")"))
  pacf(resid_ts[1:length(resid_ts)],lag.max=150, main=paste('PACF of Residual',"(", p, ", 1, ", q, ")"))
}

my_df <- data.frame(model = character(),
                    AIC = numeric(),
                    BIC = numeric(),
                    Shapiro = round(numeric(),3),
                    Ljung = round(numeric(),3))
new_row <- data.frame(model='',AIC=0,BIC=0,Shapiro=0,Ljung=0)

plot_arima_residuals <- function(arima_model, my_df) {
  resid <- residuals(arima_model)
  resid_ts <- ts(resid, start=c(2013, 311), end=c(2020,300), frequency=365)
  p <- arima_model$arma[1]
  q <- arima_model$arma[2]
  order <- paste("(", p, ", 1, ", q, ")", sep = "")
  plot_arima_resid(arima_model)
  qqnorm(resid_ts, main=paste('Residuals plot',order))
  qqline(resid_ts)
  
  shap=shapiro.test(resid_ts)
  
  ljung=Box.test(resid, lag = 20, type = "Ljung-Box")
  
  cat("AIC:", AIC(arima_model),"\n")
  cat("BIC:", BIC(arima_model),"\n")
  model_name <- paste("ARIMA", paste(order, collapse = ','), sep = " ")
  if (!model_name %in% my_df$model) {
    new_row <- data.frame(model = model_name,
                          AIC = AIC(arima_model),
                          BIC = BIC(arima_model),
                          Shapiro = round(shap$p.value, 3),
                          Ljung = round(ljung$p.value, 3))
    
    my_df <- rbind(my_df, new_row)
  }
}


arima011 <- arima(x=close_diff, order=c(0,1,1))
arima110 <- arima(x=close_diff, order=c(1,1,0))
arima111 <- arima(x=close_diff, order=c(1,1,1))
arima210 <- arima(x=close_diff, order=c(2,1,0))
arima211 <- arima(x=close_diff, order=c(2,1,1))
arima012 <- arima(x=close_diff, order=c(0,1,2))
arima112 <- arima(x=close_diff, order=c(1,1,2))
arima212 <- arima(x=close_diff, order=c(2,1,2))
arima310 <- arima(x=close_diff, order=c(3,1,0))
arima311 <- arima(x=close_diff, order=c(3,1,1))
arima312 <- arima(x=close_diff, order=c(3,1,2))
arima013 <- arima(x=close_diff, order=c(0,1,3))
arima113 <- arima(x=close_diff, order=c(1,1,3))
arima213 <- arima(x=close_diff, order=c(2,1,3))
arima313 <- arima(x=close_diff, order=c(3,1,3))



my_df <- plot_arima_residuals(arima011, my_df)
my_df <- plot_arima_residuals(arima110, my_df)
my_df <- plot_arima_residuals(arima111, my_df)
my_df <- plot_arima_residuals(arima210, my_df)
my_df <- plot_arima_residuals(arima211, my_df)
my_df <- plot_arima_residuals(arima012, my_df)
my_df <- plot_arima_residuals(arima112, my_df)
my_df <- plot_arima_residuals(arima212, my_df)
my_df <- plot_arima_residuals(arima310, my_df)
my_df <- plot_arima_residuals(arima311, my_df)
my_df <- plot_arima_residuals(arima312, my_df)
my_df <- plot_arima_residuals(arima013, my_df)
my_df <- plot_arima_residuals(arima113, my_df)
my_df <- plot_arima_residuals(arima213, my_df)
my_df <- plot_arima_residuals(arima313, my_df)

my_df

BIC_values <- c(BIC(arima011), BIC(arima110), BIC(arima111),
                BIC(arima210), BIC(arima211), BIC(arima012), BIC(arima112), BIC(arima212),
                BIC(arima310), BIC(arima311), BIC(arima312), BIC(arima013), BIC(arima113), BIC(arima213), BIC(arima313))
BIC_values

min_BIC <- min(BIC_values)
min_BIC

min_BIC_index <- which.min(BIC_values)
min_BIC_index
# Obtain the residuals from the ARIMA model
residuals <- residuals(arima011)

# Plot the ACF and PACF of residuals
acf(residuals, main = 'ACF of Residuals')
pacf(residuals, main = 'PACF of Residuals')
# Ljung-Box test for residual autocorrelation
tsdiag(arima011)


predicted_values <- predict(arima011, n.ahead = 60)
print(predicted_values)
test_data$Close
original_forecasts <- diffinv(predicted_values$pred, lag = 1, differences = 1, xi = 39.84)
original_forecasts <- original_forecasts[original_forecasts != 0]
#original_forecasts <- original_forecasts[-1]
original_forecasts

plot(test_data$Date, test_data$Close, main = 'Original Data vs Forecast for 2019', type = 'l', col = 'blue', xlab = 'Date', ylab = 'Employees')

# Adding forecasted values to the plot (if 'original_forecasts' is aligned with 'test_data$Date')
lines(test_data$Date, original_forecasts, col = 'red', type = 'l')

# Adding legend
legend('topleft', legend = c('Original Data', 'Forecasts'), col = c('blue', 'red'), lty = 1)

print("From the residual analyis, we can see that ARIMA model could not capture any dependencies")


###GARCH MODEL### normal return
return = diff(log(train_data$Close))

#Try with absolute and square of the returns
acf(return, lag.max = 100, main='ACF of return')
pacf(return, lag.max = 100, main='PACF of return')
eacf(return)
garch(x=return, grad='analytical', trace=FALSE)
garch_spec <- ugarchspec(variance.model = list(garchOrder = c(2,2)),
                         mean.model = list(armaOrder = c(3,4)),
                         distribution.model = 'sstd')

garch_fit <- ugarchfit(spec = garch_spec, data = return, solver = 'hybrid')
summary(garch_fit)
garch_fit

resid_norm = residuals(garch_fit)
qqnorm(resid_norm, main='QQ plot of residuals from GARCH ')
qqline(resid_norm)
acf(resid_norm, lag.max = 60, main='ACF of residual')
pacf(resid_norm, lag.max = 60, main='PACF of residual')

initial_term <- train_data$Close[length(train_data$Close)]
# Forecast next 60 days
forecasted <- ugarchforecast(garch_fit, n.ahead = 60, startMethod = "sample", initial.level = initial_term)
# Extract the forecasted values
forecast_values <- fitted(forecasted)

predicted_diff <- cumsum(c(0, forecast_values))
predicted_log <- log(initial_term) + predicted_diff
predicted_close <- exp(predicted_log)
print(predicted_close[-1])

plot(test_data$Date, test_data$Close, main = 'Original Data vs Forecast for 60 days', type = 'l', col = 'blue', xlab = 'Date', ylab = 'Employees')

# Adding forecasted values to the plot (if 'original_forecasts' is aligned with 'test_data$Date')
lines(test_data$Date, predicted_close, col = 'red', type = 'l')

# Adding legend
legend('topleft', legend = c('Original Data', 'Forecasts'), col = c('blue', 'red'), lty = 1)
conditional_variance <- garch_fit@fit$sigma^2
std_resid <- resid_norm / sqrt(conditional_variance)
plot(std_resid, type = "l", ylab = "Standardized Residuals")

# Portmanteau test for squared residuals
Box.test(resid_norm, lag = 20, type = "Ljung-Box")


###GARCH MODEL### abs(return)
return = abs(diff(log(train_data$Close)))

#Try with absolute and square of the returns
acf(return, lag.max = 100, main='ACF of return')
pacf(return, lag.max = 100, main='PACF of return')
eacf(return)
garch(x=return, grad='numerical', trace=FALSE)
garch_spec <- ugarchspec(variance.model = list(garchOrder = c(2,2)),
                         mean.model = list(armaOrder = c(3,5)),
                         distribution.model = 'std')

garch_fit <- ugarchfit(spec = garch_spec, data = return, solver = 'hybrid')
summary(garch_fit)
garch_fit

resid_norm = residuals(garch_fit)
qqnorm(resid_norm, main='QQ plot of residuals from GARCH ')
qqline(resid_norm)
acf(resid_norm, lag.max = 60, main='ACF of residual')
pacf(resid_norm, lag.max = 60, main='PACF of residual')

initial_term <- train_data$Close[length(train_data$Close)]
# Forecast next 60 days
forecasted <- ugarchforecast(garch_fit, n.ahead = 60, startMethod = "sample")
# Extract the forecasted values
forecast_values <- fitted(forecasted)
predicted_diff <- cumsum(c(0, forecast_values))
predicted_log <- log(initial_term) + predicted_diff
predicted_close <- exp(predicted_log)
print(predicted_close[-1])

plot(test_data$Date, test_data$Close, main = 'Original Data vs Forecast for 60 days', type = 'l', col = 'blue', xlab = 'Date', ylab = 'Employees')

# Adding forecasted values to the plot (if 'original_forecasts' is aligned with 'test_data$Date')
lines(test_data$Date, predicted_close, col = 'red', type = 'l')

# Adding legend
legend('topleft', legend = c('Original Data', 'Forecasts'), col = c('blue', 'red'), lty = 1)
conditional_variance <- garch_fit@fit$sigma^2
std_resid <- resid_norm / sqrt(conditional_variance)
plot(std_resid, type = "l", ylab = "Standardized Residuals")

# Portmanteau test for squared residuals
Box.test(resid_norm, lag = 20, type = "Ljung-Box")


###GARCH MODEL### (return)^2
return = diff(log(train_data$Close))^2

#Try with absolute and square of the returns
acf(return, lag.max = 100, main='ACF of return')
pacf(return, lag.max = 100, main='PACF of return')
eacf(return)
garch(x=return, grad='numerical', trace=FALSE)
garch_spec <- ugarchspec(variance.model = list(garchOrder = c(2,2)),
                         mean.model = list(armaOrder = c(2,2)),
                         distribution.model = 'std')

garch_fit <- ugarchfit(spec = garch_spec, data = return, solver = 'hybrid')
summary(garch_fit)
garch_fit

resid_norm = residuals(garch_fit)
qqnorm(resid_norm, main='QQ plot of residuals from GARCH ')
qqline(resid_norm)
acf(resid_norm, lag.max = 60, main='ACF of residual')
pacf(resid_norm, lag.max = 60, main='PACF of residual')

sfinal <- garch_spec
setfixed(sfinal) <- as.list(coef(garch_fit))
coef(garch_fit)
f2019 <- ugarchforecast(data = return, fitORspec = sfinal, n.ahead = 60)
plot(sigma(f2019))
sim <- ugarchpath(spec = sfinal, m.sim = 2, n.sim = 1*60, rseed = 123)
p <- 39.84*apply(fitted(sim), 2, 'cumsum') + 39.84
matplot(p, type = "l", lwd = 3)

###GJR-GARCH Model###
garch_spec <- ugarchspec(variance.model = list(model = 'gjrGARCH'),
                         mean.model = list(armaOrder = c(2,2)),
                         distribution.model = 'std')

garch_fit <- ugarchfit(spec = garch_spec, data = return, solver = 'hybrid')
summary(garch_fit)
garch_fit

resid_norm = residuals(garch_fit)
qqnorm(resid_norm, main='QQ plot of residuals from GARCH ')
qqline(resid_norm)
acf(resid_norm, lag.max = 60, main='ACF of residual')
pacf(resid_norm, lag.max = 60, main='PACF of residual')

sfinal <- garch_spec
setfixed(sfinal) <- as.list(coef(garch_fit))
coef(garch_fit)
f2019 <- ugarchforecast(data = return, fitORspec = sfinal, n.ahead = 60)
plot(sigma(f2019))
sim <- ugarchpath(spec = sfinal, m.sim = 2, n.sim = 1*60, rseed = 123)
p <- 39.84*apply(fitted(sim), 2, 'cumsum') + 39.84
matplot(p, type = "l", lwd = 3)









