source("r/get_data.R")
library("lubridate")
library("dygraphs")
library("forecast")
library("prophet")
library("xts")
library("zoo")

coin <- get_crypto()

coin$date <- ymd(coin$date)

log_returns <- diff.xts(coin[, 2], lag = 1, log = T)
log_returns <- xts(log_returns, order.by = coin$date)
log_returns <- na.omit(log_returns)

min.date  <- min(coin$date)
min.year  <- year(min.date)
min.month <- month(min.date)
min.day   <- yday(min.date)

max.date  <- max(coin$date)
max.year  <- year(max.date)
max.month <- month(max.date)
max.day   <- yday(max.date)

# This one gets used when periods_to_forecast is set to zero in App
log_ret_without_forecast <- function() {
  
  coin <- get_crypto()
  
  coin$date <- ymd(coin$date)
  
  log_returns <- diff.xts(coin[, 2], lag = 1, log = T)
  log_returns <- xts(log_returns, order.by = coin$date)
  log_returns <- na.omit(log_returns)
  
  log_returns %>%
    dygraph(main = "Log Returns (No Forecast)", ylab = "Log Return") %>%
    dyAxis("y") %>%
    dySeries("V1", color = "black", label = 'Log Return') %>%
    dyLegend(labelsSeparateLines = TRUE) %>%
    dyRangeSelector(
      height = 40,
      strokeColor = "grey",
      dateWindow = c(as.Date(index(tail(log_returns, n=1))) - 30, as.Date(index(tail(log_returns, n=1))) + 1)
    ) %>%
    dyOptions(digitsAfterDecimal = 4, drawPoints = TRUE, pointSize = 2) %>%
    dyCSS(
      textConnection(
        ".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }"
      )
    )
}

# LSTM

lstm <- function(
  periods_to_forecast = 1) {
  
  coin <- get_crypto()
  coin$date <- ymd(coin$date)
  
  log_returns <- diff.xts(coin[, 2], lag = 1, log = T)
  log_returns <- xts(log_returns, order.by = coin$date)
  log_returns <- na.omit(log_returns)
  
  train_data <- as.matrix(log_returns[1:1680])
  mean <- apply(train_data, 2, mean)
  std <- apply(train_data, 2, sd)
  
  last_30_observations <- tail(log_returns,30)
  last_observation <- tail(log_returns,1) # Used to trace line in graph
  pred_data <- as.matrix(last_30_observations)
  pred_data_1 <- scale(pred_data, center = mean, scale = std)
  pdata <- t(pred_data_1) #Transpose matrix
  dim(pdata) <- c(dim(pdata), 1)
  
  lstm_do <- load_model_tf("models/LSTM_BEST")
  
  pred_vector <- as.matrix(last_observation)[1,1]
  
  for(i in 1:periods_to_forecast) {                                        
    pred_value <-  lstm_do %>% predict(
      pdata ,
      batch_size = 40)
    
    pred_vector <- rbind(pred_vector, pred_value)
    
    pdata <- pdata[1, 2:30, 1]
    pdata[30] <- pred_value
    pdata <- as.matrix(pdata)
    pdata <- t(pdata)
    dim(pdata) <- c(dim(pdata), 1)
    
  }
  
  idx <- tk_index(log_returns)
  
  idx_future <- tk_make_future_timeseries(idx, length_out = periods_to_forecast) 
  
  idx_future <- as.Date(c(idx_future[1]-1, idx_future))
  
  
  {
    cbind(
      Log_Return = log_returns,
      forecast_mean = xts(pred_vector, idx_future)
      #forecast_mean = xts(cbind(y = .$mean), idx_future)
    )
  } %>%
    dygraph(main = 'LSTM', ylab = "Log Return") %>%
    dyAxis("y") %>%
    dyAxis("x", label = "Date") %>%
    dySeries("forecast_mean", color = "blue", label = "Forecast") %>%
    dySeries("Log_Return", color = "black", label = "Log Return") %>%
    dyLegend(labelsSeparateLines = TRUE) %>%
    dyRangeSelector(
      height = 40,
      strokeColor = "grey",
      dateWindow = c(as.Date(index(tail(log_returns, n=1))) - 30, as.Date(index(tail(log_returns, n=1))) + periods_to_forecast + 1)
    ) %>%
    dyOptions(digitsAfterDecimal = 4, drawPoints = TRUE, pointSize = 2) %>%
    dyCSS(
      textConnection(
        ".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }"
      )
    )
}


# Naive Forecast

naive_lr <- function(
                      periods_to_forecast = 1,
                      main_title = "") {
  interval_value_formatter <-
    "function(num, opts, seriesName, g, row, col) {
              value = g.getValue(row, col);
              if(value[0] != value[2]) {
                lower = Dygraph.numberValueFormatter(value[0], opts);
                upper = Dygraph.numberValueFormatter(value[2], opts);
                return '[' + lower + ', ' + upper + ']';
              } else {
                return Dygraph.numberValueFormatter(num, opts);
              }
            }"
  
  coin <- get_crypto()
  
  coin$date <- ymd(coin$date)
  
  log_returns <- diff.xts(coin[, 2], lag = 1, log = T)
  log_returns <- xts(log_returns, order.by = coin$date)
  log_returns <- na.omit(log_returns)
  
  last_observation <- tail(log_returns,1) # Used to trace line in graph
  
  idx <- tk_index(log_returns)
  
  idx_future <- tk_make_future_timeseries(idx, length_out = periods_to_forecast) 
  
  last_observation <- tail(log_returns,1) # Used to trace line in graph
  
  idx_future_2 <- as.Date(c(idx_future[1]-1, idx_future))
  
  value_today <- as.matrix(last_observation)[1,1]
  
  log_returns %>%
    naive(
      .,
      h = periods_to_forecast,
      level = c(80, 95),
      fan = FALSE,
      simulate = FALSE,
      bootstrap = FALSE,
      npaths = 5000,
      PI = TRUE,
      biasadj = NULL
    ) %>%
    {
      cbind(
        Log_Return = log_returns,
        forecast_mean = xts(c(value_today, .$mean[1:periods_to_forecast]), idx_future_2),
        lower_95 = xts(cbind(y = .$lower[, "95%"]), idx_future),
        upper_95 = xts(cbind(y = .$upper[, "95%"]), idx_future),
        lower_80 = xts(cbind(y = .$lower[, "80%"]), idx_future),
        upper_80 = xts(cbind(y = .$upper[, "80%"]), idx_future)
      )
    } %>%
    dygraph(main = 'Naive', ylab = "Log Return") %>%
    dyAxis("y", valueFormatter = interval_value_formatter) %>%
    dyAxis("x", label = "Date") %>%
    dySeries("forecast_mean", color = "blue", label = "Forecast") %>%
    dySeries(c("lower_80", "forecast_mean", "upper_80"),
             label = "80% PI",
             color = "blue") %>%
    dySeries(c("lower_95", "forecast_mean", "upper_95"),
             label = "95% PI",
             color = "blue") %>%
    dySeries("Log_Return", color = "black", label = "Log Return") %>%
    dyLegend(labelsSeparateLines = TRUE) %>%
    dyRangeSelector(
      height = 40,
      strokeColor = "grey",
      dateWindow = c(as.Date(index(tail(log_returns, n=1))) - 30, as.Date(index(tail(log_returns, n=1))) + periods_to_forecast + 1)
    ) %>%
    dyOptions(digitsAfterDecimal = 4, drawPoints = TRUE, pointSize = 2) %>%
    dyCSS(
      textConnection(
        ".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }"
      )
    )
}

# Mean Forecast

mean_lr <- function(
  periods_to_forecast = 1,
  main_title = "") {
  interval_value_formatter <-
    "function(num, opts, seriesName, g, row, col) {
              value = g.getValue(row, col);
              if(value[0] != value[2]) {
                lower = Dygraph.numberValueFormatter(value[0], opts);
                upper = Dygraph.numberValueFormatter(value[2], opts);
                return '[' + lower + ', ' + upper + ']';
              } else {
                return Dygraph.numberValueFormatter(num, opts);
              }
            }"
  
  coin <- get_crypto()
  
  coin$date <- ymd(coin$date)
  
  log_returns <- diff.xts(coin[, 2], lag = 1, log = T)
  log_returns <- xts(log_returns, order.by = coin$date)
  log_returns <- na.omit(log_returns)
  
  idx <- tk_index(log_returns)
  
  idx_future <- tk_make_future_timeseries(idx, length_out = periods_to_forecast) 
  
  last_observation <- tail(log_returns,1) # Used to trace line in graph
  
  idx_future_2 <- as.Date(c(idx_future[1]-1, idx_future))
  
  value_today <- as.matrix(last_observation)[1,1]
  
  log_returns %>%
    meanf(
      .,
      h = periods_to_forecast,
      level = c(80, 95)
    ) %>%
    {
      cbind(
        Log_Return = log_returns,
        forecast_mean = xts(c(value_today, .$mean[1:periods_to_forecast]), idx_future_2),
        lower_95 = xts(cbind(y = .$lower[, "95%"]), idx_future),
        upper_95 = xts(cbind(y = .$upper[, "95%"]), idx_future),
        lower_80 = xts(cbind(y = .$lower[, "80%"]), idx_future),
        upper_80 = xts(cbind(y = .$upper[, "80%"]), idx_future)
      )
    } %>%
    dygraph(main = 'Mean', ylab = "Log Return") %>%
    dyAxis("y", valueFormatter = interval_value_formatter) %>%
    dySeries("forecast_mean", color = "blue", label = "Forecast") %>%
    dySeries(c("y", "forecast_mean", "y.1"),
             label = "80% PI",
             color = "blue") %>%
    dySeries(c("y.2", "forecast_mean", "y.3"),
             label = "95% PI",
             color = "blue") %>%
    dySeries("Log_Return", color = "black", label = "Log Return") %>%
    dyLegend(labelsSeparateLines = TRUE) %>%
    dyRangeSelector(
      height = 40,
      strokeColor = "grey",
      dateWindow = c(as.Date(index(tail(log_returns, n=1))) - 30, as.Date(index(tail(log_returns, n=1))) + periods_to_forecast + 1)
    ) %>%
    dyOptions(digitsAfterDecimal = 4, drawPoints = TRUE, pointSize = 2) %>%
    dyCSS(
      textConnection(
        ".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }"
      )
    )
}

# ETS

ets_lr <- function(
                   periods_to_forecast = 1) {
  interval_value_formatter <-
    "function(num, opts, seriesName, g, row, col) {
              value = g.getValue(row, col);
              if(value[0] != value[2]) {
                lower = Dygraph.numberValueFormatter(value[0], opts);
                upper = Dygraph.numberValueFormatter(value[2], opts);
                return '[' + lower + ', ' + upper + ']';
              } else {
                return Dygraph.numberValueFormatter(num, opts);
              }
            }"
  
  coin <- get_crypto()
  
  coin$date <- ymd(coin$date)
  
  log_returns <- diff.xts(coin[, 2], lag = 1, log = T)
  log_returns <- xts(log_returns, order.by = coin$date)
  log_returns <- na.omit(log_returns)
 
  ETS <- readRDS(file = "models/ets.rds")
  
  idx <- tk_index(log_returns)
  
  idx_future <- tk_make_future_timeseries(idx, length_out = periods_to_forecast) 
  
  last_observation <- tail(log_returns,1) # Used to trace line in graph
  
  idx_future_2 <- as.Date(c(idx_future[1]-1, idx_future))
  
  value_today <- as.matrix(last_observation)[1,1]
  
  log_returns %>%
    ets(model = ETS,
        use.initial.values=TRUE) %>%
    forecast(
      .,
      h = periods_to_forecast,
      level = c(80, 95),
      fan = FALSE,
      simulate = FALSE,
      bootstrap = FALSE,
      npaths = 5000,
      PI = TRUE,
      lambda = .$lambda,
      biasadj = NULL
    ) %>%
    {
      cbind(
        Log_Return = log_returns,
        forecast_mean = xts(c(value_today, .$mean[1:periods_to_forecast]), idx_future_2),
        lower_95 = xts(cbind(y = .$lower[, "95%"]), idx_future),
        upper_95 = xts(cbind(y = .$upper[, "95%"]), idx_future),
        lower_80 = xts(cbind(y = .$lower[, "80%"]), idx_future),
        upper_80 = xts(cbind(y = .$upper[, "80%"]), idx_future)
      )
    } %>%
    dygraph(main = "ETS", ylab = "Log Return") %>%
    dyAxis("y", valueFormatter = interval_value_formatter) %>%
    dySeries("forecast_mean", color = "blue", label = "Forecast") %>%
    dySeries(c("lower_80", "forecast_mean", "upper_80"),
             label = "80% PI",
             color = "blue") %>%
    dySeries(c("lower_95", "forecast_mean", "upper_95"),
             label = "95% PI",
             color = "blue") %>%
    dySeries("Log_Return", color = "black", label = "Log Return") %>%
    dyLegend(labelsSeparateLines = TRUE) %>%
    dyRangeSelector(
      height = 40,
      strokeColor = "grey",
      dateWindow = c(as.Date(index(tail(log_returns, n=1))) - 30, as.Date(index(tail(log_returns, n=1))) + periods_to_forecast + 1)
    ) %>%
    dyOptions(digitsAfterDecimal = 4, drawPoints = TRUE, pointSize = 2) %>%
    dyCSS(
      textConnection(
        ".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }"
      )
    )
}

