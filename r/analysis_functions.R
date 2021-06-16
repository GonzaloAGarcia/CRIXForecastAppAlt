myacf <- function() {
  
  coin <- get_crypto()
  coin$date <- ymd(coin$date)
  log_returns <- diff(log(coin[,2]), lag=1)
  
  
  min.date  <- min(coin$date)
  min.year  <- year(min.date)
  min.month <- month(min.date)
  min.day   <- day(min.date)
  max.date  <- max(coin$date)
  max.year  <- year(max.date)
  max.month <- month(max.date)
  max.day   <- day(max.date)
  
  log_returns <- ts(log_returns, frequency = 365, 
                    start = c(min.year, min.month, min.day),
                    end = c(max.year, max.month, max.day))
  
  
  Acf(log_returns, lag.max = 33)
}


mypacf <- function() {
  
  coin <- get_crypto()
  coin$date <- ymd(coin$date)
  log_returns <- diff(log(coin[,2]), lag=1)
  
  
  min.date  <- min(coin$date)
  min.year  <- year(min.date)
  min.month <- month(min.date)
  min.day   <- day(min.date)
  max.date  <- max(coin$date)
  max.year  <- year(max.date)
  max.month <- month(max.date)
  max.day   <- day(max.date)
  
  log_returns <- ts(log_returns, frequency = 365, 
                    start = c(min.year, min.month, min.day),
                    end = c(max.year, max.month, max.day))
  
  Pacf(log_returns, lag.max = 33)
}




mdecomp <- function(part = "seasonal") {
  
  coin <- get_crypto()
  coin$date <- ymd(coin$date)
  log_returns <- diff(log(coin[,2]), lag=1)
  
  
  min.date  <- min(coin$date)
  min.year  <- year(min.date)
  min.month <- month(min.date)
  min.day   <- day(min.date)
  max.date  <- max(coin$date)
  max.year  <- year(max.date)
  max.month <- month(max.date)
  max.day   <- day(max.date)
  
  log_returns <- ts(log_returns, frequency = 365, 
                    start = c(min.year, min.month, min.day),
                    end = c(max.year, max.month, max.day))
  
  log_returns_month <- ts(log_returns, frequency = 12, 
                          start = c(min.year, min.month),
                          end = c(max.year, max.month))
  
  stl <- stl(log_returns_month, s.window="periodic")
  
  if (part == "seasonal") {
    
    dygraph(stl$time.series[,1], main = "Seasonal Component", group = "mdecomp") %>%
      dySeries("V1", color = "black", label = 'Seasonal component') %>%
      dyLegend(labelsSeparateLines=TRUE) %>%
      dyRangeSelector(height = 40, strokeColor = "Black", fillColor = "Black", retainDateWindow = TRUE) %>%
      dyOptions(digitsAfterDecimal = 4, pointSize = 2) %>%
      dyCSS(textConnection(".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }"))
    
  } else if (part == "trend") {
    
    dygraph(stl$time.series[,2], main = "Trend Component", group = "mdecomp") %>%
      dySeries("V1", color = "black", label = 'Trend component') %>%
      dyLegend(labelsSeparateLines=TRUE) %>%
      dyRangeSelector(height = 40, strokeColor = "Black", fillColor = "Black", retainDateWindow = TRUE) %>%
      dyOptions(digitsAfterDecimal = 4, pointSize = 2) %>%
      dyCSS(textConnection(".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }"))
    
  } else if (part == "remainder") {
    
    dygraph(stl$time.series[,3], main = "Remainder", group = "mdecomp") %>%
      dySeries("V1", color = "black", label = 'Remainder') %>%
      dyLegend(labelsSeparateLines=TRUE) %>%
      dyRangeSelector(height = 40, strokeColor = "Black", fillColor = "Black", retainDateWindow = TRUE) %>%
      dyOptions(digitsAfterDecimal = 4, pointSize = 2) %>%
      dyCSS(textConnection(".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }"))
    
  }
  
}

