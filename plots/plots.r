source("r/get_data.R")
library("lubridate")
library("dygraphs")
library("xts")
library("jsonlite")


# Load data
coin <- get_crypto()

prepare_data <- function(coin) {

  if (coin == "doge") {
    coin <- get_crypto(coin)
    coin$date <- ymd(as.Date(coin$date))
    coin$price <- round(coin$price,10)
    coin <- xts(coin[-c(1)], order.by = coin$date)
  } else {
    coin <- get_crypto(coin)
    coin$date <- ymd(as.Date(coin$date))
    coin <- xts(coin[-c(1)], order.by = coin$date)
  }
  
}

btc <- prepare_data('btc')
eth <- prepare_data('eth')
bnb <- prepare_data('bnb')
xrp <- prepare_data('xrp')
doge <- prepare_data('doge')
usdt <- prepare_data('usdt')
ada <- prepare_data('ada')
dot <- prepare_data('dot')
ltc <- prepare_data('ltc')
link <- prepare_data('link')

# Get date Info 
summary(btc)
summary(eth)
summary(bnb)
summary(xrp)
summary(doge)
summary(usdt)
summary(ada)
summary(dot)
summary(ltc)
summary(link)

dyList1 <- list(
  dygraph(btc, main = "BTC", group = "gr1", width = 666, height = 250) %>%
    dySeries("price", color = "black", label = 'Price') %>%
    dySeries("date", color = "white", label = NULL) %>%
    dyLegend(labelsSeparateLines=TRUE) %>%
    dyRangeSelector(height = 1, strokeColor = "Black", fillColor = "Black",
                    dateWindow = c(as.Date(index(head(btc, n=1))), as.Date(index(tail(btc, n=1)))), retainDateWindow = TRUE) %>%
    dyOptions(digitsAfterDecimal = 4) %>%
    dyCSS(textConnection(".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }")),
  
  dygraph(eth, main = "ETH", group = "gr1", width = 666, height = 250) %>%
    dySeries("price", color = "black", label = 'Price') %>%
    dySeries("date", color = "white", label = NULL) %>%
    dyLegend(labelsSeparateLines=TRUE) %>%
    dyRangeSelector(height = 1, strokeColor = "Black", fillColor = "Black", dateWindow = c(as.Date(index(head(btc, n=1))) - 28, as.Date(index(tail(btc, n=1)))), retainDateWindow = TRUE) %>%
    dyOptions(digitsAfterDecimal = 4) %>%
    dyCSS(textConnection(".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }")))

htmltools::browsable(htmltools::tagList(dyList1))

dyList2 <- list(
  dygraph(xrp, main = "XRP", group = "gr1", width = 666, height = 250) %>%
    dySeries("price", color = "black", label = 'Price') %>%
    dySeries("date", color = "white", label = NULL) %>%
    dyLegend(labelsSeparateLines=TRUE) %>%
    dyRangeSelector(height = 1, strokeColor = "Black", fillColor = "Black",
                    dateWindow = c(as.Date(index(head(btc, n=1))), as.Date(index(tail(btc, n=1)))), retainDateWindow = TRUE) %>%
    dyOptions(digitsAfterDecimal = 4) %>%
    dyCSS(textConnection(".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }")),
  
  dygraph(dot, main = "DOT", group = "gr1", width = 666, height = 250) %>%
    dySeries("price", color = "black", label = 'Price') %>%
    dySeries("date", color = "white", label = NULL) %>%
    dyLegend(labelsSeparateLines=TRUE) %>%
    dyRangeSelector(height = 1, strokeColor = "Black", fillColor = "Black", dateWindow = c(as.Date(index(head(btc, n=1))) - 28, as.Date(index(tail(btc, n=1)))), retainDateWindow = TRUE) %>%
    dyOptions(digitsAfterDecimal = 4) %>%
    dyCSS(textConnection(".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }")))

htmltools::browsable(htmltools::tagList(dyList2))

dyList3 <- list(
  dygraph(bnb, main = "BNB", group = "gr1", width = 666, height = 250) %>%
    dySeries("price", color = "black", label = 'Price') %>%
    dySeries("date", color = "white", label = NULL) %>%
    dyLegend(labelsSeparateLines=TRUE) %>%
    dyRangeSelector(height = 1, strokeColor = "Black", fillColor = "Black",
                    dateWindow = c(as.Date(index(head(btc, n=1))), as.Date(index(tail(btc, n=1)))), retainDateWindow = TRUE) %>%
    dyOptions(digitsAfterDecimal = 4) %>%
    dyCSS(textConnection(".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }")),
  
  dygraph(ada, main = "ADA", group = "gr1", width = 666, height = 250) %>%
    dySeries("price", color = "black", label = 'Price') %>%
    dySeries("date", color = "white", label = NULL) %>%
    dyLegend(labelsSeparateLines=TRUE) %>%
    dyRangeSelector(height = 1, strokeColor = "Black", fillColor = "Black", dateWindow = c(as.Date(index(head(btc, n=1))) - 28, as.Date(index(tail(btc, n=1)))), retainDateWindow = TRUE) %>%
    dyOptions(digitsAfterDecimal = 4) %>%
    dyCSS(textConnection(".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }")))

htmltools::browsable(htmltools::tagList(dyList3))

dyList4 <- list(
  dygraph(link, main = "LINK", group = "gr1", width = 666, height = 250) %>%
    dySeries("price", color = "black", label = 'Price') %>%
    dySeries("date", color = "white", label = NULL) %>%
    dyLegend(labelsSeparateLines=TRUE) %>%
    dyRangeSelector(height = 1, strokeColor = "Black", fillColor = "Black",
                    dateWindow = c(as.Date(index(head(btc, n=1))), as.Date(index(tail(btc, n=1)))), retainDateWindow = TRUE) %>%
    dyOptions(digitsAfterDecimal = 4) %>%
    dyCSS(textConnection(".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }")),
  
  dygraph(ltc, main = "LTC", group = "gr1", width = 666, height = 250) %>%
    dySeries("price", color = "black", label = 'Price') %>%
    dySeries("date", color = "white", label = NULL) %>%
    dyLegend(labelsSeparateLines=TRUE) %>%
    dyRangeSelector(height = 1, strokeColor = "Black", fillColor = "Black", dateWindow = c(as.Date(index(head(btc, n=1))) - 28, as.Date(index(tail(btc, n=1)))), retainDateWindow = TRUE) %>%
    dyOptions(digitsAfterDecimal = 4) %>%
    dyCSS(textConnection(".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }")))

htmltools::browsable(htmltools::tagList(dyList4))

dyList5 <- list(
  dygraph(doge, main = "DOGE", group = "gr1", width = 666, height = 250) %>%
    dySeries("price", color = "black", label = 'Price') %>%
    dySeries("date", color = "white", label = NULL) %>%
    dyLegend(labelsSeparateLines=TRUE) %>%
    dyRangeSelector(height = 1, strokeColor = "Black", fillColor = "Black",
                    dateWindow = c(as.Date(index(head(btc, n=1))), as.Date(index(tail(btc, n=1)))), retainDateWindow = TRUE) %>%
    dyOptions(digitsAfterDecimal = 4) %>%
    dyCSS(textConnection(".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }")),
  
  dygraph(usdt, main = "USDT", group = "gr1", width = 666, height = 250) %>%
    dySeries("price", color = "black", label = 'Price') %>%
    dySeries("date", color = "white", label = NULL) %>%
    dyLegend(labelsSeparateLines=TRUE) %>%
    dyRangeSelector(height = 1, strokeColor = "Black", fillColor = "Black", dateWindow = c(as.Date(index(head(btc, n=1))) - 28, as.Date(index(tail(btc, n=1)))), retainDateWindow = TRUE) %>%
    dyOptions(digitsAfterDecimal = 4) %>%
    dyCSS(textConnection(".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }")))

htmltools::browsable(htmltools::tagList(dyList5))

prepare_data2 <- function(coin) {
  
  if (coin == "doge") {
    coin <- get_crypto(coin)
  } else {
    coin <- get_crypto(coin)
  }
  
}

btc <- prepare_data2('btc')
eth <- prepare_data2('eth')
bnb <- prepare_data2('bnb')
xrp <- prepare_data2('xrp')
doge <- prepare_data2('doge')
usdt <- prepare_data2('usdt')
ada <- prepare_data2('ada')
dot <- prepare_data2('dot')
ltc <- prepare_data2('ltc')
link <- prepare_data2('link')

summary(btc$price)
sd(btc$price)

summary(eth$price)
sd(eth$price)

summary(bnb$price)
sd(bnb$price)

summary(doge$price)
sd(doge$price)

summary(usdt$price)
sd(usdt$price)

summary(ada$price)
sd(ada$price)

summary(dot$price)
sd(dot$price)

summary(ltc$price)
sd(ltc$price)

summary(link$price)
sd(link$price)


coin$date <- ymd(coin$date)

log_returns <- xts(coin, order.by = coin$date)
log_returns <- na.omit(log_returns)

min.date  <- min(coin$date)
min.year  <- year(min.date)
min.month <- month(min.date)
min.day   <- yday(min.date)

max.date  <- max(coin$date)
max.year  <- year(max.date)
max.month <- month(max.date)
max.day   <- yday(max.date)

# CRIX Price

log_returns %>%
  dygraph(main = "CRIX Price", ylab = "CRIX") %>%
  dyAxis("y") %>%
  dySeries("price", color = "black", label = 'CRIX') %>%
  dyLegend(labelsSeparateLines = TRUE) %>%
  dyOptions(digitsAfterDecimal = 4) %>%
  dyCSS(
    textConnection(
      ".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }"
    )
  )


# Code used to get csv files

#Obtain BTC
btc <- fromJSON(readLines('https://api.coingecko.com/api/v3/coins/bitcoin/market_chart?vs_currency=usd&days=max&interval=daily')[1])
btc <- as.data.frame(btc$prices)
colnames(btc) <- c('date', 'price')
btc$date <- as.POSIXct(btc$date/ 1000.0, origin="1970-01-01", tz="GMT")
write.csv(btc,"C:\\Users\\gonza\\Documents\\MasterArbeit\\marbeit\\plots\\btc.csv", row.names = TRUE)

#Obtain BTC
eth <- fromJSON(readLines('https://api.coingecko.com/api/v3/coins/ethereum/market_chart?vs_currency=usd&days=max&interval=daily')[1])
eth <- as.data.frame(eth$prices)
colnames(eth) <- c('date', 'price')
eth$date <- as.POSIXct(eth$date/ 1000.0, origin="1970-01-01", tz="GMT")
write.csv(eth,"C:\\Users\\gonza\\Documents\\MasterArbeit\\marbeit\\plots\\eth.csv", row.names = TRUE)

#Obtain DOGE
doge <- fromJSON(readLines('https://api.coingecko.com/api/v3/coins/dogecoin/market_chart?vs_currency=usd&days=max&interval=daily')[1])
doge <- as.data.frame(doge$prices)
colnames(doge) <- c('date', 'price')
doge$date <- as.POSIXct(doge$date/ 1000.0, origin="1970-01-01", tz="GMT")
write.csv(doge,"C:\\Users\\gonza\\Documents\\MasterArbeit\\marbeit\\plots\\doge.csv", row.names = TRUE)

#Obtain BNB
bnb <- fromJSON(readLines('https://api.coingecko.com/api/v3/coins/binancecoin/market_chart?vs_currency=usd&days=max&interval=daily')[1])
bnb <- as.data.frame(bnb$prices)
colnames(bnb) <- c('date', 'price')
bnb$date <- as.POSIXct(bnb$date/ 1000.0, origin="1970-01-01", tz="GMT")
write.csv(bnb,"C:\\Users\\gonza\\Documents\\MasterArbeit\\marbeit\\plots\\bnb.csv", row.names = TRUE)

#Obtain XRP
xrp <- fromJSON(readLines('https://api.coingecko.com/api/v3/coins/ripple/market_chart?vs_currency=usd&days=max&interval=daily')[1])
xrp <- as.data.frame(xrp$prices)
colnames(xrp) <- c('date', 'price')
xrp$date <- as.POSIXct(xrp$date/ 1000.0, origin="1970-01-01", tz="GMT")
write.csv(xrp,"C:\\Users\\gonza\\Documents\\MasterArbeit\\marbeit\\plots\\xrp.csv", row.names = TRUE)

#Obtain USDT
usdt <- fromJSON(readLines('https://api.coingecko.com/api/v3/coins/tether/market_chart?vs_currency=usd&days=max&interval=daily')[1])
usdt <- as.data.frame(usdt$prices)
colnames(usdt) <- c('date', 'price')
usdt$date <- as.POSIXct(usdt$date/ 1000.0, origin="1970-01-01", tz="GMT")
write.csv(usdt,"C:\\Users\\gonza\\Documents\\MasterArbeit\\marbeit\\plots\\usdt.csv", row.names = TRUE)

#Obtain ADA
ada <- fromJSON(readLines('https://api.coingecko.com/api/v3/coins/cardano/market_chart?vs_currency=usd&days=max&interval=daily')[1])
ada <- as.data.frame(ada$prices)
colnames(ada) <- c('date', 'price')
ada$date <- as.POSIXct(ada$date/ 1000.0, origin="1970-01-01", tz="GMT")
write.csv(ada,"C:\\Users\\gonza\\Documents\\MasterArbeit\\marbeit\\plots\\ada.csv", row.names = TRUE)

#Obtain DOT
dot <- fromJSON(readLines('https://api.coingecko.com/api/v3/coins/polkadot/market_chart?vs_currency=usd&days=max&interval=daily')[1])
dot <- as.data.frame(dot$prices)
colnames(dot) <- c('date', 'price')
dot$date <- as.POSIXct(dot$date/ 1000.0, origin="1970-01-01", tz="GMT")
write.csv(dot,"C:\\Users\\gonza\\Documents\\MasterArbeit\\marbeit\\plots\\dot.csv", row.names = TRUE)

#Obtain LTC
ltc <- fromJSON(readLines('https://api.coingecko.com/api/v3/coins/litecoin/market_chart?vs_currency=usd&days=max&interval=daily')[1])
ltc <- as.data.frame(ltc$prices)
colnames(ltc) <- c('date', 'price')
ltc$date <- as.POSIXct(ltc$date/ 1000.0, origin="1970-01-01", tz="GMT")
write.csv(ltc,"C:\\Users\\gonza\\Documents\\MasterArbeit\\marbeit\\plots\\ltc.csv", row.names = TRUE)

#Obtain LINK
link <- fromJSON(readLines('https://api.coingecko.com/api/v3/coins/chainlink/market_chart?vs_currency=usd&days=max&interval=daily')[1])
link <- as.data.frame(link$prices)
colnames(link) <- c('date', 'price')
link$date <- as.POSIXct(link$date/ 1000.0, origin="1970-01-01", tz="GMT")
write.csv(link,"C:\\Users\\gonza\\Documents\\MasterArbeit\\marbeit\\plots\\link.csv", row.names = TRUE)
