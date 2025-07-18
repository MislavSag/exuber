library(data.table)
library(finfeatures)


# paths
if (interactive()) {
  PATH_PRICES     = file.path("D:/strategies/exuber/daily_firms/prices")
  PATH_PREDICTORS = file.path("D:/strategies/exuber/predictors_daily_firms")
} else {
  PATH_PRICES = file.path("prices_daily_firms")
  PATH_PREDICTORS = file.path("predictors_daily_firms")
}

# Create directory if it doesnt exists
if (!dir.exists(PATH_PREDICTORS)) {
  dir.create(PATH_PREDICTORS)
}

# Get index
if (interactive()) {
  i = 1L
} else {
  i = as.integer(Sys.getenv('PBS_ARRAY_INDEX'))
}

# Get symbol
symbols = gsub("\\.csv", "", list.files(PATH_PRICES))
symbol_i = symbols[i]

# Import Ohlcv data
ohlcv = fread(file.path(PATH_PRICES, paste0(symbol_i, ".csv")))
ohlcv = Ohlcv$new(ohlcv[, .(symbol, date, open, high, low, close, volume)], date_col = "date")

# Exuber
exuber_init = RollingExuber$new(
  windows = c(1200),
  workers = 1L,
  at = 1:nrow(ohlcv$X),
  lag = 0L,
  exuber_lag = c(0L, 1L)
)
exuber = exuber_init$get_rolling_features(ohlcv, log_prices = TRUE)
fwrite(exuber, file.path(PATH_PREDICTORS, paste0(symbol_i, ".csv")))
