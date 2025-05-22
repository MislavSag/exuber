options(progress_enabled = FALSE)

library(data.table)
library(finfeatures)


# paths
if (interactive()) {
  PATH_PRICES     = file.path("D:/strategies/exuber/prices")
  PATH_PREDICTORS = file.path("D:/strategies/exuber/predictors")
} else {
  PATH_PRICES = file.path("prices")
  PATH_PREDICTORS = file.path("predictors")
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
if (attr(ohlcv$date, "tzone") == "UTC") {
  attr(ohlcv$date, "tzone") <- "America/New_York"
}
# tail(ohlcv, 15)
ohlcv[symbol == "a"]
head(ohlcv, 20)
ohlcv = Ohlcv$new(ohlcv[, .(symbol, date, open, high, low, close, volume)],
                  date_col = "date")

# Exuber
exuber_init = RollingExuber$new(
  windows = c(100, 200, 400, 800),
  workers = 1L,
  at = 1:220, # nrow(ohlcv$X),
  lag = 0L,
  exuber_lag = c(1L)
)
exuber = exuber_init$get_rolling_features(ohlcv, log_prices = TRUE)
fwrite(exuber, file.path(PATH_PREDICTORS, paste0(symbol_i, ".csv")))
