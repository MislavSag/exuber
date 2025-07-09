library(finutils)
library(fastverse)
library(lubridate)
library(PerformanceAnalytics)
library(TTR)
library(ggplot2)
library(roll)


# SETUP -------------------------------------------------------------------
# Globals
PATH = "D:/strategies/exuber"

# Paramters
SAMPLE = Inf # can be 100, 500, 1000, Inf
ALT    = TRUE

# DATA --------------------------------------------------------------------
# Prices
# files = list.files(file.path(PATH, "prices"), full.names = TRUE)
# prices = lapply(files, fread)
# prices = prices[, .(symbol, date, open, close)]

# Get data for SPY
data = qc_hour(
  file_path = "F:/lean/data/stocks_hour.csv",
  symbols = c("spy", "tlt"),
  duplicates = "fast"
)
spy = data[symbol == "spy"]
tlt = data[symbol == "tlt"]
spy_tlt = spy[date >= tlt[, min(date)]]

# Second try, tlt and spy together
if (ALT == TRUE) {
  prices_dt = dcast(data, date ~ symbol, value.var = c("open", "close"))
  prices_dt = na.omit(prices_dt)
}

# Import indicators
if (SAMPLE== 100) {
  indicators = fread(paste0(file.path(PATH, "indicators"), "/univ_100.csv"))
} else if (SAMPLE == 500) {
  indicators = fread(paste0(file.path(PATH, "indicators"), "/univ_500.csv"))
} else if (SAMPLE == 1000) {
  indicators = fread(paste0(file.path(PATH, "indicators"), "/univ_1000.csv"))
} else if (is.infinite(SAMPLE)) {
  indicators = fread(file.path(PATH, "indicators.csv"))
} else {
  stop("SAMPLE must be either 100, 500, 1000 or Inf")
}

# Create meta indicators as sum of individual indicators for every window
cols = colnames(indicators)
cols[grep("sd.*800_0", cols)]
indicators_agg = cbind(
  indicators[,.(date)],
  indicators[,.(sd_0800_0 = rowSums(.SD)), .SDcols = cols[grep("sd.*800_0", cols)]],
  indicators[,.(sd_0800_1 = rowSums(.SD)), .SDcols = cols[grep("sd.*800_1", cols)]],
  indicators[,.(sd_0400_0 = rowSums(.SD)), .SDcols = cols[grep("sd.*400_0", cols)]],
  indicators[,.(sd_0400_1 = rowSums(.SD)), .SDcols = cols[grep("sd.*400_1", cols)]],
  indicators[,.(sd_0200_0 = rowSums(.SD)), .SDcols = cols[grep("sd.*200_0", cols)]],
  indicators[,.(sd_0200_1 = rowSums(.SD)), .SDcols = cols[grep("sd.*200_1", cols)]],
  indicators[,.(sd_0100_0 = rowSums(.SD)), .SDcols = cols[grep("sd.*100_0", cols)]],
  indicators[,.(sd_0100_1 = rowSums(.SD)), .SDcols = cols[grep("sd.*100_1", cols)]],
  indicators[,.(mean_0800_0 = rowSums(.SD)), .SDcols = cols[grep("mean.*800_0", cols)]],
  indicators[,.(mean_0800_1 = rowSums(.SD)), .SDcols = cols[grep("mean.*800_1", cols)]],
  indicators[,.(mean_0400_0 = rowSums(.SD)), .SDcols = cols[grep("mean.*400_0", cols)]],
  indicators[,.(mean_0400_1 = rowSums(.SD)), .SDcols = cols[grep("mean.*400_1", cols)]],
  indicators[,.(mean_0200_0 = rowSums(.SD)), .SDcols = cols[grep("mean.*200_0", cols)]],
  indicators[,.(mean_0200_1 = rowSums(.SD)), .SDcols = cols[grep("mean.*200_1", cols)]],
  indicators[,.(mean_0100_0 = rowSums(.SD)), .SDcols = cols[grep("mean.*100_0", cols)]],
  indicators[,.(mean_0100_1 = rowSums(.SD)), .SDcols = cols[grep("mean.*100_1", cols)]]
)


# PREPARE DATA ------------------------------------------------------------
# Merge SPY and indicators
# backtest_data = indicators[spy, on = "date"]
# backtest_data = indicators_agg[prices_dt, on = "date"]
backtest_data = indicators_agg[spy, on = "date"]

# Add trend predictor
backtest_data[, trend := EMA(close, n = 7 * 10) > EMA(close, n = 7 * 10 * 50)]
# backtest_data[, trend := EMA(close_spy, n = 7 * 10) > EMA(close_spy, n = 7 * 10 * 50)]

# Remove missing values
backtest_data = na.omit(backtest_data)
setorder(backtest_data, date)

# Hour, daily and week Returns across deciles
backtest_data[, let(
  target_hour = shift(close, type = "lead", n = 1L) / close - 1,
  target_daily = shift(close, type = "lead", n = 7L) / close - 1,
  target_week = shift(close, type = "lead", n = 10L * 7L) / close - 1
)]
# backtest_data[, let(
#   target_hour = shift(close_spy, type = "lead", n = 1L) / close_spy - 1,
#   target_daily = shift(close_spy, type = "lead", n = 7L) / close_spy - 1,
#   target_week = shift(close_spy, type = "lead", n = 10L * 7L) / close_spy - 1,
#   target_hour_tlt = shift(close_tlt, type = "lead", n = 1L) / close_tlt - 1,
#   target_daily_tlt = shift(close_tlt, type = "lead", n = 7L) / close_tlt - 1,
#   target_week_tlt = shift(close_tlt, type = "lead", n = 10L * 7L) / close_tlt - 1
# )]
backtest_data = na.omit(backtest_data)

# Downsample
dtd = backtest_data[, .(
  date = data.table::last(date),
  close = data.table::last(close),
  open = data.table::first(open),
  close_raw = data.table::last(close_raw),
  volume = sum(volume),
  trend = data.table::last(trend),
  target_hour = data.table::last(target_hour),
  target_daily = data.table::last(target_daily),
  target_week = data.table::last(target_week)
), by = .(date)]


