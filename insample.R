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


# DATA --------------------------------------------------------------------
# Prices
files = list.files(file.path(PATH, "prices"), full.names = TRUE)
prices = lapply(files, fread)
prices = prices[, .(symbol, date, open, close)]

# Get data for SPY
spy = qc_hour(
  file_path = "F:/lean/data/stocks_hour.csv",
  symbols = "spy",
  duplicates = "fast"
)

# Import predictors in chunks Predictors
files = list.files(file.path(PATH, "predictors"), full.names = TRUE)
fread(files[1])
dt = lapply(files, fread)
dt = rbindlist(dt, fill= TRUE)
dt[, date := with_tz(date, tzone = "America/New_York")]
setorder(dt, symbol, date)

# Market cap
mcap = qc_daily(
  file_path = "F:/lean/data/stocks_daily.csv",
  duplicates = "none",
  symbols = dt[, unique(symbol)],
  profiles_fmp = TRUE,
  fmp_api_key = Sys.getenv("APIKEY")
)


# PREPARE DATA ------------------------------------------------------------
# Merge prices and predictors
dt = merge(dt, prices, by = c("symbol", "date"), all.x = TRUE, all.y = FALSE)

# Delete prices to save memory
# rm(prices)
# gc()

# Calculate returns
setorder(dt, symbol, date)
dt[, returns := close / shift(close) - 1, by = symbol]

# Remove missing values
dt = na.omit(dt)


# INDICATORS --------------------------------------------------------------
# Median aggreagation
radf_vars = colnames(dt)[grepl("exuber", colnames(dt))]
indicators_median = dt[, lapply(.SD, median, na.rm = TRUE), by = c('date'), .SDcols = radf_vars]
setnames(indicators_median, radf_vars, paste0("median_", radf_vars))

# Standard deviation aggregation
indicators_sd = dt[, lapply(.SD, sd, na.rm = TRUE), by = c('date'), .SDcols = radf_vars]
setnames(indicators_sd, radf_vars, paste0("sd_", radf_vars))

# Mean aggregation
indicators_mean = dt[, lapply(.SD, mean, na.rm = TRUE), by = c('date'), .SDcols = radf_vars]
setnames(indicators_mean, radf_vars, paste0("mean_", radf_vars))

# Sum aggreagation
indicators_sum = dt[, lapply(.SD, sum, na.rm = TRUE), by = c('date'), .SDcols = radf_vars]
setnames(indicators_sum, radf_vars, paste0("sum_", radf_vars))

# # Quantile aggregations
# indicators_q = dt[, lapply(.SD, quantile, probs = 0.99, na.rm = TRUE), by = c('date'), .SDcols = radf_vars]
# setnames(indicators_q, radf_vars, paste0("q99_", radf_vars))
# indicators_q97 = dt[, lapply(.SD, quantile, probs = 0.97, na.rm = TRUE), by = c('date'), .SDcols = radf_vars]
# setnames(indicators_q97, radf_vars, paste0("q97_", radf_vars))
# indicators_q95 = dt[, lapply(.SD, quantile, probs = 0.95, na.rm = TRUE), by = c('date'), .SDcols = radf_vars]
# setnames(indicators_q95, radf_vars, paste0("q95_", radf_vars))
#
# # Skew aggregation
# indicators_skew = dt[, lapply(.SD, skewness, na.rm = TRUE), by = c('date'), .SDcols = radf_vars]
# setnames(indicators_skew, radf_vars, paste0("skew_", radf_vars))
#
# # Kurtosis aggregation
# indicators_kurt = dt[, lapply(.SD, kurtosis, na.rm = TRUE), by = c('date'), .SDcols = radf_vars]
# setnames(indicators_kurt, radf_vars, paste0("kurtosis_", radf_vars))

# Merge indicators
indicators = Reduce(
  function(x, y)
    merge(
      x,
      y,
      by = "date",
      all.x = TRUE,
      all.y = TRUE
    ),
  list(
    indicators_sd,
    indicators_median,
    indicators_sum,
    indicators_mean
    # indicators_q,
    # indicators_q97,
    # indicators_q95,
    # indicators_skew,
    # indicators_kurt
  )
)
setorder(indicators, date)

# Remove missing values
indicators = na.omit(indicators)

# visualize
colnames(indicators)
plot(as.xts.data.table(indicators[, .(date, sd_exuber_200_0_sadf_log)]))
plot(as.xts.data.table(indicators[, .(date, median_exuber_200_0_sadf_log)]))
plot(as.xts.data.table(indicators[, .(date, mean_exuber_200_0_sadf_log)]))
# plot(as.xts.data.table(indicators[, .(date, q97_exuber_200_0_sadf_log)]))
# plot(as.xts.data.table(indicators[, .(date, q99_exuber_200_0_sadf_log)]))
# plot(as.xts.data.table(indicators[, .(date, skew_exuber_200_0_sadf_log)]))
# plot(as.xts.data.table(indicators[, .(date, kurtosis_exuber_200_0_sadf_log)]))
plot(as.xts.data.table(indicators[, .(date, EMA(sd_exuber_200_0_sadf_log, n = 7 * 10))]))
plot(as.xts.data.table(indicators[, .(date, EMA(sd_exuber_200_0_sadf_log, n = 7 * 10))])["2025"])
plot(as.xts.data.table(indicators[, .(date, EMA(sd_exuber_200_0_sadf_log, n = 7 * 10))])["2022"])
plot(as.xts.data.table(indicators[, .(date, EMA(mean_exuber_200_0_sadf_log, n = 7 * 10))])["2025"])
plot(as.xts.data.table(indicators[, .(date, EMA(mean_exuber_200_0_sadf_log, n = 7 * 10))])["2022"])
plot(as.xts.data.table(indicators[, .(date, EMA(median_exuber_200_0_sadf_log, n = 7 * 10))])["2025"])
plot(as.xts.data.table(indicators[, .(date, EMA(sd_exuber_800_0_sadf_log, n = 7 * 10))])["2025"])
plot(as.xts.data.table(indicators[, .(date, EMA(sd_exuber_800_1_sadf_log, n = 7 * 10))])["2025"])

# Merge SPY and indicators
backtest_data = indicators[spy, on = "date"]

# Add trend predictor
backtest_data[, trend := EMA(close, n = 7 * 10) > EMA(close, n = 7 * 10 * 50)]

# Lag all indicators!
# cols_exuber = colnames(backtest_data)[grep("exuber", colnames(backtest_data))]
# backtest_data[, (cols_exuber) := shift(.SD), .SDcols = cols_exuber]

# Remove missing values
backtest_data = na.omit(backtest_data)
setorder(backtest_data, date)


# RESEARCH ----------------------------------------------------------------
# Hour, daily and week Returns across deciles
backtest_data[, let(
  target_hour = shift(close, type = "lead", n = 1L) / close - 1,
  target_daily = shift(close, type = "lead", n = 7L) / close - 1,
  target_week = shift(close, type = "lead", n = 10L * 7L) / close - 1
)]

# Plot returns across deciles
cols_choose = cols_exuber[grepl("800_1", cols_exuber)]
var_ = "sd_exuber_800_1_gsadf_log"
backtest_data[, .(date, target_hour, decile = dplyr::ntile(x, 10)), env = list(x = var_)] |>
  _[, mean(target_hour), by = decile] |>
  ggplot(aes(x = decile, y = V1)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = paste0("Hour returns across deciles of ", var_),
    x = "Decile",
    y = "Mean Hourly Return"
  ) +
  theme_minimal()
backtest_data[, .(date, target_daily, decile = dplyr::ntile(x, 10)), env = list(x = var_)] |>
  _[, mean(target_daily), by = decile] |>
  ggplot(aes(x = decile, y = V1)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = paste0("Hour returns across deciles of ", var_),
    x = "Decile",
    y = "Mean Hourly Return"
  ) +
  theme_minimal()
backtest_data[, .(date, target_week, decile = dplyr::ntile(x, 10)), env = list(x = var_)] |>
  _[, mean(target_week), by = decile] |>
  ggplot(aes(x = decile, y = V1)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = paste0("Hour returns across deciles of ", var_),
    x = "Decile",
    y = "Mean Hourly Return"
  ) +
  theme_minimal()
backtest_data[, .(date, target_week, decile = dplyr::ntile(x, 10)), env = list(x = var_)] |>
  _[, mean(target_week), by = decile] |>
  ggplot(aes(x = decile, y = V1)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = paste0("Hour returns across deciles of ", var_),
    x = "Decile",
    y = "Mean Hourly Return"
  ) +
  theme_minimal()
backtest_data[trend == TRUE, .(date, target_week, decile = dplyr::ntile(x, 10)), env = list(x = var_)] |>
  _[, mean(target_week), by = decile] |>
  ggplot(aes(x = decile, y = V1)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = paste0("Hour returns across deciles of ", var_),
    x = "Decile",
    y = "Mean Hourly Return"
  ) +
  theme_minimal()
backtest_data[trend == FALSE, .(date, target_week, decile = dplyr::ntile(x, 10)), env = list(x = var_)] |>
  _[, mean(target_week), by = decile] |>
  ggplot(aes(x = decile, y = V1)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = paste0("Hour returns across deciles of ", var_),
    x = "Decile",
    y = "Mean Hourly Return"
  ) +
  theme_minimal()

# Simple backtest
dt_ = backtest_data[, .(date, target_hour, x, close), env = list(x = var_)]
dt_[, x := EMA(x, 7*2), env = list(x = var_)]
dt_[, trend := close / shift(close, n = 7 * 5) - 1]
dt_[, let(
  q90 = roll_quantile(x, width = length(x), p = 0.90, min_obs = 100L),
  q80 = roll_quantile(x, width = length(x), p = 0.80, min_obs = 100L),
  q70 = roll_quantile(x, width = length(x), p = 0.70, min_obs = 100L),
  q60 = roll_quantile(x, width = length(x), p = 0.60, min_obs = 100L),
  q50 = roll_quantile(x, width = length(x), p = 0.50, min_obs = 100L),
  q40 = roll_quantile(x, width = length(x), p = 0.40, min_obs = 100L),
  q30 = roll_quantile(x, width = length(x), p = 0.30, min_obs = 100L),
  q20 = roll_quantile(x, width = length(x), p = 0.20, min_obs = 100L),
  q10 = roll_quantile(x, width = length(x), p = 0.10, min_obs = 100L)
), env = list(x = var_)]
max_leverage = 2
# dt_[, signal := fcase(
#   x < q10 & trend > 0, max_leverage,
#   x < q20 & trend > 0, max_leverage * 0.85,
#   x < q30 & trend > 0, max_leverage * 0.7,
#   x < q40 & trend > 0, max_leverage * 0.55,
#   x < q50 & trend > 0, max_leverage * 0.40,
#   x < q60 & trend > 0, max_leverage * 0.25,
#   x < q70 & trend > 0, max_leverage * 0.2,
#   x < q80 & trend > 0, max_leverage * 0.05,
#   x < q90 & trend > 0, max_leverage * 0,
#   x > q90 & trend > 0, max_leverage * 0,
#   default = 0
# ), env = list(x = var_)]
dt_[, signal := fcase(
  x < q10, max_leverage,
  x < q20, max_leverage * 0.85,
  x < q30, max_leverage * 0.7,
  x < q40, max_leverage * 0.55,
  x < q50, max_leverage * 0.40,
  x < q60, max_leverage * 0.25,
  x < q70, max_leverage * 0.2,
  x < q80, max_leverage * 0.05,
  x < q90, max_leverage * 0,
  x > q90, max_leverage * 0,
  default = 0
  ), env = list(x = var_)]
dt_[, signal := shift(signal, type = "lag", n = 1L)]
dt_ = na.omit(dt_)
strategy = as.xts.data.table(dt_[, .(date,
                                     strategy = signal * target_hour,
                                     benchmark = target_hour)])
strategy_daily = apply.daily(strategy[, 1], function(x) {prod(1 + x) - 1})
strategy_daily$benchmark = apply.daily(strategy[, 2], function(x) {prod(1 + x) - 1})
charts.PerformanceSummary(strategy_daily)
SharpeRatio.annualized(strategy_daily)
Return.annualized(strategy_daily)
# charts.PerformanceSummary(strategy_daily["2025"])

# Choose multiple predictors
sd_predictors = colnames(backtest_data)[grepl("sd", colnames(backtest_data))]
dt_ = backtest_data[, .SD, .SD = c("date", "target_hour", "close", sd_predictors)]
dt_[, (sd_predictors) := lapply(.SD, function(x) (EMA(x, 7) + EMA(x, 7*5) + EMA(x, 7*10)) / 3),
    .SDcols = sd_predictors]
qs = dt_[, lapply(.SD, function(x) {
  q90 = roll_quantile(x, width = length(x), p = 0.90, min_obs = 100L)
  q80 = roll_quantile(x, width = length(x), p = 0.80, min_obs = 100L)
  q70 = roll_quantile(x, width = length(x), p = 0.70, min_obs = 100L)
  q60 = roll_quantile(x, width = length(x), p = 0.60, min_obs = 100L)
  q50 = roll_quantile(x, width = length(x), p = 0.50, min_obs = 100L)
  q40 = roll_quantile(x, width = length(x), p = 0.40, min_obs = 100L)
  q30 = roll_quantile(x, width = length(x), p = 0.30, min_obs = 100L)
  q20 = roll_quantile(x, width = length(x), p = 0.20, min_obs = 100L)
  q10 = roll_quantile(x, width = length(x), p = 0.10, min_obs = 100L)
  cbind(q90, q80, q70, q60, q50, q40, q30, q20, q10)
}), .SDcols = sd_predictors]
qs[, 1:6]


