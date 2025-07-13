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
backtest_data[, date_ := as.Date(date)]
dtd_1 = backtest_data[, lapply(.SD, sum), by = date_, .SDcols = patterns("sd_|mean_")]
dtd_2 = backtest_data[, data.table::last(.SD, 1), by = date_, .SDcols = !patterns("sd_|mean_")]
dtd = merge(dtd_1, dtd_2, by = "date_", all = TRUE)
setorder(dtd, date_)
dtd[, let(
  target_hour = shift(close, type = "lead", n = 1L) / close - 1,
  target_daily = shift(close, type = "lead", n = 7L) / close - 1,
  target_week = shift(close, type = "lead", n = 10L * 7L) / close - 1
)]
dtd = na.omit(dtd)


# RESEARCH ----------------------------------------------------------------
# visualize
colnames(indicators)

# Plot returns across deciles
cols_choose = colnames(backtest_data)[grepl("800_1", colnames(backtest_data))]
var_ = "mean_0800_1"
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
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, 10, 1))
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
  _[, median(target_week), by = decile] |>
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

# # Same for tlt
# backtest_data[, .(date, target_hour_tlt, decile = dplyr::ntile(x, 10)), env = list(x = var_)] |>
#   _[, mean(target_hour_tlt), by = decile] |>
#   ggplot(aes(x = decile, y = V1)) +
#   geom_col() +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(
#     title = paste0("Hour returns across deciles of ", var_),
#     x = "Decile",
#     y = "Mean Hourly Return"
#   ) +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(1, 10, 1))
# backtest_data[, .(date, target_daily_tlt, decile = dplyr::ntile(x, 10)), env = list(x = var_)] |>
#   _[, mean(target_daily_tlt), by = decile] |>
#   ggplot(aes(x = decile, y = V1)) +
#   geom_col() +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(
#     title = paste0("Hour returns across deciles of ", var_),
#     x = "Decile",
#     y = "Mean Hourly Return"
#   ) +
#   theme_minimal()
# backtest_data[, .(date, target_week_tlt, decile = dplyr::ntile(x, 10)), env = list(x = var_)] |>
#   _[, median(target_week_tlt), by = decile] |>
#   ggplot(aes(x = decile, y = V1)) +
#   geom_col() +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(
#     title = paste0("Hour returns across deciles of ", var_),
#     x = "Decile",
#     y = "Mean Hourly Return"
#   ) +
#   theme_minimal()
# backtest_data[trend == TRUE, .(date, target_week_tlt, decile = dplyr::ntile(x, 10)), env = list(x = var_)] |>
#   _[, mean(target_week_tlt), by = decile] |>
#   ggplot(aes(x = decile, y = V1)) +
#   geom_col() +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(
#     title = paste0("Hour returns across deciles of ", var_),
#     x = "Decile",
#     y = "Mean Hourly Return"
#   ) +
#   theme_minimal()
# backtest_data[trend == FALSE, .(date, target_week_tlt, decile = dplyr::ntile(x, 10)), env = list(x = var_)] |>
#   _[, mean(target_week_tlt), by = decile] |>
#   ggplot(aes(x = decile, y = V1)) +
#   geom_col() +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(
#     title = paste0("Hour returns across deciles of ", var_),
#     x = "Decile",
#     y = "Mean Hourly Return"
#   ) +
#   theme_minimal()

# Simple backtest
dt_ = dtd[, .(date, target_hour, x, close), env = list(x = var_)] # , target_hour_tlt
dt_[, x := EMA(x, 5), env = list(x = var_)]
plot(dt_[, x, env = list(x = var_)])
# dt_[, trend := close / shift(close, n = 7 * 22) - 1]
dt_[, let(
  q90 = roll_quantile(x, width = length(x), p = 0.90, min_obs = 500L),
  q80 = roll_quantile(x, width = length(x), p = 0.80, min_obs = 500L),
  q70 = roll_quantile(x, width = length(x), p = 0.70, min_obs = 500L),
  q60 = roll_quantile(x, width = length(x), p = 0.60, min_obs = 500L),
  q50 = roll_quantile(x, width = length(x), p = 0.50, min_obs = 500L),
  q40 = roll_quantile(x, width = length(x), p = 0.40, min_obs = 500L),
  q30 = roll_quantile(x, width = length(x), p = 0.30, min_obs = 500L),
  q20 = roll_quantile(x, width = length(x), p = 0.20, min_obs = 500L),
  q10 = roll_quantile(x, width = length(x), p = 0.10, min_obs = 500L)
), env = list(x = var_)]
max_leverage = 2
# dt_[, signal := fcase(
#   x < q10 & trend > 0, max_leverage,
#   x < q20 & trend > 0, max_leverage * 1,
#   x < q30 & trend > 0, max_leverage * 1,
#   x < q40 & trend > 0, max_leverage * 1,
#   x < q50 & trend > 0, max_leverage * 0,
#   x < q60 & trend > 0, max_leverage * 0,
#   x < q70 & trend > 0, max_leverage * 0,
#   x < q80 & trend > 0, max_leverage * 0,
#   x < q90 & trend > 0, max_leverage * 0,
#   x > q90 & trend > 0, max_leverage * 0,
#   default = 0
# ), env = list(x = var_)]
dt_[, signal := fcase(
  x < q10, max_leverage * 1,
  # exponential decrease
  x < q20, max_leverage * 1,
  x < q30, max_leverage * 1,
  x < q40, max_leverage * 0,
  x < q50, max_leverage * 0,
  x < q60, max_leverage * 0,
  x < q70, max_leverage * 0,
  x < q80, max_leverage * 0,
  x < q90, max_leverage * 0,
  x > q90, max_leverage * 0,
  default = 0
), env = list(x = var_)]
# dt_[, signal_tlt := fcase(
#   x < q10, max_leverage,
#   x < q20, max_leverage * 1,
#   x < q30, max_leverage * 0,
#   x < q40, max_leverage * 0,
#   x < q50, max_leverage * 0,
#   x < q60, max_leverage * 0,
#   x < q70, max_leverage * 0,
#   x < q80, max_leverage * 0,
#   x < q90, max_leverage * 0,
#   x > q90, max_leverage * 0,
#   default = 0
# ), env = list(x = var_)]
dt_[, signal := shift(signal, type = "lag", n = 1L)]
# dt_[, signal_tlt := shift(signal_tlt, type = "lag", n = 1L)]
dt_[, sum(is.na(signal))]
# dt_[, sum(is.na(signal_tlt))]
dt_ = na.omit(dt_)
strategy = as.xts.data.table(dt_[, .(date,
                                     strategy = (signal * target_hour),
                                     benchmark = target_hour)])
strategy_daily = apply.daily(strategy[, 1], function(x) {prod(1 + x) - 1})
strategy_daily$benchmark = apply.daily(strategy[, 2], function(x) {prod(1 + x) - 1})
charts.PerformanceSummary(strategy_daily)
SharpeRatio.annualized(strategy_daily)
Return.annualized(strategy_daily)
lapply(strategy_daily, function(x) min(Drawdowns(x)))
PerformanceAnalytics::sd.annualized(strategy_daily)
# charts.PerformanceSummary(strategy_daily["2020/"])
# SharpeRatio.annualized(strategy_daily["2020/"])

