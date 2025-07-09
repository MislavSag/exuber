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
# tlt = data[symbol == "tlt"]
# spy = spy[date >= tlt[, min(date)]]

# Second try, tlt and spy together
prices_dt = dcast(data, date ~ symbol, value.var = c("open", "close"))
prices_dt = na.omit(prices_dt)

# Import indicatora
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

# Market cap
# mcap = qc_daily(
#   file_path = "F:/lean/data/stocks_daily.csv",
#   duplicates = "none",
#   symbols = indicators[, unique(symbol)],
#   profiles_fmp = TRUE,
#   fmp_api_key = Sys.getenv("APIKEY")
# )


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


# RESEARCH ----------------------------------------------------------------
# visualize
colnames(indicators)
# plot(as.xts.data.table(indicators[, .(date, sd_exuber_200_0_sadf_log)]))
# plot(as.xts.data.table(indicators[, .(date, median_exuber_200_0_sadf_log)]))
# plot(as.xts.data.table(indicators[, .(date, mean_exuber_200_0_sadf_log)]))
# plot(as.xts.data.table(indicators[, .(date, EMA(sd_exuber_200_0_sadf_log, n = 7 * 10))]))
# plot(as.xts.data.table(indicators[, .(date, EMA(sd_exuber_200_0_sadf_log, n = 7 * 10))])["2025"])
# plot(as.xts.data.table(indicators[, .(date, EMA(sd_exuber_200_0_sadf_log, n = 7 * 10))])["2022"])
# plot(as.xts.data.table(indicators[, .(date, EMA(mean_exuber_200_0_sadf_log, n = 7 * 10))])["2025"])
# plot(as.xts.data.table(indicators[, .(date, EMA(mean_exuber_200_0_sadf_log, n = 7 * 10))])["2022"])
# plot(as.xts.data.table(indicators[, .(date, EMA(median_exuber_200_0_sadf_log, n = 7 * 10))])["2025"])
# plot(as.xts.data.table(indicators[, .(date, EMA(sd_exuber_800_0_sadf_log, n = 7 * 10))])["2025"])
# plot(as.xts.data.table(indicators[, .(date, EMA(sd_exuber_800_1_sadf_log, n = 7 * 10))])["2025"])

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
dt_ = backtest_data[, .(date, target_hour, x, close), env = list(x = var_)] # , target_hour_tlt
dt_[, x := EMA(x, 7*2), env = list(x = var_)]
dt_[, trend := close / shift(close, n = 7 * 22) - 1]
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


# ENSAMBLE ----------------------------------------------------------------
# Calculate quantiles for all sd predictors that are initialy smoothed by EMA
# file.remove("indicators_q.csv")
file_name = "indicators_q.csv"
if (!file.exists(file_name)) {
  exuber_predictors = colnames(backtest_data)[grepl("sd_", colnames(backtest_data))]
  dt_ = backtest_data[, .SD, .SD = c("date", "target_hour", "close", exuber_predictors)]
  dt_[, (exuber_predictors) := lapply(.SD, function(x) (x + EMA(x, 7) + EMA(x, 7*5) + EMA(x, 7*10)) / 4),
      .SDcols = exuber_predictors]
  qs = dt_[, lapply(.SD, function(x) {
    q90 = roll_quantile(x, width = length(x), p = 0.90, min_obs = 500L)
    q80 = roll_quantile(x, width = length(x), p = 0.80, min_obs = 500L)
    q70 = roll_quantile(x, width = length(x), p = 0.70, min_obs = 500L)
    q60 = roll_quantile(x, width = length(x), p = 0.60, min_obs = 500L)
    q50 = roll_quantile(x, width = length(x), p = 0.50, min_obs = 500L)
    q40 = roll_quantile(x, width = length(x), p = 0.40, min_obs = 500L)
    q30 = roll_quantile(x, width = length(x), p = 0.30, min_obs = 500L)
    q20 = roll_quantile(x, width = length(x), p = 0.20, min_obs = 500L)
    q10 = roll_quantile(x, width = length(x), p = 0.10, min_obs = 500L)
    cbind(q90, q80, q70, q60, q50, q40, q30, q20, q10)
  }), .SDcols = exuber_predictors]
  fwrite(qs, file_name)
} else {
  qs = fread(file_name)
}

# Merge quantiles and backtest data
if (nrow(qs) != nrow(backtest_data)) stop("Quantiles and backtest data do not match in number of rows.")
dt = cbind(backtest_data, qs)
colnames(dt)

# Calculate leverage based on quantiles
sd_predictors = colnames(dt)[grepl("mean.*_\\d+$|sd.*_\\d+$", colnames(dt))]
sd_predictors = sd_predictors[grepl("8", sd_predictors)]
max_leverage = 1
signals_l = list()
for (i in seq_along(sd_predictors)) {
  var_ = sd_predictors[i]
  dt_ = dt[, fcase(
    x < q10, max_leverage,
    x < q20, max_leverage * 1,
    x < q30, max_leverage * 0,
    x < q40, max_leverage * 0,
    x < q50, max_leverage * 0,
    x < q60, max_leverage * 0,
    x < q70, max_leverage * 0,
    x < q80, max_leverage * 0,
    x < q90, max_leverage * 0,
    default = 0
  ), env = list(x = var_,
                q10 = paste0(var_, ".q10"),
                q20 = paste0(var_, ".q20"),
                q30 = paste0(var_, ".q30"),
                q40 = paste0(var_, ".q40"),
                q50 = paste0(var_, ".q50"),
                q60 = paste0(var_, ".q60"),
                q70 = paste0(var_, ".q70"),
                q80 = paste0(var_, ".q80"),
                q90 = paste0(var_, ".q90")
  )
  ]
  signals_l[[i]] = dt_
}
names(signals_l) = sd_predictors
vapply(signals_l, length, FUN.VALUE = integer(1L), USE.NAMES = FALSE)
signals = do.call(cbind, signals_l)
signals = as.data.table(signals)

# Create essamble signal
library(matrixStats)
signals[, signal := matrixStats::rowMedians(as.matrix(.SD), na.rm = TRUE)] # , .SDcols = patterns("1_gsadf")

# Merge signals and SPY
if (nrow(qs) != nrow(signals)) stop("Quantiles and backtest data do not match in number of rows.")
dtsd = spy[, .(date, close)][cbind(backtest_data[, .(date)], signals[, .(signal)]), on = "date"]
setorder(dtsd, date)
dtsd[, target := shift(close, type = "lead", n = 1L) / close - 1]
# dtsd = tlt[, .(date, close_tlt = close)][dtsd, on = "date"]
# setorder(dtsd, date)
# dtsd[, target_tlt := shift(close_tlt, type = "lead", n = 1L) / close_tlt - 1]
dtsd = na.omit(dtsd)

# Plot
# plot(as.xts.data.table(dtsd[, .(date, signal)]))
# plot(EMA(as.xts.data.table(dtsd[, .(date, signal)]), n = 7*10))

# Calculate strategy returns
# dtsd[, strategy := (signal * target) + ((1-signal) * target_tlt)]
dtsd[, strategy := (signal * target)]

# Calculate benchmark returns
strategy = as.xts.data.table(dtsd[, .(date, strategy, benchmark = target)])
strategy_daily = apply.daily(strategy[, 1], function(x) {prod(1 + x) - 1})
strategy_daily$benchmark = apply.daily(strategy[, 2], function(x) {prod(1 + x) - 1})
charts.PerformanceSummary(strategy_daily)
SharpeRatio.annualized(strategy_daily)
Return.annualized(strategy_daily)
lapply(Drawdowns(strategy_daily), min)
charts.PerformanceSummary(strategy_daily["2025"])



# BUY LOW RISK ------------------------------------------------------------
# Calculate quantiles for all sd predictors that are initialy smoothed by EMA
# file.remove("indicators_q.csv")
file_name = "indicators_q.csv"
if (!file.exists(file_name)) {
  exuber_predictors = colnames(backtest_data)[grepl("800", colnames(backtest_data))]
  dt_ = backtest_data[, .SD, .SD = c("date", "target_hour", "close", exuber_predictors)]
  dt_[, (exuber_predictors) := lapply(.SD, function(x) (x + EMA(x, 7) + EMA(x, 7*5) + EMA(x, 7*10)) / 4),
      .SDcols = exuber_predictors]
  qs = dt_[, lapply(.SD, function(x) {
    q90 = roll_quantile(x, width = length(x), p = 0.90, min_obs = 500L)
    q80 = roll_quantile(x, width = length(x), p = 0.80, min_obs = 500L)
    q70 = roll_quantile(x, width = length(x), p = 0.70, min_obs = 500L)
    q60 = roll_quantile(x, width = length(x), p = 0.60, min_obs = 500L)
    q50 = roll_quantile(x, width = length(x), p = 0.50, min_obs = 500L)
    q40 = roll_quantile(x, width = length(x), p = 0.40, min_obs = 500L)
    q30 = roll_quantile(x, width = length(x), p = 0.30, min_obs = 500L)
    q20 = roll_quantile(x, width = length(x), p = 0.20, min_obs = 500L)
    q10 = roll_quantile(x, width = length(x), p = 0.10, min_obs = 500L)
    cbind(q90, q80, q70, q60, q50, q40, q30, q20, q10)
  }), .SDcols = exuber_predictors]
  fwrite(qs, file_name)
} else {
  qs = fread(file_name)
}

# Merge quantiles and backtest data
if (nrow(qs) != nrow(backtest_data)) stop("Quantiles and backtest data do not match in number of rows.")
dt = cbind(backtest_data, qs)
colnames(dt)

# Calculate leverage based on quantiles
sd_predictors = colnames(backtest_data)[grepl("mean_0800", colnames(backtest_data))]
max_leverage = 1
signals_l = list()
for (i in seq_along(sd_predictors)) {
  var_ = sd_predictors[i]
  dt_ = dt[, fcase(
    x < q10, max_leverage,
    x < q20, max_leverage * 1,
    x < q30, max_leverage * 0,
    x < q40, max_leverage * 0,
    x < q50, max_leverage * 0,
    x < q60, max_leverage * 0,
    x < q70, max_leverage * 0,
    x < q80, max_leverage * 0,
    x < q90, max_leverage * 0,
    default = 0
  ), env = list(x = var_,
                q10 = paste0(var_, ".q10"),
                q20 = paste0(var_, ".q20"),
                q30 = paste0(var_, ".q30"),
                q40 = paste0(var_, ".q40"),
                q50 = paste0(var_, ".q50"),
                q60 = paste0(var_, ".q60"),
                q70 = paste0(var_, ".q70"),
                q80 = paste0(var_, ".q80"),
                q90 = paste0(var_, ".q90")
  )
  ]
  signals_l[[i]] = dt_
}
names(signals_l) = sd_predictors
vapply(signals_l, length, FUN.VALUE = integer(1L), USE.NAMES = FALSE)
signals = do.call(cbind, signals_l)
signals = as.data.table(signals)

# Create essamble signal
library(matrixStats)
signals[, signal := matrixStats::rowSums2(as.matrix(.SD), na.rm = TRUE)] # , .SDcols = patterns("1_gsadf")
signals[, signal := signal > 0]

# Merge signals and SPY
if (nrow(qs) != nrow(signals)) stop("Quantiles and backtest data do not match in number of rows.")
dtsd = spy[, .(date, close)][cbind(backtest_data[, .(date)], signals[, .(signal)]), on = "date"]
setorder(dtsd, date)
dtsd[, target := shift(close, type = "lead", n = 1L) / close - 1]
nrow(dtsd)
# dtsd = tlt[, .(date, close_tlt = close)][dtsd, on = "date"]
setorder(dtsd, date)
# dtsd[, target_tlt := shift(close_tlt, type = "lead", n = 1L) / close_tlt - 1]
dtsd = na.omit(dtsd)

# Calculate strategy returns
# dtsd[, strategy := (signal * target) + ((1-signal) * target_tlt)]
dtsd[, strategy := (signal * target)]

# Calculate benchmark returns
strategy = as.xts.data.table(dtsd[, .(date, strategy, benchmark = target)])
strategy_daily = apply.daily(strategy[, 1], function(x) {prod(1 + x) - 1})
strategy_daily$benchmark = apply.daily(strategy[, 2], function(x) {prod(1 + x) - 1})
charts.PerformanceSummary(strategy_daily)
SharpeRatio.annualized(strategy_daily)
Return.annualized(strategy_daily)
lapply(Drawdowns(strategy_daily), min)
charts.PerformanceSummary(strategy_daily["2025"])
