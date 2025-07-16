library(data.table)
library(lubridate)
library(finutils)
library(ggplot2)
library(fExtremes)
library(TTR)
library(roll)
library(matrixStats)


# SETUP -------------------------------------------------------------------
# Globals
if (interactive()) {
  PATH = "D:/strategies/exuber"
}



# DATA --------------------------------------------------------------------
# Import daily data
list.files("F:/lean/data")
prices = qc_daily_parquet(
  file_path = "F:/lean/data/all_stocks_daily",
  min_obs = 252,
  duplicates = "fast"
)
spy = prices[symbol == "spy"]

# Exuber data
files = list.files("D:/strategies/exuber/predictors_daily_firms", full.names = TRUE)
cols = c("symbol", "date", "exuber_1000_0_adf_log", "exuber_1000_0_sadf_log",
         "exuber_1000_0_gsadf_log", "exuber_1000_0_bsadf_log")
exuber = lapply(files, function(s) fread(s, select = cols))
exuber = rbindlist(exuber, fill = TRUE)
setorder(exuber, symbol, date)
exuber = na.omit(exuber)


# DATA --------------------------------------------------------------------
# Median aggreagation
radf_vars = colnames(exuber)[grepl("exuber", colnames(exuber))]
indicators_median = exuber[, lapply(.SD, median, na.rm = TRUE), by = c('date'), .SDcols = radf_vars]
setnames(indicators_median, radf_vars, paste0("median_", radf_vars))

# Standard deviation aggregation
indicators_sd = exuber[, lapply(.SD, sd, na.rm = TRUE), by = c('date'), .SDcols = radf_vars]
setnames(indicators_sd, radf_vars, paste0("sd_", radf_vars))

# Mean aggregation
indicators_mean = exuber[, lapply(.SD, mean, na.rm = TRUE), by = c('date'), .SDcols = radf_vars]
setnames(indicators_mean, radf_vars, paste0("mean_", radf_vars))

# Sum aggreagation
indicators_sum = exuber[, lapply(.SD, sum, na.rm = TRUE), by = c('date'), .SDcols = radf_vars]
setnames(indicators_sum, radf_vars, paste0("sum_", radf_vars))

# Value at risk
indicators_var_05 = exuber[, lapply(.SD, function(x) fExtremes::VaR(x)), by = c('date'), .SDcols = radf_vars]
setnames(indicators_var_05, radf_vars, paste0("var05_", radf_vars))
indicators_var_01 = exuber[, lapply(.SD, function(x) fExtremes::VaR(x, alpha = 0.01)), by = c('date'), .SDcols = radf_vars]
setnames(indicators_var_01, radf_vars, paste0("var01_", radf_vars))

# CVAR
indicators_cvar_05 = exuber[, lapply(.SD, function(x) fExtremes::CVaR(x)), by = c('date'), .SDcols = radf_vars]
setnames(indicators_cvar_05, radf_vars, paste0("cvar05_", radf_vars))
indicators_cvar_01 = exuber[, lapply(.SD, function(x) fExtremes::CVaR(x, alpha = 0.01)), by = c('date'), .SDcols = radf_vars]
setnames(indicators_cvar_01, radf_vars, paste0("cvar01_", radf_vars))

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
    indicators_mean,
    indicators_var_05,
    indicators_var_01,
    indicators_cvar_05,
    indicators_cvar_01
  )
)
setorder(indicators, date)

# Remove missing values
indicators = na.omit(indicators)

# Merge indicators and prices
backtest_data = indicators[spy, on = "date"]
# backtest_data[, trend := EMA(close, n = 7 * 10) > EMA(close, n = 7 * 10 * 50)]
backtest_data = na.omit(backtest_data)
setorder(backtest_data, date)
backtest_data[, let(
  target_hour = shift(close, type = "lead", n = 1L) / close - 1,
  target_daily = shift(close, type = "lead", n = 7L) / close - 1,
  target_week = shift(close, type = "lead", n = 10L * 7L) / close - 1
)]
backtest_data = na.omit(backtest_data)

# Simple backtest
var_ = "mean_exuber_1000_0_sadf_log"
dt_ = backtest_data[, .(date, target_hour, x, close), env = list(x = var_)]
dt_[, x := EMA(x, 2), env = list(x = var_)]
plot(dt_[, x, env = list(x = var_)])
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
dt_[, signal := fcase(
  x < q10, max_leverage * 1,
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
dt_[, signal := shift(signal, type = "lag", n = 1L)]
dt_[, sum(is.na(signal))]
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


# BUY LOW RISK ------------------------------------------------------------
# Calculate quantiles for all sd predictors that are initialy smoothed by EMA
# file.remove("indicators_q.csv")
file_name = "indicators_daily_q.csv"
if (!file.exists(file_name)) {
  exuber_predictors = colnames(backtest_data)[grepl("1000", colnames(backtest_data))]
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
  qs = fread(file_name)
} else {
  qs = fread(file_name)
}

# Merge quantiles and backtest data
if (nrow(qs) != nrow(backtest_data)) stop("Quantiles and backtest data do not match in number of rows.")
dt = cbind(backtest_data, qs)
colnames(dt)

# Calculate leverage based on quantiles
sd_predictors = colnames(backtest_data)[grepl("median|mean", colnames(backtest_data))]
# sd_predictors = sd_predictors[grepl("sadf", sd_predictors)]
max_leverage = 1
signals_l = list()
for (i in seq_along(sd_predictors)) {
  var_ = sd_predictors[i]
  dt_ = dt[, fcase(
    x < q10, max_leverage,
    x < q20, max_leverage * 0,
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
signals[, signal := matrixStats::rowSums2(as.matrix(.SD), na.rm = TRUE)] # , .SDcols = patterns("1_gsadf")
signals[, signal := signal > 0]
# signals[, signal := signal / 1]

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
# charts.PerformanceSummary(strategy_daily["2020/"])
# SharpeRatio.annualized(strategy_daily["2020/"])

# Save for QC backtest

