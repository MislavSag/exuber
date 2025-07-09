library(data.table)
library(lubridate)
library(arrow)
library(dplyr)
library(finutils)
library(AzureStor)
library(PerformanceAnalytics)
library(RollingWindow)
library(ggplot2)


# Parameters
FREQ = "day" # can be week and month

# Import exuber monthly data
files = list.files("D:/strategies/exuber/predictors", full.names = TRUE)

# Monthly
# exuberm = lapply(files, function(s) {
#   exuberm_ = fread(s)
#   exuberm_[, lapply(.SD, function(x)
#     data.table::last(x)), by = data.table::yearmon(as.Date(date))]
# })
# exuberm = rbindlist(exuberm, fill = TRUE)
# colnames(exuberm)[1] = "month"

# Daily
cols = c("symbol", "date", "exuber_800_0_sadf_log", "exuber_800_0_gsadf_log", "exuber_800_0_bsadf_log")
exuber = lapply(files, function(s) fread(s, select = cols))
exuber = rbindlist(exuber, fill = TRUE)

# Downsample exuber to monthly
if (FREQ == "week") {
  exuber[, freq := ceiling_date(as.Date(date), unit = "week")]
} else if (FREQ == "month") {
  exuber[, freq := data.table::yearmon(as.Date(date))]
} else if (FREQ == "day") {
  exuber[, freq := as.Date(date)]
} else {
  stop("FREQ must be either 'week' or 'month'")
}
exuberm = exuber[, lapply(.SD, function(x) data.table::last(x)),
                 by = .(symbol, freq)]

# Checks
plot(exuber[symbol == "aapl", exuber_800_0_bsadf_log], type = "l")

# Import monthly prices
prices = qc_daily(
  file_path = "F:/lean/data/stocks_daily.csv",
  min_obs = 252,
  duplicates = "fast",
  symbols = c(unique(exuber$symbol), "spy"),
  market_symbol = "spy"
)
if (FREQ == "week") {
  prices[, freq := ceiling_date(as.Date(date), unit = "week")]
} else if (FREQ == "month") {
  prices[, freq := data.table::yearmon(as.Date(date))]
} else if (FREQ == "day") {
  prices[, freq := as.Date(date)]
} else {
  stop("FREQ must be either 'week' or 'month'")
}

# Calculate beta
prices[, beta := RollingBeta(x = returns, y = market_ret, window=252), by = symbol]

# Downsample to lower frequency
pricesm = prices[, .(
  close     = data.table::last(close),
  open      = data.table::first(open),
  close_raw = data.table::last(close_raw),
  volume    = sum(volume),
  beta      = data.table::last(beta)
), by = .(symbol, freq)]

# Add additional predictors
pricesm[, mom_24 := shift(close, n = 1L) / shift(close, n = 24L) - 1, by = symbol]
pricesm[, mom_12 := shift(close, n = 1L) / shift(close, n = 12L) - 1, by = symbol]
pricesm[, mom_6 := shift(close, n = 1L) / shift(close, n = 6L) - 1, by = symbol]
pricesm[, mom := close / shift(close, n = 1L) - 1, by = symbol]

# Merge exuber and prices
dt = merge(
  exuberm,
  pricesm,
  by = c("symbol", "freq"),
  all.x = TRUE,
  all.y = FALSE
)

# Create target var
dt[, target := close / open - 1]
dt[, target := shift(target, type = "lead", n = 1L), by = symbol]
dt = na.omit(dt, cols = "target")


# CROSS SECTION LONG ONLY -------------------------------------------------
# Params
var = "exuber_800_0_bsadf_log"

# Prepare
dt = na.omit(dt)
dt[, var_ntile := dplyr::ntile(get(var), 40), by = freq]

# Returns across ntiles
dt[, mean(target, na.rm = TRUE), by = var_ntile][order(var_ntile)] |>
  ggplot(aes(x = var_ntile, y = V1)) +
  geom_col() +
  labs(title = paste0("Mean Target by ", var, " Ntile"),
       x = "Ntile",
       y = "Mean Target") +
  theme_minimal()
dt[, median(target, na.rm = TRUE), by = var_ntile][order(var_ntile)] |>
  ggplot(aes(x = var_ntile, y = V1)) +
  geom_col() +
  labs(title = paste0("Mean Target by ", var, " Ntile"),
       x = "Ntile",
       y = "Mean Target") +
  theme_minimal()
dt[close_raw %between% c(1, 5), mean(target, na.rm = TRUE), by = var_ntile][order(var_ntile)] |>
  ggplot(aes(x = var_ntile, y = V1)) +
  geom_col() +
  labs(title = paste0("Mean Target by ", var, " Ntile"),
       x = "Ntile",
       y = "Mean Target") +
  theme_minimal()

# Cross section
dt_long = dt[
  # beta %between% c(0, 1) &
  close_raw %between% c(2, 1000) & volume > 100000
][var_ntile %in% 1, .(
  symbol,
  freq,
  target,
  var = get(var)
)]

# dt_long = dt_long[, data.table::last(.SD, 3), by = month] # FOR TEST
dt_long[, weight:= 1 / .N, by = freq]
dt_long[order(freq)][, length(target), by = freq]
cost = 0.000001
portfolio = dt_long[, .(target = sum((target) * weight, na.rm = TRUE) - cost), by = freq]
setorder(portfolio, freq)
hist(portfolio$target, breaks = 50, main = "Target Distribution", xlab = "Target", ylab = "Frequency")
if (FREQ == "week") {
  portfolio_xts = as.xts.data.table(portfolio)
} else if (FREQ == "month") {
  portfolio_xts = as.xts.data.table(portfolio[, .(month = zoo::as.Date.yearmon(month), target)])
} else if (FREQ == "day") {
  portfolio_xts = as.xts.data.table(portfolio[, .(date = as.Date(freq), target)])

} else {
  stop("FREQ must be either 'week' or 'month'")
}
charts.PerformanceSummary(portfolio_xts)
Return.cumulative(portfolio_xts)
Return.annualized(portfolio_xts)
SharpeRatio.annualized(portfolio_xts)
min(Drawdowns(portfolio_xts))
# charts.PerformanceSummary(portfolio_xts["2020/"])
# charts.PerformanceSummary(portfolio_xts["2025/"])
# SharpeRatio.annualized(portfolio_xts["2020/"])
# Return.annualized(portfolio_xts)

# Save to Azure
# bl_endp_key = storage_endpoint(Sys.getenv("BLOB-ENDPOINT"),
#                                Sys.getenv("BLOB-KEY"))
qc_data = dt_long[, .(symbol, date = as.Date(zoo::as.yearmon(month)))]
qc_data[, date := ceiling_date(date, unit = "month")]
setorder(qc_data, date)
qc_data[, symbol:= gsub("\\..*", "", symbol)]
qc_data[, date := as.character(date)]
qc_data = qc_data[, .(symbol = paste0(symbol, collapse = "|")), by = date]
bl_endp_key = storage_endpoint("https://snpmarketdata.blob.core.windows.net",
                               "0M4WRlV0/1b6b3ZpFKJvevg4xbC/gaNBcdtVZW+zOZcRi0ZLfOm1v/j2FZ4v+o8lycJLu1wVE6HT+ASt0DdAPQ==")
cont = storage_container(bl_endp_key, "qc-backtest")
storage_write_csv(qc_data, cont, "exubercs.csv")

# Comapre QC and local
compare_dt = dt_long[, .(symbol, date = as.Date(zoo::as.yearmon(month)), target)]
fwrite(compare_dt, "D:/strategies/exuber/compare_dt.csv")
qc_data[as.Date(date) >= as.Date("2021-01-01") & as.Date(date) <= as.Date("2021-02-01")]
