library(data.table)
library(lubridate)
library(finutils)
library(ggplot2)
library(fExtremes)


# SETUP -------------------------------------------------------------------
# Globals
if (interactive()) {
  PATH = "D:/strategies/exuber"
}

# Create indicators directory if it doesn't exist
if (interactive()) {
  if (!dir.exists(file.path(PATH, "indicators"))) {
    dir.create(file.path(PATH, "indicators"))
  }
}



# UNIVERSE ----------------------------------------------------------------
# Import daily data
list.files("F:/lean/data")
prices = qc_daily_parquet(
  file_path = "F:/lean/data/all_stocks_daily",
  min_obs = 252,
  duplicates = "fast"
)
prices[, dollar_volume := close_raw * volume]
prices[, month := data.table::yearmon(date)]
pricesm = prices[, .(
  dollar_volume = sum(dollar_volume, na.rm = TRUE)
), by = c("symbol", "month")]
setorder(pricesm, month, -dollar_volume)
symbols_dv_100  = pricesm[, head(.SD, 100), by = month]
symbols_dv_500  = pricesm[, head(.SD, 500), by = month]
symbols_dv_1000 = pricesm[, head(.SD, 1000), by = month]

# Check
symbols_dv_100[, .N, by = month][, unique(N) == 100]
symbols_dv_500[, .N, by = month][, unique(N) == 500]
symbols_dv_1000[, .N, by = month][, unique(N) == 1000]


# DATA --------------------------------------------------------------------
# Check columns
# colnames(fread(list.files(file.path(PATH, "predictors"), full.names = TRUE)[1]))

# Current column
# if (interactive()) {
#   i = 1L
# } else {
#   i = as.integer(Sys.getenv('PBS_ARRAY_INDEX'))
# }
# i = i + 2L
# cols = c(1, 2, i)

# Get files
if (interactive()) {
  files = list.files(file.path(PATH, "predictors"), full.names = TRUE)
} else {
  files = list.files("predictors", full.names = TRUE)
}

# Keep only liquid symbols
files_liquid = data.table(files, ticker = gsub("\\.csv", "", basename(files)))
files_liquid[ticker %in% symbols_dv_100[, unique(symbol)], univ_100 := TRUE]
files_liquid[ticker %in% symbols_dv_500[, unique(symbol)], univ_500 := TRUE]
files_liquid[ticker %in% symbols_dv_1000[, unique(symbol)], univ_1000 := TRUE]

# Foor loop over uni files
uni_names = c("univ_100", "univ_500", "univ_1000")
for (uni in uni_names) {
  # uni = "univ_100"
  print(uni)

  # Keep
  files_ = files_liquid[get(uni) == TRUE, files]

  # Import predictors in chunks Predictors
  system.time({
    dt = lapply(files_, fread) # , select = cols
  })
  print(format(object.size(dt), units = "auto"))
  dt = rbindlist(dt, fill= TRUE)
  dt[, date := with_tz(date, tzone = "America/New_York")]
  setorder(dt, symbol, date)

  # Keep only n stocks by month
  # 490 * 6 * 21
  dt[, month := data.table::yearmon(date)]
  dt_ = dt[symbols_dv_500[, .(symbol, month)], on = .(symbol, month)]
  dt_ = na.omit(dt_)
  dt_[, .N, by = month] |>
    ggplot(aes(x = month, y = N)) +
    geom_line() +
    labs(title = paste0("Number of stocks in ", uni, " universe")) +
    theme_minimal()
  dt_[month %between% c(1998.5, 2025.2)][, .N, by = month] |>
    ggplot(aes(x = month, y = N)) +
    geom_line() +
    labs(title = paste0("Number of stocks in ", uni, " universe")) +
    theme_minimal()
  dt_ = dt_[month %between% c(1998.5, 2025.2)]

  # Median aggreagation
  radf_vars = colnames(dt_)[grepl("exuber", colnames(dt_))]
  indicators_median = dt_[, lapply(.SD, median, na.rm = TRUE), by = c('date'), .SDcols = radf_vars]
  setnames(indicators_median, radf_vars, paste0("median_", radf_vars))

  # Standard deviation aggregation
  indicators_sd = dt_[, lapply(.SD, sd, na.rm = TRUE), by = c('date'), .SDcols = radf_vars]
  setnames(indicators_sd, radf_vars, paste0("sd_", radf_vars))

  # Mean aggregation
  indicators_mean = dt_[, lapply(.SD, mean, na.rm = TRUE), by = c('date'), .SDcols = radf_vars]
  setnames(indicators_mean, radf_vars, paste0("mean_", radf_vars))

  # Sum aggreagation
  indicators_sum = dt_[, lapply(.SD, sum, na.rm = TRUE), by = c('date'), .SDcols = radf_vars]
  setnames(indicators_sum, radf_vars, paste0("sum_", radf_vars))

  # Value at risk
  indicators_var_05 = dt_[, lapply(.SD, function(x) fExtremes::VaR(x)), by = c('date'), .SDcols = radf_vars]
  setnames(indicators_var_05, radf_vars, paste0("var05_", radf_vars))
  indicators_var_01 = dt_[, lapply(.SD, function(x) fExtremes::VaR(x, alpha = 0.01)), by = c('date'), .SDcols = radf_vars]
  setnames(indicators_var_01, radf_vars, paste0("var01_", radf_vars))

  # CVAR
  indicators_cvar_05 = dt_[, lapply(.SD, function(x) fExtremes::CVaR(x)), by = c('date'), .SDcols = radf_vars]
  setnames(indicators_cvar_05, radf_vars, paste0("cvar05_", radf_vars))
  indicators_cvar_01 = dt_[, lapply(.SD, function(x) fExtremes::CVaR(x, alpha = 0.01)), by = c('date'), .SDcols = radf_vars]
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

  # Save indicators
  fwrite(indicators, file.path(PATH, "indicators", paste0(uni, ".csv")))
}

# # Import predictors in chunks Predictors
# system.time({
#   dt = lapply(files, fread) # , select = cols
# })
# format(object.size(dt), units = "auto")
# dt = rbindlist(dt, fill= TRUE)
# dt[, date := with_tz(date, tzone = "America/New_York")]
# setorder(dt, symbol, date)
#
# # Median aggreagation
# radf_vars = colnames(dt)[grepl("exuber", colnames(dt))]
# indicators_median = dt[, lapply(.SD, median, na.rm = TRUE), by = c('date'), .SDcols = radf_vars]
# setnames(indicators_median, radf_vars, paste0("median_", radf_vars))
#
# # Standard deviation aggregation
# indicators_sd = dt[, lapply(.SD, sd, na.rm = TRUE), by = c('date'), .SDcols = radf_vars]
# setnames(indicators_sd, radf_vars, paste0("sd_", radf_vars))
#
# # Mean aggregation
# indicators_mean = dt[, lapply(.SD, mean, na.rm = TRUE), by = c('date'), .SDcols = radf_vars]
# setnames(indicators_mean, radf_vars, paste0("mean_", radf_vars))
#
# # Sum aggreagation
# indicators_sum = dt[, lapply(.SD, sum, na.rm = TRUE), by = c('date'), .SDcols = radf_vars]
# setnames(indicators_sum, radf_vars, paste0("sum_", radf_vars))
#
# # Expected Shartfall
# indicators_es = dt[, lapply(.SD, function(x) {
#   if (length(x) < 2) return(NA_real_)
#   PerformanceAnalytics::ES(diff(x), p = 0.05, method = "modified")
# }), by = date, .SDcols = radf_vars]
#
# # Merge indicators
# indicators = Reduce(
#   function(x, y)
#     merge(
#       x,
#       y,
#       by = "date",
#       all.x = TRUE,
#       all.y = TRUE
#     ),
#   list(
#     indicators_sd,
#     indicators_median,
#     indicators_sum,
#     indicators_mean
#   )
# )
# setorder(indicators, date)
#
# # Remove missing values
# indicators = na.omit(indicators)
#
# # Save indicators
# fwrite(indicators, "indicators.csv")
