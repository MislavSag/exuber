library(data.table)
library(finutils)


# Setup
PATH = "D:/strategies/exuber/daily"

# Import daily data and find 1000 most liquid stocks every month
prices = qc_daily_parquet(
  file_path = "F:/lean/data/all_stocks_daily",
  min_obs = 1500, # 1811
  duplicates = "fast",
  profiles_fmp = TRUE,
  fmp_api_key = Sys.getenv("APIKEY")
)

# Remove ETF's
dt = prices[isEtf == FALSE & isFund == FALSE]
# prices[is.na(isEtf) | is.na(isFund)][, uniqueN(symbol)]
# prices[is.na(isEtf) | is.na(isFund)][close_raw > 1][, uniqueN(symbol)]

# Checks
prices[, uniqueN(symbol)]
dt[, uniqueN(symbol)]

# Downsample to monthly data and keep most liquid symbols by month
dt[, month := data.table::yearmon(date)]
dt[, dollar_volume := volume * close_raw]
dtm = dt[, .(
  dollar_volume = sum(dollar_volume)
), by = .(symbol, month)]
dtm[, dv_rank := frankv(dollar_volume, order = -1L, ties.method = "first"), by = month]
dtm[month == 2025][order(dv_rank)]

# Extract symbols
# dtm[dv_rank <= 3099] |>
#   _[, .N, by = .(month)] |>
#   _[, all(N == 3099)]
# symbols = dtm[dv_rank <= 3099, unique(symbol)]
symbols = dtm[, unique(symbol)]
length(symbols)
length(symbols) / prices[, uniqueN(symbol)]
setkey(dt, symbol)
dt = dt[.(symbols)]

# Save every symbol separately
if (!dir.exists(PATH)) {
  dir.create(PATH, recursive = TRUE)
}
for (s in dt[, unique(symbol)]) {
  print(s)
  if (s %in% c("prn", "prn.1")) next()
  prices_ = dt[symbol == s]
  if (nrow(prices_) == 0) next
  file_name = file.path(PATH, paste0(s, ".csv"))
  fwrite(prices_, file_name)
}

# Create sh file for predictors
cont = sprintf(
"#!/bin/bash

#PBS -N exuber_predictions
#PBS -l ncpus=1
#PBS -l mem=2GB
#PBS -J 1-%d
#PBS -o logs
#PBS -j oe

cd ${PBS_O_WORKDIR}

apptainer run image.sif padobran_predictors_daily.R", length(list.files(PATH)))
writeLines(cont, "padobran_predictors_daily.sh")

# Add to padobran
# scp -r /home/sn/data/strategies/pead/prices padobran:/home/jmaric/peadml/prices
