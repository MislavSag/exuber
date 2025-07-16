library(data.table)
library(finutils)


# Setup
PATH = "D:/strategies/exuber/daily"

# Import daily data and find 1000 most liquid stocks every month
prices = qc_daily_parquet(
  file_path = "F:/lean/data/all_stocks_daily",
  min_obs = 1500, # 1811
  duplicates = "fast"
)

# Checks
prices[, uniqueN(symbol)]

# Downsample to monthly data and keep most liquid symbols by month
prices[, month := data.table::yearmon(date)]
prices[, dollar_volume := volume * close_raw]
pricesm = prices[, .(
  dollar_volume = sum(dollar_volume)
), by = .(symbol, month)]
pricesm[, dv_rank := frankv(dollar_volume, order = -1L, ties.method = "first"), by = month]
pricesm[month == 2025][order(dv_rank)]

# Extract symbols
pricesm[dv_rank <= 3099] |>
  _[, .N, by = .(month)] |>
  _[, all(N == 3099)]
symbols = pricesm[dv_rank <= 3099, unique(symbol)]
length(symbols)
length(symbols) / prices[, uniqueN(symbol)]
prices = prices[.(symbols)]

# Save every symbol separately
if (!dir.exists(PATH)) {
  dir.create(PATH, recursive = TRUE)
}
for (s in prices[, unique(symbol)]) {
  print(s)
  if (s %in% c("prn", "prn.1")) next()
  prices_ = prices[symbol == s]
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

apptainer run image.sif padobran_predictors_daily.R", length(list.files(prices_dir)))
writeLines(cont, "padobran_predictors_daily.sh")

# Add to padobran
# scp -r /home/sn/data/strategies/pead/prices padobran:/home/jmaric/peadml/prices
