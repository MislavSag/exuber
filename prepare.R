library(data.table)
library(finutils)


# Setup
PATH_PRICES = "D:/strategies/exuber"

# Import daily data and find 1000 most liquid stocks every month
prices = qc_daily(
  file_path = "F:/lean/data/stocks_daily.csv",
  min_obs = 252,
  duplicates = "fast",
  profiles_fmp = TRUE,
  fmp_api_key = Sys.getenv("APIKEY")
)

# Remove ETF's
prices = prices[isEtf == FALSE & isFund == FALSE]

# Downsample to monthly data and keep most liquid symbols by month
prices[, month := data.table::yearmon(date)]
prices[, dollar_volume := volume * close_raw]
pricesm = prices[, .(
  dollar_volume = sum(dollar_volume)
), by = .(symbol, month)]
pricesm[, dv_rank := frankv(dollar_volume, order = -1L, ties.method = "first"), by = month]
pricesm[month == 2025][order(dv_rank)]

# Extract symbols
symbols = pricesm[dv_rank <= 2000, unique(symbol)]
length(symbols)
length(symbols) / prices[, uniqueN(symbol)]

# Remove prices to free memory
rm(list = c("prices", "pricesm"))
gc()

# Import prices
prices = qc_hour(
  file_path = "F:/lean/data/stocks_hour.csv",
  symbols = symbols,
  duplicates = "fast"
)

# Save every symbol separately
prices_dir = file.path(PATH_PRICES, "prices")
if (!dir.exists(prices_dir)) {
  dir.create(prices_dir)
}
for (s in prices[, unique(symbol)]) {
  print(s)
  if (s == "prn") next()
  prices_ = prices[symbol == s]
  if (nrow(prices_) == 0) next
  file_name = file.path(prices_dir, paste0(s, ".csv"))
  fwrite(prices_, file_name)
}

# Create sh file for predictors
cont = sprintf(
  "#!/bin/bash

#PBS -N exuber_predictions
#PBS -l ncpus=1
#PBS -l mem=4GB
#PBS -J 1-%d
#PBS -o logs
#PBS -j oe

cd ${PBS_O_WORKDIR}

apptainer run image.sif padobran_predictors.R",
  length(list.files(prices_dir)))
writeLines(cont, "padobran_predictors.sh")

# Add to padobran
# scp -r /home/sn/data/strategies/pead/prices padobran:/home/jmaric/peadml/prices
