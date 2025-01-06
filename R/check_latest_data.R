check_latest_data <- function(series_ids, parse_method){

  # Adjust for parse_methods that produce more than on data series
  # Will need to ammend as data sources grow
  series_ids[parse_method == "abs_lfs_youth"] <- "abs_lfs_youth"
  series_ids[parse_method == "abs_lfs_youth_region"] <- "abs_lfs_youth_region"

  # get reference time series IDs as not to make hundreds of API calls
  reference <- read.fst(
    "metadata/reference_abs_timeseries.fst",
    as.data.table = T
  )

  # Generate lookup table
  lookup <- data.table(
    series_id = series_ids,
    parse_method = parse_method,
    ref_series = reference$ref_series[match(series_ids, reference$series_id)]
  )

  # If a series has reference series but is an ABS time series, use series as is
  lookup[parse_method == "abs_ts" & is.na(ref_series), ref_series := series_id]

  # Generate latest dates
  lookup[!is.na(ref_series), latest := latest_abs_ts(ref_series[1]), ref_series]

  # Check time series directory & return
  return(lookup$latest)
}
