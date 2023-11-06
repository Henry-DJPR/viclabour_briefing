# Generates a table of reference time series ids. 
# This table shortcuts the need to check all time series individually
# Also generates a table of abs timeseries workbook urls and the series they 
# contain to shortcut redownloading from the timeseries directory

# Ensure this code is being run in the correct directory (not root)
if(basename(getwd()) != "R") setwd(normalizePath("./R"))


# Set options 
options(repos = structure(c(CRAN="http://cran.rstudio.com/")))
options(timeout = 120)


# Load in custom functions (occurs after package management as some objects do 
# not lazy load packages)
lapply(list.files("R", full.names = TRUE), source)


# Load packages. Only packages with minimal dependencies. Tidyverse banned.
p_load(
  data.table,
  xml2,
  fst
)


# Manual series reference for LFS cubes
abs_lfs_cube_ref_series_id <- c(
  EM2a = "A84426256L",
  EM2b = "A84426256L",
  GM1 = "A84426256L",
  LM1 = "A83901539T",
  LM3 = "A83901539T",
  LM3a = "A83901539T",
  LM4 = "A83901539T",
  LM5 = "A83901539T",
  LM7 = "A83901539T",
  LM9 = "A83901539T",
  LQ1 = "A85093966K",
  LQ2 = "A85093966K",
  EQ03 = "A84601686V",
  EQ06 = "A84601686V",
  EQ07a = "A84601686V",
  EQ07b = "A84601686V",
  EQ08 = "A84601686V",
  EQ09 = "A84601686V",
  EQ10 = "A84601686V",
  EQ11 = "A84601686V",
  EQ12 = "A84601686V",
  EQ13 = "A84601686V",
  EQ14 = "A84601686V",
  EM1a = "A83901539T",
  EM1b = "A83901539T",
  EM3a = "A83901539T",
  EM3b = "A83901539T",
  EM4a = "A83901539T",
  EM4b = "A83901539T",
  EM5a = "A83901539T",
  EM5b = "A83901539T",
  UM2 = "A85145579L",
  UM3 = "A85145579L",
  UQ2a = "A84601686V",
  UQ2b = "A84601686V",
  UQ3a = "A84601686V",
  UQ3b = "A84601686V",
  EM6 = "A83901539T",
  EM6a = "A83901539T",
  EQ04 = "A84601686V",
  EQ05 = "A84601686V",
  RM1 = "A83901539T",
  RM3 = "A83901539T",
  RQ1 = "A84601686V",
  RQ2 = "A84601686V",
  FM1 = "A83901539T",
  FM2 = "A83901539T",
  FM3 = "A83901539T",
  FM4 = "A83901539T",
  EQ02 = "A84601686V",
  NM1 = "A84601686V",
  NM2 = "A84601686V",
  abs_lfs_youth = "A83901539T"
)




# Get all labour force, lfs detailed and vacancy data
lookup <- abs_ts_lookup(cat_no = c("6202.0", "6291.0.55.001", "6354.0"))
setDT(lookup)


# If there are series across catalogues, ensure we're only using the workbooks 
# which update first
lookup[, is_latest := release_date == max(release_date), series_id]
lookup <- lookup[is_latest == T]
lookup[, is_latest := NULL]


# generate reference series
ref_series <- copy(lookup)
ref_series[, ref_series := first(series_id), .(cat_no, release_date, frequency)]
ref_series <- ref_series[, .(series_id, ref_series)]


# add in lfs cubes and part time custom series
ref_series <- rbind(
  ref_series,
  data.table(
    series_id = names(abs_lfs_cube_ref_series_id),
    ref_series = abs_lfs_cube_ref_series_id
  ),
  data.table(
    series_id = "pt_emp_vic",
    ref_series = ref_series[series_id == "A84423357V", ref_series]
  )
)



# save reference series ids
write.fst(ref_series, "metadata/reference_abs_timeseries.fst")


# Generate unique workbook urls with component series ids
url_series <- unique(lookup[, .(url, series_id)])


# save workbook url series table
write.fst(url_series, "metadata/workbook_url_series.fst")
