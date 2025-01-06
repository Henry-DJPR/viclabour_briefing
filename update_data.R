# Set options
force_refresh <- interactive()
options(repos = structure(c(CRAN="http://cran.rstudio.com/")))
options(timeout = 120)


# Load in custom functions
lapply(list.files("R", full.names = TRUE), source, echo = F, verbose = F)


# Load packages.
library(lubridate)
library(data.table)
library(jsonlite)
library(xml2)
library(openxlsx)
library(fst)


# Parse table index
indx <- fread("table_index.csv")


# Check latest dates
indx[, latest := check_latest_data(series_id, parse_method)]
indx[, just_updated := is.na(last_updated) | latest > last_updated]


# Pull out unique series to update
to_update <- indx[just_updated == T, .N, .(series_id, parse_method)]


# generate new data if required
if(nrow(to_update) > 0){

  new_data <- to_update[, download_series(series_id, parse_method)]

  stopifnot(all(to_update$series_id %in% unique(new_data$series_id)))
  new_data <- new_data[series_id %in% to_update$series_id]

  indx[, last_updated := latest]
  indx[, latest := NULL]
  update_briefing <- T

  fwrite(
    indx,
    "table_index.csv"
  )

} else {
  new_data <- NULL
  update_briefing <- F
}


# Update tables if required
if(update_briefing | force_refresh){

  message("Updating brefing components")

  # parse jobs data
  jobs_data <- tryCatch(
    read.fst("data/jobs_data.fst", as.data.table = T),
    error = function(e){
      warning("Could not parse jobs_data.fst:\n", e)
      return(NULL)
    }
  )

  # bind new data
  if(!is.null(new_data) && !is.null(jobs_data)){
    jobs_data <- rbind(
      jobs_data[!(series_id %in% unique(new_data$series_id))],
      new_data,
      fill = T
    )
  } else if(is.null(new_data) && !is.null(jobs_data)){
    NULL
  } else if(!is.null(new_data) && is.null(jobs_data)){
    jobs_data <- new_data
  } else {
    stop("Cannot find existing jobs_data and no new_data provided")
  }

  # Index job data
  setkey(jobs_data, series_id, date)

  # Save
  write.fst(jobs_data, "data/jobs_data.fst", compress = 100)

  # Generate tables
  table_list <- split(indx, by = "table_name")
  lapply(
    table_list,
    \(x){
      make_table_latex(
        table_name = x$table_name[1],
        series_ids = x$series_id,
        row_headers = x$name,
        highlight_rows = x$highlight,
        smoothing = x$smoothing_months,
        up_is_good = x$up_is_good,
        notes = x$caption
      )
    }
  )

}
