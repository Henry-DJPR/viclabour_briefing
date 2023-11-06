download_series <- function(series_id, parse_method){
  
  stopifnot(
    all(
      parse_method %in% c(
        "abs_ts", 
        "abs_lfs_parttime", 
        "abs_lfs_cube", 
        "abs_lfs_youth",
        "ivi"
        )
      )
    )
  
  out <- split(series_id, parse_method)
  
  if("abs_ts" %in% parse_method){
    out$abs_ts <- read_abs_ts(out$abs_ts)
  }
  
  if("abs_lfs_parttime" %in% parse_method){
    out$abs_lfs_parttime <- read_abs_parttime(out$abs_ts)
  }
  
  if("abs_lfs_youth" %in% parse_method){
    out$abs_lfs_youth <- read_abs_lfs_youth(out$abs_lfs_youth)
  }
  
  if("abs_lfs_cube" %in% parse_method){
    out$abs_lfs_cube <- read_abs_lfs_cube(out$abs_lfs_cube)
  }
  
  if("ivi" %in% parse_method){
    out$ivi <- data.table(series_id = out$ivi, table_no = out$ivi, cat_no = "ivi")
  }
  
  rbindlist(out, fill = T)
  
}
