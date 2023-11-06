latest_abs_ts <- function(series_ids = NULL, cat_nos = NULL){
  
  if(!is.null(series_ids)){
    lookups <- paste0("https://abs.gov.au/servlet/TSSearchServlet?sid=", series_ids)
  } else {
    lookups <- paste0("https://abs.gov.au/servlet/TSSearchServlet?catno=", cat_nos)
  }
  
  out <- sapply(
    lookups,
    \(x){
      doc <- xml2::read_xml(x)
      latest <- xml2::xml_find_first(
        doc, 
        "/TimeSeriesIndex/Series/ProductReleaseDate"
        )
      xml2::xml_text(latest)
    },
    USE.NAMES = F
  )
  as.Date(out, "%d/%m/%Y")
}


read_abs_ts_xlsx <- function(path){
  
  # Get data sheets
  parse_sheets <- openxlsx::getSheetNames(path)
  parse_sheets <- parse_sheets[parse_sheets %in% paste0("Data", 1:20)]
  
  # Extract data from sheets
  out <- lapply(
    parse_sheets,
    \(x){
      # read sheet data
      df <- openxlsx::read.xlsx(path, x, startRow = 10)
      # read sheet metadata
      meta <- openxlsx::read.xlsx(path, x, rows = 1:10, colNames = F)
      # reshape metadata to long format and drop redundant metadata
      meta <- as.data.table(t(meta))
      meta <- meta[2:nrow(meta), c(1:5, 10), with = F]
      setnames(
        meta, 
        c(
          "series", 
          "unit", 
          "series_type", 
          "data_type", 
          "frequency", 
          "series_id"
        )
      )
      # Populate long metadata df with data & dates
      dates <- as.Date(df[[1]], origin = "1899-12-30")
      meta[, .(value = df[[series_id]], date = dates), names(meta)]
    }
  )
  
  rbindlist(out)
}


abs_ts_lookup <- function(
    series_id = NULL,
    cat_no = NULL, 
    progress = T,
    .baseurl = "https://abs.gov.au/servlet/TSSearchServlet?"
){
  
  # Error checking
  stopifnot(is.character(series_id) | is.null(series_id))
  stopifnot(is.character(cat_no) | is.null(cat_no))
  
  # generate query url or run recursion for vector inputs
  if(!is.null(series_id) & length(series_id) < 2){
    lookup <- paste0("sid=", series_id)
  } else if(!is.null(series_id)){
    # recusive option for multiple series ids
    return(
      do.call(
        rbind, 
        lapply(
          series_id, 
          abs_ts_lookup, 
          progress = progress, 
          .baseurl = .baseurl
        )
      )
    )
  } else if(!is.null(cat_no) & length(cat_no) < 2){
    lookup <- paste0("catno=", cat_no)
  } else if(!is.null(cat_no)){
    # recusive option for multiple catalogue numbers
    return(
      do.call(
        rbind, 
        lapply(
          cat_no, 
          \(x) abs_ts_lookup(
            series_id = NULL,
            cat_no = x,
            progress = progress, 
            .baseurl = .baseurl
          )
        )
      )
    )
  }
  
  query_url <- paste0(.baseurl, lookup)
  
  # Download first xml document and define number of remaining pages
  ts_xml1 <- xml2::read_xml(query_url)
  ts_xml_list <- list(ts_xml1)
  pages <- xml2::xml_integer(xml2::xml_find_all(ts_xml1, "/TimeSeriesIndex/NumPages"))
  
  
  # Download remaining xml files if required
  if(length(pages) > 0){
    
    if(progress) {
      message("Downloading ABS time series directory entries")
      pb <- txtProgressBar(min = 1, max = max(pages), initial = 1, style = 3)
    }
    
    for(pg in 2:pages){
      ts_xml_list[[pg]] <- xml2::read_xml(paste0(query_url, "&pg=", pg))
      if(progress) setTxtProgressBar(pb, pg)
    }
    
    if(progress) close(pb)
  }
  
  
  
  # generate data frame
  out_df <- lapply(
    ts_xml_list,
    \(x){
      col_names <- xml2::xml_find_first(x, "/TimeSeriesIndex/Series")
      col_names <- xml2::xml_name(xml2::xml_children(col_names))
      values <- sapply(
        simplify = FALSE, 
        USE.NAMES = TRUE,
        X = col_names, 
        FUN = \(y){
          series_values <- xml2::xml_find_all(
            x, 
            paste0("/TimeSeriesIndex/Series/", y)
          )
          xml2::xml_text(series_values)
        })
      as.data.frame(values)
    }
  )
  
  out_df <- do.call(rbind, out_df)
  
  
  # reclass
  to_date <- c("ProductReleaseDate", "SeriesStart", "SeriesEnd")
  to_integer <- c("TableOrder", "DataType", "CollectionMonth", "NoObs")
  
  lapply(to_date, \(x) out_df[[x]] <- as.Date(out_df[[x]], "%d/%m/%Y"))
  lapply(to_integer, \(x) out_df[[x]] <- as.integer(out_df[[x]]))
  
  
  # Rename 
  rename_vec <- c(
    "ProductNumber" = "cat_no",
    "ProductTitle" = "cat_name",
    "ProductIssue" = "release_name",
    "ProductReleaseDate" = "release_date",
    "ProductURL" = "cat_url",
    "TableURL" = "url",
    "TableTitle" = "table_name",
    "TableOrder" = "table_order",
    "Description" = "series_name",
    "Unit" = "unit",
    "SeriesType" = "series_type",
    "DataType" = "data_type",
    "Frequency" = "frequency",
    "CollectionMonth" = "collection_month",
    "SeriesStart" = "series_start",
    "SeriesEnd" = "series_end",
    "NoObs" = "n",
    "SeriesID" = "series_id"
  )
  
  for(i in seq_len(length(rename_vec))){
    indx <- which(names(out_df) == names(rename_vec)[i])
    if(length(i) > 0) names(out_df)[indx] <- rename_vec[i]
  }
  
  
  # return
  return(out_df)
  
}


read_abs_ts <- function(series_ids = NULL, cat_nos = NULL, ...){
  
  if(!is.null(series_ids)){
    # Get readymade workbook url and series table
    lookup <- read.fst("metadata/workbook_url_series.fst", as.data.table = T)
    lookup <- lookup[series_id %in% series_ids]
    # Find minimal combination of urls to acquire a copy of all series
    urls <- lookup[, minimal_sources(url, series_id)]
  } else if(!is.null(cat_nos)){
    # Generate lookup table from time series directory
    lookup <- abs_ts_lookup(series_id = series_ids, cat_no = cat_nos)
    # define the minimal number of urls needed to get all series
    urls <- minimal_sources(lookup$url, lookup$series_id)
  } else {
    stop("Please provide either series_ids or cat_nos")
  }
  
  # Download files
  dest <- file.path(tempdir(), basename(urls))
  mapply(
    download.file,
    url = urls,
    destfile = dest,
    mode = "wb", 
    MoreArgs = list(...)
  )
  
  # Parse files
  df <- lapply(dest, read_abs_ts_xlsx)
  df <- rbindlist(df)
  
  # remove unwanted series
  if(!is.null(series_ids)) df <- df[series_id %in% series_ids]
  
  # add table no
  df[
    , 
    table_no := lookup$cat_no[match(series_id[1], lookup$series_id)[1]],
    series_id
  ]
  
  # Return
  return(unique(df))
}


# Solves the set cover problem of minimal required tables to get all data series
minimal_sources <- function(source, series){
  lookup <- unique(data.frame(source = source, series = series))
  freq <- sort(table(lookup$source), decreasing = T)
  solution <- numeric()
  for(i in names(freq)){
    s <- names(freq[1])
    if(s %in% lookup$source){
      matched_series <- lookup[lookup$source == s, "series"]
      lookup <- lookup[!(lookup$series %in% matched_series), ]
      freq <- sort(table(lookup$source), decreasing = T)
      solution <- c(solution, s)
    }
    if (nrow(lookup) == 0) break
  }
  return(solution)
}


read_abs_parttime <- function(...){
  # Find data url
  req_series <- c("A84423349V", "A84423357V")
  lookup <- read.fst("metadata/workbook_url_series.fst", as.data.table = T)
  url <- lookup[series_id %in% req_series, minimal_sources(url, series_id)]
  # download and parse then filter for required series
  tmp <- tempfile(fileext = ".xlsx")
  download.file(url, tmp, mode = "wb")
  df <- read_abs_ts_xlsx(tmp)
  df <- df[series_id %in% req_series]
  col_order <- names(df)
  # pivot
  id_cols <- names(df)[!(names(df) %in% c("series", "series_id", "value"))]
  pivot_formula <- as.formula(
    sprintf("%s ~ series_id", paste0(id_cols, collapse = " + "))
  )
  df <- dcast(df, pivot_formula, value.var = "value")
  # difference and add in metadata 
  df[, value := A84423349V - A84423357V]
  df[, series := "> Employed part-time ;  Persons ;"]
  df[, series_id := "pt_emp_vic"]
  df[, (req_series) := NULL]
  setcolorder(df, col_order)
  # return
  return(df)
}