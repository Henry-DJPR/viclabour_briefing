# Minimal p_load function from the pacman package
p_load <- function(...){
  pkgs <- as.character(match.call(expand.dots = FALSE)[[2]])
  pkgs_avail <- pkgs %in% .packages(all.available = TRUE)
  pkgs_loaded <- pkgs %in% .packages()
  install.packages(pkgs[!pkgs_avail])
  lapply(pkgs[!(pkgs %in% pkgs_loaded)], library, character.only = TRUE)
  return()
}



# Path helpers
path_asset <- function(x) normalizePath(file.path("./assets", x))

path_sparkline <- function(name, x) {
  x <- gsub("_", "", x)
  if(!dir.exists(paste0("./assets/sparklines/", name))){
    dir.create(paste0("./assets/sparklines/", name))
  }
  file.path("./assets/sparklines", name, paste0(x, ".svg"))
}

path_sparkline_pdf <- function(name, x) {
  x <- gsub("_", "", x)
  if(!dir.exists(paste0("./assets/sparklines/", name))){
    dir.create(paste0("./assets/sparklines/", name))
  }
  file.path("./assets/sparklines", name, paste0(x, ".pdf"))
}

path_flextable <- function(name) {
  normalizePath(
    file.path("./assets/flextable", paste0(name, ".rds")),
    mustWork = F
  )
}

path_latex_table <- function(name) {
  normalizePath(
    file.path("./assets/latex_table", paste0(name, ".tex")),
    mustWork = F
  )
}

path_src <- function(x) normalizePath(file.path("./src/lib", x))



# Fill function
fill <- function(v){
  fixer <- function(a, b) if(is.na(b)){c(a, a[length(a)])} else {c(a, b)}
  Reduce(fixer, v)
}


# convert datetime to milliseconds since 1970 (highcharts format)
datetime_to_timestamp <- function(dt) {
  tmstmp <- as.numeric(as.POSIXct(dt))
  tmstmp <- 1000 * tmstmp
  tmstmp
}

# Extract sates from chararacter vector
state_order <- c("Vic", "Aus", "NSW", "QLD", "SA", "WA", "Tas", "NT", "ACT")

recode_states <- function(v){

  states <- c(
    NSW = "New South Wales",
    Vic = "Victoria",
    QLD = "Queensland",
    SA  = "South Australia",
    WA  = "Western Australia",
    Tas = "Tasmania",
    NT  = "Northern Territory",
    ACT = "Australian Capital Territory",
    Aus = "Australia"
  )

  v <- tstrsplit(v, split = ";")
  v <- v[sapply(v, \(x) any(grepl(paste(states, collapse = "|"), x)))]


  if(length(v) == 1){
    v <- v[[1]]
  } else if(length(v) > 1){
    stop("Multiple state name matches")
  } else {
    stop("cannot find state names")
  }


  v <- gsub("[[:punct:]]", "", v)
  v <- gsub("^[[:blank:]]*|[[:blank:]]*$", "", v)

  for(i in names(states)){
    v[v == states[i]] <- i
  }

  v
}



# Prep data as series list
to_series_list <- function(
    df,
    x,
    y,
    group,
    series_type = "line",
    inactive = c()
  ){
  df <- copy(df)
  setDT(df)
  setnames(df, c(group, x, y), c("group", "x", "y"))
  dropnames <- names(df)[!(names(df) %in% c("group", "x", "y"))]
  if(length(dropnames) > 0) df[, (dropnames) := NULL]
  date_cols <- which(
    sapply(df, inherits, what = c("Date", "POSIXt"))
  )
  if(length(date_cols) > 0){
    df[, (date_cols) := lapply(.SD, datetime_to_timestamp), .SDcols = date_cols]
  }
  df <- split(df, by = "group", keep.by = FALSE)

  out <- lapply(
    names(df),
    \(x) {
      if(x %in% inactive){
        list(
          name = x,
          type = series_type,
          data = df[[x]],
          visible = FALSE
        )
      } else {
        list(
          name = x,
          type = series_type,
          data = df[[x]],
          visible = TRUE
        )
      }
    }
  )

  return(out)

}



# date helpers
years_ago <- function(dates, n = 1){
  d <- max(dates)
  n <- n + 1
  seq.Date(d, by = "-1 years", length.out = n)[n]
}

# String split but factor before split for efficiency
tstrsplit_factor <- function(fac, split, ...){
  if(!inherits(fac, "factor")){ fac <- factor(fac)}
  lev <- levels(fac)
  ind <- as.integer(fac)
  split <- data.table::tstrsplit(lev, split = split, ...)
  lapply(split, function(x) x[ind])
}



# SA4 table
sa4_to_gcc_lookup <- data.table::fread(
  colClasses = 'character',
  header = TRUE,
  sep = ',',
  input = '
  sa4_code,           gcc_restofstate,   state
  101,                 "Rest of NSW",   "NSW"
  102,              "Greater Sydney",   "NSW"
  103,                 "Rest of NSW",   "NSW"
  104,                 "Rest of NSW",   "NSW"
  105,                 "Rest of NSW",   "NSW"
  106,                 "Rest of NSW",   "NSW"
  107,                 "Rest of NSW",   "NSW"
  108,                 "Rest of NSW",   "NSW"
  109,                 "Rest of NSW",   "NSW"
  110,                 "Rest of NSW",   "NSW"
  111,                 "Rest of NSW",   "NSW"
  112,                 "Rest of NSW",   "NSW"
  113,                 "Rest of NSW",   "NSW"
  114,                 "Rest of NSW",   "NSW"
  115,              "Greater Sydney",   "NSW"
  116,              "Greater Sydney",   "NSW"
  117,              "Greater Sydney",   "NSW"
  118,              "Greater Sydney",   "NSW"
  119,              "Greater Sydney",   "NSW"
  120,              "Greater Sydney",   "NSW"
  121,              "Greater Sydney",   "NSW"
  122,              "Greater Sydney",   "NSW"
  123,              "Greater Sydney",   "NSW"
  124,              "Greater Sydney",   "NSW"
  125,              "Greater Sydney",   "NSW"
  126,              "Greater Sydney",   "NSW"
  127,              "Greater Sydney",   "NSW"
  128,              "Greater Sydney",   "NSW"
  201,                "Rest of Vic.",   "VIC"
  202,                "Rest of Vic.",   "VIC"
  203,                "Rest of Vic.",   "VIC"
  204,                "Rest of Vic.",   "VIC"
  205,                "Rest of Vic.",   "VIC"
  206,           "Greater Melbourne",   "VIC"
  207,           "Greater Melbourne",   "VIC"
  208,           "Greater Melbourne",   "VIC"
  209,           "Greater Melbourne",   "VIC"
  210,           "Greater Melbourne",   "VIC"
  211,           "Greater Melbourne",   "VIC"
  212,           "Greater Melbourne",   "VIC"
  213,           "Greater Melbourne",   "VIC"
  214,           "Greater Melbourne",   "VIC"
  215,                "Rest of Vic.",   "VIC"
  216,                "Rest of Vic.",   "VIC"
  217,                "Rest of Vic.",   "VIC"
  301,            "Greater Brisbane",   "QLD"
  302,            "Greater Brisbane",   "QLD"
  303,            "Greater Brisbane",   "QLD"
  304,            "Greater Brisbane",   "QLD"
  305,            "Greater Brisbane",   "QLD"
  306,                 "Rest of Qld",   "QLD"
  307,                 "Rest of Qld",   "QLD"
  308,                 "Rest of Qld",   "QLD"
  309,                 "Rest of Qld",   "QLD"
  310,            "Greater Brisbane",   "QLD"
  311,            "Greater Brisbane",   "QLD"
  312,                 "Rest of Qld",   "QLD"
  313,            "Greater Brisbane",   "QLD"
  314,            "Greater Brisbane",   "QLD"
  315,                 "Rest of Qld",   "QLD"
  316,                 "Rest of Qld",   "QLD"
  317,                 "Rest of Qld",   "QLD"
  318,                 "Rest of Qld",   "QLD"
  319,                 "Rest of Qld",   "QLD"
  401,            "Greater Adelaide",   "SA"
  402,            "Greater Adelaide",   "SA"
  403,            "Greater Adelaide",   "SA"
  404,            "Greater Adelaide",   "SA"
  405,                  "Rest of SA",   "SA"
  406,                  "Rest of SA",   "SA"
  407,                  "Rest of SA",   "SA"
  501,                  "Rest of WA",   "WA"
  502,               "Greater Perth",   "WA"
  503,               "Greater Perth",   "WA"
  504,               "Greater Perth",   "WA"
  505,               "Greater Perth",   "WA"
  506,               "Greater Perth",   "WA"
  507,               "Greater Perth",   "WA"
  508,                  "Rest of WA",   "WA"
  509,                  "Rest of WA",   "WA"
  601,              "Greater Hobart",   "TAS"
  602,                "Rest of Tas.",   "TAS"
  603,                "Rest of Tas.",   "TAS"
  604,                "Rest of Tas.",   "TAS"
  701,              "Greater Darwin",   "NT"
  702,                  "Rest of NT",   "NT"
  801, Australian Capital Territory",   "ACT"')
